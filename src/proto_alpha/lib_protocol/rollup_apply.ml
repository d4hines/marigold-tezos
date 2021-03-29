(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Gabriel Alfour <gabriel.alfour@gmail.com>              *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Alpha_context
open Apply_results
include Rollup_apply_errors
open Rollup

let (let*) = (>>=?)
let (let*?) = (>>?)

type deposit = {
  source : Contract.t ;
  amount : Z.t ;
}

type on_chain_operation =
  | Deposit of deposit


module NS = New_storage
module M = NS.Patricia

let max_stream_node_size = 1000000
let max_leaf_size = max_stream_node_size - 100
let tezos_level_finality = Int32.of_int 15
let timestamp_finality = Int64.of_int (15 * 60)

module Stateful_patricia() = struct
  let t = ref M.empty
  include struct
    module Pure = NS.Patricia
    open Pure

    type nonrec t = t
    let empty = empty
    let get_full () = !t
    let set_full x = t := x
    let get k = get !t k
    let mem k = mem !t k
    let set k v =
      if Compare.Int.(Bytes.length v > max_leaf_size)
      then raise (Failure ("max_leaf_size exceeded")) (* TODO: add exception *)
      else t := set !t k v
  end
  let get_hash () = M.get_hash !t

end

module Stateful_produce_patricia() = struct
  let t = ref NS.Patricia_produce_stream.empty
  let s = ref []
  include struct
    include NS.Patricia_produce_stream
    let get_hash () = get_hash !t
    let get_full () = !t
    let set_full x = t := x ; s := []
    let get k =
      let (v , (t' , s')) = get (!t , !s) k in
      t := t' ;
      s := s' ;
      v
    let mem k =
      let (b , (t' , s')) = mem (!t , !s) k in
      t := t' ;
      s := s' ;
      b
    let set k v =
      let (t' , s') = set (!t , !s) k v in
      t := t' ;
      s := s' ;
      ()
  end
end

module Stateful_consume_patricia() = struct
  let t = ref NS.Patricia_consume_stream.empty
  let s = ref []
  include struct
    include NS.Patricia_consume_stream
    let get_hash () = get_hash !t
    let get_full () = !t
    let set_full x = t := x
    let get k =
      let (v , (t' , s')) = get (!t , !s) k in
      t := t' ;
      s := s' ;
      v
    let mem k =
      let (b , (t' , s')) = mem (!t , !s) k in
      t := t' ;
      s := s' ;
      b
    let set k v =
      let (t' , s') = set (!t , !s) k v in
      t := t' ;
      s := s' ;
      ()
  end

end


(* Common batch abstraction *)
module Batcher = struct

  type signer = Signature.public_key
  type hash = Hash of bytes

  exception Invalid_signature
  exception Double_count
  
  module type PARAMETER = sig
    type t
    val encoding : t Data_encoding.t
    module Transition : functor (M : ROLLUP_STORAGE) -> sig
      val init : unit -> unit
      val main : source:signer -> parameter:t -> unit
    end
  end

  module Make(P : PARAMETER) = struct

    module Replay_counter = struct
      type t = Z.t
      let to_key : signer -> NS.key = fun signer ->
        let bytes = Data_encoding.Binary.to_bytes_exn Signature.public_key_encoding signer in
        NS.key_of_bytes bytes

      let of_value : NS.value -> t = fun raw ->
        match Data_encoding.(Binary.of_bytes z raw) with
        | Some x -> x
        | None -> assert false
      let to_value : t -> NS.value = fun raw ->
        match Data_encoding.(Binary.to_bytes z raw) with
        | Some x -> x
        | None -> assert false
    end

    module Operation = struct
      type t = {
        counter : Replay_counter.t ;
        content : P.t ;
      }

      let encoding : t Data_encoding.t = Data_encoding.(
        conv
          (fun { counter ; content } -> (counter , content))
          (fun (counter , content) -> { counter ; content })
        @@ tup2 z P.encoding
      )

      let to_bytes : t -> bytes = Data_encoding.Binary.to_bytes_exn encoding
    end
    
    type t = {
      content : (signer * Operation.t) list ;
      aggregated_signature : Signature.signature ;
    }

    let encoding : t Data_encoding.t =
      Data_encoding.(
        conv
          (fun { content = c ; aggregated_signature = a } -> (c , a))
          (fun (c , a) -> { content = c ; aggregated_signature = a })
        @@ tup2
          (list (tup2 Signature.public_key_encoding Operation.encoding))
          Signature.signature_encoding
      )

    module Transition = functor (M : ROLLUP_STORAGE) -> struct

      module M = M
      
      let with_save f =
        let save = M.get_full () in
        try f () with
        | e -> (
            M.set_full save ;
            raise e
          )
      
      module Aux = P.Transition(M)

      let single_operation : (signer * Operation.t) -> unit = fun (signer , op) ->
        let Operation.{ counter ; content } = op in

        (* Check Replay_counter *)
        let key = Replay_counter.to_key signer in
        let stored_counter =
          if M.mem key
          then Replay_counter.of_value (M.get key)
          else Z.zero
        in
        if Compare.Z.(counter <= stored_counter) then raise Double_count ;
        M.set key (Replay_counter.to_value counter) ;
        
        (* Perform Operation *)
        Aux.main ~source:signer ~parameter:content
        
      
      let main : t -> unit = fun t ->
        with_save @@ fun () ->
        let { content ; aggregated_signature } = t in

        (* Check Signatures *)
        let open Rollup.Signature in
        let identified_hashes =
          let aux : (signer * Operation.t) -> (public_key * hash) =
            fun (signer , op) -> (signer , do_hash (Message (Operation.to_bytes op)))
          in
          List.map aux content
        in
        if (not @@ Rollup.Signature.check_signed_hashes { identified_hashes ; aggregated_signature })
        then raise Invalid_signature ;
        
        (* Perform Operations *)
        List.iter single_operation content

      let init () = Aux.init ()
      
    end

    module MakeRegular() = struct
      module S = Stateful_patricia()
      module T = Transition(S : ROLLUP_STORAGE)

      let empty : M.t =
        S.set_full S.empty ;
        T.init () ;
        S.get_full ()

      let transition : M.t -> t -> M.t = fun s t ->
        S.set_full s ;
        T.main t ;
        S.get_full ()

      let single_transition : M.t  -> (signer * Operation.t) -> M.t = fun s (signer , op) ->
        S.set_full s ;
        T.single_operation (signer , op) ;
        S.get_full ()
        
      let get_root : M.t -> _ = fun s ->
        S.set_full s ;
        Root (S.get_hash ())

    end
    
    module MakeReject() = struct
      module S = Stateful_produce_patricia()
      module T = Transition(S : ROLLUP_STORAGE)
      
      let transition : M.t -> t -> state_trace = fun s t ->
        let s' = S.of_patricia s in
        S.set_full s' ;
        S.s := [] ;
        (* TODO: Wrap this in Make_reject exception *)
        (try T.main t
        with _ -> ()) ;
        !S.s
    end

    module MakeReplay() = struct
      module S = Stateful_consume_patricia()
      module T = Transition(S : ROLLUP_STORAGE)
      
      let transition : hash:bytes -> t -> state_trace -> hash = fun ~hash t trace ->
        let s' = S.empty_hash hash in
        S.set_full s' ;
        S.s := List.rev trace ;
        T.main t ;
        Hash S.(get_hash ())
    end

  end

end

module Counter_rollup = struct

  module Internal = struct
    type t =
      | Add of Z.t

    let encoding : t Data_encoding.t =
      Data_encoding.(
        conv
          (function Add x -> x) (fun x -> Add x)
        @@ z
      )

    let z_encode = fun z -> Bytes.of_string (Z.to_string z)
    let z_decode = fun bytes -> Z.of_string (Bytes.to_string bytes)

    let single_key = NS.key_of_bytes Bytes.empty

    module MakeView = functor(M : ROLLUP_STORAGE) -> struct
      let get () = z_decode @@ M.get single_key
      let set n = M.set single_key @@ z_encode n
    end

    module MakePureView = functor(M : ROLLUP_STORAGE) -> struct
      module Stateful = MakeView(M)
      let get t =
        M.set_full t ;
        Stateful.get ()
    end

    module Transition = functor(M : ROLLUP_STORAGE) -> struct
      include MakeView(M)

      let init () : unit =
        M.set_full M.empty ;
        M.set single_key (z_encode Z.zero)
      
      let main = fun ~(source:Batcher.signer) ~(parameter:t) ->
        ignore source ;
        match parameter with
        | Add z -> (
            let counter = get () in
            set (Z.add counter z)
          )
    end
  end

  module Main = Batcher.Make(Internal)
  
  type parameter = Main.t
  let parameter_encoding = Main.encoding
    
end
  
(* let kind_to_rollup : rollup_kind -> (module ROLLUP) = function
 *   | Counter -> (module Counter_rollup) *)

let main (ctxt : Alpha_context.t) ~(source : Contract.t) (content : Rollup.operation_content) =
  let dummy_result : Rollup.dummy_result =
    {
      consumed_gas = Gas.Arith.zero;
      allocated_storage = Z.zero;
      originated_contracts = [];
    }
  in
  match content with
  | Create_rollup { operator ; kind } -> (
    let* ({ id } , ctxt) = Rollup.create_rollup ~operator ~kind ctxt in
    let result : Rollup.rollup_creation_result =
      {
        rollup_id = id;
        consumed_gas = Gas.Arith.zero; (* TODO *)
        allocated_storage = Z.zero; (* TODO *)
        originated_contracts = [];
      }
    in
    return (ctxt, Rollup_result (Rollup_creation_result result), [])
  )
  | Commit_block block_commitment -> (
      let* ({ level } , ctxt) = Rollup.commit_block ~operator:source block_commitment ctxt in
    let result : Rollup.block_commitment_result =
      {
        commitment = block_commitment;
        consumed_gas = Gas.Arith.zero; (* TODO *)
        allocated_storage = Z.zero; (* TODO *)
        originated_contracts = [];
        level ;
      }
    in
      return
        (ctxt, Rollup_result (Block_commitment_result result), [])
    )
  | Reject_block block_rejection -> (
      let Block_rejection.{ rollup_id ; level ; rejection_content } = block_rejection in
      let* block = get_block ctxt rollup_id level in
      let Block_onchain_content.{ micro_blocks ; tezos_level ; timestamp } = block in
      (* Check if rejection is too old *)
      let* () =
        let current_tezos_level = Raw_context.current_level ctxt in
        let current_timestamp = Raw_context.current_timestamp ctxt in
        let tezos_level_diff = Level_repr.diff current_tezos_level tezos_level in
        let timestamp_diff = Time.diff current_timestamp timestamp in
        let* () =
          if Compare.Int32.(tezos_level_diff > tezos_level_finality)
          then fail @@ Rollup_rejection_too_old_level {
              rollup_block_tezos_level = tezos_level ;
              current_tezos_level ;
            }
          else return ()
        in
        let* () =
          if Compare.Int64.(timestamp_diff > timestamp_finality)
          then fail @@ Rollup_rejection_too_old_timestamp {
              rollup_block_timestamp = timestamp ;
              current_timestamp ;
            }
          else return ()
        in
        return ()
      in
      match rejection_content with
      | Reject_micro_block micro_block_rejection -> (
          let Micro_block_rejection.{ micro_block_index ; state_trace } =
            micro_block_rejection
          in
          let* micro_block =
            if Compare.Int.(micro_block_index < 0)
            then fail (Rollup_invalid_rejection Rollup_bad_micro_block_index)
            else match List.nth_opt micro_blocks micro_block_index with
            | Some x -> return x
            | None -> fail (Rollup_invalid_rejection Rollup_bad_micro_block_index)
          in
          let Block_onchain_content.{
              parameter ;
              before_root = Root hash ;
              after_root = Root after_hash
            } = micro_block in
          let module Replay = Counter_rollup.Main.MakeReplay () in
          let* parameter =
            match Data_encoding.Binary.of_bytes Counter_rollup.Main.encoding parameter with
            | Some x -> return x
            | None -> failwith "bad encoding" (* TODO: add error *)
          in
          let* () =
            try (
              let Hash after_hash' = Replay.transition ~hash parameter state_trace in
              if Compare.Bytes.(after_hash = after_hash')
              then fail (Rollup_invalid_rejection Rollup_valid)
              else return ()
            ) with
            | Batcher.Invalid_signature -> return ()
            | _ -> fail (Rollup_invalid_rejection Rollup_unexpected_error)
          (* | Gas_overflow
           * | Persisting_state_too_big
           * | State_overflow
           *)
          in
          let* (indices , ctxt) = reorg_rollup ctxt ~id:rollup_id ~level in

          let result = {
            consumed_gas = Gas.Arith.zero;
            allocated_storage = Z.zero;
            originated_contracts = [];
            removed_rollup_block_indices = indices ;
            micro_block_rejection = micro_block_rejection ; 
          } in
          return (ctxt, Rollup_result (Micro_block_rejection_result result), [])
        )
    )
  | Deposit () ->
    return (ctxt, Rollup_result (Deposit_result dummy_result), [])
  | Withdraw () ->
    return (ctxt, Rollup_result (Withdrawal_result dummy_result), [])
