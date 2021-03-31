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

let hex ppf b =
  let s = Bytes.to_string b in
  Format.fprintf ppf "0x" ;
  for i = 0 to (String.length s) - 1 do
    Format.fprintf ppf "%02x" (Char.code s.[i])
  done ;
  ()


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
  let  signer_encoding : signer Data_encoding.t = Signature.public_key_encoding
  type signer_id = int
  let signer_id_encoding : signer_id Data_encoding.t = Data_encoding.int31
  type hash = Hash of bytes

  exception Invalid_signature

  
  module type ROLLUP_SIGNER_STORAGE = sig
    val of_id : signer_id -> signer
    val to_id : signer -> signer_id
  end
  
  module type PARAMETER = sig
    module InfraOperation : sig
      type t
      val encoding : t Data_encoding.t
      type explicit
      val explicit_encoding : explicit Data_encoding.t

      module MakeMakeExplicit : functor (M : ROLLUP_SIGNER_STORAGE) -> sig
        val main : t -> explicit
      end
    end

    module InfraTransition : functor (M : ROLLUP_STORAGE) -> sig
      val init : unit -> unit
      val main : source:signer -> parameter:InfraOperation.explicit -> unit
    end
  end

  module Replay_counter = struct
    type t = Z.t
    let prefix k = Bytes.(cat (of_string "replay") k)
    let to_key : signer -> NS.key = fun signer ->
      let bytes = Data_encoding.Binary.to_bytes_exn Signature.public_key_encoding signer in
      NS.key_of_bytes @@ prefix bytes
    let of_value : NS.value -> t = fun raw ->
      match Data_encoding.(Binary.of_bytes z raw) with
      | Some x -> x
      | None -> assert false
    let to_value : t -> NS.value = fun raw ->
      match Data_encoding.(Binary.to_bytes z raw) with
      | Some x -> x
      | None -> assert false
    let encoding = Data_encoding.z
  end

  module Signer_storage = struct
    (* TODO: explicit key size limit *)
    type counter = int
    let counter_encoding = Data_encoding.int31
    let counter_key = NS.key_of_bytes @@ Bytes.of_string "signer-counter"
        
    let prefix_key k = Bytes.(cat (of_string "signer-id") k)
    let signer_to_key : signer -> NS.key = fun signer ->
      let bytes = Data_encoding.Binary.to_bytes_exn Signature.public_key_encoding signer in
      NS.key_of_bytes @@ prefix_key bytes
    let prefix_id k = Bytes.(cat (of_string "id-signer") k)
    let id_to_key : signer_id -> NS.key = fun id ->
      let bytes = Data_encoding.Binary.to_bytes_exn Data_encoding.int31 id in
      NS.key_of_bytes @@ prefix_id bytes

    let counter_of_value : NS.value -> counter = fun raw ->
      match Data_encoding.(Binary.of_bytes counter_encoding raw) with
      | Some x -> x
      | None -> assert false
    let counter_to_value : counter -> NS.value = fun raw ->
      match Data_encoding.(Binary.to_bytes counter_encoding raw) with
      | Some x -> x
      | None -> assert false
    let signer_of_value : NS.value -> signer = fun raw ->
      match Data_encoding.(Binary.of_bytes signer_encoding raw) with
      | Some x -> x
      | None -> assert false
    let signer_to_value : signer -> NS.value = fun raw ->
      match Data_encoding.(Binary.to_bytes signer_encoding raw) with
      | Some x -> x
      | None -> assert false
    let id_of_value : NS.value -> signer_id = fun raw ->
      match Data_encoding.(Binary.of_bytes signer_id_encoding raw) with
      | Some x -> x
      | None -> assert false
    let id_to_value : signer_id -> NS.value = fun raw ->
      match Data_encoding.(Binary.to_bytes signer_id_encoding raw) with
      | Some x -> x
      | None -> assert false

  end
  
  
  module Make(P : PARAMETER) = struct


    module InfraOperation = struct
      type content = P.InfraOperation.explicit
      let content_encoding : content Data_encoding.t = P.InfraOperation.explicit_encoding

      type t = P.InfraOperation.t
      let encoding = P.InfraOperation.encoding
      
      type explicit = {
        counter : Replay_counter.t ;
        rollup_id : Rollup_id.t ;
        content : content ;
      }

      let aux_explicit_encoding : explicit Data_encoding.t = Data_encoding.(
        conv
          (fun { counter ; rollup_id ; content } -> (counter , rollup_id , content))
          (fun (counter , rollup_id , content) -> { counter ; rollup_id ; content })
        @@ tup3 Replay_counter.encoding Rollup_id.encoding content_encoding
      )

      let explicit_to_bytes : explicit -> bytes = Data_encoding.Binary.to_bytes_exn aux_explicit_encoding

      module MakeMakeExplicit(M : ROLLUP_SIGNER_STORAGE) = struct
        module MakeExplicit = P.InfraOperation.MakeMakeExplicit(M)
        
        let main ~counter ~rollup_id : t -> explicit = fun content ->
          let content = MakeExplicit.main content in
          { counter ; rollup_id ; content }
      end

      type signed = {
        signer : Signature.public_key ;
        content : t ;
        signature : Signature.signature ;
      }

      let signed_operation_encoding : signed Data_encoding.t =
        Data_encoding.(
          conv
            (fun { signer = se ; content = c ; signature = su } -> (se , c , su))
            (fun (se , c , su) -> { signer = se ; content = c ; signature = su })
          @@ tup3 Signature.public_key_encoding encoding Signature.signature_encoding
        )

    end

    module Parameter = struct

      type t = {
        content : (signer * InfraOperation.t) list ;
        aggregated_signature : Signature.signature ;
      }

      let encoding : t Data_encoding.t =
        Data_encoding.(
          conv
            (fun { content = c ; aggregated_signature = a } -> (c , a))
            (fun (c , a) -> { content = c ; aggregated_signature = a })
          @@ tup2
            (list (tup2 Signature.public_key_encoding InfraOperation.encoding))
            Signature.signature_encoding
        )

      type explicit = {
        content : (signer * InfraOperation.t) list ;
        rollup_id : Rollup.Rollup_id.t ;
        aggregated_signature : Signature.signature ;
      }

      let explicit_encoding : explicit Data_encoding.t =
        Data_encoding.(
          conv
            (fun { content = c ; rollup_id = r ; aggregated_signature = a } -> (c , r , a))
            (fun (c , r , a) -> { content = c ; rollup_id = r ; aggregated_signature = a })
          @@ tup3
            (list (tup2 Signature.public_key_encoding InfraOperation.encoding))
            Rollup.Rollup_id.encoding
            Signature.signature_encoding
        )

      let make_explicit ~rollup_id : t -> explicit = fun { content ; aggregated_signature } ->
        { content ; rollup_id ; aggregated_signature }

    end
      
    module Transition = functor (M : ROLLUP_STORAGE) -> struct

      module M = M
      
      let with_save f =
        let save = M.get_full () in
        try f () with
        | e -> (
            M.set_full save ;
            raise e
          )

      module StatefulView = struct
        let get_counter signer =
          let key = Replay_counter.to_key signer in
          if M.mem key
          then Replay_counter.of_value (M.get key)
          else Z.zero

        let set_counter signer counter =
          let key = Replay_counter.to_key signer in
          M.set key (Replay_counter.to_value counter)

        module Signer : ROLLUP_SIGNER_STORAGE = struct
          open Signer_storage

          let to_id : signer -> signer_id = fun signer ->
            let key = signer_to_key signer in
            if M.mem key
            then id_of_value (M.get key)
            else (
              let counter =
                if M.mem counter_key
                then counter_of_value (M.get counter_key)
                else 0
              in
              let next_counter = counter + 1 in
              M.set counter_key (counter_to_value next_counter) ;
              M.set key (id_to_value counter) ;
              counter
            )

          let of_id : signer_id -> signer = fun id ->
            let key = id_to_key id in
            signer_of_value @@ M.get key
        end
          
      end
      
      module PureView = struct
        let with_state f t =
          M.set_full t ;
          f ()

        let do_state f t =
          with_state f t ;
          M.get_full ()

        let do_with_state f t =
          let value = with_state f t in
          (value , M.get_full ())

        let get_counter s = with_state @@ fun () -> StatefulView.get_counter s
        let set_counter s k v = (do_state @@ fun () -> StatefulView.set_counter k v) s

        module Signer = struct
          let to_id signer = with_state @@ fun () -> StatefulView.Signer.to_id signer
          let of_id id = with_state @@ fun () -> StatefulView.Signer.of_id id
        end
      end

      module Stateful = struct
        module Aux = P.InfraTransition(M)
        module MakeExplicit = InfraOperation.MakeMakeExplicit(StatefulView.Signer)


        let get_explicit_operation ~rollup_id : signer * InfraOperation.t -> InfraOperation.explicit =
          fun (signer , content) ->
          let stored_counter = StatefulView.get_counter signer in
          let counter = Z.succ stored_counter in
          StatefulView.set_counter signer counter ;
          MakeExplicit.main ~counter ~rollup_id content

        let single_operation : (signer * InfraOperation.explicit) -> unit = fun (signer , content) ->
          Aux.main ~source:signer ~parameter:content.content
        
        let check_signatures ~explicit_operations ~aggregated_signature =
          let open Rollup.Signature in
          let identified_hashes =
            let aux : (signer * InfraOperation.explicit) -> (public_key * hash) =
              fun (signer , op) ->
                (signer , do_hash (Message (InfraOperation.explicit_to_bytes op)))
            in
            List.map aux explicit_operations
          in
          if (not @@ Rollup.Signature.check_signed_hashes { identified_hashes ; aggregated_signature })
          then raise Invalid_signature ;
          ()

        let main : Parameter.explicit -> unit = fun t ->
          let Parameter.{ content = ops ; aggregated_signature ; rollup_id } = t in
          let explicit_operations =
            List.map (fun (x , y) -> (x , get_explicit_operation ~rollup_id (x , y))) ops
          in
          check_signatures ~explicit_operations ~aggregated_signature ;
          (* Perform Operations *)
          List.iter single_operation explicit_operations

        let init () = Aux.init ()

        let main_incremental ~rollup_id : InfraOperation.signed -> unit =
          fun { signer ; content ; signature } ->
          let counter =
            let counter = StatefulView.get_counter signer in
            let counter = Z.succ counter in
            StatefulView.set_counter signer counter ;
            counter
          in
          let explicit = MakeExplicit.main ~rollup_id ~counter content in
          let raw_content =
            InfraOperation.explicit_to_bytes explicit
          in
          let hash = Signature.do_hash (Message raw_content) in
          (if not (Signature.check_signature signer hash signature)
           then raise Invalid_signature) ;
          single_operation (signer , explicit) ;

      end
    end
    module Regular = struct
      module S = Stateful_patricia()
      module T_aux = Transition(S : ROLLUP_STORAGE)
      module T = T_aux.Stateful
          
      let empty : M.t =
        S.set_full S.empty ;
        T.init () ;
        S.get_full ()

      let transition : M.t -> Parameter.explicit -> M.t = fun s t ->
        S.set_full s ;
        T.main t ;
        S.get_full ()

      let incremental_transition ~rollup_id
        : M.t -> InfraOperation.signed -> M.t =
        fun s x ->
        S.set_full s ;
        T.main_incremental ~rollup_id x ;
        S.get_full ()
        
      let get_root : M.t -> root = fun s ->
        S.set_full s ;
        Root (S.get_hash ())

      let operation_make_explicit ~rollup_id : M.t -> signer -> InfraOperation.t -> InfraOperation.explicit =
        fun s signer op ->
        S.set_full s ;
        T.get_explicit_operation ~rollup_id (signer , op)
      
    end
    
    module Reject = struct
      module S = Stateful_produce_patricia()
      module T_aux = Transition(S : ROLLUP_STORAGE)
      module T = T_aux.Stateful
      
      let transition : M.t -> Parameter.explicit -> state_trace = fun s t ->
        let s' = S.of_patricia s in
        S.set_full s' ;
        S.s := [] ;
        (* TODO: Wrap this in Make_reject exception *)
        (try T.main t
        with _ -> ()) ;
        !S.s
    end

    module Replay = struct
      module S = Stateful_consume_patricia()
      module T_aux = Transition(S : ROLLUP_STORAGE)
      module T = T_aux.Stateful

      let transition : hash:bytes -> Parameter.explicit -> state_trace -> hash = fun ~hash t trace ->
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

    module InfraOperation = struct
    
      type t =
        | Add of Z.t
                   
      let encoding : t Data_encoding.t =
        Data_encoding.(
          conv
            (function Add x -> x) (fun x -> Add x)
          @@ z
        )

      type explicit = t
      let explicit_encoding = encoding
      
      module MakeMakeExplicit(M : Batcher.ROLLUP_SIGNER_STORAGE) = struct
        let main x = x
      end

      
    end

    module Counter_storage = struct

      let z_encode = fun z -> Bytes.of_string (Z.to_string z)
      let z_decode = fun bytes -> Z.of_string (Bytes.to_string bytes)
          
      let single_key = NS.key_of_bytes Bytes.empty
    end

    module InfraTransition = functor(M : ROLLUP_STORAGE) -> struct
      open InfraOperation
      
      module StatefulView = struct
        open Counter_storage
        let get () = z_decode @@ M.get single_key
        let set n = M.set single_key @@ z_encode n
      end
      module PureView = struct
        let get t =
          M.set_full t ;
          StatefulView.get ()
      end
      open StatefulView

      let init () : unit =
        let open Counter_storage in
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

  module AuxBatched = Batcher.Make(Internal)

  module InfraOperation = AuxBatched.InfraOperation
  module Parameter = AuxBatched.Parameter
  
  module Regular = struct
    include AuxBatched.Regular
    module Aux = Internal.InfraTransition(S)
    module View = Aux.PureView
  end

  module Reject = struct
    include AuxBatched.Reject
    module Aux = Internal.InfraTransition(S)
    module View = Aux.PureView
  end

  module Replay = struct
    include AuxBatched.Replay
    module Aux = Internal.InfraTransition(S)
    module View = Aux.PureView
  end
      
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
          let module Replay = Counter_rollup.Replay in
          let* parameter =
            match Data_encoding.Binary.of_bytes Counter_rollup.Parameter.encoding parameter with
            | Some x -> return x
            | None -> failwith "bad encoding" (* TODO: add error *)
          in
          let* () =
            try (
              let parameter = Counter_rollup.Parameter.make_explicit ~rollup_id parameter in
              let Hash after_hash' = Replay.transition ~hash parameter state_trace in
              if Compare.Bytes.(after_hash = after_hash')
              then fail (Rollup_invalid_rejection Rollup_valid)
              else return ()
            ) with
            | Batcher.Invalid_signature -> return ()
            | _ -> fail (Rollup_invalid_rejection Rollup_unexpected_error)
            (* | exn -> raise exn *)
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
