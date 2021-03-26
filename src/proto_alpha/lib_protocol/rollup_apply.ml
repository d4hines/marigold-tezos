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

(* Generic way to deal with operation replays, but a rollup is not
   required to use it (eg, UTXOs) *)
(* TODO: add possibility to not use it *)
module Operation_replay = struct

  type 'a t = {
    block_level : Z.t ;     (* TODO: abstract rollup block level *)
    nonce : Z.t option ;
    content : 'a ;
  }

  let encoding (type a) e : a t Data_encoding.t = Data_encoding.(
      conv
        (fun { block_level ; nonce ; content } -> (block_level , nonce , content))
        (fun (block_level , nonce , content) -> { block_level ; nonce ; content })
      @@ tup3 z (option z) e
    )

end

module Counter_rollup = struct
  type internal =
    | Add of Z.t

  let internal_encoding : internal Data_encoding.t =
    Data_encoding.(
      conv
        (function Add x -> x) (fun x -> Add x)
      @@ z
    )

  type operation = internal Operation_replay.t

  let operation_encoding : operation Data_encoding.t =
    Operation_replay.encoding internal_encoding
  
  module Make = functor(M : ROLLUP_STORAGE) -> struct
  (*
     Dumb rollup.
     Used for testing purposes, and trying out new abstractions.
  *)
    module P = M
    
    let z_encode = fun z -> Bytes.of_string (Z.to_string z)
    let z_decode = fun bytes -> Z.of_string (Bytes.to_string bytes)

    let single_key = NS.key_of_bytes Bytes.empty
    
    let init () : unit =
      M.set_full M.empty ;
      M.set single_key (z_encode Z.zero)

    let get () = z_decode @@ M.get single_key
    let set n = M.set single_key @@ z_encode n
    
    let with_save f =
      let save = M.get_full () in
      try f () with
      | e -> (
          M.set_full save ;
          raise e
        )

    let transition = fun ~source:_ ~(operation:operation) ->
      with_save @@ fun () ->
      match operation.content with
      | Add z -> (
          let counter = get () in
          set (Z.add counter z)
        )


  end

end
  
(* let kind_to_rollup : rollup_kind -> (module ROLLUP) = function
 *   | Counter -> (module Counter_rollup) *)

open NS

let max_stream_node_size = 1000000
let max_leaf_size = max_stream_node_size - 100

module Stateful_patricia() = struct
  let t = ref Patricia.empty
  include struct
    module Pure = NS.Patricia
    open Pure

    type nonrec t = t
    let empty = empty
    let get_full () = !t
    let set_full x = t := x
    let get k = get !t k
    let set k v =
      if Compare.Int.(Bytes.length v > max_leaf_size)
      then raise (Failure ("max_leaf_size exceeded")) (* TODO: add exception *)
      else t := set !t k v
  end
  let get_hash () = Patricia.get_hash !t

end

module Make_create_reject_rollup(R : ROLLUP) = struct
  let t = ref NS.Patricia_produce_stream.empty
  let s = ref []
  module P = struct
    include NS.Patricia_produce_stream
    let get_hash () = get_hash !t
    let get_full () = !t
    let set_full x = t := x ; s := []
    let get k =
      let (v , (t' , s')) = get (!t , !s) k in
      t := t' ;
      s := s' ;
      v
    let set k v =
      let (t' , s') = set (!t , !s) k v in
      t := t' ;
      s := s' ;
      ()
  end
  include R.Make(P : ROLLUP_STORAGE)

  let raw_transition ~source ~message =
    let operation_opt = Data_encoding.Binary.of_bytes R.operation_encoding message in
    let operation = match operation_opt with
      | Some x -> x
      | None -> assert false     (* TODO: Add error *)
    in
    transition ~source ~operation

  
  let transitions : transaction list -> unit =
    let aux : transaction -> unit = fun { content ; signer } ->
      raw_transition ~source:signer ~message:content
    in
    List.iter aux

end

module Make_reject_replay_rollup(R : ROLLUP) : sig
  val transitions : hash:hash -> stream:stream -> transaction list -> unit
  val get_full_hash : unit -> hash
end = struct
  let t = ref NS.Patricia_consume_stream.empty
  let s = ref []
  module P = struct
    include NS.Patricia_consume_stream
    let get_hash () = get_hash !t
    let get_full () = !t
    let set_full x = t := x ; s := []
    let get k =
      let (v , (t' , s')) = get (!t , !s) k in
      t := t' ;
      s := s' ;
      v
    let set k v =
      let (t' , s') = set (!t , !s) k v in
      t := t' ;
      s := s' ;
      ()
  end
  include R.Make(P : ROLLUP_STORAGE)

  let get_full_hash () = Patricia_consume_stream.get_hash !t

  let raw_transition ~source ~message =
    let operation_opt = Data_encoding.Binary.of_bytes R.operation_encoding message in
    let operation = match operation_opt with
      | Some x -> x
      | None -> assert false     (* TODO: Add error *)
    in
    transition ~source ~operation
  
  let transitions ~(hash : hash) ~(stream : stream) : transaction list -> unit =
    let aux : transaction -> unit = fun { content ; signer } ->
      raw_transition ~source:signer ~message:content
    in
    s := stream ;
    t := NS.Patricia_consume_stream.empty_hash hash ;
    List.iter aux
  
end

module Counter_reject_replay = Make_reject_replay_rollup(Counter_rollup)

let check_micro_block_signature : Block_onchain_content.micro -> bool = fun micro_block ->
  let open Rollup.Signature in
  let Block_onchain_content.{ transactions ; aggregated_signature ; _ } = micro_block in
  let identified_hashes =
    let aux : transaction -> (public_key * hash) =
      fun { content ; signer } -> (signer , do_hash (Message content))
    in
    List.map aux transactions
  in
  Rollup.Signature.check_signed_hashes { identified_hashes ; aggregated_signature }

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
      let* (() , ctxt) = Rollup.commit_block ~operator:source block_commitment ctxt in
    let result : Rollup.block_commitment_result =
      {
        commitment = block_commitment;
        consumed_gas = Gas.Arith.zero; (* TODO *)
        allocated_storage = Z.zero; (* TODO *)
        originated_contracts = [];
      }
    in
      return
        (ctxt, Rollup_result (Block_commitment_result result), [])
    )
  | Reject_block block_rejection -> (
      let Block_rejection.{ rollup_id ; level ; rejection_content } = block_rejection in
      let* block = get_block ctxt rollup_id level in
      match rejection_content with
      | Reject_micro_block micro_block_rejection -> (
          let Block_onchain_content.{ micro_blocks ; tezos_level = _ } = block in
          let Micro_block_rejection.{ micro_block_index ; rejection_content } =
            micro_block_rejection
          in
          let* micro_block =
            match List.nth_opt micro_blocks micro_block_index with
            | Some x -> return x
            | None -> failwith "bad micro block index"
          in
          (* let* _rollup = get_rollup ctxt rollup_id in *)
          (* let module Run = Counter_reject_replay in *)
          let* () =
            match rejection_content with
            | Invalid_signature () -> (
                let check = check_micro_block_signature micro_block in
                if check
                then failwith "signatures are ok" (* TODO: add error *)
                else return ()
              )
            | Gas_overflow stream -> (
                let Block_onchain_content.{ transactions ; before_root = Root hash ; _ } = micro_block in
                try
                  let () = Counter_reject_replay.transitions ~hash ~stream transactions in
                  failwith "didn't gas overflow"
                with
                (* | Gas_overflow_exception -> return () *) (* TODO: add Gas_overflow_exception *)
                | _ -> failwith "unexpected error"
              )
            | Persisting_state_too_big stream -> (
                let Block_onchain_content.{ transactions ; before_root = Root hash ; _ } = micro_block in
                try
                  let () = Counter_reject_replay.transitions ~hash ~stream transactions in
                  failwith "didn't gas overflow"
                with
                (* | Persisting_state_too_big_exception -> return () *) (* TODO: add Persisting_state_too_Big_exception *)
                | _ -> failwith "unexpected error"
              )
            | State_overflow stream -> (
                let Block_onchain_content.{ transactions ; before_root = Root hash ; _ } = micro_block in
                try
                  let () = Counter_reject_replay.transitions ~hash ~stream transactions in
                  failwith "didn't state overflow"
                with
                (* | State_overflow_exception -> return () *) (* TODO: add State_overflow_exception *)
                | _ -> failwith "unexpected error"
              )
            | Invalid_state_hash stream -> (
                let Block_onchain_content.{ transactions ; before_root = Root hash ; after_root = Root after_hash } = micro_block in
                try (
                  let () = Counter_reject_replay.transitions ~hash ~stream transactions in
                  let after_hash' = Counter_reject_replay.get_full_hash () in
                  if Compare.Bytes.(after_hash = after_hash')
                  then failwith "good state hash"
                  else return ()
                ) with
                | _ -> failwith "unexpected error"
              )
            | Invalid_block_id tx_i -> (
                let Block_onchain_content.{ transactions ; _ } = micro_block in
                let { content ; signer = _ } =
                  match List.nth_opt transactions tx_i with
                  | Some x -> x
                  | None -> failwith "bad transaction index"
                in
                let Operation_replay.{ block_level ; _ } =
                  match Data_encoding.Binary.of_bytes Counter_rollup.operation_encoding content with
                  | Some x -> x
                  | None -> failwith "unexpected error"
                in
                if Compare.Z.(level = block_level)
                then failwith "rollup block level is ok"
                else return ()
              )
          in
          (* REJECT ALL BLOCKS FROM *level* *)
          failwith "TODO"
          (* return (ctxt, Rollup_result (Micro_block_rejection_result dummy_result), []) *)
        )
      | Double_injection ((mb_i_a , tx_i_a) , (mb_i_b , tx_i_b)) -> (
          let Block_onchain_content.{ micro_blocks ; tezos_level = _ } = block in
          let* () =
            let* micro_block_a =
              match List.nth_opt micro_blocks mb_i_a with
              | Some x -> return x
              | None -> failwith "bad micro block index"
            in
            let Block_onchain_content.{ transactions = tx_a ; _ } = micro_block_a in
            let { content = content_a ; signer = _ } =
              match List.nth_opt tx_a tx_i_a with
              | Some x -> x
              | None -> failwith "bad transaction index"
            in
            let* micro_block_b =
              match List.nth_opt micro_blocks mb_i_b with
              | Some x -> return x
              | None -> failwith "bad micro block index"
            in
            let Block_onchain_content.{ transactions = tx_b ; _ } = micro_block_b in
            let { content = content_b ; signer = _ } =
              match List.nth_opt tx_b tx_i_b with
              | Some x -> x
              | None -> failwith "bad transaction index"
            in
            if Compare.Bytes.(content_a <> content_b)
            then failwith "not a double injection"
            else return ()
          in
          failwith "TODO"
        )
    )
  | Deposit () ->
    return (ctxt, Rollup_result (Deposit_result dummy_result), [])
  | Withdraw () ->
    return (ctxt, Rollup_result (Withdrawal_result dummy_result), [])
