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

(* module M = Map.Make(Bytes)
 * type t = bytes M.t *)

type deposit = {
  source : Contract.t ;
  amount : Z.t ;
}

type on_chain_operation =
  | Deposit of deposit


module NS = New_storage
module M = NS.Patricia


module Counter_rollup : ROLLUP = functor(M : ROLLUP_STORAGE) -> struct
  (*
     Dumb rollup.
     Used for testing purposes, and trying out new abstractions.
  *)

  type operation = Add of Z.t

  let z_encode = fun z -> Bytes.of_string (Z.to_string z)
  let z_decode = fun bytes -> Z.of_string (Bytes.to_string bytes)
  
  let encode = fun (Add z) -> z_encode z
  let decode = fun bytes -> Add (z_decode bytes)
  
  let single_key = NS.key_of_bytes Bytes.empty
  
  let make_empty () : unit =
    M.set_full M.empty ;
    M.set single_key (z_encode Z.zero)

  let transition : source:Signature.public_key -> message:bytes -> unit =
    fun ~source:_ ~message ->
    let (Add z) = decode message in
    let counter =
      try z_decode @@ M.get single_key with
      | _ -> (raise (Failure "bad encoding")) (* TODO: deal with impossible errors *)
    in
    let ctxt = M.set single_key (z_encode (Z.add counter z)) in
    ctxt

  (* let on_chain : on_chain_operation -> t -> t tzresult = fun _ ctxt -> ok ctxt *)

end

(* let kind_to_rollup : rollup_kind -> (module ROLLUP) = function
 *   | Counter -> (module Counter_rollup) *)

open NS

module Make_basic_rollup(R : ROLLUP) = struct
  let t = ref Patricia.empty
  module P = struct
    include NS.Patricia

    let get_full () = !t
    let set_full x = t := x
    let get k = get !t k
    let set k v = t := set !t k v
  end
  include R(P : ROLLUP_STORAGE)

  let transitions : transaction list -> unit =
    let aux : transaction -> unit = fun { content ; signer } ->
      transition ~source:signer ~message:content
    in
    List.iter aux
end

module Make_create_reject_rollup(R : ROLLUP) = struct
  let t = ref NS.Patricia_produce_stream.empty
  let s = ref []
  module P = struct
    include NS.Patricia_produce_stream
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
  include R(P : ROLLUP_STORAGE)

  let transitions : transaction list -> unit =
    let aux : transaction -> unit = fun { content ; signer } ->
      transition ~source:signer ~message:content
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
  include R(P : ROLLUP_STORAGE)

  let get_full_hash () = Patricia_consume_stream.get_hash !t
  
  let transitions ~(hash : hash) ~(stream : stream) : transaction list -> unit =
    let aux : transaction -> unit = fun { content ; signer } ->
      transition ~source:signer ~message:content
    in
    s := stream ;
    t := NS.Patricia_consume_stream.empty_hash hash ;
    List.iter aux
  
end

module Counter_reject_replay = Make_reject_replay_rollup(Counter_rollup)

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
        rollup_number = id;
        consumed_gas = Gas.Arith.zero; (* TODO *)
        allocated_storage = Z.zero; (* TODO *)
        originated_contracts = [];
      }
    in
    return (ctxt, Rollup_result (Rollup_creation_result result), [])
  )
  | Commit_block block_commitment -> (
      let* (() , ctxt) = Rollup.commit_block ~operator:source block_commitment ctxt in
      return
        (ctxt, Rollup_result (Block_commitment_result dummy_result), [])
    )
  | Reject_micro_block micro_block_rejection -> (
      let Micro_block_rejection.{ rollup_id ; level ; rejection_content } = micro_block_rejection in
      let* _rollup = get_rollup ctxt rollup_id in
      let* block = get_block ctxt rollup_id level in
      (* let module Run = Counter_reject_replay in *)
      let* () =
        match rejection_content with
        | Invalid_signature { transaction_index = _ } -> (
            let open Rollup.Signature in
            let Block_onchain_content.{ transactions ; aggregated_signature ; _ } = block in
            let identified_hashes =
              let aux : transaction -> (public_key * hash) =
                fun { content ; signer } -> (signer , do_hash (Message content))
              in
              List.map aux transactions
            in
            let check =
              Rollup.Signature.check_signed_hashes { identified_hashes ; aggregated_signature }
            in
            if check
            then failwith "signatures are ok" (* TODO: add error *)
            else return ()
          )
        | Gas_overflow stream -> (
            let Block_onchain_content.{ transactions ; before_root = Root hash ; _ } = block in
            try
              let () = Counter_reject_replay.transitions ~hash ~stream transactions in
              failwith "didn't gas overflow"
            with
            (* | Gas_overflow_exception -> return () *) (* TODO: add Gas_overflow_exception *)
            | _ -> failwith "unexpected error"
          )
        | Persisting_state_too_big stream -> (
            let Block_onchain_content.{ transactions ; before_root = Root hash ; _ } = block in
            try
              let () = Counter_reject_replay.transitions ~hash ~stream transactions in
              failwith "didn't gas overflow"
            with
            (* | Persisting_state_too_big_exception -> return () *) (* TODO: add Persisting_state_too_Big_exception *)
            | _ -> failwith "unexpected error"
          )
        | State_overflow stream -> (
            let Block_onchain_content.{ transactions ; before_root = Root hash ; _ } = block in
            try
              let () = Counter_reject_replay.transitions ~hash ~stream transactions in
              failwith "didn't state overflow"
            with
            (* | State_overflow_exception -> return () *) (* TODO: add State_overflow_exception *)
            | _ -> failwith "unexpected error"
          )
        | Invalid_state_hash stream -> (
            let Block_onchain_content.{ transactions ; before_root = Root hash ; after_root = Root after_hash } = block in
            try
              let () = Counter_reject_replay.transitions ~hash ~stream transactions in
              let after_hash' = Counter_reject_replay.get_full_hash () in
              if Compare.Bytes.(after_hash = after_hash')
              then failwith "good state hash"
              else return ()
            with
            | _ -> failwith "unexpected error"

          )
      in
      (* REJECT ALL BLOCKS FROM *level* *)
      failwith "TODO"
      (* return (ctxt, Rollup_result (Micro_block_rejection_result dummy_result), []) *)
                    
    )
  | Deposit () ->
    return (ctxt, Rollup_result (Deposit_result dummy_result), [])
  | Withdraw () ->
    return (ctxt, Rollup_result (Withdrawal_result dummy_result), [])
