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

open Protocol

module RA = Rollup_apply
module R = Alpha_context.Rollup
module S = R.Signature

type signed_operation = {
  signer : S.public_key ;
  content : bytes ;
  signature : S.signature ;
}

let signed_operation_encoding : signed_operation Data_encoding.t =
  Data_encoding.(
    conv
    (fun { signer = se ; content = c ; signature = su } -> (se , c , su))
    (fun (se , c , su) -> { signer = se ; content = c ; signature = su })
  @@ tup3 S.public_key_encoding bytes S.signature_encoding
)

type micro_block = {
  rev_transactions : signed_operation list ;
  before_root : R.root ;
  after_root : R.root ;
  aggregated_signature_opt : S.signature option ;
}

type block = {
  rev_micro_blocks : micro_block list ;
  block_level : Z.t ;
}

module Commitment = struct
  let make_micro : micro_block -> R.Block_commitment.micro = fun mb ->
      let { rev_transactions ; before_root = _ ; after_root ; aggregated_signature_opt } = mb in
      let transactions =
        let aux { signer ; content ; signature = _ } = R.{ signer ; content } in
        List.map aux @@
        List.rev rev_transactions
      in
      let aggregated_signature = match aggregated_signature_opt with
        | Some x -> x
        | None -> raise (Failure "no aggregated signature")
      in
      { transactions ; aggregated_signature ; after_root }
    
  let make ~rollup_id : block -> R.Block_commitment.t = fun block ->
    let { rev_micro_blocks ; block_level = _ } = block in
    let micro_block_commitments =
      List.map make_micro @@
      List.rev rev_micro_blocks
    in
    let bc = R.Block_commitment.{
        micro_block_commitments ;
        rollup_id ;
      } in
    bc
end

module Rejection = struct

  let invalid_signature ~rollup_id ~level ~micro_block_index =
    let rejection_content = R.Micro_block_rejection.Invalid_signature () in
    let rejection_content = R.Block_rejection.Reject_micro_block { micro_block_index ; rejection_content } in
    R.Block_rejection.{ rollup_id ; level ; rejection_content }

  let invalid_state_hash ~rollup_id ~level ~micro_block_index state_trace =
    let rejection_content = R.Micro_block_rejection.Invalid_state_hash state_trace in
    let rejection_content = R.Block_rejection.Reject_micro_block { micro_block_index ; rejection_content } in
    R.Block_rejection.{ rollup_id ; level ; rejection_content }

end

include RA.Counter_rollup

module DelayedInit(P : sig type t end) = struct
  let value : P.t option ref = ref None
  let set : P.t -> unit = fun x -> value := (Some x)
  let get : unit -> P.t = fun () -> match !value with
    | Some x -> x
    | None -> raise (Failure "DelayedInit: get before first set")
end

module MakeOperator() = struct

  module Context = RA.Counter_rollup.Make(RA.Stateful_patricia())

  let get_root () = R.Root (Context.P.get_hash ())
  
  let () =
    Context.init () ;
    ()

  let empty_micro_block = {
    rev_transactions = [] ;
    before_root = get_root () ;
    after_root = get_root () ;
    aggregated_signature_opt = None ;
  }

  let empty_block block_level = { rev_micro_blocks = [] ; block_level }
  
  let current_micro_block = ref empty_micro_block
  let current_block = ref (empty_block Z.zero)
  module Id = DelayedInit(Z)

  let set_id = Id.set
  
  let finish_micro_block () =
    let previous_micro_block = !current_micro_block in
    let previous_root = previous_micro_block.after_root in
    let root = get_root () in
    (if previous_root <> root then assert false) ;
    current_micro_block := empty_micro_block ;
    current_block := {
      !current_block with
      rev_micro_blocks = previous_micro_block :: (!current_block).rev_micro_blocks
    } ;
    previous_micro_block

  let finish_block () =
    let _ = finish_micro_block () in
    let previous_block = !current_block in
    current_block := empty_block (Z.succ previous_block.block_level) ;
    previous_block
    
  let get_block_level () = (!current_block).block_level
  
  let aggregate_signature opt t =
    match opt with
    | None -> Some t
    | Some x -> Some (S.add_signature x t)
  
  let process_signed_operation ({ signer ; content ; signature } as signed_operation) =
    let hash = S.do_hash (Message content) in
    (if not (S.check_signature signer hash signature)
     then raise (Failure "bad signature")) ; (* TODO: Add error *)
    let operation =
      match Data_encoding.Binary.of_bytes_opt operation_encoding content with
      | Some x -> x
      | None -> raise (Failure "bad operation bytes")
    in
    Context.transition ~source:signer ~operation ;
    let { before_root ; after_root = _ ; aggregated_signature_opt ; rev_transactions } =
      !current_micro_block
    in
    let after_root = get_root () in
    current_micro_block := { 
        before_root ;
        after_root ;
        aggregated_signature_opt = aggregate_signature aggregated_signature_opt signature ;
        rev_transactions = signed_operation :: rev_transactions ;
      }

  let commit_block block = Commitment.make ~rollup_id:(Id.get ()) block
  
end

type bad_signature = {
  micro_block_index : int ;
}
exception Bad_signature of bad_signature
type bad_root = {
  micro_block_index : int ;
  state_trace : R.state_trace ;
}
exception Bad_root of bad_root

module MakeValidator(P : sig val rollup_id : Z.t end) = struct

  let rollup_id = P.rollup_id
  
  module Operator = MakeOperator()
  let () =
    Operator.set_id rollup_id

  module ReplayerContext = RA.Stateful_produce_patricia()
  module Replayer = RA.Counter_rollup.Make(ReplayerContext)

  let preserve_state f =
    let save = Operator.Context.P.get_full () in
    try f () with
    | exn -> (
        Operator.Context.P.set_full save ;
        raise exn
      )

  module type VALIDATOR_ROLLUP = sig
    val transition : source:S.public_key -> operation:operation -> unit
    val get_root : unit -> R.root
  end

  module Regular = struct
    let transition = Operator.Context.transition
    let get_root = Operator.get_root
  end

  module Replay = struct
    let transition = Replayer.transition
    let get_root () = R.Root (Replayer.P.get_hash ())
  end

  let aux_process_transaction (module V : VALIDATOR_ROLLUP) : R.transaction -> unit = fun tx ->
    let R.{ content ; signer } = tx in
    let operation =
      match Data_encoding.Binary.of_bytes_opt operation_encoding content with
      | Some x -> x
      | None -> raise (Failure "bad operation bytes")
    in
    V.transition ~source:signer ~operation ;
    ()

  exception Bad_root_aux of bad_signature
  
  let aux_process_micro_block_commitment (module V : VALIDATOR_ROLLUP) mb_i : R.Block_commitment.micro -> unit = fun mbc ->
    let R.Block_commitment.{ transactions ; aggregated_signature ; after_root } = mbc in
    (* Check signatures *)
    let () =
      let identified_hashes =
        let aux : R.transaction -> _ =
          fun { content ; signer } -> (signer , R.Signature.do_hash (Message content))
        in
        List.map aux transactions
      in
      let check =
        R.Signature.check_signed_hashes { identified_hashes ; aggregated_signature }
      in
      if not check then raise (Bad_signature { micro_block_index = mb_i }) ;
      ()
    in
    List.iter (aux_process_transaction (module V)) transactions ;
    let root = V.get_root () in
    if root <> after_root then raise (Bad_root_aux { micro_block_index = mb_i }) ;
    ()

  let wrap_process_micro_block_commitment mb_i mbc =
    try (
      preserve_state @@ fun () -> aux_process_micro_block_commitment (module Regular) mb_i mbc
    ) with
    | Bad_root_aux { micro_block_index } -> (
        let replay_state = ReplayerContext.of_patricia @@ Operator.Context.P.get_full() in
        ReplayerContext.set_full replay_state ;
        (try (
          ignore @@ aux_process_micro_block_commitment (module Replay) mb_i mbc
        ) with | Bad_root_aux _ -> () | exn -> raise exn) ;
        let state_trace = !ReplayerContext.s in
        raise (Bad_root { micro_block_index ; state_trace })
      )
    | exn -> raise exn
  
  let process_block_commitment : R.Block_commitment.t -> unit = fun bc ->
    preserve_state @@ fun () ->
    let R.Block_commitment.{ micro_block_commitments ; rollup_id = _ } = bc in
    List.iteri wrap_process_micro_block_commitment micro_block_commitments ;
    ()

  let invalid_signature = Rejection.invalid_signature ~rollup_id
  let invalid_state_hash = Rejection.invalid_state_hash ~rollup_id
  
end

module Client = struct

  let make_operation ?nonce ~block_level content =
    RA.Operation_replay.{ content ; block_level ; nonce }
  
  let sign_operation ~account ~operation =
    let content =
      Data_encoding.Binary.to_bytes_exn operation_encoding operation
    in
    let signature = S.(sign_hash account.secret_key @@ do_hash (Message content)) in
    let signer = account.public_key in
    { content ; signature ; signer }

  
end
