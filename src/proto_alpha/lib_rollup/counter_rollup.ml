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

module Counter = Rollup_apply.Counter_rollup
module Main = Counter.Main

module RA = Rollup_apply
module R = Alpha_context.Rollup
module S = R.Signature

module CR = Main.MakeRegular()
module CR_Reject = Main.MakeReject()
module CR_Replay = Main.MakeReplay()

module BatcherView = CR.T.PureView

type signed_operation = {
  signer : S.public_key ;
  content : Main.Operation.t ;
  signature : S.signature ;
}

let signed_operation_encoding : signed_operation Data_encoding.t =
  Data_encoding.(
    conv
    (fun { signer = se ; content = c ; signature = su } -> (se , c , su))
    (fun (se , c , su) -> { signer = se ; content = c ; signature = su })
  @@ tup3 S.public_key_encoding Main.Operation.encoding S.signature_encoding
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

let hex ppf b =
  let s = Bytes.to_string b in
  Format.fprintf ppf "0x" ;
  for i = 0 to (String.length s) - 1 do
    Format.fprintf ppf "%02x" (Char.code s.[i])
  done ;
  ()


module Commitment = struct
  let make_micro : micro_block -> R.Block_commitment.micro = fun mb ->
      let { rev_transactions ; before_root = _ ; after_root ; aggregated_signature_opt } = mb in
      let content =
        let aux { signer ; content ; signature = _ } = (signer , content) in
        List.map aux @@
        List.rev rev_transactions
      in
      let aggregated_signature = match aggregated_signature_opt with
        | Some x -> x
        | None -> raise (Failure "no aggregated signature")
      in
      let parameter = Data_encoding.Binary.to_bytes_exn Main.encoding Main.{ content ; aggregated_signature } in
      { parameter ; after_root }
    
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

  let invalid ~rollup_id ~level ~micro_block_index state_trace =
    let open R.Block_rejection in
    let rejection_content = Reject_micro_block { micro_block_index ; state_trace } in
    { rollup_id ; level ; rejection_content }
    
  let invalid_signature ~rollup_id ~level ~micro_block_index state_trace =
    invalid ~rollup_id ~level ~micro_block_index state_trace

  let invalid_state_hash ~rollup_id ~level ~micro_block_index state_trace =
    invalid ~rollup_id ~level ~micro_block_index state_trace

end

module Operator = struct

  module CR = Main.MakeRegular()

  type t = {
    current_micro_block : micro_block ;
    current_block : block ;
    state : CR.S.t ;
    rollup_id : Z.t ;
    operator : Alpha_context.Contract.t ;
  }
  
  let empty_root = CR.(get_root empty)
  
  let empty_micro_block = {
    rev_transactions = [] ;
    before_root = empty_root ;
    after_root = empty_root ;
    aggregated_signature_opt = None ;
  }

  let empty_block block_level = { rev_micro_blocks = [] ; block_level }

  let empty ~rollup_id ~operator = {
    current_micro_block = empty_micro_block ;
    current_block = empty_block Z.zero ;
    state = CR.empty ;
    rollup_id ;
    operator ;
  }

  let pp_root ppf (R.Root hash) =
    Printf.fprintf ppf "root(%s)" (Bytes.to_string hash)
  
  let finish_micro_block t =
    let { current_micro_block ; current_block ; state ; rollup_id = _ ; operator = _ } = t in
    let previous_micro_block = current_micro_block in
    let previous_root = previous_micro_block.after_root in
    let root = CR.get_root state in
    (if previous_root <> root then assert false) ;
    let current_micro_block = empty_micro_block in
    let current_block = {
      current_block with
      rev_micro_blocks = previous_micro_block :: current_block.rev_micro_blocks
    } in
    (previous_micro_block , { t with current_micro_block ; current_block })

  let finish_block t =
    let (_ , t) = finish_micro_block t in
    let previous_block = t.current_block in
    let current_block = empty_block (Z.succ previous_block.block_level) in
    (previous_block , { t with current_block ; current_micro_block = empty_micro_block })
    
  let get_block_level t = t.block_level
  
  let aggregate_signature opt s =
    match opt with
    | None -> Some s
    | Some x -> Some (S.add_signature x s)
  
  let process_signed_operation t ({ signer ; content ; signature } as signed_operation) =
    let (raw_content , state) =
      let { rollup_id ; state ; _ } = t in
      let counter = BatcherView.get_counter state signer in
      let counter = Z.succ counter in
      let content = Main.Operation.to_explicit ~rollup_id ~counter content in
      let state = BatcherView.set_counter state signer counter in
      (Main.Operation.explicit_to_bytes content , state)
    in
    (* Format.printf "\nProcess signed content: %a\n" hex raw_content ; *)
    let hash = S.do_hash (Message raw_content) in
    (if not (S.check_signature signer hash signature)
     then raise RA.Batcher.Invalid_signature) ; (* TODO: Add error *)
    let operation = content in
    let state = CR.single_transition state (signer , operation) in
    let { before_root ; after_root = _ ; aggregated_signature_opt ; rev_transactions } =
      t.current_micro_block
    in
    let after_root = CR.get_root state in
    let current_micro_block = { 
      before_root ;
      after_root ;
      aggregated_signature_opt = aggregate_signature aggregated_signature_opt signature ;
      rev_transactions = signed_operation :: rev_transactions ;
    }
    in
    { t with current_micro_block ; state }

  let commit_block { rollup_id ; _ } block = Commitment.make ~rollup_id block

  module Wrong = struct
    (* Include signed operation without changing the state *)
    let include_signed_operation t ({ signature ; _ } as signed_operation) =
      let { before_root ; after_root ; aggregated_signature_opt ; rev_transactions } =
        t.current_micro_block
      in
      let current_micro_block = { 
        before_root ;
        after_root ;
        aggregated_signature_opt = aggregate_signature aggregated_signature_opt signature ;
        rev_transactions = signed_operation :: rev_transactions ;
      }
      in
      { t with current_micro_block }

  end
  
end

module Validator = struct

  type bad = {
    micro_block_index : int ;
    state_trace : R.state_trace ;
  }
  exception Bad of bad
  exception Bad_aux of int
  
  
  type t = {
    state : CR.S.t ;
    rollup_id : Z.t ;
  }

  let empty ~rollup_id : t = {
    state = CR.empty ;
    rollup_id ;
  }

  let process_micro_block_commitment : t -> (int * R.Block_commitment.micro) -> t =
    fun t (micro_block_index , { parameter ; after_root }) ->
    let { state = s ; rollup_id ; _ } = t in
    let parameter =
      match Data_encoding.Binary.of_bytes_opt Main.encoding parameter with
      | Some x -> x
      | None -> raise (Failure "bad encoding")
    in
    let parameter = Main.to_explicit ~rollup_id parameter in
    try (
      let s =
        try CR.transition s parameter
        with
        | RA.Batcher.Invalid_signature -> raise (Bad_aux micro_block_index)
        (* TODO: List *all* other bad cases *)
      in
      let after_root' = CR.get_root s in
      if after_root <> after_root' then raise (Bad_aux micro_block_index) ;
      { t with state = s }
    ) with
    | Bad_aux micro_block_index -> (
        let state_trace = CR_Reject.transition s parameter in
        let Root hash = CR.get_root s in
        (try ignore @@ CR_Replay.transition ~hash parameter state_trace
        with _ -> ()) ;
        raise (Bad { state_trace ; micro_block_index })
      )
    | exn -> (
        (* Format.printf "Exception: %s\n" @@ Printexc.to_string exn ; *)
        raise exn
      )

  let process_block_commitment : t -> R.Block_commitment.t -> t = fun t bc ->
    let R.Block_commitment.{ micro_block_commitments ; rollup_id = _ } = bc in
    List.fold_left process_micro_block_commitment t
    @@ List.mapi (fun i x -> (i , x)) micro_block_commitments

  let invalid { rollup_id ; _ } = Rejection.invalid ~rollup_id
  let invalid_signature { rollup_id ; _ } = Rejection.invalid_signature ~rollup_id
  let invalid_state_hash { rollup_id ; _ } = Rejection.invalid_state_hash ~rollup_id

  module View = RA.Counter_rollup.Internal.MakePureView(CR.T.M)

end


module GenericClient = struct

  let make_add z : Main.Operation.t =
    Add z
  
  let sign_operation ~rollup_id ~counter ~account ~operation =
    let explicit_operation = Main.Operation.to_explicit ~counter ~rollup_id operation in
    let raw_content = Main.Operation.explicit_to_bytes explicit_operation in
    (* Format.printf "\nSign content: %a\n" hex raw_content ; *)
    let signature = S.(sign_hash account.secret_key @@ do_hash (Message raw_content)) in
    let signer = account.public_key in
    { content = operation ; signature ; signer }

  let sign_add ~rollup_id ~counter ~account z =
    let operation = make_add z in
    sign_operation ~account ~operation ~counter ~rollup_id

  let sign_add_int ~rollup_id ~counter ~account n =
    sign_add ~account ~counter ~rollup_id (Z.of_int n)

end


module Client = struct

  let interactive f ~(account : S.account) (t : Operator.t) =
    let Operator.{ rollup_id ; state ; _ } = t in
    let counter = BatcherView.get_counter state account.public_key in
    let counter = Z.succ counter in
    f ~rollup_id ~counter ~account

  include GenericClient
  
  let sign_operation = interactive sign_operation
  let sign_add = interactive sign_add
  let sign_add_int = interactive sign_add_int
  
end
