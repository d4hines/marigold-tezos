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

include RA.Counter_rollup

module Make() = struct

  module Context = struct
    include RA.Make_basic_rollup(RA.Counter_rollup)
  end

  let get_root () = R.Root (Context.get_hash ())
  
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
    let previous_block = !current_block in
    current_block := empty_block (Z.succ previous_block.block_level) ;
    previous_block
    
  
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
      
end

module Client = struct

  let make_operation ?nonce ~content ~block_level =
    RA.Operation_replay.{ content ; block_level ; nonce }
  
  let sign_operation ~account ~operation =
    let content =
      Data_encoding.Binary.to_bytes_exn operation_encoding operation
    in
    let signature = S.(sign_hash account.secret_key @@ do_hash (Message content)) in
    let signer = account.public_key in
    { content ; signature ; signer }

  
end


let x = 42
