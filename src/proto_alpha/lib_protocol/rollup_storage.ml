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

open Rollup_repr

let (let*) = (>>=?)
let (let*=) x f = x >>= f

module Merkle = struct
  let empty : bytes = Bytes.of_string ""
end

module Main = struct

  type t = Raw_context.t
  type contract = Contract_repr.t
  
  let init c =
    Storage.Rollups.Global_counter.init c Z.zero

  let increment_counter ctxt =
    let* counter = Storage.Rollups.Global_counter.get ctxt in
    let counter = Z.succ counter in
    let* ctxt = Storage.Rollups.Global_counter.update ctxt counter in
    return (counter , ctxt)

  type rollup_creation_internal_result = {
    id : Storage.Rollups.Global_counter.value ;
  }

  let genesis_block : Block_onchain_content.t = {
    transactions = [] ;
    aggregated_signature = Signature Bls12_381.G2.zero ;
    events = [] ;
    before_root = Root Merkle.empty ;
    after_root = Root Merkle.empty ;
    tezos_level = Level_repr.root_level @@ Raw_level_repr.root ;
  }
  
  let create_rollup ~operator ~kind ctxt =
    let* (id , ctxt) = increment_counter ctxt in
    let level = Z.zero in
    let* ctxt = Storage.Rollups.Rollup_content.init ctxt id {
        operator ;
        level ;
        kind ;
      } in
    let* ctxt = Storage.Rollups.Block_content.init ctxt (id , level) genesis_block in
    return ({ id } , ctxt)

  type block_commitment_result = unit
  
  let commit_block block_commitment ctxt ~operator =
    let Block_commitment.{ transactions ; aggregated_signature ; after_root ; rollup_id } = block_commitment in
    let* rollup =
      let* r_opt = Storage.Rollups.Rollup_content.find ctxt rollup_id in
      match r_opt with
      | Some r -> return r
      | None -> failwith "Non-existent rollup at given id" (* TODO: Add error *)
    in
    let Rollup_onchain_content.{ operator = r_operator ; level } = rollup in
    let* () =
      if Contract_repr.equal r_operator operator
      then return ()
      else failwith "Commit not by rollup operator" (* TODO: Add error *)
    in
    let* last_block = Storage.Rollups.Block_content.get ctxt (rollup_id , level) in
    let* () =
      if Level_repr.(<=) (Raw_context.current_level ctxt) last_block.tezos_level
      then return ()
      else failwith "Rollup block commitment too early" (* TODO: Add error *)
    in
    let level = Z.succ level in
    let new_rollup = { rollup with level } in
    let* ctxt = Storage.Rollups.Rollup_content.update ctxt rollup_id new_rollup in
    let block = Block_onchain_content.{
      transactions ; aggregated_signature ; after_root ;
      before_root = last_block.after_root ;
      events = [] ;
      tezos_level = Raw_context.current_level ctxt ;
    } in
    let*= ctxt = Storage.Rollups.Block_content.add ctxt (rollup_id , level) block in
    return (() , ctxt)

  let get_block ctxt rollup_id level =
    let* opt = Storage.Rollups.Block_content.find ctxt (rollup_id , level) in
    match opt with
    | Some b -> return b
    | None -> failwith "invalid rollup x block id" (* TODO: Add error *)

  let get_rollup ctxt rollup_id =
    let* opt = Storage.Rollups.Rollup_content.find ctxt rollup_id in
    match opt with
    | Some r -> return r
    | None -> failwith "invalid rollup id" (* TODO: Add error *)

  (* Used for tests, debugging, etc. *)
  module Dev = struct

    let get_counter ctxt =
      Storage.Rollups.Global_counter.get ctxt

  end

end

include Main

module type TYPE = sig

  type t
  type contract
  type nonrec rollup_creation_internal_result = rollup_creation_internal_result

  val init : t -> t tzresult Lwt.t
  val create_rollup : operator:contract -> t -> (rollup_creation_internal_result * t) tzresult Lwt.t
  val commit_block : Block_commitment.t -> t -> operator:contract -> (block_commitment_result * t) tzresult Lwt.t
  val get_block : t -> Z.t -> Z.t -> Block_onchain_content.t tzresult Lwt.t
  val get_rollup : t -> Z.t -> Rollup_onchain_content.t tzresult Lwt.t
  
  module Dev : sig
    val get_counter : t -> Z.t tzresult Lwt.t
  end

end
