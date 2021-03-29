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

type block_commitment_internal_result = {
  level : Z.t ;
}

let genesis_micro_block : Block_onchain_content.micro = {
  parameter = Bytes.empty ;
  events = [] ;
  before_root = Root Merkle.empty ;
  after_root = Root Merkle.empty ;
}

let genesis_block : Block_onchain_content.t = {
  micro_blocks = [ genesis_micro_block ] ;
  tezos_level = Level_repr.root_level @@ Raw_level_repr.root ;
  timestamp = Time_repr.of_seconds Int64.zero ;
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

let commit_block block_commitment ctxt ~operator =
  let Block_commitment.{ micro_block_commitments ; rollup_id } = block_commitment in
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
    if Level_repr.(>) (Raw_context.current_level ctxt) last_block.tezos_level
    then return ()
    else (
      let debug = Format.asprintf "last rollup level : %a\tcurrent level : %a\n" Level_repr.pp last_block.tezos_level Level_repr.pp (Raw_context.current_level ctxt) in
      failwith (debug ^ "Rollup block commitment too early. ie, multiple commitments on the same block, or commitment on the same block as when rollup is created") (* TODO: Add error *)
    )
  in
  let last_root =
    let last_micro_block =
      match List.(nth_opt last_block.micro_blocks ((length last_block.micro_blocks) - 1)) with
      | Some x -> x
      | _ -> assert false       (* There should not be empty blocks *)
    in
    last_micro_block.after_root
  in
  let level = Z.succ level in
  let new_rollup = { rollup with level } in
  let* ctxt = Storage.Rollups.Rollup_content.update ctxt rollup_id new_rollup in
  let micro_blocks =
    let rec aux last_root = function
      | [] -> []
      | hd :: tl -> (
          let Block_commitment.{
              parameter ; after_root
            } = hd in
          let hd' = Block_onchain_content.{
              parameter ; after_root ;
              before_root = last_root ;
              events = [] ;
            } in
          hd' :: (aux after_root tl)
        )
    in
    aux last_root micro_block_commitments
  in
  let block = Block_onchain_content.{
      micro_blocks ;
      tezos_level = Raw_context.current_level ctxt ;
      timestamp = Raw_context.current_timestamp ctxt ;
    } in
  let*= ctxt = Storage.Rollups.Block_content.add ctxt (rollup_id , level) block in
  return ({ level } , ctxt)

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

let reorg_rollup ctxt ~id ~level =
  let* rollup = get_rollup ctxt id in
  let rec aux_indices n =
    if Compare.Z.(n <= rollup.level)
    then n :: (aux_indices Z.(add n one))
    else []
  in
  let indices = aux_indices level in
  let aux ctxt i = Storage.Rollups.Block_content.remove ctxt (id , i) >>= return in
  let* ctxt = Error_monad.fold_left_s aux ctxt indices in
  return (indices , ctxt)

(* Used for tests, debugging, etc. *)
module Dev = struct

  let get_counter ctxt =
    Storage.Rollups.Global_counter.get ctxt

end
