(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Morigold, <contact@marigold.dev>                       *)
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

(* Aux. for wrapping *)
let wrap e = Lwt.return (Environment.wrap_error e)

(* Aux. casting cost to Z, copied from gas_costs.ml *)
let cast_cost_to_z (c : Alpha_context.Gas.cost) : Z.t =
  Data_encoding.Binary.to_bytes_exn Alpha_context.Gas.cost_encoding c
  |> Data_encoding.Binary.of_bytes_exn Data_encoding.z

(** test case:
    cache initialized as empty *)
let decache_init () =
  Context.init 1
  >>=? fun (b, _) ->
  Raw_context.prepare
    b.context
    ~level:b.header.shell.level
    ~predecessor_timestamp:b.header.shell.timestamp
    ~timestamp:b.header.shell.timestamp
    ~fitness:b.header.shell.fitness
  >>= wrap
  >>=? fun ctx ->
  let bds = Raw_context.get_decarbonated_cache ctx in
  Assert.equal_int ~loc:__LOC__ (List.length bds) 0

(** test case:
    cache initialized as empty *)
let decache_mem () =
  Context.init 1
  >>=? fun (b, contracts) ->
  let contract = List.nth contracts 0 in
  Raw_context.prepare
    b.context
    ~level:b.header.shell.level
    ~predecessor_timestamp:b.header.shell.timestamp
    ~timestamp:b.header.shell.timestamp
    ~fitness:b.header.shell.fitness
  >>= wrap
  >>=? fun ctx ->
  let i = contract in
  let v = Bytes.of_string "some value" in
  Storage.Big_map.Contents.set ctx i v
  >>=? fun (ctx, _) ->
  let op_gas_bef = cast_cost_to_z (Raw_context.gas_level ctx) in
  let bl_gas_bef = cast_cost_to_z (Raw_context.block_gas_level ctx) in
  Big_map.Contents.mem ctx i
  >>=? fun (ctx, exists) ->
  let op_gas_aft = cast_cost_to_z (Raw_context.gas_level ctx) in
  let bl_gas_aft = cast_cost_to_z (Raw_context.block_gas_level ctx) in
  Assert.equal_bool ~loc:__LOC__ (Z.equal op_gas_bef op_gas_aft) true
  Assert.equal_bool ~loc:__LOC__ (Z.equal bl_gas_bef bl_gas_aft) true

(*********************************************************************)

let tests =
  [ Test.tztest "init decarbonated cache" `Quick decache_init;
    Test.tztest "membership of decarbonated cache" `Quick decache_mem ]
