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
open Gas_limit_repr

(* Aux. for wrapping *)
let wrap e = Lwt.return (Environment.wrap_error e)

let grepr_z : Gas_limit_repr.t -> Gas_limit_repr.Arith.fp = function
  | Unaccounted -> Gas_limit_repr.Arith.fp Gas_limit_repr.Arith.zero
  | Limited x -> x.remaining

let binop_gas_arith
  ~loc binop msg (a : Gas_limit_repr.Arith.fp) (b : Gas_limit_repr.Arith.fp) =
  Assert.equal
    ~loc binop msg Gas_limit_repr.Arith.pp a b

let geq_gas_arith
  ~loc (a : Gas_limit_repr.Arith.fp) (b : Gas_limit_repr.Arith.fp) =
  binop_gas_arith
    ~loc Gas_limit_repr.Arith.(>=)
    "Gas aren't less than or equal" a b

let leq_gas_arith
  ~loc (a : Gas_limit_repr.Arith.fp) (b : Gas_limit_repr.Arith.fp) =
  binop_gas_arith
    ~loc Gas_limit_repr.Arith.(<=)
    "Gas aren't less than or equal" a b

let eq_gas_arith
  ~loc (a : Gas_limit_repr.Arith.fp) (b : Gas_limit_repr.Arith.fp) =
  binop_gas_arith
    ~loc Gas_limit_repr.Arith.(=)
    "Gas aren't equal" a b

let gt_gas_arith
  ~loc (a : Gas_limit_repr.Arith.fp) (b : Gas_limit_repr.Arith.fp) =
  binop_gas_arith
    ~loc Gas_limit_repr.Arith.(>)
    "Gas aren't greater" a b

let ls_gas_arith
  ~loc (a : Gas_limit_repr.Arith.fp) (b : Gas_limit_repr.Arith.fp) =
  binop_gas_arith
    ~loc Gas_limit_repr.Arith.(<)
    "Gas aren't less" a b

let xxx () =
  Lwt.Return (Script_repr.force_decode Script_repr.unit_parameter)

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
  (* >>=? fun (b, contracts) -> *)
  >>=? fun (b, _) ->
  (* let contract = List.nth contracts 0 in *)
  Raw_context.prepare
    b.context
    ~level:b.header.shell.level
    ~predecessor_timestamp:b.header.shell.timestamp
    ~timestamp:b.header.shell.timestamp
    ~fitness:b.header.shell.fitness
  >>= wrap
  >>=? fun ctx ->
  let op_gas_bef = grepr_z (Raw_context.gas_level ctx) in
  let bl_gas_bef = Raw_context.block_gas_level ctx in
  (* === 0:index === *)
  let idz = Lazy_storage_kind.Big_map.Id.init in
  (* === 1:index === *)
  (* let i = Script_expr_hash.encoding sexpr in *)
  let i = Script_expr_hash.zero in
  (* === 2:value === *)
  xxx ()
  >>=? fun (v, _) ->
  (* === EXEC === *)
  Storage.Big_map.Contents.set (ctx, idz) i v
  >>=? fun (ctx, _) ->
  (* Gas_limit_repr.Arith *)
  (* Storage.Big_map.Contents.mem ctx i *)
  (* >>=? fun (ctx, exists) -> *)
  let op_gas_aft = grepr_z (Raw_context.gas_level ctx) in
  let bl_gas_aft = Raw_context.block_gas_level ctx in
  eq_gas_arith ~loc:__LOC__ op_gas_bef op_gas_aft
  >>=? fun () ->
  eq_gas_arith ~loc:__LOC__ bl_gas_bef bl_gas_aft

(*********************************************************************)

let tests =
  [ Test.tztest "init decarbonated cache" `Quick decache_init;
    Test.tztest "membership of decarbonated cache" `Quick decache_mem ]
