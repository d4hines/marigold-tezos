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

(* === aux. function === *)

let wrap e = Lwt.return (Environment.wrap_error e)

let grepr_z : Gas_limit_repr.t -> Gas_limit_repr.Arith.fp = function
  | Unaccounted -> Gas_limit_repr.Arith.fp Gas_limit_repr.Arith.zero
  | Limited x -> x.remaining

let init () =
  Context.init 1
  >>=? fun (b, _) ->
  Raw_context.prepare
    b.context
    ~level:b.header.shell.level
    ~predecessor_timestamp:b.header.shell.timestamp
    ~timestamp:b.header.shell.timestamp
    ~fitness:b.header.shell.fitness
  >>= wrap

let unit_value () : Michelson_v1_primitives.prim Micheline.canonical =
  match Script_repr.force_decode Script_repr.unit_parameter with
  | Error _ ->
      assert false
  | Ok (term, _) -> term

module Gas_Cmp = struct
  type t = Gas_limit_repr.Arith.fp

  let pp = Gas_limit_repr.Arith.pp

  let binop
    ~loc (bop : 'a -> 'a -> bool) (opmsg : string) (a : t) (b : t) =
    if bop a b then
      return_unit
    else
      failwith
        "@[@[[%s]@] - @[%a is not %s to %a@]@]"
        loc pp a opmsg pp b

  let eq ~loc a b = binop ~loc Gas_limit_repr.Arith.(=) "=" a b

  let gt ~loc a b = binop ~loc Gas_limit_repr.Arith.(>) ">" a b

  let ls ~loc a b = binop ~loc Gas_limit_repr.Arith.(<) "<" a b

  let neq ~loc a b = binop ~loc Gas_limit_repr.Arith.(<>) "<>" a b

  let geq ~loc a b = binop ~loc Gas_limit_repr.Arith.(>=) ">=" a b

  let leq ~loc a b = binop ~loc Gas_limit_repr.Arith.(<=) "<=" a b
end

(** test case:
    cache initialized as empty *)
let decache_init () =
  init ()
  >>=? fun ctx ->
  let bds = Raw_context.get_decarbonated_cache ctx in
  Assert.equal_int ~loc:__LOC__ (List.length bds) 0

(** test case:
    cache initialized as empty *)
let decache_mem () =
  init ()
  >>=? fun ctx ->
  let op_gas_bef = grepr_z (Raw_context.gas_level ctx) in
  let bl_gas_bef = Raw_context.block_gas_level ctx in
  let idz = Lazy_storage_kind.Big_map.Id.init in
  let i = Script_expr_hash.zero in
  let v = unit_value () in
  Storage.Big_map.Contents.set (ctx, idz) i v
  >|= Error.deError_monad
  >>=? fun (ctx, _) ->
  let op_gas_aft = grepr_z (Raw_context.gas_level ctx) in
  let bl_gas_aft = Raw_context.block_gas_level ctx in
  Gas_Cmp.eq ~loc:__LOC__ op_gas_bef op_gas_aft
  >>=? fun () ->
  Gas_Cmp.eq ~loc:__LOC__ bl_gas_bef bl_gas_aft

(*********************************************************************)

let tests =
  [ Test.tztest "init decarbonated cache" `Quick decache_init;
    Test.tztest "membership of decarbonated cache" `Quick decache_mem ]
