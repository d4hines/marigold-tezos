(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
open Alpha_context

exception Expression_from_string

let from_string str : Script.expr =
  let (ast, errs) = Michelson_v1_parser.parse_expression ~check:false str in
  ( match errs with
  | [] ->
      ()
  | lst ->
      Format.printf "expr_from_string: %a\n" Error_monad.pp_print_error lst ;
      raise Expression_from_string ) ;
  ast.expanded

let ty_from_expr alpha_context expr =
  let node = Micheline.root expr in
  let (Ex_ty ty, alpha_context) =
    Script_ir_translator.parse_ty
      alpha_context
      ~legacy:false
      ~allow_lazy_storage:false
      ~allow_operation:false
      ~allow_ticket:false
      ~allow_contract:false
      node
    |> Util.force
  in
  let (ty_expr, alpha_context) =
    Script_ir_translator.unparse_ty alpha_context ty |> Util.force
  in
  (alpha_context, ty_expr |> Micheline.strip_locations)

let ty_from_string alpha_context str =
  str |> from_string |> ty_from_expr alpha_context
