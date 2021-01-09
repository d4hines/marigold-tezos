(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Testing
    -------
    Component:  Protocol (transfer)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/main.exe -- test "^Lookup transaction hashes in blocks$"
    Subject:    Test functioning of michelson opcode IS_TX_INCLUDED
*)

open Protocol
open Alpha_context
open Test_tez

let test_context () =
  Context.init 2
  >|=? (function
         | (_, []) | (_, [_]) ->
             assert false
         | (b, contract_1 :: contract_2 :: _) ->
             (b, contract_1, contract_2))
  >>=? fun (b, c1, c2) ->
  Incremental.begin_construction b >>=? fun v -> return (v, c1, c2)

let default_source = Contract.implicit_contract Signature.Public_key_hash.zero

let default_step_constants =
  Script_interpreter.
    {
      source = default_source;
      payer = default_source;
      self = default_source;
      amount = Tez.zero;
      chain_id = Chain_id.zero;
    }

(** Helper function that parses and types a script, its initial storage and
   parameters from strings. It then executes the typed script with the storage
   and parameter and returns the result. *)
let run_script ctx ?(step_constants = default_step_constants) contract
    ?(entrypoint = "default") ~storage ~parameter () =
  let contract_expr = Expr.from_string contract in
  let storage_expr = Expr.from_string storage in
  let parameter_expr = Expr.from_string parameter in
  let script =
    Script.{code = lazy_expr contract_expr; storage = lazy_expr storage_expr}
  in
  Script_interpreter.execute
    ctx
    Readable
    step_constants
    ~script
    ~entrypoint
    ~parameter:parameter_expr
    ~internal:false
  >|= Environment.wrap_error

let test_bad_contract_parameter () =
  test_context ()
  >>=? fun (incr, contract1, contract2) ->
  Op.transaction (I incr) ~fee:Tez.zero contract1 contract2 Tez.one
  >>=? fun op ->
  Incremental.add_operation incr op
  >>=? fun incr ->
  run_script
    (Incremental.alpha_ctxt incr)
    {|
 {parameter operation_hash; storage (option bool); code { CAR; IS_TX_INCLUDED; SOME; NIL operation; PAIR }}    
|}
    ~storage:"None"
    ~parameter:
      ( Operation.hash_packed op |> Operation_hash.to_b58check
      |> Printf.sprintf "\"%s\"" )
    ()
  >>=? fun res ->
  Assert.equal
    ~loc:__LOC__
    ( = )
    "Expected IS_TX_INCLUDED to find the provided transaction hash"
    Michelson_v1_printer.print_expr
    res.storage
    (Expr.from_string "(Some True)")
  >>=? fun () ->
  Incremental.finalize_block incr
  >>=? fun new_pred_blk ->
  Incremental.begin_construction new_pred_blk
  >>=? fun incr ->
  run_script
    (Incremental.alpha_ctxt incr)
    {|
 {parameter operation_hash; storage (option bool); code { CAR; IS_TX_INCLUDED; SOME; NIL operation; PAIR }}    
|}
    ~storage:"None"
    ~parameter:
      ( Operation.hash_packed op |> Operation_hash.to_b58check
      |> Printf.sprintf "\"%s\"" )
    ()
  >>=? fun res ->
  Assert.equal
    ~loc:__LOC__
    ( = )
    "Expected IS_TX_INCLUDED to find the provided transaction hash in one of \
     the predecessor blocks"
    Michelson_v1_printer.print_expr
    res.storage
    (Expr.from_string "(Some True)")

(*(some False)*)
let tests =
  [ Test.tztest
      "IS_TX_INCLUDED must correctly look up a transaction hash"
      `Quick
      test_bad_contract_parameter ]
