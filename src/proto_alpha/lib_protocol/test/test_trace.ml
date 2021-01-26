(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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
open Interpreter

let contract_trace_arith = read_file "contracts/trace_arith.tz"

let check_event ~topic ~ty ~data event =
  let open Alcotest in
  let open Alpha_context.Event in
  check string "topic" event.topic topic ;
  check string "ty" (expression_to_string event.ty) ty ;
  check string "data" (expression_to_string event.data) data

let test_events_are_emitted () =
  test_context ()
  >>=? fun ctx ->
  run_script
    ctx
    contract_trace_arith
    ~entrypoint:"add"
    ~storage:"1"
    ~parameter:"2"
    ()
  >|=? fun {events; _} ->
  match events with
  | [operation; output] ->
      check_event
        ~topic:"add"
        ~ty:"(pair (int @parameter.left.add) (int @storage))"
        ~data:"(Pair 2 1)"
        operation ;
      check_event ~topic:"output" ~ty:"int" ~data:"3" output
  | _ ->
      assert false

let tests =
  [Test.tztest "test_events_are_emitted" `Quick test_events_are_emitted]
