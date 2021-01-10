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

let contract_trace_failwith = read_file "contracts/trace_failwith.tz"

let contract_trace_failwith_gas = read_file "contracts/trace_failwith_gas.tz"

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
    ~storage:"0"
    ~parameter:"1"
    ()
  >|=? fun {events; _} ->
  let event = match events with [event] -> event | _ -> assert false in
  check_event
    ~topic:"add"
    ~ty:"(pair (int @parameter.left.add) (int @storage))"
    ~data:"(Pair 1 0)"
    event

let test_failwith_events () =
  test_context ()
  >>=? fun ctxt ->
  run_script ctxt contract_trace_failwith ~storage:"0" ~parameter:"1" ()
  >|=? fun {events; _} ->
  let event = match events with [event] -> event | _ -> assert false in
  check_event ~topic:"input" ~ty:"int" ~data:"1" event

(* failwith_gas is failwith with an additional CAR inside of the FAILWITH block *)
let test_failwith_gas_events () =
  let open Alpha_context in
  test_context ()
  >>=? fun ctxt ->
  run_script ctxt contract_trace_failwith ~storage:"0" ~parameter:"1" ()
  >>=? fun {ctxt = failwith_ctxt; _} ->
  run_script ctxt contract_trace_failwith_gas ~storage:"0" ~parameter:"1" ()
  >>=? fun {ctxt = failwith_gas_ctxt; _} ->
  Gas.consume failwith_ctxt Michelson_v1_gas.Cost_of.Interpreter.car
  >|? (fun failwith_ctxt_plus_car ->
        assert (
          Gas.Arith.(
            Gas.consumed ~since:ctxt ~until:failwith_gas_ctxt
            = Gas.consumed ~since:ctxt ~until:failwith_ctxt_plus_car) ))
  |> Environment.wrap_error |> Lwt.return

let tests =
  [ Test.tztest "test_events_are_emitted" `Quick test_events_are_emitted;
    Test.tztest "test_failwith_events" `Quick test_failwith_events;
    Test.tztest "test_failwith_gas_events" `Quick test_failwith_gas_events ]
