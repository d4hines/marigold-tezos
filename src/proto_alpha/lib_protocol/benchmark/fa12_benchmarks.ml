(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <team@marigold.dev>                           *)
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

open Tezos_raw_protocol_alpha
open Alpha_context
open Printf
open Util
open Pipeline

let set_up_fa12 :
    unit -> Block.t * (Contract.t * Contract.t * Contract.t) Pipeline.operation
    =
 fun () ->
  let (b, contracts) = Context.init 2 |> force_global_lwt in
  let alice = List.nth contracts 0 |> Option.get in
  let bob = List.nth contracts 1 |> Option.get in
  let initial_storage =
    sprintf
      {|Pair {Elt "%s" (Pair {} 100000000000000)} 100000000000000|}
      (Contract.to_b58check alice)
  in
  let pipeline =
    Origination
      {
        originator = alice;
        amount = 100;
        contract = read_file "./contracts/fa1.2.tz";
        initial_storage;
      }
    >>| fun (_, fa12) -> (fa12, alice, bob)
  in
  (b, pipeline)

let approve_fa12 token approver spender amount =
  let parameters =
    sprintf
      {|
        Pair "%s" %d
    |}
      (Contract.to_b58check spender)
      amount
  in
  Transfer
    {
      sender = approver;
      recipient = token;
      entrypoint = "approve";
      amount = 0;
      parameters;
    }

let approve_fa12_benchmark () =
  set_up_fa12 ()
  >>=! fun (_, (token, alice, bob)) ->
  let parameters =
    sprintf {|Left (Left (Left (Pair "%s" 100)))|} (Contract.to_b58check bob)
  in
  Transfer
    {
      sender = alice;
      recipient = token;
      entrypoint = "approve";
      amount = 0;
      parameters;
    }

let transfer_benchmark : unit -> Pipeline.goal =
 fun () ->
  set_up_fa12 ()
  >>=! fun (_, (token, alice, bob)) ->
  let parameters =
    sprintf
      {|Right (Pair "%s" (Pair "%s" 10))|}
      (Contract.to_b58check alice)
      (Contract.to_b58check bob)
  in
  Transfer
    {
      sender = alice;
      recipient = token;
      entrypoint = "transfer";
      amount = 0;
      parameters;
    }
