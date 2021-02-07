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

open Util
open Pipeline

let fact_benchmark : unit -> Pipeline.goal =
 fun () ->
  let (b, op) = Fa12_benchmarks.set_up_fa12 () in
  let (b, x) =
    ( b,
      op
      >>= fun (_, (_, alice, _)) ->
      (* Originate the Fact Contract *)
      let fact_contract = read_file "./contracts/fact.tz" in
      let initial_storage = "0" in
      Origination
        {
          originator = alice;
          amount = 0;
          contract = fact_contract;
          initial_storage;
        }
      >>| fun (b, fact) -> (b, alice, fact) )
  in
  (b, x)
  >>=! fun (_, (_, alice, fact)) ->
  let parameters = "100" in
  Transfer
    {
      sender = alice;
      recipient = fact;
      entrypoint = "default";
      amount = 1_000_000;
      parameters;
    }
