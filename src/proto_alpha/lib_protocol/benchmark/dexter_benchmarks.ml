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

open Printf
open Util
open Pipeline
open Tezos_raw_protocol_alpha
open Alpha_context

let xtzToToken_benchmark : unit -> Pipeline.goal =
 fun () ->
  let (b, op) = Fa12_benchmarks.set_up_fa12 () in
  let (b, x) =
    ( b,
      op
      >>= fun (_, (token, alice, bob)) ->
      (* Originate the Dexter Contract *)
      let dexter_contract = read_file "./contracts/dexter.tz" in
      let initial_storage =
        sprintf
          {|Pair {}
            (Pair
             (Pair (Pair False 0)
                   (Pair "%s" False))
             (Pair (Pair "%s" 0) 0))
          |}
          (Contract.to_b58check alice)
          (Contract.to_b58check token)
      in
      Origination
        {
          originator = alice;
          amount = 0;
          contract = dexter_contract;
          initial_storage;
        }
      (* Approve the spend *)
      >>= fun (_, dexter) ->
      Fa12_benchmarks.approve_fa12 token alice dexter 1_000_000
      (* Add Liquidity *)
      >>= fun (_, _) ->
      let parameters =
        sprintf
          {|
                Pair
                  (Pair "%s" 1)
                  (Pair 100000 "2030-01-01T12:00:00Z")
            |}
          (Contract.to_b58check alice)
      in
      Transfer
        {
          sender = alice;
          recipient = dexter;
          entrypoint = "addLiquidity";
          amount = 5_000_000;
          parameters;
        }
      >>| fun (b, _) -> (b, bob, dexter) )
  in
  (b, x)
  >>=! fun (_, (_, bob, dexter)) ->
  (* Do xtzToToken, exchanging 1 of bob's tez for token,
     sending the results to bob *)
  let parameters =
    sprintf
      {|
    Right
      (Right
        (Pair
          "%s"
          (Pair 1 "2050-01-29T18:00:00Z")))|}
      (Contract.to_b58check bob)
  in
  Transfer
    {
      sender = bob;
      recipient = dexter;
      entrypoint = "default";
      amount = 1_000_000;
      parameters;
    }
