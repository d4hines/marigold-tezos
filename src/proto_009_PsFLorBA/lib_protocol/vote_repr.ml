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

type proposal = Protocol_hash.t

type ballot = Yay | Nay | Pass

let ballot_encoding =
  let of_int8 = function
    | 0 ->
        Yay
    | 1 ->
        Nay
    | 2 ->
        Pass
    | _ ->
        invalid_arg "ballot_of_int8"
  in
  let to_int8 = function Yay -> 0 | Nay -> 1 | Pass -> 2 in
  let open Data_encoding in
  (* union *)
  splitted
    ~binary:(conv to_int8 of_int8 int8)
    ~json:(string_enum [("yay", Yay); ("nay", Nay); ("pass", Pass)])

let of_string = function
  | "y" | "Y" | "yay" ->
      Some Yay
  | "n" | "N" | "nay" ->
      Some Nay
  | "p" | "P" | "pass" ->
      Some Pass
  | _ ->
      None

let to_string = function Yay -> "Y" | Nay -> "N" | Pass -> "P"
