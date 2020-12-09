(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type t = int

include (Compare.Int : module type of Compare.Int with type t := int)

let saturated = max_int

let of_int t = if t < 0 then 0 else t

let of_z z = try of_int (Z.to_int z) with _ -> saturated

let to_z x = Z.of_int x

let zero = 0

let small_enough z = z land 0x7fffffff80000000 = 0

let mul x y =
  (* assert (x >= 0 && y >= 0); *)
  match x with
  | 0 ->
      0
  | x ->
      if small_enough x && small_enough y then x * y
      else if Compare.Int.(y > saturated / x) then saturated
      else x * y

let add x y =
  let z = x + y in
  if z < 0 then saturated else z

let sub x y =
  let s = x - y in
  if Compare.Int.(s < 0) then 0 else s

let sub_opt x y =
  let s = x - y in
  if Compare.Int.(s < 0) then None else Some s

let erem x y = x mod y

let ediv x y = (x / y, erem x y)

let z_encoding = Data_encoding.(conv to_z of_z z)

let n_encoding = Data_encoding.(conv to_z of_z n)

let pp fmt x = Format.fprintf fmt "%d" x
