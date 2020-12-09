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

let valid (z : Saturation_repr.t) =
  let x = (z :> int) in
  x >= 0 && x <= max_int

open Saturation_repr

exception Saturating_test_error of string

let err x = Exn (Saturating_test_error x)

let small_enough (z : t) = Compare.Int.((z :> int) land 0x7fffffff80000000 = 0)

let n = of_int 123123

let m = of_int 377337

let add () =
  fail_unless
    (add saturated (of_int 1) = saturated)
    (err "saturated + 1 <> saturated")
  >>=? fun () ->
  fail_unless (add zero n = n) (err "zero + n = n")
  >>=? fun () ->
  fail_unless (add n zero = n) (err "n + zero = n")
  >>=? fun () ->
  let r = add n m in
  fail_unless
    (valid r && r = of_int ((n :> int) + (m :> int)))
    (err "add does not behave like + on small numbers.")

let sub () =
  fail_unless (sub zero n = zero) (err "zero - n <> zero")
  >>=? fun () ->
  let n = max n m and m = min n m in
  let r = sub n m in
  fail_unless
    (valid r && r = of_int ((n :> int) - (m :> int)))
    (err "sub does not behave like - on small numbers.")

let mul () =
  fail_unless
    (mul saturated saturated = saturated)
    (err "saturated * saturated <> saturated")
  >>=? fun () ->
  fail_unless (mul zero saturated = zero) (err "zero * saturated <> zero")
  >>=? fun () ->
  fail_unless (mul saturated zero = zero) (err "saturated * zero <> zero")
  >>=? fun () ->
  let max_squared = of_int (1 lsl 31) in
  let r = mul max_squared max_squared in
  fail_unless
    (valid r && r = saturated)
    (err "2 ^ 31 * 2 ^ 31 should be saturated")
  >>=? fun () ->
  let safe_squared = of_int ((1 lsl 31) - 1) in
  let r = mul safe_squared safe_squared in
  fail_unless
    (valid r && r <> saturated)
    (err "(2 ^ 31 - 1) * (2 ^ 31 - 1) should not be saturated")
  >>=? fun () ->
  let r = mul n m in
  fail_unless
    (valid r && r = of_int ((n :> int) * (m :> int)))
    (err "mul does not behave like * on small numbers.")

let tests =
  [ Test.tztest "Addition" `Quick add;
    Test.tztest "Subtraction" `Quick sub;
    Test.tztest "Multiplication" `Quick mul ]
