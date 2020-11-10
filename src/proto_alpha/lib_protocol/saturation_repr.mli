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

(** This module provides saturated arithmetic between 0 and 2^62 - 1.

   This means that the arithmetic operations provided by this module
   do not overflow. If an operation would produce an integer [x]
   greater than [2 ^ 62 - 1], it is [saturated] to this
   value. Similarly, if an operation would produce a negative integer,
   it outputs [zero] instead.

   This saturated arithmetic is used to monitor gas levels. While the
   gas model can produce values beyond 2^62 - 1, there is no point in
   distinguishing these values from 2^62 - 1 because the amount of gas
   available is significantly lower than this limit.

*)

(** An integer of type [t] is between [0] and [saturated]. *)
type t = private int

(** 0 *)
val zero : t

(** 2^62 - 1 *)
val saturated : t

(** We inherit the order over native integers. *)
include
  Compare.S with type t := t

(** [mul x y] behaves like multiplication between native integers as
   long as its result stay below [saturated]. Otherwise, [mul] returns
   [saturated]. *)
val mul : t -> t -> t

(** [add x y] behaves like addition between native integers as long as
   its result stay below [saturated]. Otherwise, [add] returns
   [saturated]. *)
val add : t -> t -> t

(** [sub x y] behaves like subtraction between native integers as long
   as its result stay positive. Otherwise, [sub] returns [zero]. *)
val sub : t -> t -> t

(** [sub_opt x y] behaves like subtraction between native integers as
   long as its result stay positive. Otherwise, [sub] returns
   [None]. *)
val sub_opt : t -> t -> t option

(** [ediv x y] returns [x / y] and [x mod y]. These operations never
   saturate, hence they are exactly the same as their native
   counterparts. *)
val ediv : t -> t -> t * t

(** [erem x y] returns [x mod y]. *)
val erem : t -> t -> t

(** [of_int x] returns [max zero (min saturated x)]. *)
val of_int : int -> t

(** [of_z z] returns [Z.(max (of_int zero) (min (of_int saturated x))]. *)
val of_z : Z.t -> t

(** [of_z z] is [Z.of_int]. *)
val to_z : t -> Z.t

(** An encoder for native integers. *)
val encoding : t Data_encoding.t

(** A pretty-printer for native integers. *)
val pp : Format.formatter -> t -> unit
