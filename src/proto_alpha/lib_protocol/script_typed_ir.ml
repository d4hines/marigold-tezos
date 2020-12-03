(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

module type Boxed_set = sig
  type elt

  type elt_ty

  val elt_ty : elt_ty

  module OPS : S.SET with type elt = elt

  val boxed : OPS.t

  val size : int
end

module type Boxed_map = sig
  type key

  type value

  type key_ty

  val key_ty : key_ty

  module OPS : S.MAP with type key = key

  val boxed : value OPS.t * int
end

include Script_typed_ir_functor.Make (struct
  include Alpha_context
  module Operation = Alpha_context
  module Chain_id = Chain_id
  module Signature = Signature
  module Bls12_381 = Bls12_381

  type ('elt_ty, 'elt) set =
    (module Boxed_set with type elt = 'elt and type elt_ty = 'elt_ty)

  type ('key_ty, 'key, 'value) map =
    (module Boxed_map
       with type key = 'key
        and type value = 'value
        and type key_ty = 'key_ty)
end)
