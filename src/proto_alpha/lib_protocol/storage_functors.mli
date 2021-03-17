(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** Tezos Protocol Implementation - Typed storage builders. *)

open Storage_sigs

module Registered : REGISTER

module Ghost : REGISTER

module Make_subcontext (R : REGISTER) (C : Raw_context.T) (N : NAME) :
  Raw_context.T with type t = C.t

module Make_single_data_storage
    (R : REGISTER)
    (C : Raw_context.T)
    (N : NAME)
    (V : VALUE) : Single_data_storage with type t = C.t and type value = V.t

module Pair (I1 : INDEX) (I2 : INDEX) : INDEX with type t = I1.t * I2.t

module Make_data_set_storage (C : Raw_context.T) (I : INDEX) :
  Data_set_storage with type t = C.t and type elt = I.t

module Make_carbonated_data_set_storage (C : Raw_context.T) (I : INDEX) :
  Carbonated_data_set_storage with type t = C.t and type elt = I.t

module Make_indexed_data_storage (C : Raw_context.T) (I : INDEX) (V : VALUE) :
  Indexed_data_storage
    with type t = C.t
     and type key = I.t
     and type value = V.t

module Make_indexed_carbonated_data_storage
    (C : Raw_context.T)
    (I : INDEX)
    (V : VALUE) :
  Non_iterable_indexed_carbonated_data_storage
    with type t = C.t
     and type key = I.t
     and type value = V.t

module Make_indexed_data_snapshotable_storage
    (C : Raw_context.T)
    (Snapshot : INDEX)
    (I : INDEX)
    (V : VALUE) :
  Indexed_data_snapshotable_storage
    with type t = C.t
     and type snapshot = Snapshot.t
     and type key = I.t
     and type value = V.t

module Make_indexed_subcontext (C : Raw_context.T) (I : INDEX) :
  Indexed_raw_context
    with type t = C.t
     and type key = I.t
     and type 'a ipath = 'a I.ipath

module Nest_indexed_raw_context(C : Indexed_raw_context)(I' : INDEX) :
  Indexed_raw_context with type t = C.t
                       and type key = (C.key * I'.t)
                       and type 'a ipath = 'a C.I.ipath I'.ipath


module type WRAPPER = sig
  type t

  type key

  val wrap : t -> key

  val unwrap : key -> t option
end

module Wrap_indexed_data_storage
    (C : Indexed_data_storage)
    (K : WRAPPER with type key := C.key) :
  Indexed_data_storage
    with type t = C.t
     and type key = K.t
     and type value = C.value
