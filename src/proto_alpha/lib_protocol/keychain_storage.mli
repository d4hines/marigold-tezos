(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <contact@marigold.dev>                        *)
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

type pkh = Signature.Public_key_hash.t

type pk = Signature.Public_key.t

type keychain = Keychain_repr.t

type context = Raw_context.t

type error +=
  (* Permanent *)
  | Unregistered_key_hash of pkh

val exists : Raw_context.t -> pkh -> bool Lwt.t

(** Create a new keychain with two given keys as master key and spending key
    Do nothing if there is a mapping for given key hash already *)
val init : Raw_context.t -> pkh -> pk -> pk -> context tzresult Lwt.t

(** Init keychain with manager key as master key
    Do nothing if there is a mapping for given key hash already *)
val init_with_manager :
  Raw_context.t -> pkh -> pk option -> context tzresult Lwt.t

(** Find a keychain for given key hash
    Return none if these is no mapping
    Raises {!Storage_error Corrupted_data} if the deserialisation fails. *)
val find : Raw_context.t -> pkh -> keychain option tzresult Lwt.t

(** Get the current valid master key *)
val get_master_key : context -> pkh -> pk option tzresult Lwt.t

(** Get the current valid spending key *)
val get_spending_key : context -> pkh -> pk option tzresult Lwt.t

(** Get the current forsaken keys *)
val get_forsaken_key : context -> pkh -> pk list option tzresult Lwt.t

(** Update keychain with two given keys as master key and spending key
    Raises {!Unregistered_key_hash} if the keychain is non-existing *)
val set : context -> pkh -> pk option -> pk option -> context tzresult Lwt.t

(** Update record with given master key
    Raises {!Unregistered_key_hash} if the keychain is non-existing *)
val set_master_key : context -> pkh -> pk -> context tzresult Lwt.t

(** Update record with given spending key
    Raises {!Unregistered_key_hash} if the keychain is non-existing *)
val set_spending_key : context -> pkh -> pk -> context tzresult Lwt.t

(** Remove keychain for given key hash
    Do nothing if it's non existing *)
val remove : context -> pkh -> context Lwt.t
