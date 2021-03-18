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

type error +=
  | (* Permanent *)
    Unregistered_key_hash of pkh

val exists : Raw_context.t -> pkh -> bool Lwt.t

(** Create a new record for keychain.
    Do nothing if there is a mapping for given key hash already *)
val init : Raw_context.t -> pkh -> keychain -> Raw_context.t tzresult Lwt.t

(** Raises {!Storage_error Corrupted_data} if the deserialisation fails. *)
val find : Raw_context.t -> pkh -> keychain option tzresult Lwt.t

(** Raises {!Unregistered_key_hash} if the keychain is non-existing *)
val get_consensus_key : Raw_context.t -> pkh -> pk tzresult Lwt.t

(** Raises {!Unregistered_key_hash} if the keychain is non-existing *)
val get_spending_key : Raw_context.t -> pkh -> pk tzresult Lwt.t

(** Update record with given keychain
    Raises {!Unregistered_key_hash} if the keychain is non-existing *)
val set : Raw_context.t -> pkh -> keychain -> Raw_context.t tzresult Lwt.t

(** Update record with given consensus key
    Raises {!Unregistered_key_hash} if the keychain is non-existing *)
val set_consensus_key :
  Raw_context.t ->
  pkh ->
  pk ->
  Raw_context.t tzresult Lwt.t

(** Update record with given spending key
    Raises {!Unregistered_key_hash} if the keychain is non-existing *)
val set_spending_key :
  Raw_context.t ->
  pkh ->
  pk ->
  Raw_context.t tzresult Lwt.t

(** Remove keychain for given key hash
    Do nothing if it's non existing *)
val remove : Raw_context.t -> pkh -> Raw_context.t Lwt.t
