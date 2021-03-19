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
      Unregistered_key_hash of Signature.Public_key_hash.t

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"keychain.unregistered_key_hash"
    ~title:"Unregistered key_hash"
    ~description:"There is no keychain for given public key hash"
    ~pp:(fun ppf k ->
      Format.fprintf
        ppf
        "Cannot find any keychain for the provided public key (with hash %a)."
        Signature.Public_key_hash.pp
        k)
    (obj1 (req "hash" Signature.Public_key_hash.encoding))
    (function Unregistered_key_hash h -> Some h | _ -> None)
    (fun h -> Unregistered_key_hash h)

let exists ctx pkh = Storage.Keychain.mem ctx pkh

let init ctx pkh keychain = Storage.Keychain.init ctx pkh keychain

let find ctx pkh = Storage.Keychain.find ctx pkh

let get_consensus_key ctx pkh =
  exists ctx pkh
  >>= fun existing ->
  if existing then
    Storage.Keychain.get ctx pkh >|=? fun {consensus_key} -> consensus_key
  else fail (Unregistered_key_hash pkh)

let get_spending_key ctx pkh =
  exists ctx pkh
  >>= fun existing ->
  if existing then
    Storage.Keychain.get ctx pkh >|=? fun {spending_key} -> spending_key
  else fail (Unregistered_key_hash pkh)

let set ctx pkh keychain =
  exists ctx pkh
  >>= fun existing ->
  if existing then Storage.Keychain.update ctx pkh keychain
  else fail (Unregistered_key_hash pkh)

let set_consensus_key ctx pkh consensus_key =
  exists ctx pkh
  >>= fun existing ->
  if existing then
    Storage.Keychain.get ctx pkh
    >>=? fun ks -> Storage.Keychain.update ctx pkh {ks with consensus_key}
  else fail (Unregistered_key_hash pkh)

let set_spending_key ctx pkh spending_key =
  exists ctx pkh
  >>= fun existing ->
  if existing then
    Storage.Keychain.get ctx pkh
    >>=? fun ks -> Storage.Keychain.update ctx pkh {ks with spending_key}
  else fail (Unregistered_key_hash pkh)

let remove ctx pkh = Storage.Keychain.remove ctx pkh
