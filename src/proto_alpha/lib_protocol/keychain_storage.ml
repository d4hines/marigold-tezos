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

open Keychain_repr

type pkh = Signature.Public_key_hash.t

type pk = Signature.Public_key.t

type keychain = Keychain_repr.t

type context = Raw_context.t

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

let init ctx pkh consensus_key spending_key =
  let next_consensus_key = Keychain_repr.No_next_key in
  Storage.Keychain.init
    ctx
    pkh
    {consensus_key; next_consensus_key ;spending_key}

let init_with_manager ctx pkh pk_opt =
  Contract_storage.get_manager_key ctx pkh
  >>=? fun (manager_key) ->
  let spending_key =
    match pk_opt with
    | None -> manager_key
    | Some spending_key -> spending_key
  in init ctx pkh manager_key spending_key

let consensus_key_update_internal ctx keychain : Keychain_repr.t =
  match keychain.next_consensus_key with
  | No_next_key -> keychain
  | Delay update ->
    let current_cycle = (Level_storage.current ctx).cycle in
    if Cycle_repr.(current_cycle <= update.activate_cycle) then
      let consensus_key = update.pending_key in
      let next_consensus_key = No_next_key in
      {keychain with consensus_key; next_consensus_key}
    else keychain

let find ctx pkh =
  Storage.Keychain.find ctx pkh
  >>=? function
  | None -> return None
  | Some keychain ->
    let updated_keychain = consensus_key_update_internal ctx keychain in
    return @@ Some updated_keychain

let get_consensus_key ctx pkh =
  exists ctx pkh
  >>= fun existing ->
  if existing then
    Storage.Keychain.get ctx pkh
    >|=? fun keychain ->
    let updated_keychain = consensus_key_update_internal ctx keychain in
    Some updated_keychain.consensus_key
  else return_none

let get_spending_key ctx pkh =
  exists ctx pkh
  >>= fun existing ->
  if existing then
    Storage.Keychain.get ctx pkh
    >|=? fun keychain ->
    let updated_keychain = consensus_key_update_internal ctx keychain in
    Some updated_keychain.spending_key
  else return_none

let setup_next_key_internal ctx keychain pending_key : Keychain_repr.t =
  let delayed_cycles = (Raw_context.constants ctx).preserved_cycles in
  let current_cycle = (Level_storage.current ctx).cycle in
  let activate_cycle = Cycle_repr.add current_cycle delayed_cycles in
  let update = Keychain_repr.{ activate_cycle; pending_key} in
  let next = Keychain_repr.Delay update in
  {keychain with Keychain_repr.next_consensus_key = next}

let set ctx pkh consensus_key_opt spending_key_opt =
  exists ctx pkh
  >>= fun existing ->
  if existing then
    Storage.Keychain.get ctx pkh
    >>=? fun keychain ->
    let keychain = match consensus_key_opt, spending_key_opt with
      | None, None -> keychain
      | None, Some spending_key -> {keychain with spending_key}
      | Some consensus_key, None ->
        setup_next_key_internal ctx keychain consensus_key
      | Some consensus_key, Some spending_key ->
        setup_next_key_internal ctx {keychain with spending_key} consensus_key
    in Storage.Keychain.update ctx pkh keychain
  else fail (Unregistered_key_hash pkh)

let set_consensus_key ctx pkh consensus_key =
  exists ctx pkh
  >>= fun existing ->
  if existing then
    Storage.Keychain.get ctx pkh
    >>=? fun keychain ->
    let keychain_next = setup_next_key_internal ctx keychain consensus_key in
    Storage.Keychain.update ctx pkh keychain_next
  else fail (Unregistered_key_hash pkh)

let set_spending_key ctx pkh spending_key =
  exists ctx pkh
  >>= fun existing ->
  if existing then
    Storage.Keychain.get ctx pkh
    >>=? fun ks -> Storage.Keychain.update ctx pkh {ks with spending_key}
  else fail (Unregistered_key_hash pkh)

let remove ctx pkh = Storage.Keychain.remove ctx pkh
