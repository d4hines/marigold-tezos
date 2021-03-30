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
    Unregistered_key_hash of  Signature.Public_key_hash.t
  | (* Permanent *)
    Key_update_conflict
  | (* Permanent *)
    Key_update_forsaken

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
    (fun h -> Unregistered_key_hash h) ;
  register_error_kind
    `Permanent
    ~id:"keychain.Key_update_conflict"
    ~title:"???"
    ~description:"???"
    ~pp:(fun ppf () ->
        Format.fprintf
          ppf "Cannot update master key with the same master key itself.")
    Data_encoding.unit
    (function Key_update_conflict -> Some () | _ -> None)
    (fun () -> Key_update_conflict) ;
  register_error_kind
    `Permanent
    ~id:"keychain.Key_update_forsaken"
    ~title:"???"
    ~description:"???"
    ~pp:(fun ppf () ->
        Format.fprintf
          ppf "Cannot update master key with a forsaken key.")
    Data_encoding.unit
    (function Key_update_forsaken -> Some () | _ -> None)
    (fun () -> Key_update_forsaken)

let exists ctx pkh = Storage.Keychain.mem ctx pkh

let init ctx pkh master_key spending_key =
  let next_master_key = Keychain_repr.No_next_key in
  Storage.Keychain.init
    ctx
    pkh
    {master_key; next_master_key ;spending_key; forsaken_key = []}

let init_with_manager ctx pkh pk_opt =
  Contract_storage.get_manager_key ctx pkh
  >>=? fun (manager_key) ->
  let spending_key =
    match pk_opt with
    | None -> manager_key
    | Some spending_key -> spending_key
  in init ctx pkh manager_key spending_key

let master_key_update_internal ctx keychain : Keychain_repr.t =
  match keychain.next_master_key with
  | No_next_key -> keychain
  | Delay update ->
    let current_cycle = (Level_storage.current ctx).cycle in
    if Cycle_repr.(current_cycle >= update.activate_cycle) then
      let current_master_key = keychain.master_key in
      let master_key = update.pending_key in
      let next_master_key = No_next_key in
      {keychain with
       master_key;
       next_master_key;
       forsaken_key = [current_master_key]}
    else keychain

let find ctx pkh =
  Storage.Keychain.find ctx pkh
  >>=? function
  | None -> return None
  | Some keychain ->
    let updated_keychain = master_key_update_internal ctx keychain in
    return @@ Some updated_keychain

let get_master_key ctx pkh =
  exists ctx pkh
  >>= fun existing ->
  if existing then
    Storage.Keychain.get ctx pkh
    >|=? fun keychain ->
    let updated_keychain = master_key_update_internal ctx keychain in
    Some updated_keychain.master_key
  else return_none

let get_spending_key ctx pkh =
  exists ctx pkh
  >>= fun existing ->
  if existing then
    Storage.Keychain.get ctx pkh
    >|=? fun keychain ->
    let updated_keychain = master_key_update_internal ctx keychain in
    Some updated_keychain.spending_key
  else return_none

let get_forsaken_key ctx pkh =
  exists ctx pkh
  >>= fun existing ->
  if existing then
    Storage.Keychain.get ctx pkh
    >|=? fun keychain ->
    let updated_keychain = master_key_update_internal ctx keychain in
    Some updated_keychain.forsaken_key
  else return_none

let setup_next_key_internal ctx keychain pending_key =
  let mk = keychain.master_key in
  let fks = keychain.forsaken_key in
  if Signature.Public_key.equal pending_key mk then
    fail Key_update_conflict
  else if List.exists (fun k -> Signature.Public_key.equal pending_key k) fks then
    fail Key_update_forsaken
  else
    let delayed_cycles = (Raw_context.constants ctx).master_key_delay_cycles in
    let current_cycle = (Level_storage.current ctx).cycle in
    let activate_cycle = Cycle_repr.add current_cycle delayed_cycles in
    let update = Keychain_repr.{ activate_cycle; pending_key} in
    let next = Keychain_repr.Delay update in
    return {keychain with Keychain_repr.next_master_key = next}

let set ctx pkh master_key_opt spending_key_opt =
  exists ctx pkh
  >>= fun existing ->
  if existing then
    Storage.Keychain.get ctx pkh
    >>=? fun keychain ->
    begin
      match master_key_opt, spending_key_opt with
      | None, None -> return keychain
      | None, Some spending_key -> return {keychain with spending_key}
      | Some master_key, None ->
        setup_next_key_internal ctx keychain master_key
      | Some master_key, Some spending_key ->
        setup_next_key_internal ctx {keychain with spending_key} master_key
    end
    >>=? fun keychain ->
    Storage.Keychain.update ctx pkh keychain
  else fail (Unregistered_key_hash pkh)

let set_master_key ctx pkh master_key =
  exists ctx pkh
  >>= fun existing ->
  if existing then
    Storage.Keychain.get ctx pkh
    >>=? fun keychain ->
    setup_next_key_internal ctx keychain master_key
    >>=? fun keychain_next ->
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
