
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

type delayed_update = {
  activate_cycle : Cycle_repr.t;
  pending_key : Signature.Public_key.t;
}

type next_key =
  | No_next_key
  | Delay of delayed_update

type t = {
  master_key : Signature.Public_key.t;
  next_master_key : next_key;
  spending_key : Signature.Public_key.t;
  forsaken_key : Signature.Public_key.t list;
}

type keychain = t

let delayed_update_pp ppf {activate_cycle; pending_key} =
  Format.fprintf
    ppf
    "key: %a (cycle: %a)"
    Signature.Public_key.pp
    pending_key
    Cycle_repr.pp
    activate_cycle

let delayed_update_encoding =
  let open Data_encoding in
  conv
    (fun {activate_cycle; pending_key} -> (activate_cycle, pending_key))
    (fun (activate_cycle, pending_key) -> {activate_cycle; pending_key})
    (obj2
       (req
          "activate_cycle"
          ~description:
            "The cycle where update takes place"
          Cycle_repr.encoding)
       (req
          "pending_key"
          ~description:
            "The new key"
          Signature.Public_key.encoding))

let next_key_pp ppf = function
  | No_next_key ->
    Format.fprintf ppf "no_next_key"
  | Delay du ->
    Format.fprintf ppf "next_key: %a" delayed_update_pp du

let next_key_encoding =
  let open Data_encoding in
  def "keychain.next_key"
  @@ obj1 @@ req "next_key"
  @@ union
    [ case
        (Tag 0)
        ~title:"No_next_key"
        (constant "no_next_key")
        (function No_next_key -> Some () | _ -> None)
        (fun () -> No_next_key);
      case
        (Tag 1)
        ~title:"Delay"
        (obj2
           (req "kind" (constant "delayed_update"))
           (req "update" delayed_update_encoding))
        (function Delay du -> Some ((), du) | _ -> None )
        (fun ((), du) -> Delay du) ]

let rec forsaken_key_pp ppf = function
  | [] -> ()
  | (k :: ks) ->
    let () = Format.fprintf ppf "%a;" Signature.Public_key.pp k in
    forsaken_key_pp ppf ks

let pp ppf {master_key; next_master_key; spending_key; forsaken_key} =
  Format.fprintf ppf "master_key: %a (%a), spending_key: %a\n%a"
    Signature.Public_key.pp
    master_key
    next_key_pp
    next_master_key
    Signature.Public_key.pp
    spending_key
    forsaken_key_pp
    forsaken_key

let encoding =
  let open Data_encoding in
  conv
    (fun {master_key; next_master_key; spending_key; forsaken_key} ->
      (master_key, next_master_key, spending_key, forsaken_key))
    (fun (master_key, next_master_key, spending_key, forsaken_key) ->
       {master_key; next_master_key;spending_key; forsaken_key})
    (obj4
      (req
         "master_key"
         ~description:
           "The master keys"
         Signature.Public_key.encoding)
      (req
         "next_master_key"
         ~description:
           "The next master key"
         next_key_encoding)
      (req
         "spending_key"
         ~description:
           "The public key for transaction oply"
         Signature.Public_key.encoding)
      (req
         "forsaken_key"
         ~description:
           "The old keys which shouldn't be used again"
         (list Signature.Public_key.encoding))
    )
