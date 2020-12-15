(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <nomadic@tezcore.com>                    *)
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
open Script_typed_cps_ir

[@@@ocaml.warning "-32"]

(* unit type *)
let unit_ty = Unit_t None

let unit_cmp_ty = Unit_key None

(* the type of integers *)
let int_ty = Int_t None

(* the type of naturals *)
let nat_ty = Nat_t None

(* the type of strings *)
let string_ty = String_t None

(* the type of bytes *)
let bytes_ty = Bytes_t None

(* the type of booleans *)
let bool_ty = Bool_t None

(* the type of mutez *)
let mutez_ty = Mutez_t None

(* the type of key hashes *)
let key_hash_ty = Key_hash_t None

(* the type of addresses *)
let address_ty = Address_t None

(* the type of addresses *)
let chain_id_ty = Chain_id_t None

(* the type of timestamps *)
let timestamp_ty = Timestamp_t None

(* list type constructor *)
let list_ty x = List_t (x, None)

(* option type constructor *)
let option_ty x = Option_t (x, None)

(* map type constructor*)
let map_ty k v = Map_t (k, v, None)

(* set type constructor*)
let set_ty k = Set_t (k, None)

(* pair type constructor*)
let pair_ty k1 k2 = Pair_t ((k1, None, None), (k2, None, None), None)

(* union type constructor*)
let union_ty k1 k2 = Union_t ((k1, None), (k2, None), None)

(* the type of unit lists *)
let unit_list_ty = list_ty unit_ty

(* the type of string lists *)
let string_list_ty = list_ty string_ty

(* the type of unit options *)
let unit_option_ty = option_ty unit_ty

(* The type of int sets *)
let int_set_ty = set_ty (Int_key None)

(* The type of unit maps with int keys *)
let int_unit_map_ty = map_ty (Int_key None) unit_ty

(* The type of signatures *)
let signature_ty = Signature_t None

(* The type of public keys *)
let public_key_ty = Key_t None

(* the type of empty stacks *)
let empty_stack_ty = Empty_t

(* the type of stacks with one unit value *)
let unit_stack_ty = Item_t (unit_ty, Empty_t, None)

(* the type of stacks with one integer *)
let int_stack_ty = Item_t (int_ty, Empty_t, None)

(* the type of stacks with one nat *)
let nat_stack_ty = Item_t (nat_ty, Empty_t, None)

(* the type of stacks with one bool *)
let bool_stack_ty = Item_t (bool_ty, Empty_t, None)

(* the type of stacks with only one string *)
let string_stack_ty = Item_t (string_ty, Empty_t, None)

(* the type of stacks with only one Bytes.t *)
let bytes_stack_ty = Item_t (bytes_ty, Empty_t, None)

(* the type of stacks with two integers *)
let int_int_stack_ty = Item_t (int_ty, Item_t (int_ty, Empty_t, None), None)

(* the type of stacks with two nats *)
let nat_nat_stack_ty = Item_t (nat_ty, Item_t (nat_ty, Empty_t, None), None)

(* the type of stacks with two bools *)
let bool_bool_stack_ty = Item_t (bool_ty, Item_t (bool_ty, Empty_t, None), None)

(* the type of stacks with two strings *)
let string_string_stack_ty =
  Item_t (string_ty, Item_t (string_ty, Empty_t, None), None)

(* the type of stacks with only one unit list *)
let unit_list_stack_ty = Item_t (unit_list_ty, Empty_t, None)

(* the type of stacks with only one string list *)
let string_list_stack_ty = Item_t (string_list_ty, Empty_t, None)

(* the type of stacks with two strings *)
let string_string_stack_ty =
  Item_t (string_ty, Item_t (string_ty, Empty_t, None), None)

(* the type of stacks with one unit on top of one unit list *)
let unit_unit_list_stack_ty =
  Item_t (unit_ty, Item_t (unit_list_ty, Empty_t, None), None)

(* the type of stacks with one unit option *)
let unit_option_stack_ty = Item_t (unit_option_ty, Empty_t, None)

(* the type of stacks with one int set *)
let int_set_stack_ty = Item_t (int_set_ty, Empty_t, None)

(* the type of stacks with one int and one int set *)
let int_int_set_stack_ty =
  Item_t (int_ty, Item_t (int_set_ty, Empty_t, None), None)

(* stacks with one integer and one int set *)
let int_bool_int_set_stack_ty =
  Item_t
    (int_ty, Item_t (bool_ty, Item_t (int_set_ty, Empty_t, None), None), None)

(* the type of stacks with one int and one (int,unit) map *)
let int_int_unit_map_stack_ty =
  Item_t (int_ty, Item_t (int_unit_map_ty, Empty_t, None), None)

(* stacks with one integer, an optional unit and a (int, unit) map *)
let int_opt_unit_int_unit_map_stack_ty =
  Item_t
    ( int_ty,
      Item_t (option_ty unit_ty, Item_t (int_unit_map_ty, Empty_t, None), None),
      None )

(* stacks with one (int, unit) map *)
let int_unit_map_stack_ty = Item_t (int_unit_map_ty, Empty_t, None)

(* stacks with a pair of units *)
let unit_pair_stack_ty = Item_t (pair_ty unit_ty unit_ty, Empty_t, None)

(* stacks with two units *)
let unit_unit_stack_ty = Item_t (unit_ty, Item_t (unit_ty, Empty_t, None), None)

(* stacks with an element of (unit + unit) *)
let unit_union_stack_ty = Item_t (union_ty unit_ty unit_ty, Empty_t, None)

(* stacks with two mutez *)
let mutez_mutez_stack_ty =
  Item_t (mutez_ty, Item_t (mutez_ty, Empty_t, None), None)

(* stacks with a mutez and and a nat *)
let mutez_nat_stack_ty = Item_t (mutez_ty, Item_t (nat_ty, Empty_t, None), None)

(* stacks with one key hash *)
let keyhash_stack_ty = Item_t (key_hash_ty, Empty_t, None)

(* stacks with to key hashes *)
let keyhash_keyhash_stack_ty =
  Item_t (key_hash_ty, Item_t (key_hash_ty, Empty_t, None), None)

(* stacks with to addresses *)
let addr_addr_stack_ty =
  Item_t (address_ty, Item_t (address_ty, Empty_t, None), None)

(* Bls12_381 types *)
let g1_ty = Bls12_381_g1_t None

let g2_ty = Bls12_381_g2_t None

let fr_ty = Bls12_381_fr_t None

let g1_g1_stack_ty = Item_t (g1_ty, Item_t (g1_ty, Empty_t, None), None)

let g2_g2_stack_ty = Item_t (g2_ty, Item_t (g2_ty, Empty_t, None), None)

let fr_fr_stack_ty = Item_t (fr_ty, Item_t (fr_ty, Empty_t, None), None)

let g1_fr_stack_ty = Item_t (g1_ty, Item_t (fr_ty, Empty_t, None), None)

let g2_fr_stack_ty = Item_t (g2_ty, Item_t (fr_ty, Empty_t, None), None)

let g1_stack_ty = Item_t (g1_ty, Empty_t, None)

let g2_stack_ty = Item_t (g2_ty, Empty_t, None)

let fr_stack_ty = Item_t (fr_ty, Empty_t, None)

let pairing_check_stack_ty =
  Item_t
    ( List_t (Pair_t ((g1_ty, None, None), (g2_ty, None, None), None), None),
      Empty_t,
      None )

let check_signature_stack_ty =
  Item_t
    ( public_key_ty,
      Item_t (signature_ty, Item_t (bytes_ty, Empty_t, None), None),
      None )

let ticket_stack_ty = Item_t (unit_ty, Item_t (nat_ty, Empty_t, None), None)

let ticket_ty contents_ty = Ticket_t (contents_ty, None)

let read_ticket_stack_ty contents_ty =
  Item_t (ticket_ty contents_ty, Empty_t, None)

let verify_update_stack_ty ~memo_size =
  match Alpha_context.Sapling.Memo_size.parse_z (Z.of_int memo_size) with
  | Error _ ->
      invalid_arg "verify_update_stack_ty"
  | Ok memo_size ->
      Item_t
        ( Sapling_transaction_t (memo_size, None),
          Item_t (Sapling_state_t (memo_size, None), Empty_t, None),
          None )
