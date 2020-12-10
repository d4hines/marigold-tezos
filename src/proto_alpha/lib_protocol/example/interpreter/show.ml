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

open Stdlib

module Micheline = Tezos_protocol_environment_alpha.Environment.Micheline

module Michelson_v1_primitives =
  Tezos_raw_protocol_alpha.Michelson_v1_primitives

module Z = struct
  include Z

  let pp = Z.pp_print
end

type annot = string list [@@deriving show {with_path = false}]

type ('l, 'p) node = ('l, 'p) Micheline.node =
  | Int of 'l * Z.t
  | String of 'l * string
  | Bytes of 'l * Bytes.t
  | Prim of 'l * 'p * ('l, 'p) node list * annot
  | Seq of 'l * ('l, 'p) node list
[@@deriving show {with_path = false}]

type canonical_location = int [@@deriving show {with_path = false}]

type 'p canonical = Canonical of (canonical_location, 'p) node
[@@deriving show {with_path = false}]

let pp_canonical (a : Format.formatter -> 'p -> unit) f
    (v : 'p Micheline.canonical) =
  pp_canonical a f (Obj.magic v)

let show_canonical (a : Format.formatter -> 'p -> unit)
    (v : 'p Micheline.canonical) =
  show_canonical a (Obj.magic v)

type prim = Michelson_v1_primitives.prim =
  | K_parameter
  | K_storage
  | K_code
  | D_False
  | D_Elt
  | D_Left
  | D_None
  | D_Pair
  | D_Right
  | D_Some
  | D_True
  | D_Unit
  | I_PACK
  | I_UNPACK
  | I_BLAKE2B
  | I_SHA256
  | I_SHA512
  | I_ABS
  | I_ADD
  | I_AMOUNT
  | I_AND
  | I_BALANCE
  | I_CAR
  | I_CDR
  | I_CHAIN_ID
  | I_CHECK_SIGNATURE
  | I_COMPARE
  | I_CONCAT
  | I_CONS
  | I_CREATE_ACCOUNT
  | I_CREATE_CONTRACT
  | I_IMPLICIT_ACCOUNT
  | I_DIP
  | I_DROP
  | I_DUP
  | I_EDIV
  | I_EMPTY_BIG_MAP
  | I_EMPTY_MAP
  | I_EMPTY_SET
  | I_EQ
  | I_EXEC
  | I_APPLY
  | I_FAILWITH
  | I_GE
  | I_GET
  | I_GET_AND_UPDATE
  | I_GT
  | I_HASH_KEY
  | I_IF
  | I_IF_CONS
  | I_IF_LEFT
  | I_IF_NONE
  | I_INT
  | I_LAMBDA
  | I_LE
  | I_LEFT
  | I_LEVEL
  | I_LOOP
  | I_LSL
  | I_LSR
  | I_LT
  | I_MAP
  | I_MEM
  | I_MUL
  | I_NEG
  | I_NEQ
  | I_NIL
  | I_NONE
  | I_NOT
  | I_NOW
  | I_OR
  | I_PAIR
  | I_UNPAIR
  | I_PUSH
  | I_RIGHT
  | I_SIZE
  | I_SOME
  | I_SOURCE
  | I_SENDER
  | I_SELF
  | I_SELF_ADDRESS
  | I_SLICE
  | I_STEPS_TO_QUOTA
  | I_SUB
  | I_SWAP
  | I_TRANSFER_TOKENS
  | I_SET_DELEGATE
  | I_UNIT
  | I_UPDATE
  | I_XOR
  | I_ITER
  | I_LOOP_LEFT
  | I_ADDRESS
  | I_CONTRACT
  | I_ISNAT
  | I_CAST
  | I_RENAME
  | I_SAPLING_EMPTY_STATE
  | I_SAPLING_VERIFY_UPDATE
  | I_DIG
  | I_DUG
  | I_NEVER
  | I_VOTING_POWER
  | I_TOTAL_VOTING_POWER
  | I_KECCAK
  | I_SHA3
  | I_PAIRING_CHECK
  | I_TICKET
  | I_READ_TICKET
  | I_SPLIT_TICKET
  | I_JOIN_TICKETS
  | T_bool
  | T_contract
  | T_int
  | T_key
  | T_key_hash
  | T_lambda
  | T_list
  | T_map
  | T_big_map
  | T_nat
  | T_option
  | T_or
  | T_pair
  | T_set
  | T_signature
  | T_string
  | T_bytes
  | T_mutez
  | T_timestamp
  | T_unit
  | T_operation
  | T_address
  | T_sapling_transaction
  | T_sapling_state
  | T_chain_id
  | T_never
  | T_bls12_381_g1
  | T_bls12_381_g2
  | T_bls12_381_fr
  | T_ticket
[@@deriving show {with_path = false}]

let x =
  Canonical
    (Seq
       ( 0,
         [ Prim (1, K_parameter, [Prim (2, T_unit, [], [])], []);
           Prim (3, K_storage, [Prim (4, T_unit, [], [])], []);
           Prim
             ( 5,
               K_code,
               [ Seq
                   ( 6,
                     [ Prim (7, I_CDR, [], []);
                       Prim (8, I_NIL, [Prim (9, T_operation, [], [])], []);
                       Prim (10, I_PAIR, [], []) ] ) ],
               [] ) ] ))

let get_instr_name :
    type b a. (b, a) Tezos_raw_protocol_alpha.Script_typed_ir.instr -> string =
  function
  | Const _ ->
      "Const _ "
  | Cons_pair ->
      "Cons_pair "
  | Cons_some ->
      "Cons_some "
  | Cons_none _ ->
      "Cons_none _ "
  | Cons_left ->
      "Cons_left "
  | Cons_right ->
      "Cons_right "
  | Nil ->
      "Nil "
  | Empty_set _ ->
      "Empty_set _ "
  | Empty_map _ ->
      "Empty_map _ "
  | Empty_big_map _ ->
      "Empty_big_map _ "
  | Lambda _ ->
      "Lambda _ "
  | Self _ ->
      "Self _ "
  | Contract _ ->
      "Contract _ "
  | Ticket ->
      "Ticket "
  | Read_ticket ->
      "Read_ticket "
  | Split_ticket ->
      "Split_ticket "
  | Unpack _ ->
      "Unpack _ "
  | List_map _ ->
      "List_map _ "
  | Map_map _ ->
      "Map_map _ "
  | Drop ->
      "Drop "
  | Dup ->
      "Dup "
  | Swap ->
      "Swap "
  | Unpair ->
      "Unpair "
  | Car ->
      "Car "
  | Cdr ->
      "Cdr "
  | If_none _ ->
      "If_none _ "
  | If_left _ ->
      "If_left _ "
  | Cons_list ->
      "Cons_list "
  | If_cons _ ->
      "If_cons _ "
  | List_size ->
      "List_size "
  | List_iter _ ->
      "List_iter _ "
  | Set_iter _ ->
      "Set_iter _ "
  | Set_mem ->
      "Set_mem "
  | Set_update ->
      "Set_update "
  | Set_size ->
      "Set_size "
  | Map_iter _ ->
      "Map_iter _ "
  | Map_mem ->
      "Map_mem "
  | Map_get ->
      "Map_get "
  | Map_update ->
      "Map_update "
  | Map_get_and_update ->
      "Map_get_and_update "
  | Map_size ->
      "Map_size "
  | Big_map_get ->
      "Big_map_get "
  | Big_map_update ->
      "Big_map_update "
  | Big_map_get_and_update ->
      "Big_map_get_and_update "
  | Big_map_mem ->
      "Big_map_mem "
  | Concat_string ->
      "Concat_string "
  | Concat_string_pair ->
      "Concat_string_pair "
  | Slice_string ->
      "Slice_string "
  | String_size ->
      "String_size "
  | Concat_bytes ->
      "Concat_bytes "
  | Concat_bytes_pair ->
      "Concat_bytes_pair "
  | Slice_bytes ->
      "Slice_bytes "
  | Bytes_size ->
      "Bytes_size "
  | Add_seconds_to_timestamp ->
      "Add_seconds_to_timestamp "
  | Add_timestamp_to_seconds ->
      "Add_timestamp_to_seconds "
  | Sub_timestamp_seconds ->
      "Sub_timestamp_seconds "
  | Diff_timestamps ->
      "Diff_timestamps "
  | Add_tez ->
      "Add_tez "
  | Sub_tez ->
      "Sub_tez "
  | Mul_teznat ->
      "Mul_teznat "
  | Mul_nattez ->
      "Mul_nattez "
  | Ediv_teznat ->
      "Ediv_teznat "
  | Ediv_tez ->
      "Ediv_tez "
  | Or ->
      "Or "
  | And ->
      "And "
  | Xor ->
      "Xor "
  | Not ->
      "Not "
  | Is_nat ->
      "Is_nat "
  | Neg_nat ->
      "Neg_nat "
  | Neg_int ->
      "Neg_int "
  | Abs_int ->
      "Abs_int "
  | Int_nat ->
      "Int_nat "
  | Add_intint ->
      "Add_intint "
  | Add_intnat ->
      "Add_intnat "
  | Add_natint ->
      "Add_natint "
  | Add_natnat ->
      "Add_natnat "
  | Sub_int ->
      "Sub_int "
  | Mul_intint ->
      "Mul_intint "
  | Mul_intnat ->
      "Mul_intnat "
  | Mul_natint ->
      "Mul_natint "
  | Mul_natnat ->
      "Mul_natnat "
  | Ediv_intint ->
      "Ediv_intint "
  | Ediv_intnat ->
      "Ediv_intnat "
  | Ediv_natint ->
      "Ediv_natint "
  | Ediv_natnat ->
      "Ediv_natnat "
  | Lsl_nat ->
      "Lsl_nat "
  | Lsr_nat ->
      "Lsr_nat "
  | Or_nat ->
      "Or_nat "
  | And_nat ->
      "And_nat "
  | And_int_nat ->
      "And_int_nat "
  | Xor_nat ->
      "Xor_nat "
  | Not_nat ->
      "Not_nat "
  | Not_int ->
      "Not_int "
  | Seq _ ->
      "Seq _ "
  | If _ ->
      "If _ "
  | Loop _ ->
      "Loop _ "
  | Loop_left _ ->
      "Loop_left _ "
  | Dip _ ->
      "Dip _ "
  | Exec ->
      "Exec "
  | Apply _ ->
      "Apply _ "
  | Failwith _ ->
      "Failwith _ "
  | Nop ->
      "Nop "
  | Compare _ ->
      "Compare _ "
  | Eq ->
      "Eq "
  | Neq ->
      "Neq "
  | Lt ->
      "Lt "
  | Gt ->
      "Gt "
  | Le ->
      "Le "
  | Ge ->
      "Ge "
  | Address ->
      "Address "
  | Transfer_tokens ->
      "Transfer_tokens "
  | Implicit_account ->
      "Implicit_account "
  | Create_contract _ ->
      "Create_contract _ "
  | Now ->
      "Now "
  | Level ->
      "Level "
  | Balance ->
      "Balance "
  | Check_signature ->
      "Check_signature "
  | Hash_key ->
      "Hash_key "
  | Blake2b ->
      "Blake2b "
  | Sha256 ->
      "Sha256 "
  | Sha512 ->
      "Sha512 "
  | Source ->
      "Source "
  | Sender ->
      "Sender "
  | Amount ->
      "Amount "
  | Self_address ->
      "Self_address "
  | Sapling_empty_state _ ->
      "Sapling_empty_state _ "
  | Sapling_verify_update ->
      "Sapling_verify_update "
  | Set_delegate ->
      "Set_delegate "
  | Pack _ ->
      "Pack _ "
  | Dig _ ->
      "Dig _ "
  | Dug _ ->
      "Dug _ "
  | Dipn _ ->
      "Dipn _ "
  | Dropn _ ->
      "Dropn _ "
  | ChainId ->
      "ChainId "
  | Never ->
      "Never "
  | Voting_power ->
      "Voting_power "
  | Total_voting_power ->
      "Total_voting_power "
  | Keccak ->
      "Keccak "
  | Sha3 ->
      "Sha3 "
  | Add_bls12_381_g1 ->
      "Add_bls12_381_g1 "
  | Add_bls12_381_g2 ->
      "Add_bls12_381_g2 "
  | Add_bls12_381_fr ->
      "Add_bls12_381_fr "
  | Mul_bls12_381_g1 ->
      "Mul_bls12_381_g1 "
  | Mul_bls12_381_g2 ->
      "Mul_bls12_381_g2 "
  | Mul_bls12_381_fr ->
      "Mul_bls12_381_fr "
  | Mul_bls12_381_fr_z ->
      "Mul_bls12_381_fr_z "
  | Mul_bls12_381_z_fr ->
      "Mul_bls12_381_z_fr "
  | Int_bls12_381_fr ->
      "Int_bls12_381_fr "
  | Neg_bls12_381_g1 ->
      "Neg_bls12_381_g1 "
  | Neg_bls12_381_g2 ->
      "Neg_bls12_381_g2 "
  | Neg_bls12_381_fr ->
      "Neg_bls12_381_fr "
  | Pairing_check_bls12_381 ->
      "Pairing_check_bls12_381 "
  | Uncomb _ ->
      "Uncomb _ "
  | Comb_get _ ->
      "Comb_get _ "
  | Comb _ ->
      "Comb _ "
  | Comb_set _ ->
      "Comb_set _ "
  | Dup_n _ ->
      "Dup_n _ "
  | Join_tickets _ ->
      "Join_tickets _ "
