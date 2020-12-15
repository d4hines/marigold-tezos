(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <nomadic@tezcore.com>                    *)
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

(* ------------------------------------------------------------------------- *)

type id = string

let pp_id = Format.pp_print_string

let equal_id = String.equal

(* ------------------------------------------------------------------------- *)
(* Names of IR instructions together with sizes of their operands as
   encountered during evaluation. *)

type instruction_name =
  (* stack ops *)
  | N_Drop
  | N_Dup
  | N_Swap
  | N_Const
  (* pairs *)
  | N_Cons_pair
  | N_Car
  | N_Cdr
  (* options *)
  | N_Cons_some
  | N_Cons_none
  | N_If_none
  (* unions *)
  | N_Left
  | N_Right
  | N_If_left
  (* lists *)
  | N_Cons_list
  | N_Nil
  | N_If_cons
  | N_List_map
  | N_List_iter
  | N_List_size
  (* sets *)
  | N_Empty_set
  | N_Set_iter
  | N_Set_mem
  | N_Set_update
  | N_Set_size
  (* maps *)
  | N_Empty_map
  | N_Map_map
  | N_Map_iter
  | N_Map_mem
  | N_Map_get
  | N_Map_update
  | N_Map_size
  (* big maps *)
  | N_Empty_big_map
  | N_Big_map_mem
  | N_Big_map_get
  | N_Big_map_update
  (* string operations *)
  | N_Concat_string
  | N_Concat_string_pair
  | N_Slice_string
  | N_String_size
  (* bytes operations *)
  | N_Concat_bytes
  | N_Concat_bytes_pair
  | N_Slice_bytes
  | N_Bytes_size
  (* timestamp operations *)
  | N_Add_seconds_to_timestamp
  | N_Add_timestamp_to_seconds
  | N_Sub_timestamp_seconds
  | N_Diff_timestamps
  (* currency operations *)
  | N_Add_tez
  | N_Sub_tez
  | N_Mul_teznat
  | N_Mul_nattez
  | N_Ediv_teznat
  | N_Ediv_tez
  (* boolean operations - assumed O(1) *)
  | N_Or
  | N_And
  | N_Xor
  | N_Not
  (* integer operations *)
  | N_Is_nat
  | N_Neg_nat
  | N_Neg_int
  | N_Abs_int
  | N_Int_nat
  | N_Add_intint
  | N_Add_intnat
  | N_Add_natint
  | N_Add_natnat
  | N_Sub_int
  | N_Mul_intint
  | N_Mul_intnat
  | N_Mul_natint
  | N_Mul_natnat
  | N_Ediv_intint
  | N_Ediv_intnat
  | N_Ediv_natint
  | N_Ediv_natnat
  | N_Lsl_nat
  | N_Lsr_nat
  | N_Or_nat
  | N_And_nat
  | N_And_int_nat
  | N_Xor_nat
  | N_Not_nat
  | N_Not_int
  (* control *)
  | N_Seq
  | N_If
  | N_Loop
  | N_Loop_left
  | N_Dip
  | N_Exec
  | N_Lambda
  | N_Failwith
  | N_Nop
  (* comparison, warning: there is some dynamic dispatch on the type going on here. *)
  | N_Compare
  (* comparators *)
  | N_Eq
  | N_Neq
  | N_Lt
  | N_Gt
  | N_Le
  | N_Ge
  (* protocol *)
  | N_Address
  | N_Contract
  | N_Transfer_tokens
  | N_Implicit_account
  | N_Create_contract (* This one is tough and should be benchmarked on its own. TODO *)
  | N_Set_delegate
  | N_Now
  | N_Balance
  | N_Check_signature_ed25519
  | N_Check_signature_secp256k1
  | N_Check_signature_p256
  | N_Hash_key
  | N_Pack
  | N_Unpack
  | N_Blake2b
  | N_Sha256
  | N_Sha512
  | N_Source
  | N_Sender
  | N_Self
  | N_Amount
  | N_Dig
  | N_Dug
  | N_DipN
  | N_DropN
  | N_DupN
  | N_ChainId
  | N_Level
  | N_Self_address
  | N_Never
  | N_Unpair
  | N_Voting_power
  | N_Total_voting_power
  | N_Keccak
  | N_Sha3
  (* Elliptic curves *)
  | N_Add_bls12_381_g1
  | N_Add_bls12_381_g2
  | N_Add_bls12_381_fr
  | N_Mul_bls12_381_g1
  | N_Mul_bls12_381_g2
  | N_Mul_bls12_381_fr
  | N_Neg_bls12_381_g1
  | N_Neg_bls12_381_g2
  | N_Neg_bls12_381_fr
  | N_Pairing_check_bls12_381
  | N_Mul_bls12_381_fr_z
  | N_Mul_bls12_381_z_fr
  | N_Int_bls12_381_z_fr
  (* Combs *)
  | N_Comb
  | N_Uncomb
  | N_Comb_get
  | N_Comb_set
  (* Tickets *)
  | N_Ticket
  | N_Read_ticket
  | N_Split_ticket
  | N_Join_tickets
  | N_Sapling_empty_state
  | N_Sapling_verify_update
  | N_Map_get_and_update
  | N_Big_map_get_and_update

(* ------------------------------------------------------------------------- *)
(* Code that ought to be auto-generated *)

let string_of_instruction_name : instruction_name -> string =
 fun ir ->
  match ir with
  | N_Drop ->
      "N_Drop"
  | N_Dup ->
      "N_Dup"
  | N_Swap ->
      "N_Swap"
  | N_Const ->
      "N_Const"
  | N_Cons_pair ->
      "N_Cons_pair"
  | N_Car ->
      "N_Car"
  | N_Cdr ->
      "N_Cdr"
  | N_Cons_some ->
      "N_Cons_some"
  | N_Cons_none ->
      "N_Cons_none"
  | N_If_none ->
      "N_If_none"
  | N_Left ->
      "N_Left"
  | N_Right ->
      "N_Right"
  | N_If_left ->
      "N_If_left"
  | N_Cons_list ->
      "N_Cons_list"
  | N_Nil ->
      "N_Nil"
  | N_If_cons ->
      "N_If_cons"
  | N_List_map ->
      "N_List_map"
  | N_List_iter ->
      "N_List_iter"
  | N_List_size ->
      "N_List_size"
  | N_Empty_set ->
      "N_Empty_set"
  | N_Set_iter ->
      "N_Set_iter"
  | N_Set_mem ->
      "N_Set_mem"
  | N_Set_update ->
      "N_Set_update"
  | N_Set_size ->
      "N_Set_size"
  | N_Empty_map ->
      "N_Empty_map"
  | N_Map_map ->
      "N_Map_map"
  | N_Map_iter ->
      "N_Map_iter"
  | N_Map_mem ->
      "N_Map_mem"
  | N_Map_get ->
      "N_Map_get"
  | N_Map_update ->
      "N_Map_update"
  | N_Map_size ->
      "N_Map_size"
  | N_Empty_big_map ->
      "N_Empty_big_map"
  | N_Big_map_mem ->
      "N_Big_map_mem"
  | N_Big_map_get ->
      "N_Big_map_get"
  | N_Big_map_update ->
      "N_Big_map_update"
  | N_Concat_string ->
      "N_Concat_string"
  | N_Concat_string_pair ->
      "N_Concat_string_pair"
  | N_Slice_string ->
      "N_Slice_string"
  | N_String_size ->
      "N_String_size"
  | N_Concat_bytes ->
      "N_Concat_bytes"
  | N_Concat_bytes_pair ->
      "N_Concat_bytes_pair"
  | N_Slice_bytes ->
      "N_Slice_bytes"
  | N_Bytes_size ->
      "N_Bytes_size"
  | N_Add_seconds_to_timestamp ->
      "N_Add_seconds_to_timestamp"
  | N_Add_timestamp_to_seconds ->
      "N_Add_timestamp_to_seconds"
  | N_Sub_timestamp_seconds ->
      "N_Sub_timestamp_seconds"
  | N_Diff_timestamps ->
      "N_Diff_timestamps"
  | N_Add_tez ->
      "N_Add_tez"
  | N_Sub_tez ->
      "N_Sub_tez"
  | N_Mul_teznat ->
      "N_Mul_teznat"
  | N_Mul_nattez ->
      "N_Mul_nattez"
  | N_Ediv_teznat ->
      "N_Ediv_teznat"
  | N_Ediv_tez ->
      "N_Ediv_tez"
  | N_Or ->
      "N_Or"
  | N_And ->
      "N_And"
  | N_Xor ->
      "N_Xor"
  | N_Not ->
      "N_Not"
  | N_Is_nat ->
      "N_Is_nat"
  | N_Neg_nat ->
      "N_Neg_nat"
  | N_Neg_int ->
      "N_Neg_int"
  | N_Abs_int ->
      "N_Abs_int"
  | N_Int_nat ->
      "N_Int_nat"
  | N_Add_intint ->
      "N_Add_intint"
  | N_Add_intnat ->
      "N_Add_intnat"
  | N_Add_natint ->
      "N_Add_natint"
  | N_Add_natnat ->
      "N_Add_natnat"
  | N_Sub_int ->
      "N_Sub_int"
  | N_Mul_intint ->
      "N_Mul_intint"
  | N_Mul_intnat ->
      "N_Mul_intnat"
  | N_Mul_natint ->
      "N_Mul_natint"
  | N_Mul_natnat ->
      "N_Mul_natnat"
  | N_Ediv_intint ->
      "N_Ediv_intint"
  | N_Ediv_intnat ->
      "N_Ediv_intnat"
  | N_Ediv_natint ->
      "N_Ediv_natint"
  | N_Ediv_natnat ->
      "N_Ediv_natnat"
  | N_Lsl_nat ->
      "N_Lsl_nat"
  | N_Lsr_nat ->
      "N_Lsr_nat"
  | N_Or_nat ->
      "N_Or_nat"
  | N_And_nat ->
      "N_And_nat"
  | N_And_int_nat ->
      "N_And_int_nat"
  | N_Xor_nat ->
      "N_Xor_nat"
  | N_Not_nat ->
      "N_Not_nat"
  | N_Not_int ->
      "N_Not_int"
  | N_Seq ->
      "N_Seq"
  | N_If ->
      "N_If"
  | N_Loop ->
      "N_Loop"
  | N_Loop_left ->
      "N_Loop_left"
  | N_Dip ->
      "N_Dip"
  | N_Exec ->
      "N_Exec"
  | N_Lambda ->
      "N_Lambda"
  | N_Failwith ->
      "N_Failwith"
  | N_Nop ->
      "N_Nop"
  | N_Compare ->
      "N_Compare"
  | N_Eq ->
      "N_Eq"
  | N_Neq ->
      "N_Neq"
  | N_Lt ->
      "N_Lt"
  | N_Gt ->
      "N_Gt"
  | N_Le ->
      "N_Le"
  | N_Ge ->
      "N_Ge"
  | N_Address ->
      "N_Address"
  | N_Contract ->
      "N_Contract"
  | N_Transfer_tokens ->
      "N_Transfer_tokens"
  | N_Implicit_account ->
      "N_Implicit_account"
  | N_Create_contract ->
      "N_Create_contract"
  | N_Set_delegate ->
      "N_Set_delegate"
  | N_Now ->
      "N_Now"
  | N_Balance ->
      "N_Balance"
  | N_Check_signature_ed25519 ->
      "N_Check_signature_ed25519"
  | N_Check_signature_secp256k1 ->
      "N_Check_signature_secp256k1"
  | N_Check_signature_p256 ->
      "N_Check_signature_p256"
  | N_Hash_key ->
      "N_Hash_key"
  | N_Pack ->
      "N_Pack"
  | N_Unpack ->
      "N_Unpack"
  | N_Blake2b ->
      "N_Blake2b"
  | N_Sha256 ->
      "N_Sha256"
  | N_Sha512 ->
      "N_Sha512"
  | N_Source ->
      "N_Source"
  | N_Sender ->
      "N_Sender"
  | N_Self ->
      "N_Self"
  | N_Amount ->
      "N_Amount"
  | N_Dig ->
      "N_Dig"
  | N_Dug ->
      "N_Dug"
  | N_DipN ->
      "N_DipN"
  | N_DropN ->
      "N_DropN"
  | N_DupN ->
      "N_DupN"
  | N_ChainId ->
      "N_ChainId"
  | N_Level ->
      "N_Level"
  | N_Self_address ->
      "N_Level"
  | N_Never ->
      "N_Never"
  | N_Unpair ->
      "N_Unpair"
  | N_Voting_power ->
      "N_Voting_power"
  | N_Total_voting_power ->
      "N_Total_voting_power"
  | N_Keccak ->
      "N_Keccak"
  | N_Sha3 ->
      "N_Sha3"
  | N_Add_bls12_381_g1 ->
      "N_Add_bls12_381_g1"
  | N_Add_bls12_381_g2 ->
      "N_Add_bls12_381_g2"
  | N_Add_bls12_381_fr ->
      "N_Add_bls12_381_fr"
  | N_Mul_bls12_381_g1 ->
      "N_Mul_bls12_381_g1"
  | N_Mul_bls12_381_g2 ->
      "N_Mul_bls12_381_g2"
  | N_Mul_bls12_381_fr ->
      "N_Mul_bls12_381_fr"
  | N_Neg_bls12_381_g1 ->
      "N_Neg_bls12_381_g1"
  | N_Neg_bls12_381_g2 ->
      "N_Neg_bls12_381_g2"
  | N_Neg_bls12_381_fr ->
      "N_Neg_bls12_381_fr"
  | N_Pairing_check_bls12_381 ->
      "N_Pairing_check_bls12_381"
  | N_Mul_bls12_381_fr_z ->
      "N_Mul_bls12_381_fr_z"
  | N_Mul_bls12_381_z_fr ->
      "N_Mul_bls12_381_z_fr"
  | N_Int_bls12_381_z_fr ->
      "N_Int_bls12_381_z_fr"
  | N_Comb ->
      "N_Comb"
  | N_Uncomb ->
      "N_Uncomb"
  | N_Comb_get ->
      "N_Comb_get"
  | N_Comb_set ->
      "N_Comb_set"
  | N_Ticket ->
      "N_Ticket"
  | N_Read_ticket ->
      "N_Read_ticket"
  | N_Split_ticket ->
      "N_Split_ticket"
  | N_Join_tickets ->
      "N_Join_tickets"
  | N_Sapling_empty_state ->
      "N_Sapling_empty_state"
  | N_Sapling_verify_update ->
      "N_Sapling_verify_update"
  | N_Map_get_and_update ->
      "N_Map_get_and_update"
  | N_Big_map_get_and_update ->
      "N_Big_map_get_and_update"

(* ------------------------------------------------------------------------- *)

type args = arg list

and arg = {name : id; arg : Size.t}

let nullary : args = []

let unary xn x : args = [{name = xn; arg = x}]

let binary xn x yn y : args = {name = xn; arg = x} :: unary yn y

let ternary xn x yn y zn z : args = {name = xn; arg = x} :: binary yn y zn z

let pp_arg fmtr {name; arg} = Format.fprintf fmtr "%s = %a" name Size.pp arg

let pp_args fmtr args =
  Format.pp_print_list
    ~pp_sep:(fun fmtr () -> Format.fprintf fmtr ";")
    pp_arg
    fmtr
    args

type ir_sized_step = {instr_name : instruction_name; args : args}

type t = ir_sized_step list

let ir_sized_step instr_name args = {instr_name; args}

(* ------------------------------------------------------------------------- *)

let all_instructions =
  [ N_Drop;
    N_Dup;
    N_Swap;
    N_Const;
    N_Cons_pair;
    N_Car;
    N_Cdr;
    N_Cons_some;
    N_Cons_none;
    N_If_none;
    N_Left;
    N_Right;
    N_If_left;
    N_Cons_list;
    N_Nil;
    N_If_cons;
    N_List_map;
    N_List_iter;
    N_List_size;
    N_Empty_set;
    N_Set_iter;
    N_Set_mem;
    N_Set_update;
    N_Set_size;
    N_Empty_map;
    N_Map_map;
    N_Map_iter;
    N_Map_mem;
    N_Map_get;
    N_Map_update;
    N_Map_size;
    N_Empty_big_map;
    N_Big_map_mem;
    N_Big_map_get;
    N_Big_map_update;
    N_Concat_string;
    N_Concat_string_pair;
    N_Slice_string;
    N_String_size;
    N_Concat_bytes;
    N_Concat_bytes_pair;
    N_Slice_bytes;
    N_Bytes_size;
    N_Add_seconds_to_timestamp;
    N_Add_timestamp_to_seconds;
    N_Sub_timestamp_seconds;
    N_Diff_timestamps;
    N_Add_tez;
    N_Sub_tez;
    N_Mul_teznat;
    N_Mul_nattez;
    N_Ediv_teznat;
    N_Ediv_tez;
    N_Or;
    N_And;
    N_Xor;
    N_Not;
    N_Is_nat;
    N_Neg_nat;
    N_Neg_int;
    N_Abs_int;
    N_Int_nat;
    N_Add_intint;
    N_Add_intnat;
    N_Add_natint;
    N_Add_natnat;
    N_Sub_int;
    N_Mul_intint;
    N_Mul_intnat;
    N_Mul_natint;
    N_Mul_natnat;
    N_Ediv_intint;
    N_Ediv_intnat;
    N_Ediv_natint;
    N_Ediv_natnat;
    N_Lsl_nat;
    N_Lsr_nat;
    N_Or_nat;
    N_And_nat;
    N_And_int_nat;
    N_Xor_nat;
    N_Not_nat;
    N_Not_int;
    N_Seq;
    N_If;
    N_Loop;
    N_Loop_left;
    N_Dip;
    N_Exec;
    N_Lambda;
    N_Failwith;
    N_Nop;
    N_Compare;
    N_Eq;
    N_Neq;
    N_Lt;
    N_Gt;
    N_Le;
    N_Ge;
    N_Address;
    N_Contract;
    N_Transfer_tokens;
    N_Implicit_account;
    N_Create_contract;
    N_Set_delegate;
    N_Now;
    N_Balance;
    N_Check_signature_ed25519;
    N_Check_signature_secp256k1;
    N_Check_signature_p256;
    N_Hash_key;
    N_Pack;
    N_Unpack;
    N_Blake2b;
    N_Sha256;
    N_Sha512;
    N_Source;
    N_Sender;
    N_Self;
    N_Amount;
    N_Dig;
    N_Dug;
    N_DipN;
    N_DropN;
    N_DupN;
    N_ChainId;
    N_Level;
    N_Self_address;
    N_Never;
    N_Unpair;
    N_Voting_power;
    N_Total_voting_power;
    N_Keccak;
    N_Sha3;
    N_Add_bls12_381_g1;
    N_Add_bls12_381_g2;
    N_Add_bls12_381_fr;
    N_Mul_bls12_381_g1;
    N_Mul_bls12_381_g2;
    N_Mul_bls12_381_fr;
    N_Neg_bls12_381_g1;
    N_Neg_bls12_381_g2;
    N_Neg_bls12_381_fr;
    N_Pairing_check_bls12_381;
    N_Mul_bls12_381_fr_z;
    N_Mul_bls12_381_z_fr;
    N_Int_bls12_381_z_fr;
    N_Comb;
    N_Uncomb;
    N_Comb_get;
    N_Comb_set;
    N_Ticket;
    N_Read_ticket;
    N_Split_ticket;
    N_Join_tickets;
    N_Sapling_empty_state;
    N_Sapling_verify_update;
    N_Map_get_and_update;
    N_Big_map_get_and_update ]

let instruction_name_encoding =
  let open Data_encoding in
  def "ir_trace_encoding"
  @@ string_enum
       (List.map
          (fun instr_name ->
            (string_of_instruction_name instr_name, instr_name))
          all_instructions)

let args_encoding =
  let open Data_encoding in
  def "args_encoding"
  @@ list
       (conv
          (fun {name; arg} -> (name, arg))
          (fun (name, arg) -> {name; arg})
          (tup2 string Size.encoding))

let ir_sized_step_encoding =
  let open Data_encoding in
  def "ir_sized_step_encoding"
  @@ conv
       (fun {instr_name; args} -> (instr_name, args))
       (fun (instr_name, args) -> {instr_name; args})
       (tup2 instruction_name_encoding args_encoding)

let encoding =
  let open Data_encoding in
  def "interpreter_trace_encoding" @@ list ir_sized_step_encoding

(* ------------------------------------------------------------------------- *)

module Instructions = struct
  let drop = ir_sized_step N_Drop nullary

  let dup = ir_sized_step N_Dup nullary

  let swap = ir_sized_step N_Swap nullary

  let const = ir_sized_step N_Const nullary

  let cons_pair = ir_sized_step N_Cons_pair nullary

  let car = ir_sized_step N_Car nullary

  let cdr = ir_sized_step N_Cdr nullary

  let cons_some = ir_sized_step N_Cons_some nullary

  let cons_none = ir_sized_step N_Cons_none nullary

  let if_none = ir_sized_step N_If_none nullary

  let left = ir_sized_step N_Left nullary

  let right = ir_sized_step N_Right nullary

  let if_left = ir_sized_step N_If_left nullary

  let cons_list = ir_sized_step N_Cons_list nullary

  let nil = ir_sized_step N_Nil nullary

  let if_cons = ir_sized_step N_If_cons nullary

  let list_map list = ir_sized_step N_List_map (unary "list" list)

  let list_iter list = ir_sized_step N_List_iter (unary "list" list)

  let list_size _list = ir_sized_step N_List_size nullary

  let empty_set = ir_sized_step N_Empty_set nullary

  let set_iter set = ir_sized_step N_Set_iter (unary "set" set)

  let set_mem elt set = ir_sized_step N_Set_mem (binary "elt" elt "set" set)

  let set_update elt set =
    ir_sized_step N_Set_update (binary "elt" elt "set" set)

  let set_size _set = ir_sized_step N_Set_size nullary

  let empty_map = ir_sized_step N_Empty_map nullary

  let map_map map = ir_sized_step N_Map_map (unary "map" map)

  let map_iter map = ir_sized_step N_Map_iter (unary "map" map)

  let map_mem key map = ir_sized_step N_Map_mem (binary "key" key "map" map)

  let map_get key map = ir_sized_step N_Map_get (binary "key" key "map" map)

  let map_update key map =
    ir_sized_step N_Map_update (binary "key" key "map" map)

  let map_size _map = ir_sized_step N_Map_size nullary

  let empty_big_map = ir_sized_step N_Empty_big_map nullary

  let big_map_mem key big_map =
    ir_sized_step N_Big_map_mem (binary "key" key "big_map" big_map)

  let big_map_get key big_map =
    ir_sized_step N_Big_map_get (binary "key" key "big_map" big_map)

  let big_map_update key big_map =
    ir_sized_step N_Big_map_update (binary "key" key "big_map" big_map)

  let concat_string total_bytes list =
    ir_sized_step
      N_Concat_string
      (binary "total_bytes" total_bytes "list" list)

  let concat_string_pair str1 str2 =
    ir_sized_step N_Concat_string_pair (binary "str1" str1 "str2" str2)

  let slice_string string =
    ir_sized_step N_Slice_string (unary "string" string)

  let string_size _string = ir_sized_step N_String_size nullary

  let concat_bytes total_bytes list =
    ir_sized_step N_Concat_bytes (binary "total_bytes" total_bytes "list" list)

  let concat_bytes_pair str1 str2 =
    ir_sized_step N_Concat_bytes_pair (binary "str1" str1 "str2" str2)

  let slice_bytes bytes = ir_sized_step N_Slice_bytes (unary "bytes" bytes)

  let bytes_size bytes = ir_sized_step N_Bytes_size (unary "bytes" bytes)

  let add_seconds_to_timestamp seconds tstamp =
    ir_sized_step
      N_Add_seconds_to_timestamp
      (binary "seconds" seconds "tstamp" tstamp)

  let add_timestamp_to_seconds tstamp seconds =
    ir_sized_step
      N_Add_timestamp_to_seconds
      (binary "tstamp" tstamp "seconds" seconds)

  let sub_timestamp_seconds tstamp seconds =
    ir_sized_step
      N_Sub_timestamp_seconds
      (binary "tstamp" tstamp "seconds" seconds)

  let diff_timestamps tstamp1 tstamp2 =
    ir_sized_step
      N_Diff_timestamps
      (binary "tstamp1" tstamp1 "tstamp2" tstamp2)

  let add_tez _tez1 _tez2 = ir_sized_step N_Add_tez nullary

  let sub_tez _tez1 _tez2 = ir_sized_step N_Sub_tez nullary

  let mul_teznat _tez nat = ir_sized_step N_Mul_teznat (unary "nat" nat)

  let mul_nattez nat _tez = ir_sized_step N_Mul_nattez (unary "nat" nat)

  let ediv_teznat tez nat =
    ir_sized_step N_Ediv_teznat (binary "tez" tez "nat" nat)

  let ediv_tez _tez1 _tez2 = ir_sized_step N_Ediv_tez nullary

  let or_ = ir_sized_step N_Or nullary

  let and_ = ir_sized_step N_And nullary

  let xor_ = ir_sized_step N_Xor nullary

  let not_ = ir_sized_step N_Not nullary

  let is_nat _int = ir_sized_step N_Is_nat nullary

  let neg_nat nat = ir_sized_step N_Neg_nat (unary "nat" nat)

  let neg_int int = ir_sized_step N_Neg_int (unary "int" int)

  let abs_int int = ir_sized_step N_Abs_int (unary "int" int)

  let int_nat _nat = ir_sized_step N_Int_nat nullary

  let add_intint int1 int2 =
    ir_sized_step N_Add_intint (binary "int1" int1 "int2" int2)

  let add_intnat int nat =
    ir_sized_step N_Add_intnat (binary "int" int "nat" nat)

  let add_natint nat int =
    ir_sized_step N_Add_natint (binary "nat" nat "int" int)

  let add_natnat nat1 nat2 =
    ir_sized_step N_Add_natnat (binary "nat1" nat1 "nat2" nat2)

  let sub_int int1 int2 =
    ir_sized_step N_Sub_int (binary "int1" int1 "int2" int2)

  let mul_intint int1 int2 =
    ir_sized_step N_Mul_intint (binary "int1" int1 "int2" int2)

  let mul_intnat int nat =
    ir_sized_step N_Mul_intnat (binary "int" int "nat" nat)

  let mul_natint nat int =
    ir_sized_step N_Mul_natint (binary "nat" nat "int" int)

  let mul_natnat nat1 nat2 =
    ir_sized_step N_Mul_natnat (binary "nat1" nat1 "nat2" nat2)

  let ediv_intint int1 int2 =
    ir_sized_step N_Ediv_intint (binary "int1" int1 "int2" int2)

  let ediv_intnat int nat =
    ir_sized_step N_Ediv_intnat (binary "int" int "nat" nat)

  let ediv_natint nat int =
    ir_sized_step N_Ediv_natint (binary "nat" nat "int" int)

  let ediv_natnat nat1 nat2 =
    ir_sized_step N_Ediv_natnat (binary "nat1" nat1 "nat2" nat2)

  let lsl_nat nat1 _shift = ir_sized_step N_Lsl_nat (unary "nat" nat1)

  let lsr_nat nat1 _shift = ir_sized_step N_Lsr_nat (unary "nat" nat1)

  let or_nat nat1 nat2 =
    ir_sized_step N_Or_nat (binary "nat1" nat1 "nat2" nat2)

  let and_nat nat1 nat2 =
    ir_sized_step N_And_nat (binary "nat1" nat1 "nat2" nat2)

  let and_int_nat int nat =
    ir_sized_step N_And_int_nat (binary "int" int "nat" nat)

  let xor_nat nat1 nat2 =
    ir_sized_step N_Xor_nat (binary "nat1" nat1 "nat2" nat2)

  let not_nat nat = ir_sized_step N_Not_nat (unary "nat" nat)

  let not_int int = ir_sized_step N_Not_int (unary "int" int)

  let seq = ir_sized_step N_Seq nullary

  let if_ = ir_sized_step N_If nullary

  let loop = ir_sized_step N_Loop nullary

  let loop_left = ir_sized_step N_Loop_left nullary

  let dip = ir_sized_step N_Dip nullary

  let exec = ir_sized_step N_Exec nullary

  let lambda = ir_sized_step N_Lambda nullary

  let failwith_ = ir_sized_step N_Failwith nullary

  let nop = ir_sized_step N_Nop nullary

  let compare arg1 arg2 =
    ir_sized_step N_Compare (binary "arg1" arg1 "arg2" arg2)

  let eq = ir_sized_step N_Eq nullary

  let neq = ir_sized_step N_Neq nullary

  let lt = ir_sized_step N_Lt nullary

  let gt = ir_sized_step N_Gt nullary

  let le = ir_sized_step N_Le nullary

  let ge = ir_sized_step N_Ge nullary

  let address = ir_sized_step N_Address nullary

  let contract contract = ir_sized_step N_Contract (unary "contract" contract)

  let transfer_tokens arg tez contract =
    ir_sized_step
      N_Transfer_tokens
      (ternary "arg" arg "tez" tez "contract" contract)

  let set_delegate = ir_sized_step N_Set_delegate nullary

  let now = ir_sized_step N_Now nullary

  let balance = ir_sized_step N_Balance nullary

  let check_signature_ed25519 _pk _signature message =
    ir_sized_step N_Check_signature_ed25519 (unary "message" message)

  let check_signature_secp256k1 _pk _signature message =
    ir_sized_step N_Check_signature_secp256k1 (unary "message" message)

  let check_signature_p256 _pk _signature message =
    ir_sized_step N_Check_signature_p256 (unary "message" message)

  let hash_key = ir_sized_step N_Hash_key nullary

  let pack (micheline_size : Size.micheline_size) =
    ir_sized_step
      N_Pack
      (ternary
         "micheline_nodes"
         micheline_size.traversal
         "micheline_int_bytes"
         micheline_size.int_bytes
         "micheline_string_bytes"
         micheline_size.string_bytes)

  let unpack = ir_sized_step N_Unpack nullary

  let blake2b bytes = ir_sized_step N_Blake2b (unary "bytes" bytes)

  let sha256 bytes = ir_sized_step N_Sha256 (unary "bytes" bytes)

  let sha512 bytes = ir_sized_step N_Sha512 (unary "bytes" bytes)

  let source = ir_sized_step N_Source nullary

  let sender = ir_sized_step N_Sender nullary

  let self = ir_sized_step N_Self nullary

  let amount = ir_sized_step N_Amount nullary

  let dig depth = ir_sized_step N_Dig (unary "depth" depth)

  let dug depth = ir_sized_step N_Dug (unary "depth" depth)

  let dipn depth = ir_sized_step N_DipN (unary "depth" depth)

  let dropn depth = ir_sized_step N_DropN (unary "depth" depth)

  let dupn depth = ir_sized_step N_DupN (unary "depth" depth)

  let chain_id = ir_sized_step N_ChainId nullary

  let level = ir_sized_step N_Level nullary

  let self_address = ir_sized_step N_Self_address nullary

  let never = ir_sized_step N_Never nullary

  let unpair = ir_sized_step N_Unpair nullary

  let voting_power = ir_sized_step N_Voting_power nullary

  let total_voting_power = ir_sized_step N_Total_voting_power nullary

  let keccak bytes = ir_sized_step N_Keccak (unary "bytes" bytes)

  let sha3 bytes = ir_sized_step N_Sha3 (unary "bytes" bytes)

  let add_bls12_381_g1 = ir_sized_step N_Add_bls12_381_g1 nullary

  let add_bls12_381_g2 = ir_sized_step N_Add_bls12_381_g2 nullary

  let add_bls12_381_fr = ir_sized_step N_Add_bls12_381_fr nullary

  let mul_bls12_381_g1 = ir_sized_step N_Mul_bls12_381_g1 nullary

  let mul_bls12_381_g2 = ir_sized_step N_Mul_bls12_381_g2 nullary

  let mul_bls12_381_fr = ir_sized_step N_Mul_bls12_381_fr nullary

  let neg_bls12_381_g1 = ir_sized_step N_Neg_bls12_381_g1 nullary

  let neg_bls12_381_g2 = ir_sized_step N_Neg_bls12_381_g2 nullary

  let neg_bls12_381_fr = ir_sized_step N_Neg_bls12_381_fr nullary

  let pairing_check_bls12_381 length =
    ir_sized_step N_Pairing_check_bls12_381 (unary "length" length)

  let mul_bls12_381_fr_z = ir_sized_step N_Mul_bls12_381_fr_z nullary

  let mul_bls12_381_z_fr = ir_sized_step N_Mul_bls12_381_z_fr nullary

  let int_bls12_381_z_fr = ir_sized_step N_Int_bls12_381_z_fr nullary

  let comb depth = ir_sized_step N_Comb (unary "depth" depth)

  let uncomb depth = ir_sized_step N_Uncomb (unary "depth" depth)

  let comb_get key = ir_sized_step N_Comb_get (unary "key" key)

  let comb_set key = ir_sized_step N_Comb_set (unary "key" key)

  let ticket = ir_sized_step N_Ticket nullary

  let read_ticket = ir_sized_step N_Read_ticket nullary

  let split_ticket nat1 nat2 =
    ir_sized_step N_Split_ticket (binary "nat1" nat1 "nat2" nat2)

  let join_tickets size1 size2 =
    ir_sized_step N_Join_tickets (binary "contents1" size1 "contents2" size2)

  let sapling_empty_state = ir_sized_step N_Sapling_empty_state nullary

  let sapling_verify_update inputs outputs _state =
    ir_sized_step
      N_Sapling_verify_update
      (binary "inputs" inputs "outputs" outputs)

  let map_get_and_update key_size map_size =
    ir_sized_step
      N_Map_get_and_update
      (binary "key_size" key_size "map_size" map_size)
end

(* ------------------------------------------------------------------------- *)

open Script_typed_cps_ir

let rec size_of_comparable_value : type a. a comparable_ty -> a -> Size.t =
  fun (type a) (wit : a comparable_ty) (v : a) ->
   match wit with
   | Never_key _ ->
       Size.zero
   | Unit_key _ ->
       Size.unit
   | Int_key _ ->
       Size.integer v
   | Nat_key _ ->
       Size.integer v
   | String_key _ ->
       Size.string v
   | Bytes_key _ ->
       Size.bytes v
   | Mutez_key _ ->
       Size.mutez v
   | Bool_key _ ->
       Size.bool v
   | Key_hash_key _ ->
       Size.key_hash v
   | Timestamp_key _ ->
       Size.timestamp v
   | Address_key _ ->
       Size.address v
   | Pair_key ((leaf, _), (node, _), _) ->
       let (lv, rv) = v in
       let size =
         Size.add
           (size_of_comparable_value leaf lv)
           (size_of_comparable_value node rv)
       in
       Size.add size Size.one
   | Union_key ((left, _), (right, _), _) ->
       let size =
         match v with
         | L v ->
             size_of_comparable_value left v
         | R v ->
             size_of_comparable_value right v
       in
       Size.add size Size.one
   | Option_key (ty, _) -> (
     match v with
     | None ->
         Size.one
     | Some x ->
         Size.add (size_of_comparable_value ty x) Size.one )
   | Signature_key _ ->
       Size.signature v
   | Key_key _ ->
       Size.public_key v
   | Chain_id_key _ ->
       Size.chain_id v

let extract_compare_sized_step :
    type a. a comparable_ty -> a -> a -> ir_sized_step =
 fun comparable_ty x y ->
  Instructions.compare
    (size_of_comparable_value comparable_ty x)
    (size_of_comparable_value comparable_ty y)

(* Extracting the size of arguments from a pair of an IR instruction and a
   stack. *)
(* let extract_ir_sized_step : *)
    (* type a s b u. Alpha_context.t -> (a, s, b, u) descr -> b -> ir_sized_step = fun assert false *)
  (* fun (type a s b u) ctxt (descr : (a, s, b, u) descr) (stack : b) ->
   match (descr.instr, stack) with
   | (KDrop, _) ->
       Instructions.drop
   | (Dup, _) ->
       Instructions.dup
   | (Swap, _) ->
       Instructions.swap
   | (Const _, _) ->
       Instructions.const
   | (Cons_some, _) ->
       Instructions.cons_some
   | (Cons_none _, _) ->
       Instructions.cons_none
   | (If_none (_, _), _) ->
       Instructions.if_none
   | (Cons_pair, _) ->
       Instructions.cons_pair
   | (Car, _) ->
       Instructions.car
   | (Cdr, _) ->
       Instructions.cdr
   | (Cons_left, _) ->
       Instructions.left
   | (Cons_right, _) ->
       Instructions.right
   | (If_left (_, _), _) ->
       Instructions.if_left
   | (Cons_list, _) ->
       Instructions.cons_list
   | (Nil, _) ->
       Instructions.nil
   | (If_cons (_, _), _) ->
       Instructions.if_cons
   | (List_map _, (l, _)) ->
       Instructions.list_map (Size.list l)
   | (List_size, (l, _)) ->
       Instructions.list_size (Size.list l)
   | (List_iter _, (l, _)) ->
       Instructions.list_iter (Size.list l)
   | (Empty_set _, _) ->
       Instructions.empty_set
   | (Set_iter _, (set, _)) ->
       Instructions.set_iter (Size.set set)
   | (Set_mem, (v, (set, _))) -> (
     match descr.bef with
     | Item_t (_, Item_t (Set_t (comparable_ty, _), _, _), _) ->
         let sz = size_of_comparable_value comparable_ty v in
         Instructions.set_mem sz (Size.set set)
     | _ ->
         Stdlib.failwith "Deps.extract_ir_sized_step: unreachable case." )
   | (Set_update, (v, (_presence, (set, _)))) -> (
     match descr.bef with
     | Item_t
         ( _vtyp,
           Item_t (_presencetyp, Item_t (Set_t (comparable_ty, _), _, _), _),
           _ ) ->
         let sz = size_of_comparable_value comparable_ty v in
         Instructions.set_update sz (Size.set set)
     | _ ->
         Stdlib.failwith "Deps.extract_ir_sized_step: unreachable case." )
   | (Set_size, (set, _)) ->
       Instructions.set_size (Size.set set)
   | (Empty_map (_, _), _) ->
       Instructions.empty_map
   | (Map_map _, (map, _)) ->
       Instructions.map_map (Size.map map)
   | (Map_iter _, (map, _)) ->
       Instructions.map_iter (Size.map map)
   | (Map_mem, (key, (((module Map) as map), _))) ->
       let key_size = size_of_comparable_value Map.key_ty key in
       Instructions.map_mem key_size (Size.map map)
   | (Map_get, (key, (((module Map) as map), _))) ->
       let key_size = size_of_comparable_value Map.key_ty key in
       Instructions.map_get key_size (Size.map map)
   | (Map_update, (key, (_elt_opt, (((module Map) as map), _)))) ->
       let key_size = size_of_comparable_value Map.key_ty key in
       Instructions.map_update key_size (Size.map map)
   | (Map_size, (map, _)) ->
       Instructions.map_size (Size.map map)
   (* Big map operations *)
   | (Empty_big_map (_, _), _) ->
       Instructions.empty_big_map
   | (Big_map_mem, (key, ({diff = (module Map) as map; _}, _))) ->
       let key_size = size_of_comparable_value Map.key_ty key in
       Instructions.big_map_mem key_size (Size.map map)
   | (Big_map_get, (key, ({diff = (module Map) as map; _}, _))) ->
       let key_size = size_of_comparable_value Map.key_ty key in
       Instructions.big_map_get key_size (Size.map map)
   | (Big_map_update, (key, (_, ({diff = (module Map) as map; _}, _)))) ->
       let key_size = size_of_comparable_value Map.key_ty key in
       Instructions.big_map_update key_size (Size.map map)
   (* timestamp operations *)
   | (Add_seconds_to_timestamp, (s, (t, _))) ->
       Instructions.add_seconds_to_timestamp
         (Size.integer s)
         (Size.timestamp t)
   | (Add_timestamp_to_seconds, (t, (s, _))) ->
       Instructions.add_timestamp_to_seconds
         (Size.timestamp t)
         (Size.integer s)
   | (Sub_timestamp_seconds, (t, (s, _))) ->
       Instructions.sub_timestamp_seconds (Size.timestamp t) (Size.integer s)
   | (Diff_timestamps, (t1, (t2, _))) ->
       Instructions.diff_timestamps (Size.timestamp t1) (Size.timestamp t2)
   (* string operations *)
   | (Concat_string_pair, (x, (y, _))) ->
       Instructions.concat_string_pair (Size.string x) (Size.string y)
   | (Concat_string, (ss, _)) ->
       let list_size = Size.list ss in
       let total_bytes =
         List.fold_left
           (fun x s -> Size.(add x (string s)))
           Size.zero
           ss.elements
       in
       Instructions.concat_string list_size total_bytes
   | (Slice_string, (_offset, (_length, (s, _)))) ->
       Instructions.slice_string (Size.string s)
   | (String_size, (s, _)) ->
       Instructions.string_size (Size.string s)
   (* bytes operations *)
   | (Concat_bytes_pair, (x, (y, _))) ->
       Instructions.concat_bytes_pair (Size.bytes x) (Size.bytes y)
   | (Concat_bytes, (ss, _)) ->
       let list_size = Size.list ss in
       let total_bytes =
         List.fold_left
           (fun x s -> Size.(add x (bytes s)))
           Size.zero
           ss.elements
       in
       Instructions.concat_bytes total_bytes list_size
   | (Slice_bytes, (_offset, (_length, (b, _)))) ->
       Instructions.slice_bytes (Size.bytes b)
   | (Bytes_size, (b, _)) ->
       Instructions.bytes_size (Size.bytes b)
   (* currency operations *)
   | (Add_tez, (x, (y, _))) ->
       Instructions.add_tez (Size.mutez x) (Size.mutez y)
   | (Sub_tez, (x, (y, _))) ->
       Instructions.sub_tez (Size.mutez x) (Size.mutez y)
   | (Mul_teznat, (x, (y, _))) ->
       Instructions.mul_teznat (Size.mutez x) (Size.integer y)
   | (Mul_nattez, (x, (y, _))) ->
       Instructions.mul_nattez (Size.integer x) (Size.mutez y)
   (* boolean operations *)
   | (Or, _) ->
       Instructions.or_
   | (And, _) ->
       Instructions.and_
   | (Xor, _) ->
       Instructions.xor_
   | (Not, _) ->
       Instructions.not_
   (* integer operations *)
   | (Is_nat, (x, _)) ->
       Instructions.is_nat (Size.integer x)
   | (Abs_int, (x, _)) ->
       Instructions.abs_int (Size.integer x)
   | (Int_nat, (x, _)) ->
       Instructions.int_nat (Size.integer x)
   | (Neg_int, (x, _)) ->
       Instructions.neg_int (Size.integer x)
   | (Neg_nat, (x, _)) ->
       Instructions.neg_nat (Size.integer x)
   | (Add_intint, (x, (y, _))) ->
       Instructions.add_intint (Size.integer x) (Size.integer y)
   | (Add_intnat, (x, (y, _))) ->
       Instructions.add_intnat (Size.integer x) (Size.integer y)
   | (Add_natint, (x, (y, _))) ->
       Instructions.add_natint (Size.integer x) (Size.integer y)
   | (Add_natnat, (x, (y, _))) ->
       Instructions.add_natnat (Size.integer x) (Size.integer y)
   | (Sub_int, (x, (y, _))) ->
       Instructions.sub_int (Size.integer x) (Size.integer y)
   | (Mul_intint, (x, (y, _))) ->
       Instructions.mul_intint (Size.integer x) (Size.integer y)
   | (Mul_intnat, (x, (y, _))) ->
       Instructions.mul_intnat (Size.integer x) (Size.integer y)
   | (Mul_natint, (x, (y, _))) ->
       Instructions.mul_natint (Size.integer x) (Size.integer y)
   | (Mul_natnat, (x, (y, _))) ->
       Instructions.mul_natnat (Size.integer x) (Size.integer y)
   | (Ediv_teznat, (x, (y, _))) ->
       Instructions.ediv_teznat (Size.mutez x) (Size.integer y)
   | (Ediv_tez, (x, (y, _))) ->
       Instructions.ediv_tez (Size.mutez x) (Size.mutez y)
   | (Ediv_intint, (x, (y, _))) ->
       Instructions.ediv_intint (Size.integer x) (Size.integer y)
   | (Ediv_intnat, (x, (y, _))) ->
       Instructions.ediv_intnat (Size.integer x) (Size.integer y)
   | (Ediv_natint, (x, (y, _))) ->
       Instructions.ediv_natint (Size.integer x) (Size.integer y)
   | (Ediv_natnat, (x, (y, _))) ->
       Instructions.ediv_natnat (Size.integer x) (Size.integer y)
   | (Lsl_nat, (x, (y, _))) ->
       Instructions.lsl_nat (Size.integer x) (Size.integer y)
   | (Lsr_nat, (x, (y, _))) ->
       Instructions.lsr_nat (Size.integer x) (Size.integer y)
   | (Or_nat, (x, (y, _))) ->
       Instructions.or_nat (Size.integer x) (Size.integer y)
   | (And_nat, (x, (y, _))) ->
       Instructions.and_nat (Size.integer x) (Size.integer y)
   | (And_int_nat, (x, (y, _))) ->
       Instructions.and_int_nat (Size.integer x) (Size.integer y)
   | (Xor_nat, (x, (y, _))) ->
       Instructions.xor_nat (Size.integer x) (Size.integer y)
   | (Not_int, (x, _)) ->
       Instructions.not_int (Size.integer x)
   | (Not_nat, (x, _)) ->
       Instructions.not_int (Size.integer x)
   (* control *)
   | (Seq (_, _), _) ->
       Instructions.seq
   | (If (_, _), _) ->
       Instructions.if_
   | (Loop _, _) ->
       Instructions.loop
   | (Loop_left _, _) ->
       Instructions.loop_left
   | (Dip _, _) ->
       Instructions.dip
   | (Exec, _) ->
       Instructions.exec
   | (Lambda _, _) ->
       Instructions.lambda
   | (Failwith _tv, _) ->
       Instructions.failwith_
   | (Nop, _) ->
       Instructions.nop
   (* comparison *)
   | (Compare cmp_ty, (a, (b, _))) ->
       extract_compare_sized_step cmp_ty a b
   (* comparators *)
   | (Eq, (_cmpres, _)) ->
       Instructions.eq
   | (Neq, (_cmpres, _)) ->
       Instructions.neq
   | (Lt, (_cmpres, _)) ->
       Instructions.lt
   | (Le, (_cmpres, _)) ->
       Instructions.le
   | (Gt, (_cmpres, _)) ->
       Instructions.gt
   | (Ge, (_cmpres, _)) ->
       Instructions.ge
   (* packing *)
   | (Pack ty, (v, _)) ->
       let encoding_size = Size.of_encoded_value ctxt ty v in
       Instructions.pack encoding_size
   | (Unpack _, _) ->
       Instructions.unpack
   (* protocol *)
   | (Address, _) ->
       Instructions.address
   | (Contract _, (contract, _)) ->
       Instructions.contract (Size.address contract)
   | (Transfer_tokens, _) ->
       Stdlib.failwith
         "Deps.extract_ir_sized_step: Transfer_tokens not handled yet"
   | (Implicit_account, _) ->
       Stdlib.failwith
         "Deps.extract_ir_sized_step: Implicit_account not handled yet"
   | (Create_contract _, _) ->
       Stdlib.failwith
         "Deps.extract_ir_sized_step: Create_contract not handled yet"
   | (Set_delegate, _) ->
       Stdlib.failwith
         "Deps.extract_ir_sized_step: Set_delegate not handled yet"
   | (Balance, _) ->
       Instructions.balance
   | (Now, _) ->
       Instructions.now
   | (Check_signature, (public_key, (_signature, (message, _)))) -> (
     match public_key with
     | Signature.Ed25519 _pk ->
         let pk = Size.of_int Ed25519.size in
         let signature = Size.of_int Signature.size in
         let message = Size.bytes message in
         Instructions.check_signature_ed25519 pk signature message
     | Signature.Secp256k1 _pk ->
         let pk = Size.of_int Secp256k1.size in
         let signature = Size.of_int Signature.size in
         let message = Size.bytes message in
         Instructions.check_signature_secp256k1 pk signature message
     | Signature.P256 _pk ->
         let pk = Size.of_int P256.size in
         let signature = Size.of_int Signature.size in
         let message = Size.bytes message in
         Instructions.check_signature_p256 pk signature message )
   (* Stdlib.failwith
    *   "Deps.extract_ir_sized_step: Check_signature not handled yet" *)
   | (Hash_key, _) ->
       Instructions.hash_key
   | (Blake2b, (bytes, _)) ->
       Instructions.blake2b (Size.bytes bytes)
   | (Sha256, (bytes, _)) ->
       Instructions.sha256 (Size.bytes bytes)
   | (Sha512, (bytes, _)) ->
       Instructions.sha512 (Size.bytes bytes)
   | (Source, _) ->
       Instructions.source
   | (Sender, _) ->
       Instructions.sender
   | (Self _, _) ->
       Instructions.self
   | (Amount, _) ->
       Instructions.amount
   | (Dig (depth, _wit), _) ->
       Instructions.dig (Size.of_int depth)
   | (Dug (depth, _wit), _) ->
       Instructions.dug (Size.of_int depth)
   | (Dipn (depth, _wit, _code), _) ->
       Instructions.dipn (Size.of_int depth)
   | (Dropn (depth, _wit), _) ->
       Instructions.dropn (Size.of_int depth)
   | (ChainId, _) ->
       Instructions.chain_id
   | (Level, _) ->
       Instructions.level
   | (Self_address, _) ->
       Instructions.self_address
   | (Never, _) ->
       .
   | (Unpair, _) ->
       Instructions.unpair
   | (Voting_power, _) ->
       Instructions.voting_power
   | (Total_voting_power, _) ->
       Instructions.total_voting_power
   | (Keccak, (bytes, _)) ->
       Instructions.keccak (Size.bytes bytes)
   | (Sha3, (bytes, _)) ->
       Instructions.sha3 (Size.bytes bytes)
   | (Add_bls12_381_g1, _) ->
       Instructions.add_bls12_381_g1
   | (Add_bls12_381_g2, _) ->
       Instructions.add_bls12_381_g2
   | (Add_bls12_381_fr, _) ->
       Instructions.add_bls12_381_fr
   | (Mul_bls12_381_g1, _) ->
       Instructions.mul_bls12_381_g1
   | (Mul_bls12_381_g2, _) ->
       Instructions.mul_bls12_381_g2
   | (Mul_bls12_381_fr, _) ->
       Instructions.mul_bls12_381_fr
   | (Neg_bls12_381_g1, _) ->
       Instructions.neg_bls12_381_g1
   | (Neg_bls12_381_g2, _) ->
       Instructions.neg_bls12_381_g2
   | (Neg_bls12_381_fr, _) ->
       Instructions.neg_bls12_381_fr
   | (Pairing_check_bls12_381, (l, _)) ->
       let list_size = Size.list l in
       Instructions.pairing_check_bls12_381 list_size
   | (Mul_bls12_381_fr_z, _) ->
       Instructions.mul_bls12_381_fr_z
   | (Mul_bls12_381_z_fr, _) ->
       Instructions.mul_bls12_381_z_fr
   | (Int_bls12_381_fr, _) ->
       Instructions.int_bls12_381_z_fr
   | (Dup_n (n, _), _) ->
       Instructions.dupn (Size.of_int n)
   | (Apply _, _) ->
       Stdlib.failwith
         "Interpreter_workload.extract_ir_sized_step: Apply not handled yet"
   | (Comb (n, _), _) ->
       Instructions.comb (Size.of_int n)
   | (Uncomb (n, _), _) ->
       Instructions.uncomb (Size.of_int n)
   | (Comb_get (n, _), _) ->
       Instructions.comb_get (Size.of_int n)
   | (Comb_set (n, _), _) ->
       Instructions.comb_set (Size.of_int n)
   | (Ticket, _) ->
       Instructions.ticket
   | (Read_ticket, _) ->
       Instructions.read_ticket
   | (Split_ticket, (_ticket, ((amount_a, amount_b), _))) ->
       Instructions.split_ticket
         (Size.integer amount_a)
         (Size.integer amount_b)
   | (Join_tickets cmp_ty, ((ticket1, ticket2), _)) ->
       let size1 = size_of_comparable_value cmp_ty ticket1.contents in
       let size2 = size_of_comparable_value cmp_ty ticket2.contents in
       Instructions.join_tickets size1 size2
   | (Sapling_empty_state _, _) ->
       Instructions.sapling_empty_state
   | (Sapling_verify_update, (transaction, (_state, _))) ->
       let inputs = Size.sapling_transaction_inputs transaction in
       let outputs = Size.sapling_transaction_outputs transaction in
       let state = Size.zero in
       Instructions.sapling_verify_update inputs outputs state
   | (Map_get_and_update, (key, (_elt_opt, (((module Map) as map), _)))) ->
       let key_size = size_of_comparable_value Map.key_ty key in
       Instructions.map_get_and_update key_size (Size.map map)
   | ( Big_map_get_and_update,
       (key, (_elt_opt, ({diff = (module Map) as map; _}, _))) ) ->
       let key_size = size_of_comparable_value Map.key_ty key in
       Instructions.map_get_and_update key_size (Size.map map) *)

let extract_deps :
    type a s b u.
    Alpha_context.t ->
    Script_interpreter.step_constants ->
    (a, s, b, u) Script_typed_cps_ir.descr ->
    b ->
    a * t * Alpha_context.t =
  fun (type b a) _ctxt _step_constants (_descr : (a, s, b, u) descr) (_stack : b) ->
   let trace = ref [] in
   (* let module Logger : Script_interpreter.STEP_LOGGER = struct
     let log_interp _ctxt _descr _stack = ()

     let log_entry ctxt descr stack =
       trace := extract_ir_sized_step ctxt descr stack :: !trace

     let log_exit _ctxt _descr _stack = ()

     let get_log () = Environment.Error_monad.return_none
   end in *)
   let res = assert false
     (* Lwt_main.run
       (Script_interpreter.step
          (*(module Logger)*) None
          ctxt
          step_constants
          descr
          stack) *)
   in
   match res with
   | Error errs ->
       Format.eprintf
         "%a@."
         Error_monad.pp_print_error
         (List.map (fun x -> Environment.Ecoproto_error x) errs) ;
       raise (Failure "Interpreter_workload.extract_deps: error in step")
   | Ok (stack, ctxt) ->
       (stack, List.rev !trace, ctxt)

let arguments_of_sized_step {instr_name; args} =
  let instr = string_of_instruction_name instr_name in
  List.fold_left
    (fun acc {name; arg} ->
      Sparse_vec.String.(
        add acc (of_list [(instr ^ "_" ^ name, float_of_int arg)])))
    Sparse_vec.String.zero
    args

let trace_to_sparse_vec trace =
  List.fold_left
    (fun acc step -> Sparse_vec.String.add acc (arguments_of_sized_step step))
    Sparse_vec.String.zero
    trace
