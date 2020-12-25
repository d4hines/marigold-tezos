(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Alpha_context
open Script_typed_ir

[@@@warning "-21"]


let instr_to_string : type a b. (a, b) instr -> string = function
  | Drop ->
      "Drop"
  | Dup ->
      "Dup"
  | Swap ->
      "Swap"
  | Const _ ->
      "Const"
  | Cons_pair ->
      "Cons_pair"
  | Car ->
      "Car"
  | Cdr ->
      "Cdr"
  | Cons_some ->
      "Cons_some"
  | Cons_none _ ->
      "Cons_none"
  | If_none _ ->
      "If_none"
  | Cons_left ->
      "Cons_left"
  | Cons_right ->
      "Cons_right"
  | If_left _ ->
      "If_left"
  | Cons_list ->
      "Cons_list"
  | Nil ->
      "Nil"
  | If_cons _ ->
      "If_cons"
  | List_map _ ->
      "List_map"
  | List_iter _ ->
      "List_iter"
  | List_size ->
      "List_size"
  | Empty_set _ ->
      "Empty_set"
  | Set_iter _ ->
      "Set_iter"
  | Set_mem ->
      "Set_mem"
  | Set_update ->
      "Set_update"
  | Set_size ->
      "Set_size"
  | Empty_map _ ->
      "Empty_map"
  | Map_map _ ->
      "Map_map"
  | Map_iter _ ->
      "Map_iter"
  | Map_mem ->
      "Map_mem"
  | Map_get ->
      "Map_get"
  | Map_update ->
      "Map_update"
  | Map_size ->
      "Map_size"
  | Empty_big_map _ ->
      "Empty_big_map"
  | Big_map_mem ->
      "Big_map_mem"
  | Big_map_get ->
      "Big_map_get"
  | Big_map_update ->
      "Big_map_update"
  | Concat_string ->
      "Concat_string"
  | Concat_string_pair ->
      "Concat_string_pair"
  | Slice_string ->
      "Slice_string"
  | String_size ->
      "String_size"
  | Concat_bytes ->
      "Concat_bytes"
  | Concat_bytes_pair ->
      "Concat_bytes_pair"
  | Slice_bytes ->
      "Slice_bytes"
  | Bytes_size ->
      "Bytes_size"
  | Add_seconds_to_timestamp ->
      "Add_seconds_to_timestamp"
  | Add_timestamp_to_seconds ->
      "Add_timestamp_to_seconds"
  | Sub_timestamp_seconds ->
      "Sub_timestamp_seconds"
  | Diff_timestamps ->
      "Diff_timestamps"
  | Add_tez ->
      "Add_tez"
  | Sub_tez ->
      "Sub_tez"
  | Mul_teznat ->
      "Mul_teznat"
  | Mul_nattez ->
      "Mul_nattez"
  | Ediv_teznat ->
      "Ediv_teznat"
  | Ediv_tez ->
      "Ediv_tez"
  | Or ->
      "Or"
  | And ->
      "And"
  | Xor ->
      "Xor"
  | Not ->
      "Not"
  | Is_nat ->
      "Is_nat"
  | Neg_nat ->
      "Neg_nat"
  | Neg_int ->
      "Neg_int"
  | Abs_int ->
      "Abs_int"
  | Int_nat ->
      "Int_nat"
  | Add_intint ->
      "Add_intint"
  | Add_intnat ->
      "Add_intnat"
  | Add_natint ->
      "Add_natint"
  | Add_natnat ->
      "Add_natnat"
  | Sub_int ->
      "Sub_int"
  | Mul_intint ->
      "Mul_intint"
  | Mul_intnat ->
      "Mul_intnat"
  | Mul_natint ->
      "Mul_natint"
  | Mul_natnat ->
      "Mul_natnat"
  | Ediv_intint ->
      "Ediv_intint"
  | Ediv_intnat ->
      "Ediv_intnat"
  | Ediv_natint ->
      "Ediv_natint"
  | Ediv_natnat ->
      "Ediv_natnat"
  | Lsl_nat ->
      "Lsl_nat"
  | Lsr_nat ->
      "Lsr_nat"
  | Or_nat ->
      "Or_nat"
  | And_nat ->
      "And_nat"
  | And_int_nat ->
      "And_int_nat"
  | Xor_nat ->
      "Xor_nat"
  | Not_nat ->
      "Not_nat"
  | Not_int ->
      "Not_int"
  | Seq _ ->
      "Seq"
  | If _ ->
      "If"
  | Loop _ ->
      "Loop"
  | Loop_left _ ->
      "Loop_left"
  | Dip _ ->
      "Dip"
  | Exec ->
      "Exec"
  | Apply _ ->
      "Apply"
  | Lambda _ ->
      "Lambda"
  | Failwith _ ->
      "Failwith"
  | Nop ->
      "Nop"
  | Compare _ ->
      "Compare"
  | Eq ->
      "Eq"
  | Neq ->
      "Neq"
  | Lt ->
      "Lt"
  | Gt ->
      "Gt"
  | Le ->
      "Le"
  | Ge ->
      "Ge"
  | Address ->
      "Address"
  | Contract _ ->
      "Contract"
  | Transfer_tokens ->
      "Transfer_tokens"
  | Implicit_account ->
      "Implicit_account"
  | Create_contract _ ->
      "Create_contract"
  | Set_delegate ->
      "Set_delegate"
  | Now ->
      "Now"
  | Balance ->
      "Balance"
  | Level ->
      "Level"
  | Check_signature ->
      "Check_signature"
  | Hash_key ->
      "Hash_key"
  | Pack _ ->
      "Pack"
  | Unpack _ ->
      "Unpack"
  | Blake2b ->
      "Blake2b"
  | Sha256 ->
      "Sha256"
  | Sha512 ->
      "Sha512"
  | Source ->
      "Source"
  | Sender ->
      "Sender"
  | Self _ ->
      "Self"
  | Self_address ->
      "Self_address"
  | Amount ->
      "Amount"
  | Dig _ ->
      "Dig"
  | Dug _ ->
      "Dug"
  | Dipn _ ->
      "Dipn"
  | Dropn _ ->
      "Dropn"
  | ChainId ->
      "ChainId"
  | Never ->
      "Never"
  | Unpair ->
      "Unpair"
  | _ ->
      "Go fish..."

type my_item =
  | My_bool of bool
  | My_nat of Script_int.n Script_int.num
  | My_int of Script_int.z Script_int.num
  | My_pair of (my_item * my_item)
  | My_list of my_item list
  | My_unit

and my_instr =
  (* Stack instructions *)
  | My_dup
  | My_swap
  | My_push of my_item
  | My_dip
  | My_undip
  | My_drop
  (* Loop *)
  | My_loop_if_not of int
  | My_loop_jump of int (* Pair instructions *)
  | My_car
  | My_cons_pair
  | My_nil
  (* Arithmetic instructions *)
  | My_compare
  | My_neq
  | My_sub
  | My_mul_int
  | My_halt
  | My_add

let my_instr_to_str i =
  match i with
  | My_dup ->
      "My_dup"
  | My_swap ->
      "My_swap"
  | My_push _ ->
      "My_push"
  | My_dip ->
      "My_dip"
  | My_undip ->
      "My_undip"
  | My_drop ->
      "My_drop"
  | My_loop_if_not n ->
      "My_loop_if_not " ^ Int32.to_string @@ Int32.of_int n
  | My_loop_jump n ->
      "My_loop_jump " ^ Int32.to_string @@ Int32.of_int n
  | My_car ->
      "My_car"
  | My_compare ->
      "My_compare"
  | My_neq ->
      "My_neq"
  | My_sub ->
      "My_sub"
  | My_mul_int ->
      "My_mul_int"
  | My_halt ->
      "My_halt"
  | My_cons_pair ->
      "My_cons_pair"
  | My_nil ->
      "My_nil"
  | My_add ->
      "My_add"

type my_stack = my_item list

type my_dip_stack = my_stack
let ty_to_string : type a. a ty -> string = function
  | Unit_t _ ->
      "Unit_t"
  | Int_t _ ->
      "Int_t"
  | Nat_t _ ->
      "Nat_t"
  | Signature_t _ ->
      "Signature_t"
  | String_t _ ->
      "String_t"
  | Bytes_t _ ->
      "Bytes_t"
  | Mutez_t _ ->
      "Mutez_t"
  | Key_hash_t _ ->
      "Key_hash_t"
  | Key_t _ ->
      "Key_t"
  | Timestamp_t _ ->
      "Timestamp_t"
  | Address_t _ ->
      "Address_t"
  | Bool_t _ ->
      "Bool_t"
  | Pair_t _ ->
      "Pair_t"
  | Union_t _ ->
      "Union_t"
  | Lambda_t _ ->
      "Lambda_t"
  | Option_t _ ->
      "Option_t"
  | List_t _ ->
      "List_t"
  | Set_t _ ->
      "Set_t"
  | Map_t _ ->
      "Map_t"
  | Big_map_t _ ->
      "Big_map_t"
  | Contract_t _ ->
      "Contract_t"
  | Operation_t _ ->
      "Operation_t"
  | Chain_id_t _ ->
      "Chain_id_t"
  | Never_t _ ->
      "Never_t"
  | _ ->
      raise @@ Failure "ty_to_string fail"

let rec myfy_item : type a. a ty * a -> my_item = function
  | (Nat_t _, n) ->
      My_nat n
  | (Int_t _, n) ->
      My_int n
  | (Bool_t _, b) ->
      My_bool b
  | (Pair_t ((a_ty, _, _), (b_ty, _, _), _), (a, b)) ->
      My_pair (myfy_item (a_ty, a), myfy_item (b_ty, b))
  | (Unit_t _, _) ->
      My_unit
  | (ty, _) ->
      raise (Failure ("myfy item:" ^ ty_to_string ty))

let rec repeat x = function 0 -> [x] | n -> x :: repeat x (n - 1)

let rec translate : type bef aft. (bef, aft) descr -> my_instr list =
 fun descr ->
  match descr.instr with
  | Dup ->
      [My_dup]
  | Swap ->
      [My_swap]
  | Const x ->
      let (Item_t (x_ty, _, _)) = descr.aft in
      [My_push (myfy_item (x_ty, x))]
  | Dip x ->
      [My_dip] @ translate x @ [My_undip]
  | Dipn (n, _, x) ->
      (* raise (Failure "dipn"); *)
      repeat My_dip (n - 1) @ translate x @ [My_undip]
  | Drop ->
      [My_drop]
  | Loop d ->
      let cbody = translate d in
      let body_len = List.length cbody in
      (My_loop_if_not (body_len + 2) :: cbody) @ [My_loop_jump (-body_len - 1)]
  | Car ->
      [My_car]
  | Compare _ ->
      [My_compare]
  | Neq ->
      [My_neq]
  | Sub_int ->
      [My_sub]
  | Mul_intint ->
      [My_mul_int]
  | Seq (d, d') ->
      translate d @ translate d'
  | Nop ->
      []
  | Nil ->
      [My_nil]
  | Cons_pair ->
      [My_cons_pair]
  | Add_intint ->
      [My_add]
  | x ->
      raise @@ Failure ("Failed tranlsating " ^ instr_to_string x)

let translate : type bef aft. (bef, aft) descr -> my_instr list =
 fun descr -> translate descr @ [My_halt]

let rec my_item_to_string = function
  | My_bool true ->
      "true"
  | My_bool false ->
      "false"
  | My_nat n ->
      "N" ^ Script_int.to_string n
  | My_int n ->
      "Z" ^ Script_int.to_string n
  | My_list lst ->
      "["
      ^ List.fold_right (fun x y -> my_item_to_string x ^ ":" ^ y) lst "()"
      ^ "]"
  | My_pair (a, b) ->
      "P(" ^ my_item_to_string a ^ " , " ^ my_item_to_string b ^ ")"
  | My_unit ->
      "unit"
let myfy_item : type a. a ty * a -> my_item = function
  | (Nat_t _, n) ->
      My_nat n
  | (Int_t _, n) ->
      My_int n
  | (Bool_t _, b) ->
      My_bool b
  | (Pair_t ((a_ty, _, _), (b_ty, _, _), _), (a, b)) ->
      My_pair (myfy_item (a_ty, a), myfy_item (b_ty, b))
  | (_ty, _) ->
      raise (Failure "unimplemented")

let rec myfy_stack : type a. a stack_ty * a -> my_stack = function
  | (Empty_t, ()) ->
      []
  | (Item_t (item_ty, stack_ty, _), (item, stack)) ->
      myfy_item (item_ty, item) :: myfy_stack (stack_ty, stack)

let rec yfym_item : type a. a ty * my_item -> a = function
  | (Nat_t _, My_nat n) ->
      n
  | (Int_t _, My_int n) ->
      n
  | (Bool_t _, My_bool b) ->
      b
  | (Pair_t ((a_ty, _, _), (b_ty, _, _), _), My_pair (a, b)) ->
      (yfym_item (a_ty, a), yfym_item (b_ty, b))
  | (Pair_t ((_a_ty, _, _), (_b_ty, _, _), _), x) ->
      raise (Failure ("foo " ^ my_item_to_string x))
  | (List_t (ele_ty, _), My_list lst) ->
      {
        elements = List.map (fun ele -> yfym_item (ele_ty, ele)) lst;
        length = List.length lst;
      }
  | (ty, _) ->
      raise (Failure ("yfym item: " ^ ty_to_string ty))

let rec yfym_stack : type a. a stack_ty * my_stack -> a = function
  | (Empty_t, []) ->
      ()
  | (Item_t (hd_ty, tl_ty, _), hd :: tl) ->
      (yfym_item (hd_ty, hd), yfym_stack (tl_ty, tl))
  | ((Item_t _ as _stack_ty), _stack) ->
      raise (Failure "yfym stack ty. type: ")
  | (_stack_ty, (_ :: _ as _stack)) ->
      raise (Failure "yfym stack item. type: ")

let ty_to_string : type a. a ty -> string = function
  | Unit_t _ ->
      "Unit_t"
  | Int_t _ ->
      "Int_t"
  | Nat_t _ ->
      "Nat_t"
  | Signature_t _ ->
      "Signature_t"
  | String_t _ ->
      "String_t"
  | Bytes_t _ ->
      "Bytes_t"
  | Mutez_t _ ->
      "Mutez_t"
  | Key_hash_t _ ->
      "Key_hash_t"
  | Key_t _ ->
      "Key_t"
  | Timestamp_t _ ->
      "Timestamp_t"
  | Address_t _ ->
      "Address_t"
  | Bool_t _ ->
      "Bool_t"
  | Pair_t _ ->
      "Pair_t"
  | Union_t _ ->
      "Union_t"
  | Lambda_t _ ->
      "Lambda_t"
  | Option_t _ ->
      "Option_t"
  | List_t _ ->
      "List_t"
  | Set_t _ ->
      "Set_t"
  | Map_t _ ->
      "Map_t"
  | Big_map_t _ ->
      "Big_map_t"
  | Contract_t _ ->
      "Contract_t"
  | Operation_t _ ->
      "Operation_t"
  | Chain_id_t _ ->
      "Chain_id_t"
  | Never_t _ ->
      "Never_t"
  | _ ->
      "Go fish..."

(* | stack_ty , stack ->
 *    raise (Failure ("yfym stack. type: " ^ (stack_ty_to_string stack_ty) ^ ". content: " ^ (my_stack_to_string stack))) *)
let rec stack_ty_to_string : type a. a stack_ty -> string = function
  | Item_t (hd, tl, _) ->
      ty_to_string hd ^ " :: " ^ stack_ty_to_string tl
  | Empty_t ->
      "()"

let rec my_stack_to_string = function
  | [] ->
      "()"
  | hd :: tl ->
      my_item_to_string hd ^ " :: " ^ my_stack_to_string tl

let rec yfym_stack : type a. a stack_ty * my_stack -> a = function
  | (Empty_t, []) ->
      ()
  | (Item_t (hd_ty, tl_ty, _), hd :: tl) ->
      (yfym_item (hd_ty, hd), yfym_stack (tl_ty, tl))
  | ((Item_t _ as stack_ty), stack) ->
      raise
        (Failure
           ( "yfym stack ty. type: "
           ^ stack_ty_to_string stack_ty
           ^ ". content: " ^ my_stack_to_string stack ))
  | (stack_ty, (_ :: _ as stack)) ->
      raise
        (Failure
           ( "yfym stack item. type: "
           ^ stack_ty_to_string stack_ty
           ^ ". content: " ^ my_stack_to_string stack ))

(* | stack_ty , stack ->
 *    raise (Failure ("yfym stack. type: " ^ (stack_ty_to_string stack_ty) ^ ". content: " ^ (my_stack_to_string stack))) *)

(* match x with
  | (Empty_t, []) ->
      ()
  | (Item_t (hd_ty, tl_ty, _), hd :: tl) ->
      (yfym_item (hd_ty, hd), yfym_stack (tl_ty, tl))
  | ((Item_t _ as stack_ty), stack) ->
    raise (Failure ("yfym stack ty. type: " ^ (stack_ty_to_string stack_ty) ^ ". content: " ^ (my_stack_to_string stack)))
  | (stack_ty, (_ :: _ as stack)) ->
    raise (Failure ("yfym stack item. type: " ^ (stack_ty_to_string stack_ty) ^ ". content: " ^ (my_stack_to_string stack))) *)
