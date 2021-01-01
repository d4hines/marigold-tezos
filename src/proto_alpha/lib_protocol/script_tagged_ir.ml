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

[@@@warning "-26"]

[@@@warning "-27"]

module Option = struct
  include Option

  let get x = match x with None -> assert false | Some x -> x
end

external ( = ) : 'a -> 'a -> bool = "%equal"

module Script_typed_ir = struct
  include Script_typed_ir

  let equal_ex_ty = ( = )
end

let rec ty_to_string : type a. a ty -> string = function
  | Unit_t None -> "Unit_t None"
  | Int_t _ -> "Int_t"
  | Nat_t _ -> "Nat_t"
  | Signature_t _ -> "Signature_t"
  | String_t _ -> "String_t"
  | Bytes_t _ -> "Bytes_t"
  | Mutez_t _ -> "Mutez_t"
  | Key_hash_t _ -> "Key_hash_t"
  | Key_t _ -> "Key_t"
  | Timestamp_t _ -> "Timestamp_t"
  | Address_t _ -> "Address_t"
  | Bool_t _ -> "Bool_t"
  | Pair_t ((x, _, _), (y, _, _), None) ->
      "Pair_t (" ^ ty_to_string x ^ " , " ^ ty_to_string y ^ ")"
  | Union_t _ -> "Union_t"
  | Lambda_t _ -> "Lambda_t"
  | Option_t _ -> "Option_t"
  | List_t _ -> "List_t"
  | Set_t _ -> "Set_t"
  | Map_t _ -> "Map_t"
  | Big_map_t _ -> "Big_map_t"
  | Contract_t _ -> "Contract_t"
  | Operation_t _ -> "Operation_t"
  | Chain_id_t _ -> "Chain_id_t"
  | Never_t _ -> "Never_t"
  | _ -> raise @@ Failure "ty_to_string fail"

  let rec cty_to_string : type a. a comparable_ty -> string = function
  | Unit_key None -> "Unit_key None"
  | Int_key _ -> "Int_key"
  | Nat_key _ -> "Nat_key"
  | Signature_key _ -> "Signature_key"
  | String_key _ -> "String_key"
  | Bytes_key _ -> "Bytes_key"
  | Mutez_key _ -> "Mutez_key"
  | Key_hash_key _ -> "Key_hash_key"
  | Key_key _ -> "Key_key"
  | Timestamp_key _ -> "Timestamp_key"
  | Address_key _ -> "Address_key"
  | Bool_key _ -> "Bool_key"
  | Pair_key ((x, _), (y, _), None) ->
      "Pair_key (" ^ cty_to_string x ^ " , " ^ cty_to_string y ^ ")"
  | Union_key _ -> "Union_key"
  | Option_key _ -> "Option_key"
  | Chain_id_key _ -> "Chain_id_key"
  | Never_key _ -> "Never_key"
  | _ -> raise @@ Failure "ty_to_string fail"

(* | stack_ty , stack ->
 *    raise (Failure ("yfym stack. type: " ^ (stack_ty_to_string stack_ty) ^ ". content: " ^ (my_stack_to_string stack))) *)
let rec stack_ty_to_string : type a. a stack_ty -> string = function
  | Item_t (hd, tl, _) -> ty_to_string hd ^ "\n::\n" ^ stack_ty_to_string tl
  | Empty_t -> "()"

module Obj = struct
  external magic : 'a -> 'b = "%identity"
end

let print_endline x = Format.pp_print_string Format.std_formatter (x ^ "\n")

module Format = struct
  include Format
end

let instr_to_string : type a b. (a, b) instr -> string = function
  | Drop -> "Drop"
  | Dup -> "Dup"
  | Swap -> "Swap"
  | Const _ -> "Const"
  | Cons_pair -> "Cons_pair"
  | Car -> "Car"
  | Cdr -> "Cdr"
  | Cons_some -> "Cons_some"
  | Cons_none _ -> "Cons_none"
  | If_none _ -> "If_none"
  | Cons_left -> "Cons_left"
  | Cons_right -> "Cons_right"
  | If_left _ -> "If_left"
  | Cons_list -> "Cons_list"
  | Nil -> "Nil"
  | If_cons _ -> "If_cons"
  | List_map _ -> "List_map"
  | List_iter _ -> "List_iter"
  | List_size -> "List_size"
  | Empty_set _ -> "Empty_set"
  | Set_iter _ -> "Set_iter"
  | Set_mem -> "Set_mem"
  | Set_update -> "Set_update"
  | Set_size -> "Set_size"
  | Empty_map _ -> "Empty_map"
  | Map_map _ -> "Map_map"
  | Map_iter _ -> "Map_iter"
  | Map_mem -> "Map_mem"
  | Map_get -> "Map_get"
  | Map_update -> "Map_update"
  | Map_size -> "Map_size"
  | Empty_big_map _ -> "Empty_big_map"
  | Big_map_mem -> "Big_map_mem"
  | Big_map_get -> "Big_map_get"
  | Big_map_update -> "Big_map_update"
  | Concat_string -> "Concat_string"
  | Concat_string_pair -> "Concat_string_pair"
  | Slice_string -> "Slice_string"
  | String_size -> "String_size"
  | Concat_bytes -> "Concat_bytes"
  | Concat_bytes_pair -> "Concat_bytes_pair"
  | Slice_bytes -> "Slice_bytes"
  | Bytes_size -> "Bytes_size"
  | Add_seconds_to_timestamp -> "Add_seconds_to_timestamp"
  | Add_timestamp_to_seconds -> "Add_timestamp_to_seconds"
  | Sub_timestamp_seconds -> "Sub_timestamp_seconds"
  | Diff_timestamps -> "Diff_timestamps"
  | Add_tez -> "Add_tez"
  | Sub_tez -> "Sub_tez"
  | Mul_teznat -> "Mul_teznat"
  | Mul_nattez -> "Mul_nattez"
  | Ediv_teznat -> "Ediv_teznat"
  | Ediv_tez -> "Ediv_tez"
  | Or -> "Or"
  | And -> "And"
  | Xor -> "Xor"
  | Not -> "Not"
  | Is_nat -> "Is_nat"
  | Neg_nat -> "Neg_nat"
  | Neg_int -> "Neg_int"
  | Abs_int -> "Abs_int"
  | Int_nat -> "Int_nat"
  | Add_intint -> "Add_intint"
  | Add_intnat -> "Add_intnat"
  | Add_natint -> "Add_natint"
  | Add_natnat -> "Add_natnat"
  | Sub_int -> "Sub_int"
  | Mul_intint -> "Mul_intint"
  | Mul_intnat -> "Mul_intnat"
  | Mul_natint -> "Mul_natint"
  | Mul_natnat -> "Mul_natnat"
  | Ediv_intint -> "Ediv_intint"
  | Ediv_intnat -> "Ediv_intnat"
  | Ediv_natint -> "Ediv_natint"
  | Ediv_natnat -> "Ediv_natnat"
  | Lsl_nat -> "Lsl_nat"
  | Lsr_nat -> "Lsr_nat"
  | Or_nat -> "Or_nat"
  | And_nat -> "And_nat"
  | And_int_nat -> "And_int_nat"
  | Xor_nat -> "Xor_nat"
  | Not_nat -> "Not_nat"
  | Not_int -> "Not_int"
  | Seq _ -> "Seq"
  | If _ -> "If"
  | Loop _ -> "Loop"
  | Loop_left _ -> "Loop_left"
  | Dip _ -> "Dip"
  | Exec -> "Exec"
  | Apply _ -> "Apply"
  | Lambda _ -> "Lambda"
  | Failwith _ -> "Failwith"
  | Nop -> "Nop"
  | Compare _ -> "Compare"
  | Eq -> "Eq"
  | Neq -> "Neq"
  | Lt -> "Lt"
  | Gt -> "Gt"
  | Le -> "Le"
  | Ge -> "Ge"
  | Address -> "Address"
  | Contract _ -> "Contract"
  | Transfer_tokens -> "Transfer_tokens"
  | Implicit_account -> "Implicit_account"
  | Create_contract _ -> "Create_contract"
  | Set_delegate -> "Set_delegate"
  | Now -> "Now"
  | Balance -> "Balance"
  | Level -> "Level"
  | Check_signature -> "Check_signature"
  | Hash_key -> "Hash_key"
  | Pack _ -> "Pack"
  | Unpack _ -> "Unpack"
  | Blake2b -> "Blake2b"
  | Sha256 -> "Sha256"
  | Sha512 -> "Sha512"
  | Source -> "Source"
  | Sender -> "Sender"
  | Self _ -> "Self"
  | Self_address -> "Self_address"
  | Amount -> "Amount"
  | Dig _ -> "Dig"
  | Dug _ -> "Dug"
  | Dipn _ -> "Dipn"
  | Dropn _ -> "Dropn"
  | ChainId -> "ChainId"
  | Never -> "Never"
  | Unpair -> "Unpair"
  | _ -> "Go fish..."

module type My_boxed_map = sig
  type key

  type value

  module OPS : S.MAP with type key = key

  val value : value OPS.t
end

module Format_ = Format
module List_ = List

let show_to_pp show fmt v = Format.fprintf fmt "%s" (show v)

let pp_z_num = show_to_pp Script_int.to_string

let pp_n_num = show_to_pp Script_int.to_string

module Script_timestamp = struct
  include Script_timestamp

  let pp = show_to_pp Script_timestamp.to_string
end

type n_num = Script_int.n Script_int.num

type z_num = Script_int.z Script_int.num

let eq_script_int a b =
  match Script_int.compare a b with 0 -> true | _ -> false

let equal_z_num = eq_script_int

let equal_n_num = eq_script_int

let compare_n_num = Script_int.compare

let compare_z_num = Script_int.compare

module Lazy_storage = struct
  include Lazy_storage

  let compare_diffs x y = 0

  let pp_diffs fmt x = Format.fprintf fmt "<lazy_storage>"
end

module Big_map = struct
  include Big_map

  module Id = struct
    include Id

    let compare : t -> t -> int = Obj.magic Z.compare

    let pp : Format.formatter -> t -> unit = Obj.magic Z.pp_print
  end
end

let compare_ty x y = 0

let poly_a x = assert false

type contract_type = Contract_type : 'a ty * string -> contract_type

(* type transfer_type = 
| Transfer_type : 'a ty * Tez.t * contract_type -> transfer_type *)

let pp_contract_type : Format.formatter -> contract_type -> unit =
  show_to_pp @@ fun (Contract_type (t, s)) -> ty_to_string t ^ ":" ^ s

let compare_contract_type x y = 0

let compare_packed_internal_operation x y = 0

let pp_packed_internal_operation fmt _ = Format.fprintf fmt "<operation>"

type compare_type =
  | Compare_type : 'a comparable_ty -> compare_type [@deriving show]

let pp_compare_type fmt x =
  let (Compare_type x) = x in
  Format.fprintf fmt "<comparable ty>"

(* let pp_transfer_type fmt _ = Format.fprintf fmt "<transfer type>" *)

type my_item =
  | My_bool of bool
  | My_nat of n_num
  | My_int of z_num
  | My_pair of (my_item * my_item)
  | My_left of my_item
  | My_right of my_item
  | My_some of my_item
  | My_none
  | My_list of my_item list
  | My_unit
  | My_big_map of my_big_map
  | My_map of my_map
  | My_string of string
  | My_mutez of Tez.t
  | My_lambda_item of int * my_item list
  | My_address_item of Contract.t * string
  | My_contract_item of Contract.t * contract_type
  | My_ret_address of int
  | My_timestamp of Script_timestamp.t
  | My_operation of packed_internal_operation * Lazy_storage.diffs option
[@@deriving ord, show { with_path = false }]

and my_map =
  ((module My_boxed_map with type key = my_item and type value = my_item)
  [@equal fun a b -> a = b]
  [@compare fun a b -> 0]
  [@printer
    fun fmt x ->
      let module Boxed_map = ( val x : My_boxed_map
                                 with type key = my_item
                                  and type value = my_item )
      in
      let x =
        Boxed_map.OPS.bindings Boxed_map.value
        |> List.map (fun (k, v) ->
               let v_str = show_my_item v in
               show_my_item k ^ " = " ^ v_str ^ ";")
        |> String.concat ""
      in
      Format.fprintf fmt "my_map { %s }" x])

and my_big_map = {
  id : Big_map.Id.t option;
  diff : my_big_map_diff;
  value_type : Script_typed_ir.ex_ty;
      [@compare fun a b -> 0]
      [@printer
        fun fmt (Ex_ty x) -> Format.fprintf fmt "Ex_ty %s" (ty_to_string x)]
}

and my_big_map_diff =
  ((module My_boxed_map with type key = my_item and type value = my_item option)
  [@equal fun a b -> a = b]
  [@compare fun a b -> 0]
  [@printer
    fun fmt x ->
      let module Boxed_map = ( val x : My_boxed_map
                                 with type key = my_item
                                  and type value = my_item option )
      in
      let x =
        Boxed_map.OPS.bindings Boxed_map.value
        |> List.map (fun (k, v) ->
               let v_str =
                 match v with
                 | None -> "None"
                 | Some x -> "Some " ^ show_my_item x
               in
               show_my_item k ^ " = " ^ v_str ^ ";")
        |> String.concat ""
      in
      Format.fprintf fmt "my_big_map { %s }" x])

type my_instr =
  (* Stack instructions *)
  | My_dup
  | My_swap
  | My_push : my_item -> my_instr
  | My_dip
  | My_undip
  | My_drop
  | My_dropn : int -> my_instr
  | My_dig : int -> my_instr
  | My_dug : int -> my_instr
  (* Loop *)
  | My_loop_if_not : int -> my_instr
  | My_jump : int -> my_instr (* Pair instructions *)
  | My_car
  | My_cons_pair
  | My_nil
  (* Arithmetic instructions *)
  | My_compare
  | My_neq
  | My_sub
  | My_mul_int
  | My_mul_nattez
  | My_halt
  | My_add_int_int
  | My_add_nat_nat
  | My_add_tez
  | My_ediv_tez
  | My_ediv_natnat
  | My_abs
  | My_mul_natnat
  | My_sub_tez
  (* Union *)
  | My_if_left : int -> my_instr
  | My_address_instr
  | My_amount
  | My_apply
  | My_cdr
  | My_contract_instr : contract_type -> my_instr
  | My_ediv
  | My_EMPTY_MAP
  | My_EQ
  | My_EXEC
  | My_RET
  | My_FAILWITH
  | My_GE
  | My_big_map_get
  | My_map_get
  | My_GT
  | My_IF : int -> my_instr
  | My_IF_NONE : int -> my_instr
  | My_IMPLICIT_ACCOUNT
  | My_lambda_instr : int -> my_instr
  | My_LT
  | My_NOT
  | My_NOW
  | My_OR
  | My_PAIR
  | My_SELF : contract_type -> my_instr
  | My_SENDER
  | My_SET_DELEGATE
  | My_cons_some
  | My_TRANSFER_TOKENS
  | My_UNIT
  | My_big_map_update
  | My_map_update
  | My_cons_list
[@@deriving show { with_path = false }]

module My_big_map = Map.Make (struct
  type t = my_item

  let compare = compare_my_item
end)

type my_stack = my_item list

type my_dip_stack = my_stack

let show_my_item_list x =
  match x with
  | [] -> "[]"
  | _ -> List.map show_my_item x |> String.concat "\n::\n"

let int_to_string n = Int64.to_string @@ Int64.of_int n

let show_my_instr_list x =
  match x with
  | [] -> "[]"
  | _ ->
      List.mapi (fun i x -> int_to_string i ^ ": " ^ show_my_instr x) x
      |> String.concat "\n::\n"

let rec myfy_item : type a. a ty * a -> my_item = function
  | (Nat_t _, n) -> My_nat n
  | (Int_t _, n) -> My_int n
  | (Bool_t _, b) -> My_bool b
  | (Pair_t ((a_ty, _, _), (b_ty, _, _), _), (a, b)) ->
      My_pair (myfy_item (a_ty, a), myfy_item (b_ty, b))
  | (Unit_t _, _) -> My_unit
  | (Big_map_t (cty, ty, _), m) ->
      (* id : Big_map.Id.t option;
         diff : (my_item, my_item option) map; *)
      let module Boxed_map = (val m.diff) in
      let (x, _) = Boxed_map.boxed in
      let value =
        Boxed_map.OPS.bindings x
        |> List.map (fun (key, v) ->
               ( myfy_cty_item (cty, key),
                 match v with
                 | None -> None
                 | Some v -> Some (myfy_item (ty, v)) ))
        |> List.fold_left
             (fun map (k, v) -> My_big_map.add k v map)
             My_big_map.empty
      in
      let diff =
        ( module struct
          type key = my_item

          type value = my_item option

          let value = value

          module OPS = My_big_map
        end : My_boxed_map
          with type key = my_item
           and type value = my_item option )
      in
      My_big_map { id = m.id; diff; value_type = Ex_ty m.value_type }
  | (Union_t ((lty, _), (rty, _), _), u) -> (
      match u with
      | L x -> My_left (myfy_item (lty, x))
      | R x -> My_right (myfy_item (rty, x)) )
  | (String_t _, x) -> My_string x
  | (Mutez_t _, x) -> My_mutez x
  | (Address_t _, (contract, entrypoint)) ->
      My_address_item (contract, entrypoint)
  | (Map_t (cty, ty, _), m) -> 
    let module Boxed_map = (val m) in
      let (x, _) = Boxed_map.boxed in
      let value =
        Boxed_map.OPS.bindings x
        |> List.map (fun (key, v) ->
               ( myfy_cty_item (cty, key), (myfy_item (ty, v)) ))
        |> List.fold_left
             (fun map (k, v) -> My_big_map.add k v map)
             My_big_map.empty
      in
      let diff =
        ( module struct
          type key = my_item

          type value = my_item

          let value = value

          module OPS = My_big_map
        end : My_boxed_map
          with type key = my_item
           and type value = my_item )
      in
      My_map diff
  | (Timestamp_t _, x) -> My_timestamp x
  | (ty, _) -> raise (Failure ("myfy item:" ^ ty_to_string ty))

and myfy_cty_item : type a. a Script_typed_ir.comparable_ty * a -> my_item =
  function
  | (Nat_key _, n) -> My_nat n
  | (Int_key _, n) -> My_int n
  | (Bool_key _, b) -> My_bool b
  | (Pair_key ((a_cty, _), (b_cty, _), _), (a, b)) ->
      My_pair (myfy_cty_item (a_cty, a), myfy_cty_item (b_cty, b))
  | (Unit_key _, _) -> My_unit
  | (Union_key ((lty, _), (rty, _), _), u) -> (
      match u with
      | L x -> My_left (myfy_cty_item (lty, x))
      | R x -> My_right (myfy_cty_item (rty, x)) )
  | (Address_key _, (contract, entrypoint)) ->
    My_address_item (contract, entrypoint)
  | (ty, ty_) -> raise (Failure ("myfy item:" ^ (cty_to_string ty)))

let rec repeat x = function 0 -> [ x ] | n -> x :: repeat x (n - 1)

let rec translate : type bef aft. (bef, aft) descr -> my_instr list =
 fun descr ->
  match descr.instr with
  | Dup -> [ My_dup ]
  | Swap -> [ My_swap ]
  | Const x ->
      let (Item_t (x_ty, _, _)) = descr.aft in
      [ My_push (myfy_item (x_ty, x)) ]
  | Dip x -> [ My_dip ] @ translate x @ [ My_undip ]
  | Dipn (n, _, x) ->
      (* raise (Failure "dipn"); *)
      repeat My_dip (n - 1) @ translate x @ [ My_undip ]
  | Drop -> [ My_drop ]
  | Loop d ->
      let cbody = translate d in
      let body_len = List.length cbody in
      (My_loop_if_not (body_len + 2) :: cbody) @ [ My_jump (-body_len - 1) ]
  | Car -> [ My_car ]
  | Compare t -> [ My_compare ]
  | Neq -> [ My_neq ]
  | Sub_int -> [ My_sub ]
  | Mul_intint -> [ My_mul_int ]
  | Seq (d, d') -> translate d @ translate d'
  | Nop -> []
  | Nil -> [ My_nil ]
  | Cons_pair -> [ My_cons_pair ]
  | Add_intint -> [ My_add_int_int ]
  | Abs_int -> [ My_abs ]
  | If_left (l, r) ->
      let l = translate l in
      let r = translate r in
      [ My_if_left (List.length l + 1 + 1) ] @ l @ [ My_jump (List.length r + 1) ] @ r
  | If_none (l, r) ->
      let l = translate l in
      let r = translate r in
      [ My_IF_NONE (List.length l + 1 + 1) ] @ l @ [ My_jump (List.length r + 1) ] @ r
  | If (l, r) ->
      let l = translate l in
      let r = translate r in
      [ My_IF (List.length l + 1 + 1) ] @ l @ [ My_jump (List.length r + 1) ] @ r
  | Big_map_update -> [ My_big_map_update ]
  | Sender -> [ My_SENDER ]
  | Cons_some -> [ My_cons_some ]
  | Map_update -> [ My_map_update ]
  | Dig (n, _) -> [ My_dig n ]
  | Dug (n, _) -> [ My_dug n ]
  | Cdr -> [ My_cdr ]
  | Add_natnat -> [ My_add_nat_nat ]
  | Empty_map (_, _) -> [ My_EMPTY_MAP ]
  | Big_map_get -> [ My_big_map_get ]
  | Failwith x -> [ My_FAILWITH ]
  | Ge -> [ My_GE ]
  | Gt -> [ My_GT ]
  | Map_get -> [ My_map_get ]
  | Exec -> [ My_EXEC ]
  | Dropn (n, _) -> [ My_dropn n ]
  | Eq -> [ My_EQ ]
  | Cons_list -> [ My_cons_list ]
  | Transfer_tokens -> [ My_TRANSFER_TOKENS ]
  | Amount -> [ My_amount ]
  | Self (t, entrypoint) -> [ My_SELF (Contract_type (t, entrypoint)) ]
  | Contract (ty, entrypoint) ->
      [ My_contract_instr (Contract_type (ty, entrypoint)) ]
  | Lambda (Lam (code, _)) ->
      let body = translate code in
      (My_lambda_instr (List.length body) :: body) @ [ My_RET ]
  | Address -> [ My_address_instr ]
  | Add_tez -> [ My_add_tez ]
  | Ediv_natnat -> [ My_ediv_natnat ]
  | Mul_natnat -> [ My_mul_natnat ]
  | Lt -> [ My_LT ]
  | Now -> [ My_NOW ]
  | Sub_tez -> [ My_sub_tez ]
  | Set_delegate -> [ My_SET_DELEGATE ]
  | Implicit_account -> [ My_IMPLICIT_ACCOUNT ]
  | Or -> [ My_OR ]
  | Not -> [ My_NOT ]
  | Apply _ -> [ My_apply ]
  | Mul_nattez -> [ My_mul_nattez ]
  | Ediv_tez -> [ My_ediv_tez ]
  | x -> raise @@ Failure ("Failed translating " ^ instr_to_string x)

let translate : type bef aft. (bef, aft) descr -> my_instr list =
 fun descr -> translate descr @ [ My_halt ]

let rec myfy_stack : type a. a stack_ty * a -> my_stack = function
  | (Empty_t, ()) -> []
  | (Item_t (item_ty, stack_ty, _), (item, stack)) ->
      let x = myfy_item (item_ty, item) in
      x :: myfy_stack (stack_ty, stack)

let wrap_compare compare a b =
  let res = compare a b in
  if Compare.Int.(res = 0) then 0 else if Compare.Int.(res > 0) then 1 else -1

let compare_address (x, ex) (y, ey) =
  let lres = Contract.compare x y in
  if Compare.Int.(lres = 0) then Compare.String.compare ex ey else lres

let rec compare_comparable : type a. a comparable_ty -> a -> a -> int =
 fun kind ->
  match kind with
  | Unit_key _ -> fun () () -> 0
  | Never_key _ -> ( function _ -> . )
  | Signature_key _ -> wrap_compare Signature.compare
  | String_key _ -> wrap_compare Compare.String.compare
  | Bool_key _ -> wrap_compare Compare.Bool.compare
  | Mutez_key _ -> wrap_compare Tez.compare
  | Key_hash_key _ -> wrap_compare Signature.Public_key_hash.compare
  | Key_key _ -> wrap_compare Signature.Public_key.compare
  | Int_key _ -> wrap_compare Script_int.compare
  | Nat_key _ -> wrap_compare Script_int.compare
  | Timestamp_key _ -> wrap_compare Script_timestamp.compare
  | Address_key _ -> wrap_compare compare_address
  | Bytes_key _ -> wrap_compare Compare.Bytes.compare
  | Chain_id_key _ -> wrap_compare Chain_id.compare
  | Pair_key ((tl, _), (tr, _), _) ->
      fun (lx, rx) (ly, ry) ->
        let lres = compare_comparable tl lx ly in
        if Compare.Int.(lres = 0) then compare_comparable tr rx ry else lres
  | Union_key ((tl, _), (tr, _), _) -> (
      fun x y ->
        match (x, y) with
        | (L x, L y) -> compare_comparable tl x y
        | (L _, R _) -> -1
        | (R _, L _) -> 1
        | (R x, R y) -> compare_comparable tr x y )
  | Option_key (t, _) -> (
      fun x y ->
        match (x, y) with
        | (None, None) -> 0
        | (None, Some _) -> -1
        | (Some _, None) -> 1
        | (Some x, Some y) -> compare_comparable t x y )

let empty_map : type a b. a comparable_ty -> (a, b) map =
 fun ty ->
  let module OPS = Map.Make (struct
    type t = a

    let compare = compare_comparable ty
  end) in
  ( module struct
    type key = a

    type value = b

    let key_ty = ty

    module OPS = OPS

    let boxed = (OPS.empty, 0)
  end )

let map_update : type a b. a -> b option -> (a, b) map -> (a, b) map =
 fun k v (module Box) ->
  ( module struct
    type key = a

    type value = b

    let key_ty = Box.key_ty

    module OPS = Box.OPS

    let boxed =
      let (map, size) = Box.boxed in
      let contains = Box.OPS.mem k map in
      match v with
      | Some v -> (Box.OPS.add k v map, size + if contains then 0 else 1)
      | None -> (Box.OPS.remove k map, size - if contains then 1 else 0)
  end )

let rec ty_of_comparable_ty : type a. a comparable_ty -> a ty =
 fun s ->
  match s with
  | Unit_key _ -> Unit_t None
  | Never_key _ -> Never_t None
  | Int_key _ -> Int_t None
  | Nat_key _ -> Nat_t None
  | Signature_key _ -> Signature_t None
  | String_key _ -> String_t None
  | Bytes_key _ -> Bytes_t None
  | Mutez_key _ -> Mutez_t None
  | Bool_key _ -> Bool_t None
  | Key_hash_key _ -> Key_hash_t None
  | Key_key _ -> Key_t None
  | Timestamp_key _ -> Timestamp_t None
  | Chain_id_key _ -> Chain_id_t None
  | Address_key _ -> Address_t None
  | Pair_key ((a, _), (b, _), _) ->
      Pair_t
        ( (ty_of_comparable_ty a, None, None),
          (ty_of_comparable_ty b, None, None),
          None )
  | Union_key ((a, _), (b, _), _) ->
      Union_t
        ((ty_of_comparable_ty a, None), (ty_of_comparable_ty b, None), None)
  | Option_key (t, _) -> Option_t (ty_of_comparable_ty t, None)

let rec yfym_item : type a. a ty * my_item -> a = function
  | (Nat_t _, My_nat n) -> n
  (* This is probably due to some mistake in the interpreter :/ *)
  | (Nat_t _, My_mutez n) ->
      n |> Tez.to_mutez |> Script_int.of_int64 |> Obj.magic
  | (Int_t _, My_int n) -> n
  | (Mutez_t _, My_mutez x) -> x
  | (Bool_t _, My_bool b) -> b
  | (Pair_t ((a_ty, _, _), (b_ty, _, _), _), My_pair (a, b)) ->
      (yfym_item (a_ty, a), yfym_item (b_ty, b))
  | (Pair_t ((_a_ty, _, _), (_b_ty, _, _), _), x) ->
      raise (Failure ("foo " ^ show_my_item x))
  | (List_t (ele_ty, _), My_list lst) ->
      {
        elements = List.map (fun ele -> yfym_item (ele_ty, ele)) lst;
        length = List.length lst;
      }
  | (Unit_t _, My_unit) -> ()
  | (Address_t _, My_address_item (contract, entrypoint)) ->
      (contract, entrypoint)
  | (Map_t (kt, vt, _), My_map m) ->
      let module Boxed_map = ( val m : My_boxed_map
                                 with type key = my_item
                                  and type value = my_item )
      in
      let kv_pairs = Boxed_map.OPS.bindings Boxed_map.value in
      List.fold_left
        (fun acc (k, v) ->
          let ty = ty_of_comparable_ty kt in
          let k = yfym_item (ty, k) in
          let v = yfym_item (vt, v) in
          map_update k (Some v) acc)
        (empty_map kt)
        kv_pairs
  | (Operation_t _, My_operation (x, y)) -> (x, y)
  | (Big_map_t (kt, vt, _), My_big_map { id; diff; value_type }) ->
      let module Boxed_map = ( val diff : My_boxed_map
                                 with type key = my_item
                                  and type value = my_item option )
      in
      let kv_pairs = Boxed_map.OPS.bindings Boxed_map.value in
      let diff : (_, _ option) map =
        List.fold_left
          (fun acc (k, v) ->
            let ty = ty_of_comparable_ty kt in
            let k = yfym_item (ty, k) in
            match v with
            | Some v ->
                let v = yfym_item (vt, v) in
                map_update k (Some (Some v)) acc
            | None -> map_update k None acc)
          (empty_map kt)
          kv_pairs
      in
      let bm : (_, _) big_map = { id; diff; value_type = vt; key_type = kt } in
      bm
  | (ty, x) ->
      raise
        (Failure
           ("yfym item: " ^ ty_to_string ty ^ ", my_item: " ^ show_my_item x))

(* let rec yfym_stack : type a. a stack_ty * my_stack -> a = function
  | (Empty_t, []) -> ()
  | (Item_t (hd_ty, tl_ty, _), hd :: tl) ->
      (yfym_item (hd_ty, hd), yfym_stack (tl_ty, tl))
  | ((Item_t _ as _stack_ty), _stack) -> raise (Failure "yfym stack ty. type: ")
  | (_stack_ty, (_ :: _ as _stack)) -> raise (Failure "yfym stack item. type: ") *)

let rec my_stack_to_string = function
  | [] -> "()"
  | hd :: tl -> show_my_item hd ^ "\n::\n" ^ my_stack_to_string tl

let rec yfym_stack : type a. a stack_ty * my_stack -> a = function
  | (Empty_t, []) -> ()
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

exception Micheline_exception of (Script.location, Script.prim) Micheline.node

let my_empty_map : my_map =
  ( module struct
    type key = my_item

    type value = my_item

    module OPS = My_big_map

    let value = My_big_map.empty
  end )

let my_empty_big_map : my_big_map_diff =
  ( module struct
    type key = my_item

    type value = my_item option

    module OPS = My_big_map

    let value = My_big_map.empty
  end )

let my_compare_comparable : my_item -> my_item -> int =
 fun a b ->
  match (a, b) with
  | (My_nat a, My_nat b) -> Script_int.compare a b
  | (My_int a, My_int b) -> Script_int.compare a b
  | (My_mutez a, My_mutez b) -> wrap_compare Tez.compare a b
  | (My_address_item (a, a_entrypoint), My_address_item (b, b_entrypoint)) ->
      wrap_compare compare_address (a, a_entrypoint) (b, b_entrypoint)
  | (My_timestamp a, My_timestamp b) -> Script_timestamp.compare a b
  | _ ->
      raise
      @@ Failure
           ("Failed to compare " ^ show_my_item a ^ " with " ^ show_my_item b)
