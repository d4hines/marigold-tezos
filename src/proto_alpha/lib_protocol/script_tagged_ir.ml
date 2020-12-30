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

[@@@warning "-27"]

external ( = ) : 'a -> 'a -> bool = "%equal"

module Script_typed_ir = struct
  include Script_typed_ir

  let equal_ex_ty = ( = )
end

let ty_to_string : type a. a ty -> string = function
  | Unit_t _ -> "Unit_t"
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
  | Pair_t _ -> "Pair_t"
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
  | _ -> "Go fish..."

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

module Ppx_deriving_runtime = struct
  include Ppx_deriving_runtime
  module Format = Format_
  module List = List_
end

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

module Big_map = struct
  include Big_map

  module Id = struct
    include Id

    let compare : t -> t -> int = Obj.magic Z.compare

    let pp : Format.formatter -> t -> unit = Obj.magic Z.pp_print
  end
end

let compare_comparable_ty x y = 0

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
  | My_contract_item of Contract.t * string
  | My_timestamp of Script_timestamp.t
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
      Format.fprintf fmt "my_big_map { %s }" x])

and my_big_map = {
  id : Big_map.Id.t option;
  diff : my_big_map_diff;
  value_type : Script_typed_ir.ex_ty;
      [@compare fun a b -> 0] [@printer fun fmt _ -> Format.fprintf fmt "ex_ty"]
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

and my_instr =
  (* Stack instructions *)
  | My_dup
  | My_swap
  | My_push of my_item
  | My_dip
  | My_undip
  | My_drop
  | My_dropn of int
  | My_dig of int
  | My_dug of int
  (* Loop *)
  | My_loop_if_not of int
  | My_jump of int (* Pair instructions *)
  | My_car
  | My_cons_pair
  | My_nil
  (* Arithmetic instructions *)
  | My_compare
  | My_neq
  | My_sub
  | My_mul_int
  | My_halt
  | My_add_int_int
  | My_add_nat_nat
  | My_abs
  (* Union *)
  | My_if_left of int
  | My_address_instr
  | My_amount
  | My_apply
  | My_cdr
  | My_contract_instr of string
  | My_ediv
  | My_EMPTY_MAP
  | My_EQ
  | My_EXEC
  | My_FAILWITH
  | My_GE
  | My_big_map_get
  | My_map_get
  | My_GT
  | My_IF of int
  | My_IF_NONE of int
  | My_IMPLICIT_ACCOUNT
  | My_lambda_instr of int
  | My_LT
  | My_NOT
  | My_NOW
  | My_OR
  | My_PAIR
  | My_PUSH
  | My_SELF of string
  | My_SENDER
  | My_SET_DELEGATE
  | My_cons_some
  | My_TRANSFER_TOKENS
  | My_UNIT
  | My_map_update
  | My_cons_list
[@@deriving ord, show { with_path = false }]
module My_big_map = Map.Make (struct
  type t = my_item

  let compare = compare_my_item
end)

type my_stack = my_item list

type my_dip_stack = my_stack

let ty_to_string : type a. a ty -> string = function
  | Unit_t _ -> "Unit_t"
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
  | Pair_t _ -> "Pair_t"
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
  | (Address_t _, (contract, entrypoint)) -> My_address_item (contract, entrypoint)
  | (Map_t _, x) -> assert false
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
  | (ty, _) -> raise (Failure "mycfy item")

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
  | Compare _ -> [ My_compare ]
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
      [ My_if_left (List.length l + 1) ]
      @ l
      @ [ My_jump (List.length r + 1) ]
      @ r
  | Big_map_update -> [ My_big_map_get ]
  | Sender -> [ My_SENDER ]
  | Cons_some -> [ My_cons_some ]
  | Map_update -> [ My_map_update ]
  | Dig (n, _) -> [ My_dig n ]
  | Dug (n, _) -> [ My_dug n ]
  | If_none (l, r) ->
      let l = translate l in
      let r = translate r in
      [ My_IF_NONE (List.length l) ] @ l @ [ My_jump (List.length r + 1) ] @ r
  | Cdr -> [ My_cdr ]
  | If (l, r) ->
      let l = translate l in
      let r = translate r in
      [ My_IF (List.length l) ] @ l @ [ My_jump (List.length r + 1) ] @ r
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
  | Self (x, entrypoint) -> [My_SELF entrypoint]
  | Contract (ty, entrypoint) -> [My_contract_instr entrypoint]
  | Lambda (Lam (code, _)) ->
      let body = translate code in
      [ My_lambda_instr (List.length body) ] @ body
  | x -> raise @@ Failure ("Failed translating " ^ instr_to_string x)

let translate : type bef aft. (bef, aft) descr -> my_instr list =
 fun descr -> translate descr @ [ My_halt ]

let rec myfy_stack : type a. a stack_ty * a -> my_stack = function
  | (Empty_t, ()) -> []
  | (Item_t (item_ty, stack_ty, _), (item, stack)) ->
      let x = myfy_item (item_ty, item) in
      x :: myfy_stack (stack_ty, stack)

let rec yfym_item : type a. a ty * my_item -> a = function
  | (Nat_t _, My_nat n) -> n
  | (Int_t _, My_int n) -> n
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
  | (ty, _) -> raise (Failure ("yfym item: " ^ ty_to_string ty))

let rec yfym_stack : type a. a stack_ty * my_stack -> a = function
  | (Empty_t, []) -> ()
  | (Item_t (hd_ty, tl_ty, _), hd :: tl) ->
      (yfym_item (hd_ty, hd), yfym_stack (tl_ty, tl))
  | ((Item_t _ as _stack_ty), _stack) -> raise (Failure "yfym stack ty. type: ")
  | (_stack_ty, (_ :: _ as _stack)) -> raise (Failure "yfym stack item. type: ")

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
