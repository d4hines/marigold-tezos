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

[@@@warning "-32"]

[@@@warning "-39"]

[@@@warning "-27"]

[@@@warning "-26"]

[@@@warning "-21"]

open Alpha_context
open Script
open Script_typed_ir
open Script_tagged_ir
open Script_ir_translator

module Option = struct
  include Option

  let get x = match x with None -> assert false | Some x -> x
end

let print_balance : (string -> Alpha_context.t -> unit) ref =
  ref (fun _ _ -> ())

let entrypoint_running = ref ""

(* ---- Run-time errors -----------------------------------------------------*)

type execution_trace =
  (Script.location * Gas.t * (Script.expr * string option) list) list

type error += Reject of Script.location * Script.expr * execution_trace option

type error += Overflow of Script.location * execution_trace option

type error += Runtime_contract_error : Contract.t * Script.expr -> error

type error += Bad_contract_parameter of Contract.t (* `Permanent *)

type error += Cannot_serialize_failure

type error += Cannot_serialize_storage

type error += Michelson_too_many_recursive_calls

let () =
  let open Data_encoding in
  let trace_encoding =
    list
    @@ obj3
         (req "location" Script.location_encoding)
         (req "gas" Gas.encoding)
         (req
            "stack"
            (list (obj2 (req "item" Script.expr_encoding) (opt "annot" string))))
  in
  (* Reject *)
  register_error_kind
    `Temporary
    ~id:"michelson_v1.script_rejected"
    ~title:"Script failed"
    ~description:"A FAILWITH instruction was reached"
    (obj3
       (req "location" Script.location_encoding)
       (req "with" Script.expr_encoding)
       (opt "trace" trace_encoding))
    (function Reject (loc, v, trace) -> Some (loc, v, trace) | _ -> None)
    (fun (loc, v, trace) -> Reject (loc, v, trace)) ;
  (* Overflow *)
  register_error_kind
    `Temporary
    ~id:"michelson_v1.script_overflow"
    ~title:"Script failed (overflow error)"
    ~description:
      "A FAIL instruction was reached due to the detection of an overflow"
    (obj2
       (req "location" Script.location_encoding)
       (opt "trace" trace_encoding))
    (function Overflow (loc, trace) -> Some (loc, trace) | _ -> None)
    (fun (loc, trace) -> Overflow (loc, trace)) ;
  (* Runtime contract error *)
  register_error_kind
    `Temporary
    ~id:"michelson_v1.runtime_error"
    ~title:"Script runtime error"
    ~description:"Toplevel error for all runtime script errors"
    (obj2
       (req "contract_handle" Contract.encoding)
       (req "contract_code" Script.expr_encoding))
    (function
      | Runtime_contract_error (contract, expr) -> Some (contract, expr)
      | _ -> None)
    (fun (contract, expr) -> Runtime_contract_error (contract, expr)) ;
  (* Bad contract parameter *)
  register_error_kind
    `Permanent
    ~id:"michelson_v1.bad_contract_parameter"
    ~title:"Contract supplied an invalid parameter"
    ~description:
      "Either no parameter was supplied to a contract with a non-unit \
       parameter type, a non-unit parameter was passed to an account, or a \
       parameter was supplied of the wrong type"
    Data_encoding.(obj1 (req "contract" Contract.encoding))
    (function Bad_contract_parameter c -> Some c | _ -> None)
    (fun c -> Bad_contract_parameter c) ;
  (* Cannot serialize failure *)
  register_error_kind
    `Temporary
    ~id:"michelson_v1.cannot_serialize_failure"
    ~title:"Not enough gas to serialize argument of FAILWITH"
    ~description:
      "Argument of FAILWITH was too big to be serialized with the provided gas"
    Data_encoding.empty
    (function Cannot_serialize_failure -> Some () | _ -> None)
    (fun () -> Cannot_serialize_failure) ;
  (* Cannot serialize storage *)
  register_error_kind
    `Temporary
    ~id:"michelson_v1.cannot_serialize_storage"
    ~title:"Not enough gas to serialize execution storage"
    ~description:
      "The returned storage was too big to be serialized with the provided gas"
    Data_encoding.empty
    (function Cannot_serialize_storage -> Some () | _ -> None)
    (fun () -> Cannot_serialize_storage) ;
  (* Michelson Stack Overflow *)
  register_error_kind
    `Permanent
    ~id:"michelson_v1.interp_too_many_recursive_calls"
    ~title:"Too many recursive calls during interpretation"
    ~description:
      "Too many recursive calls were needed for interpretation of a Michelson \
       script"
    Data_encoding.empty
    (function Michelson_too_many_recursive_calls -> Some () | _ -> None)
    (fun () -> Michelson_too_many_recursive_calls)

(* ---- interpreter ---------------------------------------------------------*)

module Interp_costs = Michelson_v1_gas.Cost_of.Interpreter

let rec interp_stack_prefix_preserving_operation :
    type fbef bef faft aft result.
    (fbef -> faft * result) ->
    (fbef, faft, bef, aft) stack_prefix_preservation_witness ->
    bef ->
    aft * result =
 fun f n stk ->
  match (n, stk) with
  | ( Prefix
        (Prefix
          (Prefix
            (Prefix
              (Prefix
                (Prefix
                  (Prefix
                    (Prefix
                      (Prefix
                        (Prefix
                          (Prefix
                            (Prefix (Prefix (Prefix (Prefix (Prefix n))))))))))))))),
      ( v0,
        ( v1,
          ( v2,
            ( v3,
              ( v4,
                ( v5,
                  ( v6,
                    (v7, (v8, (v9, (va, (vb, (vc, (vd, (ve, (vf, rest)))))))))
                  ) ) ) ) ) ) ) ) ->
      interp_stack_prefix_preserving_operation f n rest
      |> fun (rest', result) ->
      ( ( v0,
          ( v1,
            ( v2,
              ( v3,
                ( v4,
                  ( v5,
                    ( v6,
                      (v7, (v8, (v9, (va, (vb, (vc, (vd, (ve, (vf, rest')))))))))
                    ) ) ) ) ) ) ),
        result )
  | (Prefix (Prefix (Prefix (Prefix n))), (v0, (v1, (v2, (v3, rest))))) ->
      interp_stack_prefix_preserving_operation f n rest
      |> fun (rest', result) -> ((v0, (v1, (v2, (v3, rest')))), result)
  | (Prefix n, (v, rest)) ->
      interp_stack_prefix_preserving_operation f n rest
      |> fun (rest', result) -> ((v, rest'), result)
  | (Rest, v) -> f v

type step_constants = {
  source : Contract.t;
  payer : Contract.t;
  self : Contract.t;
  amount : Tez.t;
  chain_id : Chain_id.t;
}

type logging_function =
  context -> Script.location -> my_instr -> my_stack -> unit

module type STEP_LOGGER = sig
  val log_interp : logging_function

  val log_entry : logging_function

  val log_exit : logging_function

  val get_log : unit -> execution_trace option tzresult Lwt.t
end

type logger = (module STEP_LOGGER)

let cost_of_instr' : my_instr -> my_stack -> Gas.cost =
 fun instr stack ->
  match (instr, stack) with
  | (My_dup, _) -> Interp_costs.dup
  | (My_swap, _) -> Interp_costs.swap
  | (My_push _, _) -> Interp_costs.push
  | (My_dip, _) -> Interp_costs.dip
  | (My_undip, _) -> Gas.free
  | (My_drop, _) -> Interp_costs.drop
  | (My_loop_if_not _, _) -> Interp_costs.loop
  | (My_jump _, _) -> Gas.free
  | (My_car, _) -> Interp_costs.car
  (* ========= TODO: FIXME:  ==========*)
  | (My_compare, _) -> Gas.free
  | (My_neq, _) -> Interp_costs.neq
  | (My_sub, x :: y :: _) -> (
      match (x, y) with
      | (My_int x', My_int y') -> Interp_costs.sub_bigint x' y'
      (* FIXME: *)
      | (x, y) -> Gas.free )
  | (My_mul_int, x :: y :: _) -> (
      match (x, y) with
      | (My_int x', My_int y') -> Interp_costs.mul_bigint x' y'
      | _ -> assert false )
  (* Hack to get it working... *)
  | (_, _) -> Gas.free

(* | (i, _) ->
      raise @@ Failure  (my_instr_to_str i) *)

let unpack ctxt ~ty ~bytes =
  Gas.check_enough ctxt (Script.serialized_cost bytes) >>?= fun () ->
  if
    Compare.Int.(Bytes.length bytes >= 1)
    && Compare.Int.(TzEndian.get_uint8 bytes 0 = 0x05)
  then
    let bytes = Bytes.sub bytes 1 (Bytes.length bytes - 1) in
    match Data_encoding.Binary.of_bytes Script.expr_encoding bytes with
    | None ->
        Lwt.return
          ( Gas.consume ctxt (Interp_costs.unpack_failed bytes) >|? fun ctxt ->
            (None, ctxt) )
    | Some expr -> (
        Gas.consume ctxt (Script.deserialized_cost expr) >>?= fun ctxt ->
        parse_data
          ctxt
          ~legacy:false
          ~allow_forged:false
          ty
          (Micheline.root expr)
        >|= function
        | Ok (value, ctxt) -> ok (Some value, ctxt)
        | Error _ignored ->
            Gas.consume ctxt (Interp_costs.unpack_failed bytes) >|? fun ctxt ->
            (None, ctxt) )
  else return (None, ctxt)

let[@inline] option_iter opt what =
  match opt with None -> () | Some l -> what l

let[@inline] lwt_option_iter opt what =
  match opt with None -> Lwt.return (Ok None) | Some l -> what l Int64.equal

let lengths_equal l l' = Compare.Int.( = ) (List.length l) (List.length l')

let rec split_at n l (acc, acc') =
  match l with
  | [] -> (acc, acc')
  | _ ->
      if Compare.Int.( = ) n (List.length acc) then
        split_at n (List.tl l) (acc, acc' @ [ List.hd l ])
      else split_at n (List.tl l) (acc @ [ List.hd l ], acc')

let split_at n l = split_at n l ([], [])

let take n l = fst @@ split_at n l

let rec drop n acc =
  (match n, acc with
  | (0, acc) -> acc
  | (_, []) -> []
  | (n, _ :: t) -> drop (n - 1) t)

let int_to_string n = Int64.to_string (Int64.of_int n)

let rec step_bounded :
    logger option ->
    context ->
    step_constants ->
    my_instr array ->
    my_stack ->
    (my_stack * context) tzresult Lwt.t =
 fun logger ctxt step_constants instrs stack ->
  let[@inline] log_entry ctxt pc instr stack =
    option_iter logger (fun logger ->
        let module Log = (val logger) in
        Log.log_entry ctxt pc instr stack)
  in
  let[@inline] log_exit ctxt pc instr stack =
    option_iter logger (fun logger ->
        let module Log = (val logger) in
        Log.log_exit ctxt pc instr stack)
  in
  let rec step :
      context ->
      int ->
      my_instr Array.t ->
      my_stack ->
      my_dip_stack ->
      (my_stack * context) tzresult Lwt.t =
   fun ctxt pc instr_array stack dip_stack ->
    let instr = Array.unsafe_get instr_array pc in
    let gas = cost_of_instr' instr stack in
    let step_return ctx pc instr_array stack dip_stack =
      log_exit ctxt (-1) instr stack ;
      step ctxt pc instr_array stack dip_stack
    in
    Gas.consume ctxt gas >>?= fun ctxt ->
    log_entry ctxt pc instr stack ;

    (* (!print_balance) "Inside step bounded\n" (Obj.magic ctxt); *)
    (* print_endline @@ "~ " ^ show_my_item_list (take 3 stack);
    print_endline @@ "> "
       ^ Int64.to_string (Int64.of_int pc)
       ^ ": " ^ show_my_instr instr ; *)
    (* let () =
         if String.equal !entrypoint_running "xtzToToken" then assert false else ()
       in *)

    (* let () =
         match instr with
         | My_mul_int | My_mul_natnat | My_mul_nattez | My_ediv | My_ediv_natnat
         | My_ediv_tez | My_apply | My_NOW ->

             (* assert false *)
         | _ -> ()
       in *)

    (* let () = (match pc with | 22 -> assert false |  _ -> ()) in *)
    (* let n = 94 in
       let s = take 5 stack in
       let () =
         if Compare.Int.( = ) pc n then (
           print_endline @@ "> "
           ^ Int64.to_string (Int64.of_int pc)
           ^ ": " ^ show_my_instr instr )
       in
       let () =
         if Compare.Int.( = ) pc (n + 1) then
           print_endline @@ "~" ^ my_stack_to_string s
       in *)
    match (instr, stack) with
    | (My_dup, h :: t) ->
        step_return ctxt (pc + 1) instr_array (h :: h :: t) dip_stack
    | (My_swap, h :: h' :: t) ->
        step_return ctxt (pc + 1) instr_array (h' :: h :: t) dip_stack
    | (My_push x, s) -> step_return ctxt (pc + 1) instr_array (x :: s) dip_stack
    | (My_dip, h :: t) ->
        step_return ctxt (pc + 1) instr_array t (h :: dip_stack)
    | (My_undip, s) -> (
        match dip_stack with
        | h :: dip_stack' ->
            step_return ctxt (pc + 1) instr_array (h :: s) dip_stack'
        | [] -> step_return ctxt (pc + 1) instr_array s dip_stack )
    | (My_drop, _ :: t) -> step_return ctxt (pc + 1) instr_array t dip_stack
    | (My_dropn n, s) ->
        step_return ctxt (pc + 1) instr_array (drop n s) dip_stack
    | (My_loop_if_not n, My_bool b :: t) ->
        if b then step_return ctxt (pc + 1) instr_array t dip_stack
        else step_return ctxt (pc + n) instr_array t dip_stack
    | (My_jump n, s) -> (
        match n with
        | 0 -> raise @@ Failure "Received a 'JMP 0' command, which is invalid."
        | _ -> step_return ctxt (pc + n) instr_array s dip_stack )
    | (My_car, My_pair (l, _) :: s) ->
        step_return ctxt (pc + 1) instr_array (l :: s) dip_stack
    | (My_cons_pair, h :: h' :: t) ->
        step_return ctxt (pc + 1) instr_array (My_pair (h, h') :: t) dip_stack
    (* | (My_compare, My_int a :: My_int b :: t) ->
        let cmp = My_int (Script_int.of_int @@ Script_int.compare a b) in
        step_return ctxt (pc + 1) instr_array (cmp :: t) dip_stack *)
    | (My_neq, My_int n :: t) ->
        let ns = Script_int.(compare n zero) in
        let neq = Compare.Int.( <> ) Script_int.(compare n zero) 0 in
        step_return ctxt (pc + 1) instr_array (My_bool neq :: t) dip_stack
    | (My_EQ, My_int n :: t) ->
        let ns = Script_int.(compare n zero) in
        let eq = Compare.Int.( = ) Script_int.(compare n zero) 0 in
        step_return ctxt (pc + 1) instr_array (My_bool eq :: t) dip_stack
    | (My_GE, My_int n :: t) ->
        let ns = Script_int.(compare n zero) in
        let ge = Compare.Int.( >= ) Script_int.(compare n zero) 0 in
        step_return ctxt (pc + 1) instr_array (My_bool ge :: t) dip_stack
    | (My_GT, My_int n :: t) ->
        let ns = Script_int.(compare n zero) in
        let gt = Compare.Int.( > ) Script_int.(compare n zero) 0 in
        step_return ctxt (pc + 1) instr_array (My_bool gt :: t) dip_stack
    | (My_LT, My_int n :: t) ->
        let ns = Script_int.(compare n zero) in
        let gt = Compare.Int.( < ) Script_int.(compare n zero) 0 in
        step_return ctxt (pc + 1) instr_array (My_bool gt :: t) dip_stack
    | (My_sub, My_int a :: My_int b :: t) ->
        let sub = Script_int.sub a b in
        step_return ctxt (pc + 1) instr_array (My_int sub :: t) dip_stack
    | (My_sub, My_nat a :: My_nat b :: t) ->
        let sub = Script_int.sub a b in
        step_return ctxt (pc + 1) instr_array (My_int sub :: t) dip_stack
    | (My_abs, My_int a :: t) ->
        let abs = Script_int.abs a in
        step_return ctxt (pc + 1) instr_array (My_nat abs :: t) dip_stack
    | (My_mul_int, My_int a :: My_int b :: t) ->
        let mul = Script_int.mul a b in
        step_return ctxt (pc + 1) instr_array (My_int mul :: t) dip_stack
    | (My_mul_natnat, My_nat a :: My_nat b :: t) ->
        let mul = Script_int.mul_n a b in
        step_return ctxt (pc + 1) instr_array (My_nat mul :: t) dip_stack
    | (My_ediv_natnat, My_nat a :: My_nat b :: t) ->
        let result =
          match Script_int.ediv_n a b with
          | None -> My_none
          | Some (a, b) -> My_some (My_pair (My_nat a, My_nat b))
        in
        step_return ctxt (pc + 1) instr_array (result :: t) dip_stack
    | (My_add_int_int, My_int a :: My_int b :: t) ->
        let add = Script_int.add a b in
        step_return ctxt (pc + 1) instr_array (My_int add :: t) dip_stack
    | (My_add_nat_nat, My_nat a :: My_nat b :: t) ->
        (* assert false *)
        let add = Script_int.add_n a b in
        step_return ctxt (pc + 1) instr_array (My_nat add :: t) dip_stack
    | (My_add_tez, My_mutez a :: My_mutez b :: t) -> (
        match Tez.( +? ) a b with
        | Ok add ->
            step_return ctxt (pc + 1) instr_array (My_mutez add :: t) dip_stack
        | Error _ ->
            raise @@ Failure "Failed adding tez. It's to much mmmonnnneeeeeyyy!"
        )
    | (My_halt, s) -> Lwt.return (Ok (s, ctxt))
    | (My_nil, s) ->
        step_return ctxt (pc + 1) instr_array (My_list [] :: s) dip_stack
    | (My_NOT, My_bool b :: s) ->
        step_return ctxt (pc + 1) instr_array (My_bool (not b) :: s) dip_stack
    | (My_OR, My_bool b :: My_bool b' :: s) ->
        step_return ctxt (pc + 1) instr_array (My_bool (b || b') :: s) dip_stack
    | (My_lambda_instr n, s) ->
        step_return
          ctxt
          (pc + n + 2)
          instr_array
          (My_lambda_item (pc + 1, []) :: s)
          dip_stack
    | (My_apply, arg :: My_lambda_item (addr, args) :: s) ->
        step_return
          ctxt
          (pc + 1)
          instr_array
          (My_lambda_item (addr, arg :: args) :: s)
          dip_stack
    | (My_EXEC, arg :: My_lambda_item (addr, args) :: s) ->
        let arg =
          args |> List.fold_left (fun right left -> My_pair (left, right)) arg
        in
        step_return
          ctxt
          addr
          instr_array
          (arg :: My_ret_address (pc + 1) :: s)
          dip_stack
    | (My_RET, ret :: My_ret_address addr :: s) ->
        step_return ctxt addr instr_array (ret :: s) dip_stack
    | (My_PAIR, x :: x' :: s) ->
        step_return ctxt (pc + 1) instr_array (My_pair (x, x') :: s) dip_stack
    | (My_dig n, s) -> (
        let (s, s') = split_at n s in
        match s' with
        | [] ->
            raise
            @@ Failure
                 (Format.sprintf
                    "Dug %d called on stack with less than %d elements."
                    n
                    n)
        | x :: s' ->
            step_return ctxt (pc + 1) instr_array ((x :: s) @ s') dip_stack )
    | (My_dug n, x :: s) ->
        let (s, s') = split_at n s in
        step_return ctxt (pc + 1) instr_array (s @ (x :: s')) dip_stack
    | (My_cdr, My_pair (a, b) :: s) ->
        step_return ctxt (pc + 1) instr_array (b :: s) dip_stack
    | (My_amount, s) ->
        step_return
          ctxt
          (pc + 1)
          instr_array
          (My_mutez step_constants.amount :: s)
          dip_stack
    | (My_compare, a :: b :: s) ->
        let cmp = my_compare_comparable a b in
        step_return
          ctxt
          (pc + 1)
          instr_array
          (My_int (Script_int.of_int cmp) :: s)
          dip_stack
    | (My_IF ln, My_bool x :: s) ->
        let offset = if x then 1 else ln in
        step_return ctxt (pc + offset) instr_array s dip_stack
    | (My_if_left ln, x :: s) ->
        let (n, x) =
          match x with
          | My_left x -> (1, x)
          | My_right x -> (ln, x)
          | _ ->
              raise
              @@ Failure "Failed interpreting IF_LEFT on type other than union."
        in
        step_return ctxt (pc + n) instr_array (x :: s) dip_stack
    | (My_IF_NONE ln, x :: s) -> (
        match x with
        | My_none -> step_return ctxt (pc + 1) instr_array s dip_stack
        | My_some x ->
            step_return ctxt (pc + ln) instr_array (x :: s) dip_stack
        | _ ->
            raise
            @@ Failure "Failed interpreting IF_NONE on type other than option."
        )
    | (My_SENDER, s) ->
        let sender = step_constants.source in
        step_return
          ctxt
          (pc + 1)
          instr_array
          (My_address_item (sender, "default") :: s)
          dip_stack
    | (My_UNIT, s) ->
        step_return ctxt (pc + 1) instr_array (My_unit :: s) dip_stack
    | (My_cons_list, x :: My_list l :: s) ->
        step_return ctxt (pc + 1) instr_array (My_list (x :: l) :: s) dip_stack
    | (My_SELF type_and_entry, s) ->
        let sender = step_constants.self in
        step_return
          ctxt
          (pc + 1)
          instr_array
          (My_contract_item (sender, type_and_entry) :: s)
          dip_stack
    | (My_big_map_get, k :: My_big_map m :: s) -> (
        my_big_map_get ctxt k m >>=? fun x ->
        match x with
        | (Some x, ctxt) ->
            step_return ctxt (pc + 1) instr_array (My_some x :: s) dip_stack
        | (None, ctxt) ->
            step_return ctxt (pc + 1) instr_array (My_none :: s) dip_stack )
    | (My_EMPTY_MAP, s) ->
        step_return
          ctxt
          (pc + 1)
          instr_array
          (My_map Script_tagged_ir.my_empty_map :: s)
          dip_stack
    | (My_map_get, k :: My_map m :: s) -> (
        match my_map_get k m with
        | None -> step_return ctxt (pc + 1) instr_array (My_none :: s) dip_stack
        | Some x ->
            step_return ctxt (pc + 1) instr_array (My_some x :: s) dip_stack )
    | (My_cons_some, x :: s) ->
        step_return ctxt (pc + 1) instr_array (My_some x :: s) dip_stack
    | (My_map_update, k :: v :: My_map m :: s) -> (
        match v with
        | My_some v ->
            let m = my_map_set k v m in
            step_return ctxt (pc + 1) instr_array (My_map m :: s) dip_stack
        | My_none -> assert false
        | _ -> raise @@ Failure "Map update called on non-option value." )
    | (My_big_map_update, k :: v :: My_big_map m :: s) -> (
        match v with
        | My_some v ->
            let m = my_big_map_update k v m in
            step_return ctxt (pc + 1) instr_array (My_big_map m :: s) dip_stack
        | My_none -> assert false
        | _ -> raise @@ Failure "Map update called on non-option value." )
    | ( My_address_instr,
        My_contract_item (contract, Contract_type (_, entrypoint)) :: s ) ->
        step_return
          ctxt
          (pc + 1)
          instr_array
          (My_address_item (contract, entrypoint) :: s)
          dip_stack
    | (My_ediv, My_int a :: My_int b :: s) -> (
        match Script_int.ediv a b with
        | Some (x, y) ->
            step_return
              ctxt
              (pc + 1)
              instr_array
              (My_some (My_pair (My_int x, My_nat y)) :: s)
              dip_stack
        | None -> step_return ctxt (pc + 1) instr_array (My_none :: s) dip_stack
        )
    | (My_ediv_tez, My_mutez x :: My_mutez y :: s) ->
        let x = Script_int.abs (Script_int.of_int64 (Tez.to_mutez x)) in
        let y = Script_int.abs (Script_int.of_int64 (Tez.to_mutez y)) in
        let result =
          match Script_int.ediv_n x y with
          | None -> My_none
          | Some (q, r) -> (
              match Script_int.to_int64 r with
              | None -> assert false (* Cannot overflow *)
              | Some r -> (
                  match Tez.of_mutez r with
                  | None -> assert false (* Cannot overflow *)
                  | Some r -> My_some (My_pair (My_nat q, My_mutez r)) ) )
        in
        step_return ctxt (pc + 1) instr_array (result :: s) dip_stack
    | (My_NOW, s) ->
        let now = Script_timestamp.now ctxt in
        step_return ctxt (pc + 1) instr_array (My_timestamp now :: s) dip_stack
    | ( My_contract_instr (Contract_type (t, target_entrypoint)),
        My_address_item (contract, entrypoint) :: s ) -> (
        match ((contract, entrypoint), target_entrypoint) with
        | ((contract, "default"), entrypoint)
        | ((contract, entrypoint), "default") -> (
            let loc = -1 in
            Script_ir_translator.parse_contract_for_script
              ~legacy:false
              ctxt
              loc
              t
              contract
              ~entrypoint
            >>=? fun (ctxt, maybe_contract) ->
            match maybe_contract with
            | None ->
                step_return ctxt (pc + 1) instr_array (My_none :: s) dip_stack
            | Some (_, (contract, entrypoint)) ->
                step_return
                  ctxt
                  (pc + 1)
                  instr_array
                  ( My_some
                      (My_contract_item
                         (contract, Contract_type (t, target_entrypoint)))
                  :: s )
                  dip_stack )
        | _ -> step_return ctxt (pc + 1) instr_array (My_none :: s) dip_stack )
    | ( My_TRANSFER_TOKENS,
        x
        :: My_mutez amount
           :: My_contract_item (destination, Contract_type (tp, entrypoint))
              :: s ) ->
        let p = yfym_item (tp, x) in
        collect_lazy_storage ctxt tp p >>?= fun (to_duplicate, ctxt) ->
        let to_update = no_lazy_storage_id in
        extract_lazy_storage_diff
          ctxt
          Optimized
          tp
          p
          ~to_duplicate
          ~to_update
          ~temporary:true
        >>=? fun (p, lazy_storage_diff, ctxt) ->
        unparse_data ctxt Optimized tp p >>=? fun (p, ctxt) ->
        Gas.consume ctxt (Script.strip_locations_cost p) >>?= fun ctxt ->
        let operation =
          Transaction
            {
              amount;
              destination;
              entrypoint;
              parameters = Script.lazy_expr (Micheline.strip_locations p);
            }
        in
        fresh_internal_nonce ctxt >>?= fun (ctxt, nonce) ->
        let op =
          My_operation
            ( Internal_operation
                { source = step_constants.self; operation; nonce },
              lazy_storage_diff )
        in
        step_return ctxt (pc + 1) instr_array (op :: s) dip_stack
    | (x, _s) ->
        let () = log_exit ctxt pc x _s in
        let instrs =
          instr_array |> Array.to_list |> List.map show_my_instr
          |> String.concat ",\n"
        in
        (* raise @@ Failure (Format.sprintf "%s\n\n%s" instrs (my_stack_to_string _s)) *)
        let program = take 98 @@ Array.to_list instr_array in
        let (s, _) = split_at 3 @@ _s in
        let pc = Int64.to_string @@ Int64.of_int pc in
        raise
        @@ Failure ("Failed interpreting instr " ^ pc ^ ": " ^ show_my_instr x)
  in
  step
    ctxt
    (* Initialise the program counter at 0 *)
    0
    (* Translate the old ir to the new ir. This will go away *)
    instrs
    (* Translate the old stack format to the new stack format. This will also go away *)
    stack
    (* Intiailise with empty dip stack *)
    []
  >>=? fun (stack, ctxt) ->
  log_exit ctxt (-1) My_nil stack ;
  Error_monad.return (stack, ctxt)

(* FIXME: This function will disappear when elaboration is ready. *)
and step_descr :
    type b a.
    logger option ->
    context ->
    step_constants ->
    (b, a) descr ->
    b ->
    (a * context) tzresult Lwt.t =
 fun logger ctxt step_constants descr stack ->
  (* FIXME: That's ugly but this is only temporary. *)
  ( match descr.bef with
  | Item_t (ty, _, _) ->
      let (v, _) = stack in
      Script_ir_translator.unparse_data ctxt Readable ty v
  | _ -> assert false )
  >>=? fun (micheline, ctxt) ->
  let input_stack = myfy_stack (descr.bef, stack) in
  (* ( match input_stack with
     | [ My_pair (_, storage) ] ->
         print_endline @@ "~~~~~~~~~~~~~ " ^ show_my_item storage
     | _ -> assert false ) ; *)
  step_bounded
    logger
    ctxt
    step_constants
    (Array.of_list (translate descr))
    input_stack
  >>=? fun (stack, ctxt) ->
  (* ( match stack with
     | [ My_pair (_, storage) ] ->
         print_endline @@ "~~~~~~~~~~~~~ " ^ show_my_item storage
     | _ -> assert false ) ; *)
  return @@ (yfym_stack (descr.aft, stack), ctxt)

let rec step_tagged :
    logger option ->
    context ->
    step_constants ->
    my_instr Array.t ->
    my_stack ->
    (my_stack * context) tzresult Lwt.t =
 fun logger ctxt step_constants instrs stack ->
  (* FIXME: That's ugly but this is only temporary. *)
  (* print_endline @@ "================ Step Tagged Entrypoint: "
     ^ !entrypoint_running ^ "================" ; *)
  step_bounded logger ctxt step_constants instrs stack >>=? fun (stack, ctxt) ->
  return (stack, ctxt)

and interp :
    type p r.
    logger option ->
    context ->
    step_constants ->
    (p, r) lambda ->
    p ->
    (r * context) tzresult Lwt.t =
 fun logger ctxt step_constants (Lam (code, _)) arg ->
  let stack = (arg, ()) in
  step_descr logger ctxt step_constants code stack >|=? fun ((ret, ()), ctxt) ->
  (ret, ctxt)

let step = step_descr

(* ---- contract handling ---------------------------------------------------*)
let execute logger ctxt mode step_constants ~entrypoint ~internal
    unparsed_script arg :
    ( Script.expr
    * packed_internal_operation list
    * context
    * Lazy_storage.diffs option )
    tzresult
    Lwt.t =
  (* print_endline @@ "================ Execute Entrypoint: " ^ entrypoint
     ^ "================" ; *)
  parse_script ctxt unparsed_script ~legacy:true ~allow_forged_in_storage:true
  >>=? fun (Ex_script { code; arg_type; storage; storage_type; root_name }, ctxt)
    ->
  record_trace
    (Bad_contract_parameter step_constants.self)
    (find_entrypoint arg_type ~root_name entrypoint)
  >>?= fun (box, _) ->
  trace
    (Bad_contract_parameter step_constants.self)
    (parse_data ctxt ~legacy:false ~allow_forged:internal arg_type (box arg))
  >>=? fun (arg, ctxt) ->
  Script.force_decode_in_context ctxt unparsed_script.code
  >>?= fun (script_code, ctxt) ->
  Script_ir_translator.collect_lazy_storage ctxt arg_type arg
  >>?= fun (to_duplicate, ctxt) ->
  Script_ir_translator.collect_lazy_storage ctxt storage_type storage
  >>?= fun (to_update, ctxt) ->
  trace
    (Runtime_contract_error (step_constants.self, script_code))
    (interp logger ctxt step_constants code (arg, storage))
  >>=? fun ((ops, storage), ctxt) ->
  (* !print_balance ("Execute 1" ^ entrypoint ^ "\n") ctxt ; *)
  Script_ir_translator.extract_lazy_storage_diff
    ctxt
    mode
    ~temporary:false
    ~to_duplicate
    ~to_update
    storage_type
    storage
  >>=? fun (storage, lazy_storage_diff, ctxt) ->
  (* !print_balance "Execute 2.\n" ctxt ; *)
  trace
    Cannot_serialize_storage
    ( unparse_data ctxt mode storage_type storage >>=? fun (storage, ctxt) ->
      Lwt.return
        ( Gas.consume ctxt (Script.strip_locations_cost storage) >>? fun ctxt ->
          ok (Micheline.strip_locations storage, ctxt) ) )
  >|=? fun (storage, ctxt) ->
  (* !print_balance "Execute 3.\n" ctxt ; *)
  let (ops, op_diffs) = List.split ops.elements in
  let lazy_storage_diff =
    match
      List.flatten
        (List.map (Option.value ~default:[]) (op_diffs @ [ lazy_storage_diff ]))
    with
    | [] -> None
    | diff -> Some diff
  in
  (* !print_balance "Execute 4.\n" ctxt ; *)
  (storage, ops, ctxt, lazy_storage_diff)

type execution_result = {
  ctxt : context;
  storage : Script.expr;
  lazy_storage_diff : Lazy_storage.diffs option;
  operations : packed_internal_operation list;
}

let execute ?logger ctxt mode step_constants ~script ~entrypoint ~parameter
    ~internal =
  execute
    logger
    ctxt
    mode
    step_constants
    ~entrypoint
    ~internal
    script
    (Micheline.root parameter)
  >|=? fun (storage, operations, ctxt, lazy_storage_diff) ->
  { ctxt; storage; lazy_storage_diff; operations }
