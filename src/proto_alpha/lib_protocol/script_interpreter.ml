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
open Script_typed_cps_ir
open Script_ir_translator

(* ---- Run-time errors -----------------------------------------------------*)

type execution_trace =
  (Script.location * Gas.t * (Script.expr * string option) list) list

type error +=
  | Reject of Script.location * Script.expr * execution_trace option

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
      | Runtime_contract_error (contract, expr) ->
          Some (contract, expr)
      | _ ->
          None)
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
                      ( v7,
                        (v8, (v9, (va, (vb, (vc, (vd, (ve, (vf, rest'))))))))
                      ) ) ) ) ) ) ) ),
        result )
  | (Prefix (Prefix (Prefix (Prefix n))), (v0, (v1, (v2, (v3, rest))))) ->
      interp_stack_prefix_preserving_operation f n rest
      |> fun (rest', result) -> ((v0, (v1, (v2, (v3, rest')))), result)
  | (Prefix n, (v, rest)) ->
      interp_stack_prefix_preserving_operation f n rest
      |> fun (rest', result) -> ((v, rest'), result)
  | (Rest, v) ->
      f v

type step_constants = {
  source : Contract.t;
  payer : Contract.t;
  self : Contract.t;
  amount : Tez.t;
  chain_id : Chain_id.t;
}

type my_stack = my_item list

type my_dip_stack = my_stack

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
  | (My_dup, _) ->
      Interp_costs.dup
  | (My_swap, _) ->
      Interp_costs.swap
  | (My_push _, _) ->
      Interp_costs.push
  | (My_dip, _) ->
      Interp_costs.dip
  | (My_undip, _) ->
      Gas.free
  | (My_drop, _) ->
      Interp_costs.drop
  | (My_loop_if_not _, _) ->
      Interp_costs.loop
  | (My_loop_jump _, _) ->
      Gas.free
  | (My_car, _) ->
      Interp_costs.car
  (* ========= TODO: FIX ME  ==========*)
  | (My_compare, _) ->
      Gas.free
  | (My_neq, _) ->
      Interp_costs.neq
  | (My_sub, x :: y :: _) -> (
    match (x, y) with
    | (My_int x', My_int y') ->
        Interp_costs.sub_bigint x' y'
    | (_, _) ->
        assert false )
  | (My_mul_int, x :: y :: _) -> (
    match (x, y) with
    | (My_int x', My_int y') ->
        Interp_costs.mul_bigint x' y'
    | _ ->
        assert false )
  (* Hack to get it working... *)
  | (_, _) ->
      Gas.free

(* | (i, _) ->
      raise @@ Failure  (my_instr_to_str i) *)

let unpack ctxt ~ty ~bytes =
  Gas.check_enough ctxt (Script.serialized_cost bytes)
  >>?= fun () ->
  if
    Compare.Int.(Bytes.length bytes >= 1)
    && Compare.Int.(TzEndian.get_uint8 bytes 0 = 0x05)
  then
    let bytes = Bytes.sub bytes 1 (Bytes.length bytes - 1) in
    match Data_encoding.Binary.of_bytes Script.expr_encoding bytes with
    | None ->
        Lwt.return
          ( Gas.consume ctxt (Interp_costs.unpack_failed bytes)
          >|? fun ctxt -> (None, ctxt) )
    | Some expr -> (
        Gas.consume ctxt (Script.deserialized_cost expr)
        >>?= fun ctxt ->
        parse_data
          ctxt
          ~legacy:false
          ~allow_forged:false
          ty
          (Micheline.root expr)
        >|= function
        | Ok (value, ctxt) ->
            ok (Some value, ctxt)
        | Error _ignored ->
            Gas.consume ctxt (Interp_costs.unpack_failed bytes)
            >|? fun ctxt -> (None, ctxt) )
  else return (None, ctxt)

let[@inline] option_iter opt what =
  match opt with None -> () | Some l -> what l

let[@inline] lwt_option_iter opt what =
  match opt with None -> Lwt.return (Ok None) | Some l -> what l

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
  | (Pair_t ((a_ty, _, _), (b_ty, _, _), _), x) ->
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

let rec step_bounded :
    type bef aft.
    logger option ->
    context ->
    step_constants ->
    (bef, aft) descr ->
    bef ->
    (aft * context) tzresult Lwt.t =
 fun logger ctxt _step_constants descr stack ->
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
  (* let[@inline] log_exit ctxt pc stack =
    option_iter logger (fun logger ->
        let module Log = (val logger) in
        Log.log_exit ctxt pc stack)
  in *)
  (* let[@inline] get_log () =
    lwt_option_iter logger (fun logger ->
        let module Log = (val logger) in
        Log.get_log ())
  in *)
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
      log_exit ctxt (-1) instr stack;
      step ctxt pc instr_array stack dip_stack in
    Gas.consume ctxt gas
    >>?= fun ctxt ->
    (* ====== TODO: Fix logging function here ====== *)
    log_entry ctxt pc instr stack ;
    match (instr, stack) with
    (* stack ops *)
    | (My_dup, h :: t) ->
      step_return ctxt (pc + 1) instr_array (h :: h :: t) dip_stack
    (* | (My_swap, _) -> *)
    | (My_swap, h :: h' :: t) ->
        (* raise (Failure "swap case") *)
        step_return ctxt (pc + 1) instr_array (h' :: h :: t) dip_stack
    | (My_push x, s) ->
      step_return ctxt (pc + 1) instr_array (x :: s) dip_stack
    | (My_dip, h :: t) ->
        step ctxt (pc + 1) instr_array t (h :: dip_stack)
    | (My_undip, s) -> (
      match dip_stack with
      | h :: dip_stack' ->
          step_return ctxt (pc + 1) instr_array (h :: s) dip_stack'
      | [] ->
          step_return ctxt (pc + 1) instr_array s dip_stack )
    | (My_drop, _ :: t) ->
        step_return ctxt (pc + 1) instr_array t dip_stack
    | (My_loop_if_not n, My_bool b :: t) ->
        if b then step_return ctxt (pc + 1) instr_array t dip_stack
        else step_return ctxt (pc + n) instr_array t dip_stack
    | (My_loop_jump n, s) ->
        step_return ctxt (pc + n) instr_array s dip_stack
    | (My_car, My_pair (l, _) :: s) ->
        step_return ctxt (pc + 1) instr_array (l :: s) dip_stack
    | (My_cons_pair, h :: h' :: t) ->
        step_return ctxt (pc + 1) instr_array (My_pair (h, h') :: t) dip_stack
    | (My_compare, My_int a :: My_int b :: t) ->
        let cmp = My_int (Script_int.of_int @@ Script_int.compare a b) in
        step_return ctxt (pc + 1) instr_array (cmp :: t) dip_stack
    | (My_neq, My_int n :: t) ->
        let neq = Compare.Int.( <> ) Script_int.(compare n zero) 0 in
        step_return ctxt (pc + 1) instr_array (My_bool neq :: t) dip_stack
    | (My_sub, My_int a :: My_int b :: t) ->
        let sub = Script_int.sub a b in
        step_return ctxt (pc + 1) instr_array (My_int sub :: t) dip_stack
    | (My_mul_int, My_int a :: My_int b :: t) ->
        let mul = Script_int.mul a b in
        step_return ctxt (pc + 1) instr_array (My_int mul :: t) dip_stack
    | (My_add, My_int a :: My_int b :: t) ->
        let add = Script_int.add a b in
        step_return ctxt (pc + 1) instr_array (My_int add :: t) dip_stack
    | (My_halt, s) ->
        Lwt.return (Ok (s, ctxt))
    | (My_nil, s) ->
      step_return ctxt (pc + 1) instr_array (My_list [] :: s) dip_stack
    | (_, _) ->
        assert false
  in
  step
    ctxt
    (* Initialise the program counter at 0 *)
    0
    (* Translate the old ir to the new ir. This will go away *)
    (Array.of_list (translate descr))
    (* Translate the old stack format to the new stack format. This will also go away *)
    (myfy_stack (descr.bef, stack))
    (* Intiailise with empty dip stack *)
    []
  >>=? fun (stack, ctxt) ->
    log_exit ctxt (-1) My_nil stack;
    return @@ (yfym_stack (descr.aft, stack), ctxt)

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
  step_bounded logger ctxt step_constants descr stack

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
  step_descr logger ctxt step_constants code stack
  >|=? fun ((ret, ()), ctxt) -> (ret, ctxt)

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
  parse_script ctxt unparsed_script ~legacy:true ~allow_forged_in_storage:true
  >>=? fun (Ex_script {code; arg_type; storage; storage_type; root_name}, ctxt) ->
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
  Script_ir_translator.extract_lazy_storage_diff
    ctxt
    mode
    ~temporary:false
    ~to_duplicate
    ~to_update
    storage_type
    storage
  >>=? fun (storage, lazy_storage_diff, ctxt) ->
  trace
    Cannot_serialize_storage
    ( unparse_data ctxt mode storage_type storage
    >>=? fun (storage, ctxt) ->
    Lwt.return
      ( Gas.consume ctxt (Script.strip_locations_cost storage)
      >>? fun ctxt -> ok (Micheline.strip_locations storage, ctxt) ) )
  >|=? fun (storage, ctxt) ->
  let (ops, op_diffs) = List.split ops.elements in
  let lazy_storage_diff =
    match
      List.flatten
        (List.map (Option.value ~default:[]) (op_diffs @ [lazy_storage_diff]))
    with
    | [] ->
        None
    | diff ->
        Some diff
  in
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
  {ctxt; storage; lazy_storage_diff; operations}
