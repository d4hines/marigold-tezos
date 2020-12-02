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
open Script
open Script_typed_ir
open Script_typed_cps_ir
open Script_ir_translator
module S = Saturation_repr

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

type 's logging_function =
  context -> Script.location -> 's stack_ty -> 's -> unit

module type STEP_LOGGER = sig
  val log_interp : 's logging_function

  val log_entry : 's logging_function

  val log_exit : 's logging_function

  val get_log : unit -> execution_trace option tzresult Lwt.t
end

type logger = (module STEP_LOGGER)

(*

   Computing the cost of Michelson instructions.

   The cost model for Michelson classifies instructions in two
   categories: the ones whose cost depends on the data on the stack
   and the ones that have a statically fixed cost.

   The function [cost_of_instr] implements this cost model.  It is
   used in the interpreter to track the consumption of gas (see the
   function [step] below). This function is also used to regroup
   together the sequence of instructions whose cost can be determined
   statically (see [insert_pay_gas_in_advance] below).

 *)

(*

   We need to know if an instruction has a purely static cost: this
   piece of information is provided by [static_cost_of_instr].

   To avoid code duplication between [static_cost_of_instr] and
   [datadependent_cost_of_instr], the instruction's kind of cost
   is refined thanks to the following GADT:

 *)
type _ cost_of_instr =
  | StaticCost : Gas.cost -> static_cost cost_of_instr
  | DynamicCost : dynamic_cost cost_of_instr

let static_cost_of_instr :
    type a s r f c. (a, s, r, f, c) kinstr -> c cost_of_instr = function
  | KHalt _ ->
      (* FIXME *)
      StaticCost Gas.free
  | KDrop _ ->
      StaticCost Interp_costs.drop
  | KDup _ ->
      StaticCost Interp_costs.dup
  | KSwap _ ->
      StaticCost Interp_costs.swap
  | KConst _ ->
      StaticCost Interp_costs.push
  | KCons_some _ ->
      StaticCost Interp_costs.cons_some
  | KCons_none _ ->
      StaticCost Interp_costs.cons_none
  | KIf_none _ ->
      StaticCost Interp_costs.if_none
  | KCons_pair _ ->
      StaticCost Interp_costs.cons_pair
  | KUnpair _ ->
      StaticCost Interp_costs.unpair
  | KCar _ ->
      StaticCost Interp_costs.car
  | KCdr _ ->
      StaticCost Interp_costs.cdr
  | KCons_left _ ->
      StaticCost Interp_costs.cons_left
  | KCons_right _ ->
      StaticCost Interp_costs.cons_right
  | KIf_left _ ->
      StaticCost Interp_costs.if_left
  | KCons_list _ ->
      StaticCost Interp_costs.cons_list
  | KNil _ ->
      StaticCost Interp_costs.nil
  | KIf_cons _ ->
      StaticCost Interp_costs.if_cons
  | KList_map _ ->
      DynamicCost
  | KList_mapping _ ->
      DynamicCost
  | KList_mapped _ ->
      DynamicCost
  | KList_size _ ->
      StaticCost Interp_costs.list_size
  | KList_iter _ ->
      DynamicCost
  | KIter _ ->
      DynamicCost
  | KEmpty_set _ ->
      StaticCost Interp_costs.empty_set
  | KSet_iter _ ->
      DynamicCost
  | KSet_mem _ ->
      DynamicCost
  | KSet_update _ ->
      DynamicCost
  | KSet_size _ ->
      StaticCost Interp_costs.set_size
  | KEmpty_map _ ->
      StaticCost Interp_costs.empty_map
  | KMap_map _ ->
      DynamicCost
  | KMap_get_and_update _ ->
      DynamicCost
  | KMap_mapping _ ->
      DynamicCost
  | KMap_mapped _ ->
      DynamicCost
  | KMap_iter _ ->
      DynamicCost
  | KMap_mem _ ->
      DynamicCost
  | KMap_get _ ->
      DynamicCost
  | KMap_update _ ->
      DynamicCost
  | KMap_size _ ->
      StaticCost Interp_costs.map_size
  | KEmpty_big_map _ ->
      StaticCost Interp_costs.empty_map
  | KBig_map_mem _ ->
      DynamicCost
  | KBig_map_get _ ->
      DynamicCost
  | KBig_map_update _ ->
      DynamicCost
  | KBig_map_get_and_update _ ->
      DynamicCost
  | KAdd_seconds_to_timestamp _ ->
      DynamicCost
  | KAdd_timestamp_to_seconds _ ->
      DynamicCost
  | KSub_timestamp_seconds _ ->
      DynamicCost
  | KDiff_timestamps _ ->
      DynamicCost
  | KConcat_string_pair _ ->
      DynamicCost
  | KConcat_string _ ->
      DynamicCost
  | KSlice_string _ ->
      DynamicCost
  | KString_size _ ->
      StaticCost Interp_costs.string_size
  | KConcat_bytes_pair _ ->
      DynamicCost
  | KConcat_bytes _ ->
      DynamicCost
  | KSlice_bytes _ ->
      DynamicCost
  | KBytes_size _ ->
      StaticCost Interp_costs.bytes_size
  | KAdd_tez _ ->
      StaticCost Interp_costs.add_tez
  | KSub_tez _ ->
      StaticCost Interp_costs.sub_tez
  | KMul_teznat _ ->
      DynamicCost
  | KMul_nattez _ ->
      DynamicCost
  | KOr _ ->
      StaticCost Interp_costs.bool_or
  | KAnd _ ->
      StaticCost Interp_costs.bool_and
  | KXor _ ->
      StaticCost Interp_costs.bool_xor
  | KNot _ ->
      StaticCost Interp_costs.bool_not
  | KIs_nat _ ->
      StaticCost Interp_costs.is_nat
  | KAbs_int _ ->
      DynamicCost
  | KInt_nat _ ->
      StaticCost Interp_costs.int_nat
  | KInt_bls12_381_fr _ ->
      StaticCost Interp_costs.int_bls12_381_fr
  | KNeg_int _ ->
      DynamicCost
  | KNeg_nat _ ->
      DynamicCost
  | KAdd_intint _ ->
      DynamicCost
  | KAdd_intnat _ ->
      DynamicCost
  | KAdd_natint _ ->
      DynamicCost
  | KAdd_natnat _ ->
      DynamicCost
  | KSub_int _ ->
      DynamicCost
  | KMul_intint _ ->
      DynamicCost
  | KMul_intnat _ ->
      DynamicCost
  | KMul_natint _ ->
      DynamicCost
  | KMul_natnat _ ->
      DynamicCost
  | KEdiv_teznat _ ->
      DynamicCost
  | KEdiv_tez _ ->
      StaticCost Interp_costs.ediv_tez
  | KEdiv_intint _ ->
      DynamicCost
  | KEdiv_intnat _ ->
      DynamicCost
  | KEdiv_natint _ ->
      DynamicCost
  | KEdiv_natnat _ ->
      DynamicCost
  | KLsl_nat _ ->
      DynamicCost
  | KLsr_nat _ ->
      DynamicCost
  | KOr_nat _ ->
      DynamicCost
  | KAnd_nat _ ->
      DynamicCost
  | KAnd_int_nat _ ->
      DynamicCost
  | KXor_nat _ ->
      DynamicCost
  | KNot_int _ ->
      DynamicCost
  | KNot_nat _ ->
      DynamicCost
  | KIf _ ->
      StaticCost Interp_costs.if_
  | KLoop _ ->
      StaticCost Interp_costs.loop
  | KLoop_left _ ->
      StaticCost Interp_costs.loop_left
  | KDip _ ->
      StaticCost Interp_costs.dip
  | KExec _ ->
      StaticCost Interp_costs.exec
  | KApply _ ->
      StaticCost Interp_costs.apply
  | KLambda _ ->
      StaticCost Interp_costs.push
  | KFailwith _ ->
      StaticCost Gas.free
  | KNop _ ->
      StaticCost Interp_costs.nop
  | KCompare _ ->
      DynamicCost
  | KEq _ ->
      StaticCost Interp_costs.neq
  | KNeq _ ->
      StaticCost Interp_costs.neq
  | KLt _ ->
      StaticCost Interp_costs.neq
  | KLe _ ->
      StaticCost Interp_costs.neq
  | KGt _ ->
      StaticCost Interp_costs.neq
  | KGe _ ->
      StaticCost Interp_costs.neq
  | KPack _ ->
      StaticCost Gas.free
  | KUnpack _ ->
      StaticCost Gas.free
  | KAddress _ ->
      StaticCost Interp_costs.address
  | KContract _ ->
      StaticCost Interp_costs.contract
  | KTransfer_tokens _ ->
      StaticCost Interp_costs.transfer_tokens
  | KImplicit_account _ ->
      StaticCost Interp_costs.implicit_account
  | KSet_delegate _ ->
      StaticCost Interp_costs.set_delegate
  | KBalance _ ->
      StaticCost Interp_costs.balance
  | KLevel _ ->
      StaticCost Interp_costs.level
  | KNow _ ->
      StaticCost Interp_costs.now
  | KSapling_empty_state _ ->
      StaticCost Interp_costs.sapling_empty_state
  | KSapling_verify_update _ ->
      DynamicCost
  | KCheck_signature _ ->
      DynamicCost
  | KHash_key _ ->
      DynamicCost
  | KBlake2b _ ->
      DynamicCost
  | KSha256 _ ->
      DynamicCost
  | KSha512 _ ->
      DynamicCost
  | KSource _ ->
      StaticCost Interp_costs.source
  | KSender _ ->
      StaticCost Interp_costs.source
  | KSelf _ ->
      StaticCost Interp_costs.self
  | KSelf_address _ ->
      StaticCost Interp_costs.self
  | KAmount _ ->
      StaticCost Interp_costs.amount
  | KDig (_, n, _, _) ->
      StaticCost (Interp_costs.dign n)
  | KDug (_, n, _, _) ->
      StaticCost (Interp_costs.dugn n)
  | KDipn (_, n, _, _, _) ->
      StaticCost (Interp_costs.dipn n)
  | KDropn (_, n, _, _) ->
      StaticCost (Interp_costs.dropn n)
  | KChainId _ ->
      StaticCost Interp_costs.chain_id
  | KCreate_contract _ ->
      StaticCost Interp_costs.create_contract
  | KNever _ ->
      StaticCost Gas.free
  | KVoting_power _ ->
      StaticCost Interp_costs.voting_power
  | KTotal_voting_power _ ->
      StaticCost Interp_costs.total_voting_power
  | KKeccak _ ->
      DynamicCost
  | KSha3 _ ->
      DynamicCost
  | KAdd_bls12_381_g1 _ ->
      StaticCost Interp_costs.add_bls12_381_g1
  | KAdd_bls12_381_g2 _ ->
      StaticCost Interp_costs.add_bls12_381_g2
  | KAdd_bls12_381_fr _ ->
      StaticCost Interp_costs.add_bls12_381_fr
  | KMul_bls12_381_g1 _ ->
      StaticCost Interp_costs.mul_bls12_381_g1
  | KMul_bls12_381_g2 _ ->
      StaticCost Interp_costs.mul_bls12_381_g2
  | KMul_bls12_381_fr _ ->
      StaticCost Interp_costs.mul_bls12_381_fr
  | KNeg_bls12_381_g1 _ ->
      StaticCost Interp_costs.neg_bls12_381_g1
  | KNeg_bls12_381_g2 _ ->
      StaticCost Interp_costs.neg_bls12_381_g2
  | KNeg_bls12_381_fr _ ->
      StaticCost Interp_costs.neg_bls12_381_fr
  | KMul_bls12_381_fr_z _ ->
      StaticCost Interp_costs.mul_bls12_381_fr_z
  | KMul_bls12_381_z_fr _ ->
      StaticCost Interp_costs.mul_bls12_381_fr_z
  | KPairing_check_bls12_381 _ ->
      DynamicCost
  | KDup_n (_, n, _, _) ->
      StaticCost (Interp_costs.dupn n)
  | KComb (_, n, _, _) ->
      StaticCost (Interp_costs.comb n)
  | KUncomb (_, n, _, _) ->
      StaticCost (Interp_costs.uncomb n)
  | KComb_get (_, n, _, _) ->
      StaticCost (Interp_costs.comb_get n)
  | KComb_set (_, n, _, _) ->
      StaticCost (Interp_costs.comb_set n)
  | KTicket _ ->
      StaticCost Interp_costs.ticket
  | KRead_ticket _ ->
      StaticCost Interp_costs.read_ticket
  | KSplit_ticket _ ->
      DynamicCost
  | KJoin_tickets _ ->
      DynamicCost
  | KPayGas (_, _, n, _, _) ->
      StaticCost n
  | KCountGas _ ->
      (* FIXME *)
      StaticCost Gas.free
  [@@inline always]

let datadependent_cost_of_instr :
    type a s r f. (a, s, r, f, dynamic_cost) kinstr -> a -> s -> Gas.cost =
 fun instr accu stack ->
  match (instr, accu, stack) with
  | (KList_map _, list, _) ->
      Interp_costs.list_map list
  | (KList_mapping _, _, _) ->
      (* FIXME *)
      Gas.free
  | (KList_mapped _, _, _) ->
      (* FIXME *)
      Gas.free
  | (KList_iter _, l, _) ->
      Interp_costs.list_iter l
  | (KIter _, _, _) ->
      (* FIXME *)
      Gas.free
  | (KSet_iter _, set, _) ->
      Interp_costs.set_iter set
  | (KSet_mem _, v, (set, _)) ->
      Interp_costs.set_mem v set
  | (KSet_update _, v, (_, (set, _))) ->
      Interp_costs.set_update v set
  | (KMap_map _, map, _) ->
      Interp_costs.map_map map
  | (KMap_mapping _, _, _) ->
      (* FIXME *)
      Gas.free
  | (KMap_mapped _, _, _) ->
      (* FIXME *)
      Gas.free
  | (KMap_iter _, map, _) ->
      Interp_costs.map_iter map
  | (KMap_mem _, v, (map, _rest)) ->
      Interp_costs.map_mem v map
  | (KMap_get _, v, (map, _rest)) ->
      Interp_costs.map_get v map
  | (KMap_update _, k, (_, (map, _))) ->
      Interp_costs.map_update k map
  | (KMap_get_and_update _, k, (_, (map, _))) ->
      Interp_costs.map_get_and_update k map
  | (KBig_map_mem _, key, (map, _)) ->
      Interp_costs.map_mem key map.diff
  | (KBig_map_get _, key, (map, _)) ->
      Interp_costs.map_get key map.diff
  | (KBig_map_update _, key, (_, (map, _))) ->
      Interp_costs.map_update key map.diff
  | (KBig_map_get_and_update _, key, (_, (map, _))) ->
      Interp_costs.map_get_and_update key map.diff
  | (KAdd_seconds_to_timestamp _, n, (t, _)) ->
      Interp_costs.add_seconds_timestamp n t
  | (KAdd_timestamp_to_seconds _, t, (n, _)) ->
      Interp_costs.add_seconds_timestamp n t
  | (KSub_timestamp_seconds _, t, (n, _)) ->
      Interp_costs.sub_seconds_timestamp n t
  | (KDiff_timestamps _, t1, (t2, _)) ->
      Interp_costs.diff_timestamps t1 t2
  | (KConcat_string_pair _, x, (y, _)) ->
      Interp_costs.concat_string_pair x y
  | (KConcat_string _, ss, _) ->
      Interp_costs.concat_string_precheck ss
  | (KSlice_string _, _offset, (_length, (s, _))) ->
      Interp_costs.slice_string s
  | (KConcat_bytes_pair _, x, (y, _)) ->
      Interp_costs.concat_bytes_pair x y
  | (KConcat_bytes _, ss, _) ->
      Interp_costs.concat_string_precheck ss
  | (KSlice_bytes _, _offset, (_length, (s, _))) ->
      Interp_costs.slice_bytes s
  | (KMul_teznat _, _, (n, _)) ->
      Interp_costs.mul_teznat n
  | (KMul_nattez _, n, (_, _)) ->
      Interp_costs.mul_teznat n
  | (KAbs_int _, x, _) ->
      Interp_costs.abs_int x
  | (KNeg_int _, x, _) ->
      Interp_costs.neg_int x
  | (KNeg_nat _, x, _) ->
      Interp_costs.neg_nat x
  | (KAdd_intint _, x, (y, _)) ->
      Interp_costs.add_bigint x y
  | (KAdd_intnat _, x, (y, _)) ->
      Interp_costs.add_bigint x y
  | (KAdd_natint _, x, (y, _)) ->
      Interp_costs.add_bigint x y
  | (KAdd_natnat _, x, (y, _)) ->
      Interp_costs.add_bigint x y
  | (KSub_int _, x, (y, _)) ->
      Interp_costs.sub_bigint x y
  | (KMul_intint _, x, (y, _)) ->
      Interp_costs.mul_bigint x y
  | (KMul_intnat _, x, (y, _)) ->
      Interp_costs.mul_bigint x y
  | (KMul_natint _, x, (y, _)) ->
      Interp_costs.mul_bigint x y
  | (KMul_natnat _, x, (y, _)) ->
      Interp_costs.mul_bigint x y
  | (KEdiv_teznat _, x, (y, _)) ->
      Interp_costs.ediv_teznat x y
  | (KEdiv_intint _, x, (y, _)) ->
      Interp_costs.ediv_bigint x y
  | (KEdiv_intnat _, x, (y, _)) ->
      Interp_costs.ediv_bigint x y
  | (KEdiv_natint _, x, (y, _)) ->
      Interp_costs.ediv_bigint x y
  | (KEdiv_natnat _, x, (y, _)) ->
      Interp_costs.ediv_bigint x y
  | (KLsl_nat _, x, _) ->
      Interp_costs.lsl_nat x
  | (KLsr_nat _, x, _) ->
      Interp_costs.lsr_nat x
  | (KOr_nat _, x, (y, _)) ->
      Interp_costs.or_nat x y
  | (KAnd_nat _, x, (y, _)) ->
      Interp_costs.and_nat x y
  | (KAnd_int_nat _, x, (y, _)) ->
      Interp_costs.and_nat x y
  | (KXor_nat _, x, (y, _)) ->
      Interp_costs.xor_nat x y
  | (KNot_int _, x, _) ->
      Interp_costs.not_nat x
  | (KNot_nat _, x, _) ->
      Interp_costs.not_nat x
  | (KCompare (_, ty, _), a, (b, _)) ->
      Interp_costs.compare ty a b
  | (KCheck_signature _, key, (_, (message, _))) ->
      Interp_costs.check_signature key message
  | (KHash_key _, pk, _) ->
      Interp_costs.hash_key pk
  | (KBlake2b _, bytes, _) ->
      Interp_costs.blake2b bytes
  | (KSha256 _, bytes, _) ->
      Interp_costs.sha256 bytes
  | (KSha512 _, bytes, _) ->
      Interp_costs.sha512 bytes
  | (KKeccak _, bytes, _) ->
      Interp_costs.keccak bytes
  | (KSha3 _, bytes, _) ->
      Interp_costs.sha3 bytes
  | (KPairing_check_bls12_381 _, pairs, _) ->
      Interp_costs.pairing_check_bls12_381 pairs
  | (KSapling_verify_update _, tx, _) ->
      let inputs = List.length tx.inputs in
      let outputs = List.length tx.outputs in
      Interp_costs.sapling_verify_update ~inputs ~outputs
  | (KSplit_ticket _, ticket, ((amount_a, amount_b), _)) ->
      Interp_costs.split_ticket ticket.amount amount_a amount_b
  | (KJoin_tickets (_, ty, _), (ticket_a, ticket_b), _) ->
      Interp_costs.join_tickets ty ticket_a ticket_b
 [@@inline always]

let cost_of_instr :
    type a s r f c. (a, s, r, f, c) kinstr -> a -> s -> Gas.cost =
 fun instr accu stack ->
  match static_cost_of_instr instr with
  | DynamicCost ->
      datadependent_cost_of_instr instr accu stack
  | StaticCost g ->
      g
 [@@inline always]

(*

   Factorization of gas monitoring
   ===============================

   We apply a preprocessing to Michelson scripts before executing
   them. This preprocessor inserts new instructions of the form
   [KPayGas (d, k1, k2)] where [k1] represents a sequence of
   instructions of statically determined cost [d].

   The preprocessing accumulates a statically determined cost
   by traversing the script abstract syntax tree in a bottom-up
   way. When an instruction has a dynamic cost, we insert a
   [KPayGas] instruction unless there is only a single instruction
   in the sequence of statically determined cost.

   For instructions with more than two continuations, typically
   conditional and looping instructions, we treat the two continuations
   separately. For looping instruction, this would be unsound to do
   otherwise. For conditional instructions, we could merge two
   sequences with the same cost but this case is probably too seldom
   to justify extra complexity.

   A notable exception to that rule is [Dip] because its two continuations
   are actually applied in sequence: hence, their costs can be factorized
   out if they are static.

 *)

let pay_gas_in_advance k =
  let rec insert :
      type a s b t r f c.
      (a, s) kinfo ->
      (a, s, b, t, static_cost) kinstr_context ->
      static_cost cost_of_instr ->
      (b, t, r, f, c) kinstr ->
      bool * (a, s, r, f, static_cost) kinstr =
   fun kinfo i cost_of_instr k ->
    match (cost_of_instr, k) with
    | (StaticCost g, KPayGas (_, kinfo2, g', k1, k2)) ->
        (false, KPayGas (kinfo, kinfo2, Gas.(g +@ g'), i.graft k1, k2))
    | (StaticCost g, k) ->
        let kinfo2 = kinfo_of_kinstr k in
        (true, KPayGas (kinfo, kinfo2, g, i.graft (KHalt kinfo2), k))
  and recurse :
      type a s r f c. (a, s, r, f, c) kinstr -> (a, s, r, f, c) kinstr =
   fun i ->
    let (single, i') = decompose i in
    if single then i else i'
  and decompose :
      type a s r f c. (a, s, r, f, c) kinstr -> bool * (a, s, r, f, c) kinstr =
   fun i ->
    (*

        Only (a subset of the) instructions with a static cost can
        take part in this rewriting. The type assigned to [rewrite]
        helps us to properly skip instructions with a dynamic cost
        here.

      *)
    let[@inline] rewrite : (_, _, _, _, static_cost) kinstr -> _ -> _ -> _ -> _
        =
     fun i kinfo k kinstr_ctxt ->
      let cost = static_cost_of_instr i in
      let (_, k') = decompose k in
      insert kinfo kinstr_ctxt cost k'
    in
    (*

         Other instructions are simply returned with their potentially
         rewritten subterms.

      *)
    let ret i = (true, i) in
    match i with
    | KDrop (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KDrop (kinfo, k))}
    | KDup (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KDup (kinfo, k))}
    | KSwap (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KSwap (kinfo, k))}
    | KConst (kinfo, x, k) ->
        rewrite i kinfo k {graft = (fun k -> KConst (kinfo, x, k))}
    | KCons_pair (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KCons_pair (kinfo, k))}
    | KCar (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KCar (kinfo, k))}
    | KCdr (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KCdr (kinfo, k))}
    | KUnpair (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KUnpair (kinfo, k))}
    | KCons_some (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KCons_some (kinfo, k))}
    | KCons_none (kinfo, ty, k) ->
        rewrite i kinfo k {graft = (fun k -> KCons_none (kinfo, ty, k))}
    | KIf_none (kinfo, k1, k2) ->
        ret (KIf_none (kinfo, recurse k1, recurse k2))
    | KCons_left (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KCons_left (kinfo, k))}
    | KCons_right (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KCons_right (kinfo, k))}
    | KIf_left (kinfo, k1, k2) ->
        (true, KIf_left (kinfo, recurse k1, recurse k2))
    | KCons_list (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KCons_list (kinfo, k))}
    | KNil (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KNil (kinfo, k))}
    | KIf_cons (kinfo, k1, k2) ->
        (true, KIf_cons (kinfo, recurse k1, recurse k2))
    | KList_map (kinfo, k1, k2) ->
        (true, KList_map (kinfo, recurse k1, recurse k2))
    | KList_mapping _
    | KList_mapped _
    | KIter _
    | KMap_mapping _
    | KMap_mapped _ ->
        ret i (* These cases never occur in source programs. *)
    | KList_iter (kinfo, kinfo2, kf, k) ->
        ret (KList_iter (kinfo, kinfo2, recurse kf, recurse k))
    | KList_size (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KList_size (kinfo, k))}
    | KEmpty_set (kinfo, ty, k) ->
        rewrite i kinfo k {graft = (fun k -> KEmpty_set (kinfo, ty, k))}
    | KSet_iter (kinfo, kinfo2, kf, k) ->
        ret (KSet_iter (kinfo, kinfo2, recurse kf, recurse k))
    | KSet_mem (kinfo, k) ->
        ret (KSet_mem (kinfo, recurse k))
    | KSet_update (kinfo, k) ->
        ret (KSet_update (kinfo, recurse k))
    | KSet_size (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KSet_size (kinfo, k))}
    | KEmpty_map (kinfo, cty, ty, k) ->
        rewrite i kinfo k {graft = (fun k -> KEmpty_map (kinfo, cty, ty, k))}
    | KMap_map (kinfo, kinfo1, kinfo2, kf, k) ->
        ret (KMap_map (kinfo, kinfo1, kinfo2, recurse kf, recurse k))
    | KMap_iter (kinfo, kinfo1, kf, k) ->
        ret (KMap_iter (kinfo, kinfo1, recurse kf, recurse k))
    | KMap_mem (kinfo, k) ->
        ret (KMap_mem (kinfo, recurse k))
    | KMap_get (kinfo, k) ->
        ret (KMap_get (kinfo, recurse k))
    | KMap_update (kinfo, k) ->
        ret (KMap_update (kinfo, recurse k))
    | KMap_get_and_update (kinfo, k) ->
        ret (KMap_get_and_update (kinfo, recurse k))
    | KMap_size (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KMap_size (kinfo, k))}
    | KEmpty_big_map (kinfo, cty, ty, k) ->
        rewrite
          i
          kinfo
          k
          {graft = (fun k -> KEmpty_big_map (kinfo, cty, ty, k))}
    | KBig_map_mem (kinfo, k) ->
        ret (KBig_map_mem (kinfo, recurse k))
    | KBig_map_get (kinfo, k) ->
        ret (KBig_map_get (kinfo, recurse k))
    | KBig_map_update (kinfo, k) ->
        ret (KBig_map_update (kinfo, recurse k))
    | KBig_map_get_and_update (kinfo, k) ->
        ret (KBig_map_get_and_update (kinfo, recurse k))
    | KConcat_string (kinfo, k) ->
        ret (KConcat_string (kinfo, recurse k))
    | KConcat_string_pair (kinfo, k) ->
        ret (KConcat_string_pair (kinfo, recurse k))
    | KSlice_string (kinfo, k) ->
        ret (KSlice_string (kinfo, recurse k))
    | KString_size (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KString_size (kinfo, k))}
    | KConcat_bytes (kinfo, k) ->
        ret (KConcat_bytes (kinfo, recurse k))
    | KConcat_bytes_pair (kinfo, k) ->
        ret (KConcat_bytes_pair (kinfo, recurse k))
    | KSlice_bytes (kinfo, k) ->
        ret (KSlice_bytes (kinfo, recurse k))
    | KBytes_size (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KBytes_size (kinfo, k))}
    | KAdd_seconds_to_timestamp (kinfo, k) ->
        ret (KAdd_seconds_to_timestamp (kinfo, recurse k))
    | KAdd_timestamp_to_seconds (kinfo, k) ->
        ret (KAdd_timestamp_to_seconds (kinfo, recurse k))
    | KSub_timestamp_seconds (kinfo, k) ->
        ret (KSub_timestamp_seconds (kinfo, recurse k))
    | KDiff_timestamps (kinfo, k) ->
        ret (KDiff_timestamps (kinfo, recurse k))
    | KAdd_tez (kinfo, k) ->
        ret (KAdd_tez (kinfo, recurse k))
    | KSub_tez (kinfo, k) ->
        ret (KSub_tez (kinfo, recurse k))
    | KMul_teznat (kinfo, k) ->
        ret (KMul_teznat (kinfo, recurse k))
    | KMul_nattez (kinfo, k) ->
        ret (KMul_nattez (kinfo, recurse k))
    | KEdiv_teznat (kinfo, k) ->
        ret (KEdiv_teznat (kinfo, recurse k))
    | KEdiv_tez (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KEdiv_tez (kinfo, k))}
    | KOr (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KOr (kinfo, k))}
    | KAnd (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KAnd (kinfo, k))}
    | KXor (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KXor (kinfo, k))}
    | KNot (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KNot (kinfo, k))}
    | KIs_nat (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KIs_nat (kinfo, k))}
    | KNeg_nat (kinfo, k) ->
        ret (KNeg_nat (kinfo, recurse k))
    | KNeg_int (kinfo, k) ->
        ret (KNeg_int (kinfo, recurse k))
    | KAbs_int (kinfo, k) ->
        ret (KAbs_int (kinfo, recurse k))
    | KInt_nat (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KInt_nat (kinfo, k))}
    | KAdd_intint (kinfo, k) ->
        ret (KAdd_intint (kinfo, recurse k))
    | KAdd_intnat (kinfo, k) ->
        ret (KAdd_intnat (kinfo, recurse k))
    | KAdd_natint (kinfo, k) ->
        ret (KAdd_natint (kinfo, recurse k))
    | KAdd_natnat (kinfo, k) ->
        ret (KAdd_natnat (kinfo, recurse k))
    | KSub_int (kinfo, k) ->
        ret (KSub_int (kinfo, recurse k))
    | KMul_intint (kinfo, k) ->
        ret (KMul_intint (kinfo, recurse k))
    | KMul_intnat (kinfo, k) ->
        ret (KMul_intnat (kinfo, recurse k))
    | KMul_natint (kinfo, k) ->
        ret (KMul_natint (kinfo, recurse k))
    | KMul_natnat (kinfo, k) ->
        ret (KMul_natnat (kinfo, recurse k))
    | KEdiv_intint (kinfo, k) ->
        ret (KEdiv_intint (kinfo, recurse k))
    | KEdiv_intnat (kinfo, k) ->
        ret (KEdiv_intnat (kinfo, recurse k))
    | KEdiv_natint (kinfo, k) ->
        ret (KEdiv_natint (kinfo, recurse k))
    | KEdiv_natnat (kinfo, k) ->
        ret (KEdiv_natnat (kinfo, recurse k))
    | KLsl_nat (kinfo, k) ->
        ret (KLsl_nat (kinfo, recurse k))
    | KLsr_nat (kinfo, k) ->
        ret (KLsr_nat (kinfo, recurse k))
    | KOr_nat (kinfo, k) ->
        ret (KOr_nat (kinfo, recurse k))
    | KAnd_nat (kinfo, k) ->
        ret (KAnd_nat (kinfo, recurse k))
    | KAnd_int_nat (kinfo, k) ->
        ret (KAnd_int_nat (kinfo, recurse k))
    | KXor_nat (kinfo, k) ->
        ret (KXor_nat (kinfo, recurse k))
    | KNot_nat (kinfo, k) ->
        ret (KNot_nat (kinfo, recurse k))
    | KNot_int (kinfo, k) ->
        ret (KNot_int (kinfo, recurse k))
    | KIf (kinfo, kt, kf) ->
        ret (KIf (kinfo, recurse kt, recurse kf))
    | KLoop (kinfo, kbody, k) ->
        ret (KLoop (kinfo, recurse kbody, recurse k))
    | KLoop_left (kinfo, kbody, k) ->
        ret (KLoop_left (kinfo, recurse kbody, recurse k))
    | KDip (kinfo, kinfo2, kf, k) -> (
        let (StaticCost cost) = static_cost_of_instr i in
        match (recurse kf, recurse k) with
        | (KPayGas (_, _, df, kf, KHalt _), KPayGas (_, kinfo2', d, k, k2)) ->
            ( false,
              KPayGas
                ( kinfo,
                  kinfo2',
                  Gas.(cost +@ df +@ d),
                  KDip (kinfo, kinfo2, kf, k),
                  k2 ) )
        | (KPayGas (_, _kfinfo2, df, kf, KHalt _), k) ->
            let kinfo' = kinfo_of_kinstr k in
            ( false,
              KPayGas
                ( kinfo,
                  kinfo',
                  Gas.(cost +@ df),
                  KDip (kinfo, kinfo2, kf, KHalt kinfo'),
                  k ) )
        | (kf, k) ->
            ret (KDip (kinfo, kinfo2, kf, k)) )
    | KExec (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KExec (kinfo, k))}
    | KApply (kinfo, ty, k) ->
        ret (KApply (kinfo, ty, k))
    | KLambda (kinfo, lambda, k) ->
        rewrite i kinfo k {graft = (fun k -> KLambda (kinfo, lambda, k))}
    | KFailwith (kinfo, loc, ty, k) ->
        rewrite i kinfo k {graft = (fun k -> KFailwith (kinfo, loc, ty, k))}
    | KNop (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KNop (kinfo, k))}
    | KCompare (kinfo, ty, k) ->
        ret (KCompare (kinfo, ty, recurse k))
    | KEq (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KEq (kinfo, k))}
    | KNeq (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KNeq (kinfo, k))}
    | KLt (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KLt (kinfo, k))}
    | KGt (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KGt (kinfo, k))}
    | KLe (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KLe (kinfo, k))}
    | KGe (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KGe (kinfo, k))}
    | KAddress (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KAddress (kinfo, k))}
    | KContract (kinfo, ty, code, k) ->
        rewrite i kinfo k {graft = (fun k -> KContract (kinfo, ty, code, k))}
    | KTransfer_tokens (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KTransfer_tokens (kinfo, k))}
    | KImplicit_account (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KImplicit_account (kinfo, k))}
    | KCreate_contract (kinfo, ty1, ty2, l, a, k) ->
        let c =
          {graft = (fun k -> KCreate_contract (kinfo, ty1, ty2, l, a, k))}
        in
        rewrite i kinfo k c
    | KSet_delegate (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KSet_delegate (kinfo, k))}
    | KNow (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KNow (kinfo, k))}
    | KBalance (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KBalance (kinfo, k))}
    | KLevel (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KLevel (kinfo, k))}
    | KCheck_signature (kinfo, k) ->
        ret (KCheck_signature (kinfo, recurse k))
    | KHash_key (kinfo, k) ->
        ret (KHash_key (kinfo, recurse k))
    | KPack (kinfo, s, k) ->
        rewrite i kinfo k {graft = (fun k -> KPack (kinfo, s, k))}
    | KUnpack (kinfo, s, k) ->
        rewrite i kinfo k {graft = (fun k -> KUnpack (kinfo, s, k))}
    | KBlake2b (kinfo, k) ->
        ret (KBlake2b (kinfo, recurse k))
    | KSha256 (kinfo, k) ->
        ret (KSha256 (kinfo, recurse k))
    | KSha512 (kinfo, k) ->
        ret (KSha512 (kinfo, recurse k))
    | KSource (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KSource (kinfo, k))}
    | KSender (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KSender (kinfo, k))}
    | KSelf (kinfo, ty, code, k) ->
        rewrite i kinfo k {graft = (fun k -> KSelf (kinfo, ty, code, k))}
    | KSelf_address (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KSelf_address (kinfo, k))}
    | KAmount (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KAmount (kinfo, k))}
    | KSapling_empty_state (kinfo, ms, k) ->
        ret (KSapling_empty_state (kinfo, ms, recurse k))
    | KSapling_verify_update (kinfo, k) ->
        ret (KSapling_verify_update (kinfo, recurse k))
    | KDig (kinfo, n, w, k) ->
        rewrite i kinfo k {graft = (fun k -> KDig (kinfo, n, w, k))}
    | KDug (kinfo, n, w, k) ->
        rewrite i kinfo k {graft = (fun k -> KDug (kinfo, n, w, k))}
    | KDipn (kinfo, n, w, kf, k) -> (
        let (StaticCost cost) = static_cost_of_instr i in
        match (recurse kf, recurse k) with
        | (KPayGas (_, _, df, kf, KHalt _), KPayGas (_, kinfo2', d, k, k2)) ->
            ( false,
              KPayGas
                ( kinfo,
                  kinfo2',
                  Gas.(cost +@ df +@ d),
                  KDipn (kinfo, n, w, kf, k),
                  k2 ) )
        | (KPayGas (_, _kfinfo2, df, kf, KHalt _), k) ->
            let kinfo' = kinfo_of_kinstr k in
            ( false,
              KPayGas
                ( kinfo,
                  kinfo',
                  Gas.(cost +@ df),
                  KDipn (kinfo, n, w, kf, KHalt kinfo'),
                  k ) )
        | (kf, k) ->
            ret (KDipn (kinfo, n, w, kf, k)) )
    | KDropn (kinfo, n, w, k) ->
        rewrite i kinfo k {graft = (fun k -> KDropn (kinfo, n, w, k))}
    | KChainId (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KChainId (kinfo, k))}
    | KNever (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KNever (kinfo, k))}
    | KVoting_power (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KVoting_power (kinfo, k))}
    | KTotal_voting_power (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KTotal_voting_power (kinfo, k))}
    | KKeccak (kinfo, k) ->
        ret (KKeccak (kinfo, recurse k))
    | KSha3 (kinfo, k) ->
        ret (KSha3 (kinfo, recurse k))
    | KAdd_bls12_381_g1 (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KAdd_bls12_381_g1 (kinfo, k))}
    | KAdd_bls12_381_g2 (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KAdd_bls12_381_g2 (kinfo, k))}
    | KAdd_bls12_381_fr (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KAdd_bls12_381_fr (kinfo, k))}
    | KMul_bls12_381_g1 (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KMul_bls12_381_g1 (kinfo, k))}
    | KMul_bls12_381_g2 (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KMul_bls12_381_g2 (kinfo, k))}
    | KMul_bls12_381_fr (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KMul_bls12_381_fr (kinfo, k))}
    | KMul_bls12_381_z_fr (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KMul_bls12_381_z_fr (kinfo, k))}
    | KMul_bls12_381_fr_z (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KMul_bls12_381_fr_z (kinfo, k))}
    | KInt_bls12_381_fr (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KInt_bls12_381_fr (kinfo, k))}
    | KNeg_bls12_381_g1 (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KNeg_bls12_381_g1 (kinfo, k))}
    | KNeg_bls12_381_g2 (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KNeg_bls12_381_g2 (kinfo, k))}
    | KNeg_bls12_381_fr (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KNeg_bls12_381_fr (kinfo, k))}
    | KPairing_check_bls12_381 (kinfo, k) ->
        ret (KPairing_check_bls12_381 (kinfo, recurse k))
    | KComb (kinfo, n, w, k) ->
        rewrite i kinfo k {graft = (fun k -> KComb (kinfo, n, w, k))}
    | KUncomb (kinfo, n, w, k) ->
        rewrite i kinfo k {graft = (fun k -> KUncomb (kinfo, n, w, k))}
    | KComb_get (kinfo, n, w, k) ->
        rewrite i kinfo k {graft = (fun k -> KComb_get (kinfo, n, w, k))}
    | KComb_set (kinfo, n, w, k) ->
        rewrite i kinfo k {graft = (fun k -> KComb_set (kinfo, n, w, k))}
    | KDup_n (kinfo, n, w, k) ->
        rewrite i kinfo k {graft = (fun k -> KDup_n (kinfo, n, w, k))}
    | KTicket (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KTicket (kinfo, k))}
    | KRead_ticket (kinfo, k) ->
        rewrite i kinfo k {graft = (fun k -> KRead_ticket (kinfo, k))}
    | KSplit_ticket (kinfo, k) ->
        ret (KSplit_ticket (kinfo, recurse k))
    | KJoin_tickets (kinfo, cty, k) ->
        ret (KJoin_tickets (kinfo, cty, recurse k))
    | KHalt kinfo ->
        ret (KHalt kinfo)
    | KPayGas _ ->
        (* This case should never occur. *)
        ret i
    | KCountGas _ ->
        (* This case should never occur. *)
        ret i
  in
  snd (decompose k)

(* ---- evaluation  ---------------------------------------------------*)

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

type (_, _, _, _) konts =
  | KNil : ('r, 'f, 'r, 'f) konts
  | KCons :
      ('a, 's, 'b, 't, _) kinstr * ('b, 't, 'r, 'f) konts
      -> ('a, 's, 'r, 'f) konts

type (_, _, _) exkinstr =
  | ExKInstr :
      ('t, 'x * 'z) eq * ('x, 'z, 'b, 'u, _) kinstr
      -> ('t, 'b, 'u) exkinstr

let option_iter opt what =
  match opt with None -> () | Some l -> what l
  [@@inline.always]

let lwt_option_iter opt what =
  match opt with None -> Lwt.return (Ok None) | Some l -> what l
  [@@inline.always]

(*

   Gas update and check for gas exhaustion
   =======================================

   Each instruction has a cost. For many instructions, this cost can
   be statically determined. When a sequence of instructions has a
   statically determined cost, the [pay_gas_in_advance] inserts the
   instruction [PayGas] so that the gas is paid in advance for this
   sequence and not at each execution step.

   The [gas_monitor_status] is true if the gas has already been paid
   by a [PayGas] or if the gas does not have to be counted.

*)
type gas_monitor_status = bool

let monitor_gas = true

let dont_monitor_gas = false

(*

   Updating the gas counter is a critical aspect to Michelson
   execution because it is done very frequently (sometimes,
   at each execution step).

   For this reason, the interpreter must read and update the
   gas counter as quickly as possible. Hence, the gas counter
   should be stored in a machine register. To motivate the
   OCaml compiler to make that choice, we represent the gas
   counter as a local parameter of [step].

*)

type local_gas_counter = int

(*

   The gas counter stored in the context is desynchronized with the
   local gas counter within the interpretation loop. When we have to
   call a gas-consuming function which lives outside the interpreter,
   we must update the context so that it carries an up-to-date gas
   counter. Besides, when we return from such function, the local gas
   counter must be updated as well.

   To statically track these points where the context's gas counter
   must be updated, we introduce a type for outdated contexts.

*)
type outdated_context = OutDatedContext of context [@@unboxed]

let update_context gas_counter : outdated_context -> context = function
  | OutDatedContext ctxt ->
      Gas.update_gas_counter ctxt (Saturation_repr.of_int gas_counter)
  [@@inline.always]

let outdated ctxt = OutDatedContext ctxt

let outdated_context (OutDatedContext ctxt) = ctxt

let use_gas_counter_in_ctxt ctxt gas_counter f =
  let ctxt = update_context gas_counter ctxt in
  f ctxt
  >>=? fun (y, ctxt) ->
  let gas_counter = Gas.gas_counter ctxt in
  return (y, outdated ctxt, (gas_counter :> int))
  [@@inline always]

let[@inline] update_and_check gas_counter cost =
  let gas_counter = gas_counter - cost in
  if Compare.Int.(gas_counter < 0) then None else Some gas_counter

let[@inline] consume :
    type a s r f cost.
    local_gas_counter ->
    gas_monitor_status ->
    (a, s, r, f, cost) kinstr ->
    a ->
    s ->
    local_gas_counter option =
 fun gas_counter count_gas k accu stack ->
  if count_gas then
    let cost = cost_of_instr k accu stack in
    update_and_check gas_counter (cost :> int)
  else Some gas_counter

let[@inline] consume' ctxt gas_counter cost =
  match update_and_check gas_counter cost with
  | None ->
      Gas.gas_exhausted_error (update_context gas_counter ctxt)
  | Some gas_counter ->
      Ok gas_counter

(*

  Interpretation loop
  ===================

*)
let rec step_bounded :
    type bef aft.
    logger option ->
    context ->
    step_constants ->
    (bef, aft) kdescr ->
    bef ->
    (aft * context) tzresult Lwt.t =
 fun logger ctxt step_constants descr stack ->
  let log_entry ctxt gas k accu stack =
    option_iter logger (fun logger ->
        let module Log = (val logger) in
        let kinfo = kinfo_of_kinstr k in
        let ctxt = update_context gas ctxt in
        Log.log_entry ctxt kinfo.kloc kinfo.kstack_ty (accu, stack))
    [@@inline.always]
  in
  let log_exit ctxt gas kprev k accu stack =
    option_iter logger (fun logger ->
        let module Log = (val logger) in
        let ctxt = update_context gas ctxt in
        let kinfo_prev = kinfo_of_kinstr kprev and kinfo = kinfo_of_kinstr k in
        Log.log_exit ctxt kinfo_prev.kloc kinfo.kstack_ty (accu, stack))
    [@@inline.always]
  in
  let get_log () =
    lwt_option_iter logger (fun logger ->
        let module Log = (val logger) in
        Log.get_log ())
    [@@inline.always]
  in
  let (KDescr {kinstr; kli; klo}) = descr in
  let rec step :
      type a s b t r f c.
      outdated_context ->
      local_gas_counter ->
      gas_monitor_status ->
      (a, s, b, t, c) kinstr ->
      (b, t, r, f) konts ->
      a ->
      s ->
      (r * f * outdated_context * local_gas_counter) tzresult Lwt.t =
   fun ctxt gas gms k ks accu stack ->
    let run :
        type a s b t r f c.
        outdated_context ->
        local_gas_counter ->
        gas_monitor_status ->
        (a, s, b, t, c) kinstr ->
        (b, t, r, f) konts ->
        a ->
        s ->
        (r * f * outdated_context * local_gas_counter) tzresult Lwt.t =
     fun ctxt gas gms k' ks accu stack ->
      log_exit ctxt gas k k' accu stack ;
      (step [@ocaml.tailcall]) ctxt gas gms k' ks accu stack
     [@@inline.always]
    in
    match consume gas gms k accu stack with
    | None ->
        Lwt.return (Gas.gas_exhausted_error (update_context gas ctxt))
    | Some gas -> (
        log_entry ctxt gas k accu stack ;
        match k with
        | KPayGas (_, kinfo2, _, k1, k2) ->
            let ks = KCons (KCountGas (kinfo2, k2), ks) in
            (step [@ocaml.tailcall]) ctxt gas dont_monitor_gas k1 ks accu stack
        | KCountGas (_, k) ->
            (step [@ocaml.tailcall]) ctxt gas monitor_gas k ks accu stack
        | KHalt _ -> (
          match ks with
          | KNil ->
              ( Lwt.return (Ok (accu, stack, ctxt, gas))
                : (r * f * outdated_context * local_gas_counter) tzresult Lwt.t
                )
          | KCons (k, ks) ->
              (step [@ocaml.tailcall]) ctxt gas gms k ks accu stack )
        (* stack ops *)
        | KDrop (_, k) ->
            (run [@ocaml.tailcall]) ctxt gas gms k ks (fst stack) (snd stack)
        | KDup (_, k) ->
            (run [@ocaml.tailcall]) ctxt gas gms k ks accu (accu, stack)
        | KSwap (_, k) ->
            let (top, stack) = stack in
            (run [@ocaml.tailcall]) ctxt gas gms k ks top (accu, stack)
        | KConst (_, v, k) ->
            (run [@ocaml.tailcall]) ctxt gas gms k ks v (accu, stack)
        (* options *)
        | KCons_some (_, k) ->
            (run [@ocaml.tailcall]) ctxt gas gms k ks (Some accu) stack
        | KCons_none (_, _, k) ->
            (run [@ocaml.tailcall]) ctxt gas gms k ks None (accu, stack)
        | KIf_none (_, bt, bf) -> (
          match accu with
          | None ->
              (run [@ocaml.tailcall])
                ctxt
                gas
                gms
                bt
                ks
                (fst stack)
                (snd stack)
          | Some v ->
              (run [@ocaml.tailcall]) ctxt gas gms bf ks v stack )
        (* pairs *)
        | KCons_pair (_, k) ->
            let (b, stack) = stack in
            (run [@ocaml.tailcall]) ctxt gas gms k ks (accu, b) stack
        | KUnpair (_, k) ->
            let (a, b) = accu in
            (run [@ocaml.tailcall]) ctxt gas gms k ks a (b, stack)
        | KCar (_, k) ->
            let (a, _) = accu in
            (run [@ocaml.tailcall]) ctxt gas gms k ks a stack
        | KCdr (_, k) ->
            let (_, b) = accu in
            (run [@ocaml.tailcall]) ctxt gas gms k ks b stack
        (* unions *)
        | KCons_left (_, k) ->
            (run [@ocaml.tailcall]) ctxt gas gms k ks (L accu) stack
        | KCons_right (_, k) ->
            (run [@ocaml.tailcall]) ctxt gas gms k ks (R accu) stack
        | KIf_left (_, bl, br) -> (
          match accu with
          | L v ->
              (run [@ocaml.tailcall]) ctxt gas gms bl ks v stack
          | R v ->
              (run [@ocaml.tailcall]) ctxt gas gms br ks v stack )
        (* lists *)
        | KCons_list (_, k) ->
            let (tl, stack) = stack in
            (run [@ocaml.tailcall]) ctxt gas gms k ks (list_cons accu tl) stack
        | KNil (_, k) ->
            (run [@ocaml.tailcall]) ctxt gas gms k ks list_empty (accu, stack)
        | KIf_cons (_, bc, bn) -> (
          match accu.elements with
          | [] ->
              (run [@ocaml.tailcall])
                ctxt
                gas
                gms
                bn
                ks
                (fst stack)
                (snd stack)
          | hd :: tl ->
              let tl = {elements = tl; length = accu.length - 1} in
              (run [@ocaml.tailcall]) ctxt gas gms bc ks hd (tl, stack) )
        | KList_map (kinfo, body, k) ->
            let xs = accu.elements in
            let ys = [] in
            let len = accu.length in
            let kinfo_mapped =
              match (kinfo_of_kinstr k).kstack_ty with
              | Item_t (ty, s, a) ->
                  {kinfo with kstack_ty = Item_t (unlist_ty ty, s, a)}
            in
            let kinfo_mapping =
              match kinfo.kstack_ty with
              | Item_t (_, kstack_ty, _) ->
                  {kloc = kinfo.kloc; kstack_ty}
            in
            let k =
              KList_mapping (kinfo_mapping, kinfo_mapped, body, xs, ys, len, k)
            in
            (step [@ocaml.tailcall]) ctxt gas gms k ks (fst stack) (snd stack)
        | KList_mapping (kinfo_mapping, kinfo_mapped, body, xs, ys, len, k)
          -> (
          match xs with
          | [] ->
              let ys = {elements = List.rev ys; length = len} in
              (step [@ocaml.tailcall]) ctxt gas gms k ks ys (accu, stack)
          | x :: xs ->
              let ks =
                KCons
                  ( KList_mapped
                      (kinfo_mapped, kinfo_mapping, body, xs, ys, len, k),
                    ks )
              in
              (step [@ocaml.tailcall]) ctxt gas gms body ks x (accu, stack) )
        | KList_mapped (kinfo_mapped, kinfo_mapping, body, xs, ys, len, k) ->
            let k =
              KList_mapping
                (kinfo_mapping, kinfo_mapped, body, xs, accu :: ys, len, k)
            in
            (step [@ocaml.tailcall]) ctxt gas gms k ks (fst stack) (snd stack)
        | KList_size (_, k) ->
            let list = accu in
            let len = Script_int.(abs (of_int list.length)) in
            (run [@ocaml.tailcall]) ctxt gas gms k ks len stack
        | KList_iter (_, kinfo_iter, body, k) ->
            let xs = accu.elements in
            let k = KIter (kinfo_iter, body, xs, k) in
            (step [@ocaml.tailcall]) ctxt gas gms k ks (fst stack) (snd stack)
        | KIter (kinfo, body, xs, k) -> (
          match xs with
          | [] ->
              (step [@ocaml.tailcall]) ctxt gas gms k ks accu stack
          | x :: xs ->
              let ks = KCons (KIter (kinfo, body, xs, k), ks) in
              (step [@ocaml.tailcall]) ctxt gas gms body ks x (accu, stack) )
        (* sets *)
        | KEmpty_set (_, ty, k) ->
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (empty_set ty)
              (accu, stack)
        | KSet_iter (_, kinfo_iter, body, k) ->
            let set = accu in
            let l = List.rev (set_fold (fun e acc -> e :: acc) set []) in
            let kiter = KIter (kinfo_iter, body, l, k) in
            (step [@ocaml.tailcall])
              ctxt
              gas
              gms
              kiter
              ks
              (fst stack)
              (snd stack)
        | KSet_mem (_, k) ->
            let (set, stack) = stack in
            (run [@ocaml.tailcall]) ctxt gas gms k ks (set_mem accu set) stack
        | KSet_update (_, k) ->
            let (presence, (set, stack)) = stack in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (set_update accu presence set)
              stack
        | KSet_size (_, k) ->
            (run [@ocaml.tailcall]) ctxt gas gms k ks (set_size accu) stack
        (* maps *)
        | KEmpty_map (_, ty, _, k) ->
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (empty_map ty)
              (accu, stack)
        | KMap_map (_, kinfo_mapping, kinfo_mapped, body, k) ->
            let map = accu in
            let xs =
              List.rev (map_fold (fun k v acc -> (k, v) :: acc) map [])
            in
            let ys = empty_map (map_key_ty map) in
            let km =
              KMap_mapping (kinfo_mapping, kinfo_mapped, body, xs, ys, k)
            in
            (step [@ocaml.tailcall]) ctxt gas gms km ks (fst stack) (snd stack)
        | KMap_mapping (kinfo_mapping, kinfo_mapped, body, xs, ys, k) -> (
          match xs with
          | [] ->
              (step [@ocaml.tailcall]) ctxt gas gms k ks ys (accu, stack)
          | (xk, xv) :: xs ->
              let ks =
                KCons
                  ( KMap_mapped
                      (kinfo_mapped, kinfo_mapping, body, xs, ys, xk, k),
                    ks )
              in
              (step [@ocaml.tailcall])
                ctxt
                gas
                gms
                body
                ks
                (xk, xv)
                (accu, stack) )
        | KMap_mapped (kinfo_mapped, kinfo_mapping, body, xs, ys, yk, k) ->
            let ys = map_update yk (Some accu) ys in
            let k =
              KMap_mapping (kinfo_mapping, kinfo_mapped, body, xs, ys, k)
            in
            (step [@ocaml.tailcall]) ctxt gas gms k ks (fst stack) (snd stack)
        | KMap_iter (_, kinfo_iter, body, k) ->
            let map = accu in
            let l =
              List.rev (map_fold (fun k v acc -> (k, v) :: acc) map [])
            in
            let kiter = KIter (kinfo_iter, body, l, k) in
            (step [@ocaml.tailcall])
              ctxt
              gas
              gms
              kiter
              ks
              (fst stack)
              (snd stack)
        | KMap_mem (_, k) ->
            let (map, stack) = stack in
            (run [@ocaml.tailcall]) ctxt gas gms k ks (map_mem accu map) stack
        | KMap_get (_, k) ->
            let (map, stack) = stack in
            (run [@ocaml.tailcall]) ctxt gas gms k ks (map_get accu map) stack
        | KMap_update (_, k) ->
            let (v, (map, stack)) = stack in
            let key = accu in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (map_update key v map)
              stack
        | KMap_get_and_update (_, k) ->
            let key = accu in
            let (v, (map, rest)) = stack in
            let map' = map_update key v map in
            let v' = map_get key map in
            (run [@ocaml.tailcall]) ctxt gas gms k ks v' (map', rest)
        | KMap_size (_, k) ->
            (run [@ocaml.tailcall]) ctxt gas gms k ks (map_size accu) stack
        (* Big map operations *)
        | KEmpty_big_map (_, tk, tv, k) ->
            let ebm = Script_ir_translator.empty_big_map tk tv in
            (run [@ocaml.tailcall]) ctxt gas gms k ks ebm (accu, stack)
        | KBig_map_mem (_, k) ->
            let (map, stack) = stack in
            let key = accu in
            ( use_gas_counter_in_ctxt ctxt gas
            @@ fun ctxt -> Script_ir_translator.big_map_mem ctxt key map )
            >>=? fun (res, ctxt, gas) ->
            (run [@ocaml.tailcall]) ctxt gas gms k ks res stack
        | KBig_map_get (_, k) ->
            let (map, stack) = stack in
            let key = accu in
            ( use_gas_counter_in_ctxt ctxt gas
            @@ fun ctxt -> Script_ir_translator.big_map_get ctxt key map )
            >>=? fun (res, ctxt, gas) ->
            (run [@ocaml.tailcall]) ctxt gas gms k ks res stack
        | KBig_map_update (_, k) ->
            let key = accu in
            let (maybe_value, (map, stack)) = stack in
            let big_map =
              Script_ir_translator.big_map_update key maybe_value map
            in
            (run [@ocaml.tailcall]) ctxt gas gms k ks big_map stack
        | KBig_map_get_and_update (_, k) ->
            let key = accu in
            let (v, (map, stack)) = stack in
            let map' = Script_ir_translator.big_map_update key v map in
            ( use_gas_counter_in_ctxt ctxt gas
            @@ fun ctxt -> Script_ir_translator.big_map_get ctxt key map )
            >>=? fun (v', ctxt, gas) ->
            (run [@ocaml.tailcall]) ctxt gas gms k ks v' (map', stack)
        (* timestamp operations *)
        | KAdd_seconds_to_timestamp (_, k) ->
            let n = accu in
            let (t, stack) = stack in
            let result = Script_timestamp.add_delta t n in
            (run [@ocaml.tailcall]) ctxt gas gms k ks result stack
        | KAdd_timestamp_to_seconds (_, k) ->
            let t = accu in
            let (n, stack) = stack in
            let result = Script_timestamp.add_delta t n in
            (run [@ocaml.tailcall]) ctxt gas gms k ks result stack
        | KSub_timestamp_seconds (_, k) ->
            let t = accu in
            let (s, stack) = stack in
            let result = Script_timestamp.sub_delta t s in
            (run [@ocaml.tailcall]) ctxt gas gms k ks result stack
        | KDiff_timestamps (_, k) ->
            let t1 = accu in
            let (t2, stack) = stack in
            let result = Script_timestamp.diff t1 t2 in
            (run [@ocaml.tailcall]) ctxt gas gms k ks result stack
        (* string operations *)
        | KConcat_string_pair (_, k) ->
            let x = accu in
            let (y, stack) = stack in
            let s = String.concat "" [x; y] in
            (run [@ocaml.tailcall]) ctxt gas gms k ks s stack
        | KConcat_string (_, k) ->
            let ss = accu in
            (* The cost for this fold_left has been paid upfront *)
            let total_length =
              List.fold_left
                (fun acc s -> S.add acc (S.of_int (String.length s)))
                S.zero
                accu.elements
            in
            consume' ctxt gas (Interp_costs.concat_string total_length :> int)
            >>?= fun gas ->
            let s = String.concat "" ss.elements in
            (run [@ocaml.tailcall]) ctxt gas gms k ks s stack
        | KSlice_string (_, k) ->
            let offset = accu and (length, (s, stack)) = stack in
            let s_length = Z.of_int (String.length s) in
            let offset = Script_int.to_zint offset in
            let length = Script_int.to_zint length in
            if Compare.Z.(offset < s_length && Z.add offset length <= s_length)
            then
              let s = String.sub s (Z.to_int offset) (Z.to_int length) in
              (run [@ocaml.tailcall]) ctxt gas gms k ks (Some s) stack
            else (run [@ocaml.tailcall]) ctxt gas gms k ks None stack
        | KString_size (_, k) ->
            let s = accu in
            let result = Script_int.(abs (of_int (String.length s))) in
            (run [@ocaml.tailcall]) ctxt gas gms k ks result stack
        (* bytes operations *)
        | KConcat_bytes_pair (_, k) ->
            let x = accu in
            let (y, stack) = stack in
            let s = Bytes.cat x y in
            (run [@ocaml.tailcall]) ctxt gas gms k ks s stack
        | KConcat_bytes (_, k) ->
            let ss = accu in
            (* The cost for this fold_left has been paid upfront *)
            let total_length =
              List.fold_left
                (fun acc s -> S.add acc (S.of_int (Bytes.length s)))
                S.zero
                accu.elements
            in
            consume' ctxt gas (Interp_costs.concat_string total_length :> int)
            >>?= fun gas ->
            let s = Bytes.concat Bytes.empty ss.elements in
            (run [@ocaml.tailcall]) ctxt gas gms k ks s stack
        | KSlice_bytes (_, k) ->
            let offset = accu and (length, (s, stack)) = stack in
            let s_length = Z.of_int (Bytes.length s) in
            let offset = Script_int.to_zint offset in
            let length = Script_int.to_zint length in
            if Compare.Z.(offset < s_length && Z.add offset length <= s_length)
            then
              let s = Bytes.sub s (Z.to_int offset) (Z.to_int length) in
              (run [@ocaml.tailcall]) ctxt gas gms k ks (Some s) stack
            else (run [@ocaml.tailcall]) ctxt gas gms k ks None stack
        | KBytes_size (_, k) ->
            let s = accu in
            let result = Script_int.(abs (of_int (Bytes.length s))) in
            (run [@ocaml.tailcall]) ctxt gas gms k ks result stack
        (* currency operations *)
        | KAdd_tez (_, k) ->
            let x = accu in
            let (y, stack) = stack in
            Tez.(x +? y)
            >>?= fun res -> (run [@ocaml.tailcall]) ctxt gas gms k ks res stack
        | KSub_tez (_, k) ->
            let x = accu in
            let (y, stack) = stack in
            Tez.(x -? y)
            >>?= fun res -> (run [@ocaml.tailcall]) ctxt gas gms k ks res stack
        | KMul_teznat (kinfo, k) -> (
            let x = accu in
            let (y, stack) = stack in
            match Script_int.to_int64 y with
            | None ->
                get_log () >>=? fun log -> fail (Overflow (kinfo.kloc, log))
            | Some y ->
                Tez.(x *? y)
                >>?= fun res ->
                (run [@ocaml.tailcall]) ctxt gas gms k ks res stack )
        | KMul_nattez (kinfo, k) -> (
            let y = accu in
            let (x, stack) = stack in
            match Script_int.to_int64 y with
            | None ->
                get_log () >>=? fun log -> fail (Overflow (kinfo.kloc, log))
            | Some y ->
                Tez.(x *? y)
                >>?= fun res ->
                (run [@ocaml.tailcall]) ctxt gas gms k ks res stack )
        (* boolean operations *)
        | KOr (_, k) ->
            let x = accu in
            let (y, stack) = stack in
            (run [@ocaml.tailcall]) ctxt gas gms k ks (x || y) stack
        | KAnd (_, k) ->
            let x = accu in
            let (y, stack) = stack in
            (run [@ocaml.tailcall]) ctxt gas gms k ks (x && y) stack
        | KXor (_, k) ->
            let x = accu in
            let (y, stack) = stack in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              Compare.Bool.(x <> y)
              stack
        | KNot (_, k) ->
            let x = accu in
            (run [@ocaml.tailcall]) ctxt gas gms k ks (not x) stack
        (* integer operations *)
        | KIs_nat (_, k) ->
            let x = accu in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Script_int.is_nat x)
              stack
        | KAbs_int (_, k) ->
            let x = accu in
            (run [@ocaml.tailcall]) ctxt gas gms k ks (Script_int.abs x) stack
        | KInt_nat (_, k) ->
            let x = accu in
            (run [@ocaml.tailcall]) ctxt gas gms k ks (Script_int.int x) stack
        | KNeg_int (_, k) ->
            let x = accu in
            (run [@ocaml.tailcall]) ctxt gas gms k ks (Script_int.neg x) stack
        | KNeg_nat (_, k) ->
            let x = accu in
            (run [@ocaml.tailcall]) ctxt gas gms k ks (Script_int.neg x) stack
        | KAdd_intint (_, k) ->
            let x = accu and (y, stack) = stack in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Script_int.add x y)
              stack
        | KAdd_intnat (_, k) ->
            let x = accu and (y, stack) = stack in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Script_int.add x y)
              stack
        | KAdd_natint (_, k) ->
            let x = accu and (y, stack) = stack in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Script_int.add x y)
              stack
        | KAdd_natnat (_, k) ->
            let x = accu and (y, stack) = stack in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Script_int.add_n x y)
              stack
        | KSub_int (_, k) ->
            let x = accu and (y, stack) = stack in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Script_int.sub x y)
              stack
        | KMul_intint (_, k) ->
            let x = accu and (y, stack) = stack in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Script_int.mul x y)
              stack
        | KMul_intnat (_, k) ->
            let x = accu and (y, stack) = stack in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Script_int.mul x y)
              stack
        | KMul_natint (_, k) ->
            let x = accu and (y, stack) = stack in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Script_int.mul x y)
              stack
        | KMul_natnat (_, k) ->
            let x = accu and (y, stack) = stack in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Script_int.mul_n x y)
              stack
        | KEdiv_teznat (_, k) ->
            let x = accu and (y, stack) = stack in
            let x = Script_int.of_int64 (Tez.to_mutez x) in
            let result =
              match Script_int.ediv x y with
              | None ->
                  None
              | Some (q, r) -> (
                match (Script_int.to_int64 q, Script_int.to_int64 r) with
                | (Some q, Some r) -> (
                  match (Tez.of_mutez q, Tez.of_mutez r) with
                  | (Some q, Some r) ->
                      Some (q, r)
                  (* Cannot overflow *)
                  | _ ->
                      assert false )
                (* Cannot overflow *)
                | _ ->
                    assert false )
            in
            (run [@ocaml.tailcall]) ctxt gas gms k ks result stack
        | KEdiv_tez (_, k) ->
            let x = accu and (y, stack) = stack in
            let x = Script_int.abs (Script_int.of_int64 (Tez.to_mutez x)) in
            let y = Script_int.abs (Script_int.of_int64 (Tez.to_mutez y)) in
            let result =
              match Script_int.ediv_n x y with
              | None ->
                  None
              | Some (q, r) -> (
                match Script_int.to_int64 r with
                | None ->
                    assert false (* Cannot overflow *)
                | Some r -> (
                  match Tez.of_mutez r with
                  | None ->
                      assert false (* Cannot overflow *)
                  | Some r ->
                      Some (q, r) ) )
            in
            (run [@ocaml.tailcall]) ctxt gas gms k ks result stack
        | KEdiv_intint (_, k) ->
            let x = accu and (y, stack) = stack in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Script_int.ediv x y)
              stack
        | KEdiv_intnat (_, k) ->
            let x = accu and (y, stack) = stack in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Script_int.ediv x y)
              stack
        | KEdiv_natint (_, k) ->
            let x = accu and (y, stack) = stack in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Script_int.ediv x y)
              stack
        | KEdiv_natnat (_, k) ->
            let x = accu and (y, stack) = stack in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Script_int.ediv_n x y)
              stack
        | KLsl_nat (kinfo, k) -> (
            let x = accu and (y, stack) = stack in
            match Script_int.shift_left_n x y with
            | None ->
                get_log () >>=? fun log -> fail (Overflow (kinfo.kloc, log))
            | Some x ->
                (run [@ocaml.tailcall]) ctxt gas gms k ks x stack )
        | KLsr_nat (kinfo, k) -> (
            let x = accu and (y, stack) = stack in
            match Script_int.shift_right_n x y with
            | None ->
                get_log () >>=? fun log -> fail (Overflow (kinfo.kloc, log))
            | Some r ->
                (run [@ocaml.tailcall]) ctxt gas gms k ks r stack )
        | KOr_nat (_, k) ->
            let x = accu and (y, stack) = stack in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Script_int.logor x y)
              stack
        | KAnd_nat (_, k) ->
            let x = accu and (y, stack) = stack in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Script_int.logand x y)
              stack
        | KAnd_int_nat (_, k) ->
            let x = accu and (y, stack) = stack in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Script_int.logand x y)
              stack
        | KXor_nat (_, k) ->
            let x = accu and (y, stack) = stack in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Script_int.logxor x y)
              stack
        | KNot_int (_, k) ->
            let x = accu in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Script_int.lognot x)
              stack
        | KNot_nat (_, k) ->
            let x = accu in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Script_int.lognot x)
              stack
        (* control *)
        | KIf (_, bt, bf) ->
            if accu then
              (run [@ocaml.tailcall])
                ctxt
                gas
                gms
                bt
                ks
                (fst stack)
                (snd stack)
            else
              (run [@ocaml.tailcall])
                ctxt
                gas
                gms
                bf
                ks
                (fst stack)
                (snd stack)
        | KLoop (_, body, k) as self ->
            if accu then
              let ks = KCons (self, ks) in
              (run [@ocaml.tailcall])
                ctxt
                gas
                gms
                body
                ks
                (fst stack)
                (snd stack)
            else
              (run [@ocaml.tailcall]) ctxt gas gms k ks (fst stack) (snd stack)
        | KLoop_left (_, bl, br) as self -> (
          match accu with
          | L v ->
              (run [@ocaml.tailcall])
                ctxt
                gas
                gms
                bl
                (KCons (self, ks))
                v
                stack
          | R v ->
              (run [@ocaml.tailcall]) ctxt gas gms br ks v stack )
        | KDip (_, kinfo_const, b, k) ->
            let ign = accu in
            let ks = KCons (KConst (kinfo_const, ign, k), ks) in
            (run [@ocaml.tailcall]) ctxt gas gms b ks (fst stack) (snd stack)
        | KExec (_, k) ->
            let arg = accu and (code, stack) = stack in
            ( use_gas_counter_in_ctxt ctxt gas
            @@ fun ctxt -> interp logger ctxt step_constants code arg )
            >>=? fun (res, ctxt, gas) ->
            (run [@ocaml.tailcall]) ctxt gas gms k ks res stack
        | KApply (_, capture_ty, k) -> (
            let capture = accu in
            let (lam, stack) = stack in
            let (Lam (descr, expr)) = lam in
            let (Item_t (full_arg_ty, _, _)) = descr.bef in
            let ctxt = update_context gas ctxt in
            unparse_data ctxt Optimized capture_ty capture
            >>=? fun (const_expr, ctxt) ->
            unparse_ty ctxt capture_ty
            >>?= fun (ty_expr, ctxt) ->
            match full_arg_ty with
            | Pair_t ((capture_ty, _, _), (arg_ty, _, _), _) ->
                let arg_stack_ty = Item_t (arg_ty, Empty_t, None) in
                let const_descr =
                  ( {
                      loc = descr.loc;
                      bef = arg_stack_ty;
                      aft = Item_t (capture_ty, arg_stack_ty, None);
                      instr = Const capture;
                    }
                    : (_, _) descr )
                in
                let pair_descr =
                  ( {
                      loc = descr.loc;
                      bef = Item_t (capture_ty, arg_stack_ty, None);
                      aft = Item_t (full_arg_ty, Empty_t, None);
                      instr = Cons_pair;
                    }
                    : (_, _) descr )
                in
                let seq_descr =
                  ( {
                      loc = descr.loc;
                      bef = arg_stack_ty;
                      aft = Item_t (full_arg_ty, Empty_t, None);
                      instr = Seq (const_descr, pair_descr);
                    }
                    : (_, _) descr )
                in
                let full_descr =
                  ( {
                      loc = descr.loc;
                      bef = arg_stack_ty;
                      aft = descr.aft;
                      instr = Seq (seq_descr, descr);
                    }
                    : (_, _) descr )
                in
                let full_expr =
                  Micheline.Seq
                    ( 0,
                      [ Prim (0, I_PUSH, [ty_expr; const_expr], []);
                        Prim (0, I_PAIR, [], []);
                        expr ] )
                in
                let lam' = Lam (full_descr, full_expr) in
                let gas = (Gas.gas_counter ctxt :> int) in
                (run [@ocaml.tailcall])
                  (OutDatedContext ctxt)
                  gas
                  gms
                  k
                  ks
                  lam'
                  stack
            | _ ->
                assert false )
        | KLambda (_, lam, k) ->
            (run [@ocaml.tailcall]) ctxt gas gms k ks lam (accu, stack)
        | KFailwith (_, kloc, tv, _) ->
            let v = accu in
            let ctxt = update_context gas ctxt in
            trace Cannot_serialize_failure (unparse_data ctxt Optimized tv v)
            >>=? fun (v, _ctxt) ->
            let v = Micheline.strip_locations v in
            get_log () >>=? fun log -> fail (Reject (kloc, v, log))
        | KNop (_, k) ->
            (run [@ocaml.tailcall]) ctxt gas gms k ks accu stack
        (* comparison *)
        | KCompare (_, ty, k) ->
            let a = accu in
            let (b, stack) = stack in
            let r =
              Script_int.of_int
              @@ Script_ir_translator.compare_comparable ty a b
            in
            (run [@ocaml.tailcall]) ctxt gas gms k ks r stack
        (* comparators *)
        | KEq (_, k) ->
            let cmpres = accu in
            let cmpres = Script_int.compare cmpres Script_int.zero in
            let cmpres = Compare.Int.(cmpres = 0) in
            (run [@ocaml.tailcall]) ctxt gas gms k ks cmpres stack
        | KNeq (_, k) ->
            let cmpres = accu in
            let cmpres = Script_int.compare cmpres Script_int.zero in
            let cmpres = Compare.Int.(cmpres <> 0) in
            (run [@ocaml.tailcall]) ctxt gas gms k ks cmpres stack
        | KLt (_, k) ->
            let cmpres = accu in
            let cmpres = Script_int.compare cmpres Script_int.zero in
            let cmpres = Compare.Int.(cmpres < 0) in
            (run [@ocaml.tailcall]) ctxt gas gms k ks cmpres stack
        | KLe (_, k) ->
            let cmpres = accu in
            let cmpres = Script_int.compare cmpres Script_int.zero in
            let cmpres = Compare.Int.(cmpres <= 0) in
            (run [@ocaml.tailcall]) ctxt gas gms k ks cmpres stack
        | KGt (_, k) ->
            let cmpres = accu in
            let cmpres = Script_int.compare cmpres Script_int.zero in
            let cmpres = Compare.Int.(cmpres > 0) in
            (run [@ocaml.tailcall]) ctxt gas gms k ks cmpres stack
        | KGe (_, k) ->
            let cmpres = accu in
            let cmpres = Script_int.compare cmpres Script_int.zero in
            let cmpres = Compare.Int.(cmpres >= 0) in
            (run [@ocaml.tailcall]) ctxt gas gms k ks cmpres stack
        (* packing *)
        | KPack (_, ty, k) ->
            let value = accu in
            ( use_gas_counter_in_ctxt ctxt gas
            @@ fun ctxt -> Script_ir_translator.pack_data ctxt ty value )
            >>=? fun (bytes, ctxt, gas) ->
            (run [@ocaml.tailcall]) ctxt gas gms k ks bytes stack
        | KUnpack (_, ty, k) ->
            let bytes = accu in
            ( use_gas_counter_in_ctxt ctxt gas
            @@ fun ctxt -> unpack ctxt ~ty ~bytes )
            >>=? fun (opt, ctxt, gas) ->
            (run [@ocaml.tailcall]) ctxt gas gms k ks opt stack
        | KAddress (_, k) ->
            let (_, address) = accu in
            (run [@ocaml.tailcall]) ctxt gas gms k ks address stack
        | KContract (kinfo, t, entrypoint, k) -> (
            let contract = accu in
            match (contract, entrypoint) with
            | ((contract, "default"), entrypoint)
            | ((contract, entrypoint), "default") ->
                let ctxt = update_context gas ctxt in
                Script_ir_translator.parse_contract_for_script
                  ~legacy:false
                  ctxt
                  kinfo.kloc
                  t
                  contract
                  ~entrypoint
                >>=? fun (ctxt, maybe_contract) ->
                let gas = (Gas.gas_counter ctxt :> int) in
                let ctxt = OutDatedContext ctxt in
                (run [@ocaml.tailcall]) ctxt gas gms k ks maybe_contract stack
            | _ ->
                (run [@ocaml.tailcall]) ctxt gas gms k ks None stack )
        | KTransfer_tokens (_, k) ->
            let p = accu in
            let (amount, ((tp, (destination, entrypoint)), stack)) = stack in
            let ctxt = update_context gas ctxt in
            collect_lazy_storage ctxt tp p
            >>?= fun (to_duplicate, ctxt) ->
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
            unparse_data ctxt Optimized tp p
            >>=? fun (p, ctxt) ->
            Gas.consume ctxt (Script.strip_locations_cost p)
            >>?= fun ctxt ->
            let operation =
              Transaction
                {
                  amount;
                  destination;
                  entrypoint;
                  parameters = Script.lazy_expr (Micheline.strip_locations p);
                }
            in
            fresh_internal_nonce ctxt
            >>?= fun (ctxt, nonce) ->
            let iop = {source = step_constants.self; operation; nonce} in
            let accu = (Internal_operation iop, lazy_storage_diff) in
            let gas = (Gas.gas_counter ctxt :> int) in
            let ctxt = OutDatedContext ctxt in
            (run [@ocaml.tailcall]) ctxt gas gms k ks accu stack
        | KImplicit_account (_, k) ->
            let key = accu in
            let contract = Contract.implicit_contract key in
            let res = (Unit_t None, (contract, "default")) in
            (run [@ocaml.tailcall]) ctxt gas gms k ks res stack
        | KCreate_contract
            (_, storage_type, param_type, Lam (_, code), root_name, k) ->
            (* Removed the instruction's arguments manager, spendable and delegatable *)
            let delegate = accu in
            let (credit, (init, stack)) = stack in
            let ctxt = update_context gas ctxt in
            unparse_ty ctxt param_type
            >>?= fun (unparsed_param_type, ctxt) ->
            let unparsed_param_type =
              Script_ir_translator.add_field_annot
                root_name
                None
                unparsed_param_type
            in
            unparse_ty ctxt storage_type
            >>?= fun (unparsed_storage_type, ctxt) ->
            let code =
              Micheline.strip_locations
                (Seq
                   ( 0,
                     [ Prim (0, K_parameter, [unparsed_param_type], []);
                       Prim (0, K_storage, [unparsed_storage_type], []);
                       Prim (0, K_code, [code], []) ] ))
            in
            collect_lazy_storage ctxt storage_type init
            >>?= fun (to_duplicate, ctxt) ->
            let to_update = no_lazy_storage_id in
            extract_lazy_storage_diff
              ctxt
              Optimized
              storage_type
              init
              ~to_duplicate
              ~to_update
              ~temporary:true
            >>=? fun (init, lazy_storage_diff, ctxt) ->
            unparse_data ctxt Optimized storage_type init
            >>=? fun (storage, ctxt) ->
            Gas.consume ctxt (Script.strip_locations_cost storage)
            >>?= fun ctxt ->
            let storage = Micheline.strip_locations storage in
            Contract.fresh_contract_from_current_nonce ctxt
            >>?= fun (ctxt, contract) ->
            let operation =
              Origination
                {
                  credit;
                  delegate;
                  preorigination = Some contract;
                  script =
                    {
                      code = Script.lazy_expr code;
                      storage = Script.lazy_expr storage;
                    };
                }
            in
            fresh_internal_nonce ctxt
            >>?= fun (ctxt, nonce) ->
            let res =
              ( Internal_operation
                  {source = step_constants.self; operation; nonce},
                lazy_storage_diff )
            in
            let gas = (Gas.gas_counter ctxt :> int) in
            let ctxt = OutDatedContext ctxt in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              res
              ((contract, "default"), stack)
        | KSet_delegate (_, k) ->
            let delegate = accu in
            let operation = Delegation delegate in
            let ctxt = update_context gas ctxt in
            fresh_internal_nonce ctxt
            >>?= fun (ctxt, nonce) ->
            let res =
              ( Internal_operation
                  {source = step_constants.self; operation; nonce},
                None )
            in
            let gas = (Gas.gas_counter ctxt :> int) in
            let ctxt = OutDatedContext ctxt in
            (run [@ocaml.tailcall]) ctxt gas gms k ks res stack
        | KBalance (_, k) ->
            let ctxt = update_context gas ctxt in
            Contract.get_balance_carbonated ctxt step_constants.self
            >>=? fun (ctxt, balance) ->
            let gas = (Gas.gas_counter ctxt :> int) in
            let ctxt = OutDatedContext ctxt in
            (run [@ocaml.tailcall]) ctxt gas gms k ks balance (accu, stack)
        | KLevel (_, k) ->
            let level =
              (Level.current (outdated_context ctxt)).level
              |> Raw_level.to_int32 |> Script_int.of_int32 |> Script_int.abs
            in
            (run [@ocaml.tailcall]) ctxt gas gms k ks level (accu, stack)
        | KNow (_, k) ->
            let now = Script_timestamp.now (outdated_context ctxt) in
            (run [@ocaml.tailcall]) ctxt gas gms k ks now (accu, stack)
        | KCheck_signature (_, k) ->
            let key = accu and (signature, (message, stack)) = stack in
            let res = Signature.check key signature message in
            (run [@ocaml.tailcall]) ctxt gas gms k ks res stack
        | KHash_key (_, k) ->
            let key = accu in
            let res = Signature.Public_key.hash key in
            (run [@ocaml.tailcall]) ctxt gas gms k ks res stack
        | KBlake2b (_, k) ->
            let bytes = accu in
            let hash = Raw_hashes.blake2b bytes in
            (run [@ocaml.tailcall]) ctxt gas gms k ks hash stack
        | KSha256 (_, k) ->
            let bytes = accu in
            let hash = Raw_hashes.sha256 bytes in
            (run [@ocaml.tailcall]) ctxt gas gms k ks hash stack
        | KSha512 (_, k) ->
            let bytes = accu in
            let hash = Raw_hashes.sha512 bytes in
            (run [@ocaml.tailcall]) ctxt gas gms k ks hash stack
        | KSource (_, k) ->
            let res = (step_constants.payer, "default") in
            (run [@ocaml.tailcall]) ctxt gas gms k ks res (accu, stack)
        | KSender (_, k) ->
            let res = (step_constants.source, "default") in
            (run [@ocaml.tailcall]) ctxt gas gms k ks res (accu, stack)
        | KSelf (_, ty, entrypoint, k) ->
            let res = (ty, (step_constants.self, entrypoint)) in
            (run [@ocaml.tailcall]) ctxt gas gms k ks res (accu, stack)
        | KSelf_address (_, k) ->
            let res = (step_constants.self, "default") in
            (run [@ocaml.tailcall]) ctxt gas gms k ks res (accu, stack)
        | KAmount (_, k) ->
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              step_constants.amount
              (accu, stack)
        | KDig (_, _n, n', k) ->
            let (stack, accu) =
              interp_stack_prefix_preserving_operation
                (fun (v, stack) -> (stack, v))
                n'
                (accu, stack)
            in
            (run [@ocaml.tailcall]) ctxt gas gms k ks accu stack
        | KDug (_, _n, n', k) ->
            let v = accu in
            let (stack, ()) =
              interp_stack_prefix_preserving_operation
                (fun stack -> ((v, stack), ()))
                n'
                stack
            in
            (run [@ocaml.tailcall]) ctxt gas gms k ks (fst stack) (snd stack)
        | KDipn (_, _n, n', b, k) -> (
            (*

           The following function pops n elements from the stack
           and push their reintroduction in the continuations stack.

        *)
            let rec ktransfer :
                type w u v s.
                (w, v, s, u) kstack_prefix_preservation_witness ->
                s ->
                (u, b, t) exkinstr ->
                w * (v, b, t) exkinstr =
             fun w stack k ->
              match (w, stack) with
              | (KPrefix (kinfo, _, IsLifted lu', w), (x, stack)) -> (
                match k with
                | ExKInstr (Eq, k) -> (
                  match inverse_lift lu' with
                  | ExLiftInverse Refl ->
                      ktransfer w stack (ExKInstr (Eq, KConst (kinfo, x, k))) )
                )
              | (KRest (_, _), _) ->
                  (stack, k)
            in
            match ktransfer n' (accu, stack) (ExKInstr (Eq, k)) with
            | (stack, ExKInstr (Eq, restore_prefix)) ->
                let ks = KCons (restore_prefix, ks) in
                (run [@ocaml.tailcall])
                  ctxt
                  gas
                  gms
                  b
                  ks
                  (fst stack)
                  (snd stack) )
        | KDropn (_, _n, n', k) ->
            let (_, stack) =
              interp_stack_prefix_preserving_operation
                (fun stack -> (stack, stack))
                n'
                (accu, stack)
            in
            (run [@ocaml.tailcall]) ctxt gas gms k ks (fst stack) (snd stack)
        | KSapling_empty_state (_, memo_size, k) ->
            let state = Sapling.empty_state ~memo_size () in
            (run [@ocaml.tailcall]) ctxt gas gms k ks state (accu, stack)
        | KSapling_verify_update (_, k) -> (
            let transaction = accu in
            let (state, stack) = stack in
            let address = Contract.to_b58check step_constants.self in
            let chain_id = Chain_id.to_b58check step_constants.chain_id in
            let anti_replay = address ^ chain_id in
            let ctxt = update_context gas ctxt in
            Sapling.verify_update ctxt state transaction anti_replay
            >>=? fun (ctxt, balance_state_opt) ->
            let gas = (Gas.gas_counter ctxt :> int) in
            let ctxt = OutDatedContext ctxt in
            match balance_state_opt with
            | Some (balance, state) ->
                let state = Some (Script_int.of_int64 balance, state) in
                (run [@ocaml.tailcall]) ctxt gas gms k ks state stack
            | None ->
                (run [@ocaml.tailcall]) ctxt gas gms k ks None stack )
        | KChainId (_, k) ->
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              step_constants.chain_id
              (accu, stack)
        | KNever _ -> (
          match accu with _ -> . )
        | KVoting_power (_, k) ->
            let key_hash = accu in
            let ctxt = update_context gas ctxt in
            Vote.get_voting_power ctxt key_hash
            >>=? fun (ctxt, rolls) ->
            let power = Script_int.(abs (of_int32 rolls)) in
            let gas = (Gas.gas_counter ctxt :> int) in
            let ctxt = OutDatedContext ctxt in
            (run [@ocaml.tailcall]) ctxt gas gms k ks power stack
        | KTotal_voting_power (_, k) ->
            let ctxt = update_context gas ctxt in
            Vote.get_total_voting_power ctxt
            >>=? fun (ctxt, rolls) ->
            let power = Script_int.(abs (of_int32 rolls)) in
            let gas = (Gas.gas_counter ctxt :> int) in
            let ctxt = OutDatedContext ctxt in
            (run [@ocaml.tailcall]) ctxt gas gms k ks power (accu, stack)
        | KKeccak (_, k) ->
            let bytes = accu in
            let hash = Raw_hashes.keccak256 bytes in
            (run [@ocaml.tailcall]) ctxt gas gms k ks hash stack
        | KSha3 (_, k) ->
            let bytes = accu in
            let hash = Raw_hashes.sha3_256 bytes in
            (run [@ocaml.tailcall]) ctxt gas gms k ks hash stack
        | KAdd_bls12_381_g1 (_, k) ->
            let x = accu and (y, stack) = stack in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Bls12_381.G1.add x y)
              stack
        | KAdd_bls12_381_g2 (_, k) ->
            let x = accu and (y, stack) = stack in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Bls12_381.G2.add x y)
              stack
        | KAdd_bls12_381_fr (_, k) ->
            let x = accu and (y, stack) = stack in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Bls12_381.Fr.add x y)
              stack
        | KMul_bls12_381_g1 (_, k) ->
            let x = accu and (y, stack) = stack in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Bls12_381.G1.mul x y)
              stack
        | KMul_bls12_381_g2 (_, k) ->
            let x = accu and (y, stack) = stack in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Bls12_381.G2.mul x y)
              stack
        | KMul_bls12_381_fr (_, k) ->
            let x = accu and (y, stack) = stack in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Bls12_381.Fr.mul x y)
              stack
        | KMul_bls12_381_fr_z (_, k) ->
            let x = accu and (y, stack) = stack in
            let x = Bls12_381.Fr.of_z (Script_int.to_zint x) in
            let res = Bls12_381.Fr.mul x y in
            (run [@ocaml.tailcall]) ctxt gas gms k ks res stack
        | KMul_bls12_381_z_fr (_, k) ->
            let y = accu and (x, stack) = stack in
            let x = Bls12_381.Fr.of_z (Script_int.to_zint x) in
            let res = Bls12_381.Fr.mul x y in
            (run [@ocaml.tailcall]) ctxt gas gms k ks res stack
        | KInt_bls12_381_fr (_, k) ->
            let x = accu in
            let res = Script_int.of_zint (Bls12_381.Fr.to_z x) in
            (run [@ocaml.tailcall]) ctxt gas gms k ks res stack
        | KNeg_bls12_381_g1 (_, k) ->
            let x = accu in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Bls12_381.G1.negate x)
              stack
        | KNeg_bls12_381_g2 (_, k) ->
            let x = accu in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Bls12_381.G2.negate x)
              stack
        | KNeg_bls12_381_fr (_, k) ->
            let x = accu in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (Bls12_381.Fr.negate x)
              stack
        | KPairing_check_bls12_381 (_, k) ->
            let pairs = accu in
            let check =
              match pairs.elements with
              | [] ->
                  true
              | pairs ->
                  Bls12_381.(
                    miller_loop pairs |> final_exponentiation_opt
                    |> Option.map Gt.(eq one))
                  |> Option.value ~default:false
            in
            (run [@ocaml.tailcall]) ctxt gas gms k ks check stack
        | KComb (_, _, witness, k) ->
            let rec aux :
                type before after.
                (before, after) comb_gadt_witness -> before -> after =
             fun witness stack ->
              match (witness, stack) with
              | (Comb_one, stack) ->
                  stack
              | (Comb_succ witness', (a, tl)) ->
                  let (b, tl') = aux witness' tl in
                  ((a, b), tl')
            in
            let stack = aux witness (accu, stack) in
            (run [@ocaml.tailcall]) ctxt gas gms k ks (fst stack) (snd stack)
        | KUncomb (_, _, witness, k) ->
            let rec aux :
                type before after.
                (before, after) uncomb_gadt_witness -> before -> after =
             fun witness stack ->
              match (witness, stack) with
              | (Uncomb_one, stack) ->
                  stack
              | (Uncomb_succ witness', ((a, b), tl)) ->
                  (a, aux witness' (b, tl))
            in
            let stack = aux witness (accu, stack) in
            (run [@ocaml.tailcall]) ctxt gas gms k ks (fst stack) (snd stack)
        | KComb_get (_, _, witness, k) ->
            let comb = accu in
            let rec aux :
                type before after.
                (before, after) comb_get_gadt_witness -> before -> after =
             fun witness comb ->
              match (witness, comb) with
              | (Comb_get_zero, v) ->
                  v
              | (Comb_get_one, (a, _)) ->
                  a
              | (Comb_get_plus_two witness', (_, b)) ->
                  aux witness' b
            in
            let accu = aux witness comb in
            (run [@ocaml.tailcall]) ctxt gas gms k ks accu stack
        | KComb_set (_, _, witness, k) ->
            let value = accu and (comb, stack) = stack in
            let rec aux :
                type value before after.
                (value, before, after) comb_set_gadt_witness ->
                value ->
                before ->
                after =
             fun witness value item ->
              match (witness, item) with
              | (Comb_set_zero, _) ->
                  value
              | (Comb_set_one, (_hd, tl)) ->
                  (value, tl)
              | (Comb_set_plus_two witness', (hd, tl)) ->
                  (hd, aux witness' value tl)
            in
            let accu = aux witness value comb in
            (run [@ocaml.tailcall]) ctxt gas gms k ks accu stack
        | KDup_n (_, _, witness, k) ->
            let rec aux :
                type before after.
                (before, after) dup_n_gadt_witness -> before -> after =
             fun witness stack ->
              match (witness, stack) with
              | (Dup_n_zero, (a, _)) ->
                  a
              | (Dup_n_succ witness', (_, tl)) ->
                  aux witness' tl
            in
            let stack = (accu, stack) in
            let accu = aux witness stack in
            (run [@ocaml.tailcall]) ctxt gas gms k ks accu stack
        (* Tickets *)
        | KTicket (_, k) ->
            let contents = accu and (amount, stack) = stack in
            let ticketer = (step_constants.self, "default") in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              {ticketer; contents; amount}
              stack
        | KRead_ticket (_, k) ->
            let {ticketer; contents; amount} = accu in
            let stack = (accu, stack) in
            (run [@ocaml.tailcall])
              ctxt
              gas
              gms
              k
              ks
              (ticketer, (contents, amount))
              stack
        | KSplit_ticket (_, k) ->
            let ticket = accu and ((amount_a, amount_b), stack) = stack in
            let result =
              if
                Compare.Int.(
                  Script_int.(compare (add_n amount_a amount_b) ticket.amount)
                  = 0)
              then
                Some
                  ( {ticket with amount = amount_a},
                    {ticket with amount = amount_b} )
              else None
            in
            (run [@ocaml.tailcall]) ctxt gas gms k ks result stack
        | KJoin_tickets (_, contents_ty, k) ->
            let (ticket_a, ticket_b) = accu in
            let result =
              if
                Compare.Int.(
                  compare_address ticket_a.ticketer ticket_b.ticketer = 0
                  && compare_comparable
                       contents_ty
                       ticket_a.contents
                       ticket_b.contents
                     = 0)
              then
                Some
                  {
                    ticketer = ticket_a.ticketer;
                    contents = ticket_a.contents;
                    amount = Script_int.add_n ticket_a.amount ticket_b.amount;
                  }
              else None
            in
            (run [@ocaml.tailcall]) ctxt gas monitor_gas k ks result stack )
  in
  let (accu, stack) = lift kli stack in
  let gas = Gas.gas_counter ctxt in
  step (OutDatedContext ctxt) (gas :> int) true kinstr KNil accu stack
  >>=? fun (accu, stack, ctxt, gas) ->
  return (unlift klo (accu, stack), update_context gas ctxt)

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
  let (KDescr d) = translate descr in
  let kdescr = KDescr {d with kinstr = pay_gas_in_advance d.kinstr} in
  step_bounded logger ctxt step_constants kdescr stack

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
  ( match logger with
  | None ->
      ()
  | Some logger ->
      let module Log = (val logger) in
      Log.log_interp ctxt code.loc code.bef stack ) ;
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
