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

let cost_of_instr : type a s r f. (a, s, r, f) kinstr -> a -> s -> Gas.cost =
 fun instr accu stack ->
  let stack = (accu, stack) in
  match (instr, stack) with
  | (KHalt _, _) ->
      (* FIXME *)
      Gas.free
  | (KDrop _, _) ->
      Interp_costs.drop
  | (KDup _, _) ->
      Interp_costs.dup
  | (KSwap _, _) ->
      Interp_costs.swap
  | (KConst _, _) ->
      Interp_costs.push
  | (KCons_some _, _) ->
      Interp_costs.cons_some
  | (KCons_none _, _) ->
      Interp_costs.cons_none
  | (KIf_none _, _) ->
      Interp_costs.if_none
  | (KCons_pair _, _) ->
      Interp_costs.cons_pair
  | (KUnpair _, _) ->
      Interp_costs.unpair
  | (KCar _, _) ->
      Interp_costs.car
  | (KCdr _, _) ->
      Interp_costs.cdr
  | (KCons_left _, _) ->
      Interp_costs.cons_left
  | (KCons_right _, _) ->
      Interp_costs.cons_right
  | (KIf_left _, _) ->
      Interp_costs.if_left
  | (KCons_list _, _) ->
      Interp_costs.cons_list
  | (KNil _, _) ->
      Interp_costs.nil
  | (KIf_cons _, _) ->
      Interp_costs.if_cons
  | (KList_map _, (list, _)) ->
      Interp_costs.list_map list
  | (KList_mapping _, _) ->
      (* FIXME *)
      Gas.free
  | (KList_mapped _, _) ->
      (* FIXME *)
      Gas.free
  | (KList_size _, _) ->
      Interp_costs.list_size
  | (KList_iter _, (l, _)) ->
      Interp_costs.list_iter l
  | (KIter _, _) ->
      (* FIXME *)
      Gas.free
  | (KEmpty_set _, _) ->
      Interp_costs.empty_set
  | (KSet_iter _, (set, _)) ->
      Interp_costs.set_iter set
  | (KSet_mem _, (v, (set, _))) ->
      Interp_costs.set_mem v set
  | (KSet_update _, (v, (_, (set, _)))) ->
      Interp_costs.set_update v set
  | (KSet_size _, _) ->
      Interp_costs.set_size
  | (KEmpty_map _, _) ->
      Interp_costs.empty_map
  | (KMap_map _, (map, _)) ->
      Interp_costs.map_map map
  | (KMap_mapping _, _) ->
      (* FIXME *)
      Gas.free
  | (KMap_mapped _, _) ->
      (* FIXME *)
      Gas.free
  | (KMap_iter _, (map, _)) ->
      Interp_costs.map_iter map
  | (KMap_mem _, (v, (map, _rest))) ->
      Interp_costs.map_mem v map
  | (KMap_get _, (v, (map, _rest))) ->
      Interp_costs.map_get v map
  | (KMap_update _, (k, (_, (map, _)))) ->
      Interp_costs.map_update k map
  | (KMap_get_and_update _, (k, (_, (map, _)))) ->
      Interp_costs.map_get_and_update k map
  | (KMap_size _, _) ->
      Interp_costs.map_size
  | (KEmpty_big_map _, _) ->
      Interp_costs.empty_map
  | (KBig_map_mem _, (key, (map, _))) ->
      Interp_costs.map_mem key map.diff
  | (KBig_map_get _, (key, (map, _))) ->
      Interp_costs.map_get key map.diff
  | (KBig_map_update _, (key, (_, (map, _)))) ->
      Interp_costs.map_update key map.diff
  | (KBig_map_get_and_update _, (key, (_, (map, _)))) ->
      Interp_costs.map_get_and_update key map.diff
  | (KAdd_seconds_to_timestamp _, (n, (t, _))) ->
      Interp_costs.add_seconds_timestamp n t
  | (KAdd_timestamp_to_seconds _, (t, (n, _))) ->
      Interp_costs.add_seconds_timestamp n t
  | (KSub_timestamp_seconds _, (t, (n, _))) ->
      Interp_costs.sub_seconds_timestamp n t
  | (KDiff_timestamps _, (t1, (t2, _))) ->
      Interp_costs.diff_timestamps t1 t2
  | (KConcat_string_pair _, (x, (y, _))) ->
      Interp_costs.concat_string_pair x y
  | (KConcat_string _, (ss, _)) ->
      Interp_costs.concat_string_precheck ss
  | (KSlice_string _, (_offset, (_length, (s, _)))) ->
      Interp_costs.slice_string s
  | (KString_size _, _) ->
      Interp_costs.string_size
  | (KConcat_bytes_pair _, (x, (y, _))) ->
      Interp_costs.concat_bytes_pair x y
  | (KConcat_bytes _, (ss, _)) ->
      Interp_costs.concat_string_precheck ss
  | (KSlice_bytes _, (_offset, (_length, (s, _)))) ->
      Interp_costs.slice_bytes s
  | (KBytes_size _, _) ->
      Interp_costs.bytes_size
  | (KAdd_tez _, _) ->
      Interp_costs.add_tez
  | (KSub_tez _, _) ->
      Interp_costs.sub_tez
  | (KMul_teznat _, (_, (n, _))) ->
      Interp_costs.mul_teznat n
  | (KMul_nattez _, (n, (_, _))) ->
      Interp_costs.mul_teznat n
  | (KOr _, _) ->
      Interp_costs.bool_or
  | (KAnd _, _) ->
      Interp_costs.bool_and
  | (KXor _, _) ->
      Interp_costs.bool_xor
  | (KNot _, _) ->
      Interp_costs.bool_not
  | (KIs_nat _, _) ->
      Interp_costs.is_nat
  | (KAbs_int _, (x, _)) ->
      Interp_costs.abs_int x
  | (KInt_nat _, _) ->
      Interp_costs.int_nat
  | (KNeg_int _, (x, _)) ->
      Interp_costs.neg_int x
  | (KNeg_nat _, (x, _)) ->
      Interp_costs.neg_nat x
  | (KAdd_intint _, (x, (y, _))) ->
      Interp_costs.add_bigint x y
  | (KAdd_intnat _, (x, (y, _))) ->
      Interp_costs.add_bigint x y
  | (KAdd_natint _, (x, (y, _))) ->
      Interp_costs.add_bigint x y
  | (KAdd_natnat _, (x, (y, _))) ->
      Interp_costs.add_bigint x y
  | (KSub_int _, (x, (y, _))) ->
      Interp_costs.sub_bigint x y
  | (KMul_intint _, (x, (y, _))) ->
      Interp_costs.mul_bigint x y
  | (KMul_intnat _, (x, (y, _))) ->
      Interp_costs.mul_bigint x y
  | (KMul_natint _, (x, (y, _))) ->
      Interp_costs.mul_bigint x y
  | (KMul_natnat _, (x, (y, _))) ->
      Interp_costs.mul_bigint x y
  | (KEdiv_teznat _, (x, (y, _))) ->
      Interp_costs.ediv_teznat x y
  | (KEdiv_tez _, _) ->
      Interp_costs.ediv_tez
  | (KEdiv_intint _, (x, (y, _))) ->
      Interp_costs.ediv_bigint x y
  | (KEdiv_intnat _, (x, (y, _))) ->
      Interp_costs.ediv_bigint x y
  | (KEdiv_natint _, (x, (y, _))) ->
      Interp_costs.ediv_bigint x y
  | (KEdiv_natnat _, (x, (y, _))) ->
      Interp_costs.ediv_bigint x y
  | (KLsl_nat _, (x, _)) ->
      Interp_costs.lsl_nat x
  | (KLsr_nat _, (x, _)) ->
      Interp_costs.lsr_nat x
  | (KOr_nat _, (x, (y, _))) ->
      Interp_costs.or_nat x y
  | (KAnd_nat _, (x, (y, _))) ->
      Interp_costs.and_nat x y
  | (KAnd_int_nat _, (x, (y, _))) ->
      Interp_costs.and_nat x y
  | (KXor_nat _, (x, (y, _))) ->
      Interp_costs.xor_nat x y
  | (KNot_int _, (x, _)) ->
      Interp_costs.not_nat x
  | (KNot_nat _, (x, _)) ->
      Interp_costs.not_nat x
  | (KIf _, _) ->
      Interp_costs.if_
  | (KLoop _, _) ->
      Interp_costs.loop
  | (KLoop_left _, _) ->
      Interp_costs.loop_left
  | (KDip _, _) ->
      Interp_costs.dip
  | (KExec _, _) ->
      Interp_costs.exec
  | (KApply _, _) ->
      Interp_costs.apply
  | (KLambda _, _) ->
      Interp_costs.push
  | (KFailwith _, _) ->
      Gas.free
  | (KNop _, _) ->
      Interp_costs.nop
  | (KCompare (_, ty, _), (a, (b, _))) ->
      Interp_costs.compare ty a b
  | (KEq _, _) ->
      Interp_costs.neq
  | (KNeq _, _) ->
      Interp_costs.neq
  | (KLt _, _) ->
      Interp_costs.neq
  | (KLe _, _) ->
      Interp_costs.neq
  | (KGt _, _) ->
      Interp_costs.neq
  | (KGe _, _) ->
      Interp_costs.neq
  | (KPack _, _) ->
      Gas.free
  | (KUnpack _, _) ->
      Gas.free
  | (KAddress _, _) ->
      Interp_costs.address
  | (KContract _, _) ->
      Interp_costs.contract
  | (KTransfer_tokens _, _) ->
      Interp_costs.transfer_tokens
  | (KImplicit_account _, _) ->
      Interp_costs.implicit_account
  | (KSet_delegate _, _) ->
      Interp_costs.set_delegate
  | (KBalance _, _) ->
      Interp_costs.balance
  | (KLevel _, _) ->
      Interp_costs.level
  | (KNow _, _) ->
      Interp_costs.now
  | (KCheck_signature _, (key, (_, (message, _)))) ->
      Interp_costs.check_signature key message
  | (KHash_key _, (pk, _)) ->
      Interp_costs.hash_key pk
  | (KBlake2b _, (bytes, _)) ->
      Interp_costs.blake2b bytes
  | (KSha256 _, (bytes, _)) ->
      Interp_costs.sha256 bytes
  | (KSha512 _, (bytes, _)) ->
      Interp_costs.sha512 bytes
  | (KSource _, _) ->
      Interp_costs.source
  | (KSender _, _) ->
      Interp_costs.source
  | (KSelf _, _) ->
      Interp_costs.self
  | (KSelf_address _, _) ->
      Interp_costs.self
  | (KAmount _, _) ->
      Interp_costs.amount
  | (KDig (_, n, _, _), _) ->
      Interp_costs.dign n
  | (KDug (_, n, _, _), _) ->
      Interp_costs.dugn n
  | (KDipn (_, n, _, _, _), _) ->
      Interp_costs.dipn n
  | (KDropn (_, n, _, _), _) ->
      Interp_costs.dropn n
  | (KChainId _, _) ->
      Interp_costs.chain_id
  | (KCreate_contract _, _) ->
      Interp_costs.create_contract
  | (KNever _, (_, _)) ->
      .
  | (KVoting_power _, _) ->
      Interp_costs.voting_power
  | (KTotal_voting_power _, _) ->
      Interp_costs.total_voting_power
  | (KKeccak _, (bytes, _)) ->
      Interp_costs.keccak bytes
  | (KSha3 _, (bytes, _)) ->
      Interp_costs.sha3 bytes
  | (KAdd_bls12_381_g1 _, _) ->
      Interp_costs.add_bls12_381_g1
  | (KAdd_bls12_381_g2 _, _) ->
      Interp_costs.add_bls12_381_g2
  | (KAdd_bls12_381_fr _, _) ->
      Interp_costs.add_bls12_381_fr
  | (KMul_bls12_381_g1 _, _) ->
      Interp_costs.mul_bls12_381_g1
  | (KMul_bls12_381_g2 _, _) ->
      Interp_costs.mul_bls12_381_g2
  | (KMul_bls12_381_fr _, _) ->
      Interp_costs.mul_bls12_381_fr
  | (KMul_bls12_381_fr_z _, _) ->
      Interp_costs.mul_bls12_381_fr_z
  | (KMul_bls12_381_z_fr _, _) ->
      Interp_costs.mul_bls12_381_fr_z
  | (KInt_bls12_381_fr _, _) ->
      Interp_costs.int_bls12_381_fr
  | (KNeg_bls12_381_g1 _, _) ->
      Interp_costs.neg_bls12_381_g1
  | (KNeg_bls12_381_g2 _, _) ->
      Interp_costs.neg_bls12_381_g2
  | (KNeg_bls12_381_fr _, _) ->
      Interp_costs.neg_bls12_381_fr
  | (KPairing_check_bls12_381 _, (pairs, _)) ->
      Interp_costs.pairing_check_bls12_381 pairs
  | (KComb (_, n, _, _), _) ->
      Interp_costs.comb n
  | (KUncomb (_, n, _, _), _) ->
      Interp_costs.uncomb n
  | (KComb_get (_, n, _, _), _) ->
      Interp_costs.comb_get n
  | (KComb_set (_, n, _, _), _) ->
      Interp_costs.comb_set n
  | (KDup_n (_, n, _, _), _) ->
      Interp_costs.dupn n
  | (KSapling_empty_state _, _) ->
      Interp_costs.sapling_empty_state
  | (KSapling_verify_update _, (tx, _)) ->
      let inputs = List.length tx.inputs in
      let outputs = List.length tx.outputs in
      Interp_costs.sapling_verify_update ~inputs ~outputs
  | (KTicket _, _) ->
      Interp_costs.ticket
  | (KRead_ticket _, _) ->
      Interp_costs.read_ticket
  | (KSplit_ticket _, (ticket, ((amount_a, amount_b), _))) ->
      Interp_costs.split_ticket ticket.amount amount_a amount_b
  | (KJoin_tickets (_, ty, _), ((ticket_a, ticket_b), _)) ->
      Interp_costs.join_tickets ty ticket_a ticket_b

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
      ('a, 's, 'b, 't) kinstr * ('b, 't, 'r, 'f) konts
      -> ('a, 's, 'r, 'f) konts

type (_, _, _) exkinstr =
  | ExKInstr :
      ('t, 'x * 'z) eq * ('x, 'z, 'b, 'u) kinstr
      -> ('t, 'b, 'u) exkinstr

let[@inline] option_iter opt what =
  match opt with None -> () | Some l -> what l

let[@inline] lwt_option_iter opt what =
  match opt with None -> Lwt.return (Ok None) | Some l -> what l

let rec step_bounded :
    type bef aft.
    logger option ->
    context ->
    step_constants ->
    (bef, aft) kdescr ->
    bef ->
    (aft * context) tzresult Lwt.t =
 fun logger ctxt step_constants descr stack ->
  let[@inline] log_entry ctxt k accu stack =
    option_iter logger (fun logger ->
        let module Log = (val logger) in
        let kinfo = kinfo_of_kinstr k in
        Log.log_entry ctxt kinfo.kloc kinfo.kstack_ty (accu, stack))
  in
  let[@inline] log_exit ctxt kprev k accu stack =
    option_iter logger (fun logger ->
        let module Log = (val logger) in
        let kinfo_prev = kinfo_of_kinstr kprev and kinfo = kinfo_of_kinstr k in
        Log.log_exit ctxt kinfo_prev.kloc kinfo.kstack_ty (accu, stack))
  in
  let[@inline] get_log () =
    lwt_option_iter logger (fun logger ->
        let module Log = (val logger) in
        Log.get_log ())
  in
  let (KDescr {kinstr; kli; klo}) = descr in
  let rec step :
      type a s b t r f.
      context ->
      (a, s, b, t) kinstr ->
      (b, t, r, f) konts ->
      a ->
      s ->
      (r * f * context) tzresult Lwt.t =
   fun ctxt k ks accu stack ->
    let[@inline] continue :
        type a s b t r f.
        context ->
        (a, s, b, t) kinstr ->
        (b, t, r, f) konts ->
        a ->
        s ->
        (r * f * context) tzresult Lwt.t =
     fun ctxt k' ks accu stack ->
      log_exit ctxt k k' accu stack ;
      (step [@ocaml.tailcall]) ctxt k' ks accu stack
    in
    let gas = cost_of_instr k accu stack in
    Gas.consume ctxt gas
    >>?= fun ctxt ->
    log_entry ctxt k accu stack ;
    match k with
    | KHalt _ -> (
      match ks with
      | KNil ->
          ( Lwt.return (Ok (accu, stack, ctxt))
            : (r * f * context) tzresult Lwt.t )
      | KCons (k, ks) ->
          (continue [@ocaml.tailcall]) ctxt k ks accu stack )
    (* stack ops *)
    | KDrop (_, k) ->
        (continue [@ocaml.tailcall]) ctxt k ks (fst stack) (snd stack)
    | KDup (_, k) ->
        (continue [@ocaml.tailcall]) ctxt k ks accu (accu, stack)
    | KSwap (_, k) ->
        let (top, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks top (accu, stack)
    | KConst (_, v, k) ->
        (continue [@ocaml.tailcall]) ctxt k ks v (accu, stack)
    (* options *)
    | KCons_some (_, k) ->
        (continue [@ocaml.tailcall]) ctxt k ks (Some accu) stack
    | KCons_none (_, _, k) ->
        (continue [@ocaml.tailcall]) ctxt k ks None (accu, stack)
    | KIf_none (_, bt, bf) -> (
      match accu with
      | None ->
          (continue [@ocaml.tailcall]) ctxt bt ks (fst stack) (snd stack)
      | Some v ->
          (continue [@ocaml.tailcall]) ctxt bf ks v stack )
    (* pairs *)
    | KCons_pair (_, k) ->
        let (b, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (accu, b) stack
    | KUnpair (_, k) ->
        let (a, b) = accu in
        (continue [@ocaml.tailcall]) ctxt k ks a (b, stack)
    | KCar (_, k) ->
        let (a, _) = accu in
        (continue [@ocaml.tailcall]) ctxt k ks a stack
    | KCdr (_, k) ->
        let (_, b) = accu in
        (continue [@ocaml.tailcall]) ctxt k ks b stack
    (* unions *)
    | KCons_left (_, k) ->
        (continue [@ocaml.tailcall]) ctxt k ks (L accu) stack
    | KCons_right (_, k) ->
        (continue [@ocaml.tailcall]) ctxt k ks (R accu) stack
    | KIf_left (_, bl, br) -> (
      match accu with
      | L v ->
          (continue [@ocaml.tailcall]) ctxt bl ks v stack
      | R v ->
          (continue [@ocaml.tailcall]) ctxt br ks v stack )
    (* lists *)
    | KCons_list (_, k) ->
        let (tl, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (list_cons accu tl) stack
    | KNil (_, k) ->
        (continue [@ocaml.tailcall]) ctxt k ks list_empty (accu, stack)
    | KIf_cons (_, bc, bn) -> (
      match accu.elements with
      | [] ->
          (continue [@ocaml.tailcall]) ctxt bn ks (fst stack) (snd stack)
      | hd :: tl ->
          let tl = {elements = tl; length = accu.length - 1} in
          (continue [@ocaml.tailcall]) ctxt bc ks hd (tl, stack) )
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
        (continue [@ocaml.tailcall]) ctxt k ks (fst stack) (snd stack)
    | KList_mapping (kinfo_mapping, kinfo_mapped, body, xs, ys, len, k) -> (
      match xs with
      | [] ->
          let ys = {elements = List.rev ys; length = len} in
          (continue [@ocaml.tailcall]) ctxt k ks ys (accu, stack)
      | x :: xs ->
          let ks =
            KCons
              ( KList_mapped (kinfo_mapped, kinfo_mapping, body, xs, ys, len, k),
                ks )
          in
          (continue [@ocaml.tailcall]) ctxt body ks x (accu, stack) )
    | KList_mapped (kinfo_mapped, kinfo_mapping, body, xs, ys, len, k) ->
        let k =
          KList_mapping
            (kinfo_mapping, kinfo_mapped, body, xs, accu :: ys, len, k)
        in
        (continue [@ocaml.tailcall]) ctxt k ks (fst stack) (snd stack)
    | KList_size (_, k) ->
        let list = accu in
        let len = Script_int.(abs (of_int list.length)) in
        (continue [@ocaml.tailcall]) ctxt k ks len stack
    | KList_iter (_, kinfo_iter, body, k) ->
        let xs = accu.elements in
        let k = KIter (kinfo_iter, body, xs, k) in
        (continue [@ocaml.tailcall]) ctxt k ks (fst stack) (snd stack)
    | KIter (kinfo, body, xs, k) -> (
      match xs with
      | [] ->
          (continue [@ocaml.tailcall]) ctxt k ks accu stack
      | x :: xs ->
          let ks = KCons (KIter (kinfo, body, xs, k), ks) in
          (continue [@ocaml.tailcall]) ctxt body ks x (accu, stack) )
    (* sets *)
    | KEmpty_set (_, ty, k) ->
        (continue [@ocaml.tailcall]) ctxt k ks (empty_set ty) (accu, stack)
    | KSet_iter (_, kinfo_iter, body, k) ->
        let set = accu in
        let l = List.rev (set_fold (fun e acc -> e :: acc) set []) in
        (continue [@ocaml.tailcall])
          ctxt
          (KIter (kinfo_iter, body, l, k))
          ks
          (fst stack)
          (snd stack)
    | KSet_mem (_, k) ->
        let (set, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (set_mem accu set) stack
    | KSet_update (_, k) ->
        let (presence, (set, stack)) = stack in
        (continue [@ocaml.tailcall])
          ctxt
          k
          ks
          (set_update accu presence set)
          stack
    | KSet_size (_, k) ->
        (continue [@ocaml.tailcall]) ctxt k ks (set_size accu) stack
    (* maps *)
    | KEmpty_map (_, ty, _, k) ->
        (continue [@ocaml.tailcall]) ctxt k ks (empty_map ty) (accu, stack)
    | KMap_map (_, kinfo_mapping, kinfo_mapped, body, k) ->
        let map = accu in
        let xs = List.rev (map_fold (fun k v acc -> (k, v) :: acc) map []) in
        let ys = empty_map (map_key_ty map) in
        (continue [@ocaml.tailcall])
          ctxt
          (KMap_mapping (kinfo_mapping, kinfo_mapped, body, xs, ys, k))
          ks
          (fst stack)
          (snd stack)
    | KMap_mapping (kinfo_mapping, kinfo_mapped, body, xs, ys, k) -> (
      match xs with
      | [] ->
          (continue [@ocaml.tailcall]) ctxt k ks ys (accu, stack)
      | (xk, xv) :: xs ->
          let ks =
            KCons
              ( KMap_mapped (kinfo_mapped, kinfo_mapping, body, xs, ys, xk, k),
                ks )
          in
          (continue [@ocaml.tailcall]) ctxt body ks (xk, xv) (accu, stack) )
    | KMap_mapped (kinfo_mapped, kinfo_mapping, body, xs, ys, yk, k) ->
        let ys = map_update yk (Some accu) ys in
        let k = KMap_mapping (kinfo_mapping, kinfo_mapped, body, xs, ys, k) in
        (continue [@ocaml.tailcall]) ctxt k ks (fst stack) (snd stack)
    | KMap_iter (kinfo, body, k) ->
        let map = accu in
        let l = List.rev (map_fold (fun k v acc -> (k, v) :: acc) map []) in
        let kinfo_iter =
          match kinfo.kstack_ty with
          | Item_t (_, kstack_ty, _) ->
              {kinfo with kstack_ty}
        in
        (continue [@ocaml.tailcall])
          ctxt
          (KIter (kinfo_iter, body, l, k))
          ks
          (fst stack)
          (snd stack)
    | KMap_mem (_, k) ->
        let (map, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (map_mem accu map) stack
    | KMap_get (_, k) ->
        let (map, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (map_get accu map) stack
    | KMap_update (_, k) ->
        let (v, (map, stack)) = stack in
        let key = accu in
        (continue [@ocaml.tailcall]) ctxt k ks (map_update key v map) stack
    | KMap_get_and_update (_, k) ->
        let key = accu in
        let (v, (map, rest)) = stack in
        let map' = map_update key v map in
        let v' = map_get key map in
        (continue [@ocaml.tailcall]) ctxt k ks v' (map', rest)
    | KMap_size (_, k) ->
        (continue [@ocaml.tailcall]) ctxt k ks (map_size accu) stack
    (* Big map operations *)
    | KEmpty_big_map (_, tk, tv, k) ->
        let ebm = Script_ir_translator.empty_big_map tk tv in
        (continue [@ocaml.tailcall]) ctxt k ks ebm (accu, stack)
    | KBig_map_mem (_, k) ->
        let (map, stack) = stack in
        let key = accu in
        Script_ir_translator.big_map_mem ctxt key map
        >>=? fun (res, ctxt) ->
        (continue [@ocaml.tailcall]) ctxt k ks res stack
    | KBig_map_get (_, k) ->
        let (map, stack) = stack in
        let key = accu in
        Script_ir_translator.big_map_get ctxt key map
        >>=? fun (res, ctxt) ->
        (continue [@ocaml.tailcall]) ctxt k ks res stack
    | KBig_map_update (_, k) ->
        let key = accu in
        let (maybe_value, (map, stack)) = stack in
        let big_map =
          Script_ir_translator.big_map_update key maybe_value map
        in
        (continue [@ocaml.tailcall]) ctxt k ks big_map stack
    | KBig_map_get_and_update (_, k) ->
        let key = accu in
        let (v, (map, stack)) = stack in
        let map' = Script_ir_translator.big_map_update key v map in
        Script_ir_translator.big_map_get ctxt key map
        >>=? fun (v', ctxt) ->
        (continue [@ocaml.tailcall]) ctxt k ks v' (map', stack)
    (* timestamp operations *)
    | KAdd_seconds_to_timestamp (_, k) ->
        let n = accu in
        let (t, stack) = stack in
        let result = Script_timestamp.add_delta t n in
        (continue [@ocaml.tailcall]) ctxt k ks result stack
    | KAdd_timestamp_to_seconds (_, k) ->
        let t = accu in
        let (n, stack) = stack in
        let result = Script_timestamp.add_delta t n in
        (continue [@ocaml.tailcall]) ctxt k ks result stack
    | KSub_timestamp_seconds (_, k) ->
        let t = accu in
        let (s, stack) = stack in
        let result = Script_timestamp.sub_delta t s in
        (continue [@ocaml.tailcall]) ctxt k ks result stack
    | KDiff_timestamps (_, k) ->
        let t1 = accu in
        let (t2, stack) = stack in
        let result = Script_timestamp.diff t1 t2 in
        (continue [@ocaml.tailcall]) ctxt k ks result stack
    (* string operations *)
    | KConcat_string_pair (_, k) ->
        let x = accu in
        let (y, stack) = stack in
        let s = String.concat "" [x; y] in
        (continue [@ocaml.tailcall]) ctxt k ks s stack
    | KConcat_string (_, k) ->
        let ss = accu in
        (* The cost for this fold_left has been paid upfront *)
        let total_length =
          List.fold_left
            (fun acc s -> S.add acc (S.of_int (String.length s)))
            S.zero
            accu.elements
        in
        Gas.consume ctxt (Interp_costs.concat_string total_length)
        >>?= fun ctxt ->
        let s = String.concat "" ss.elements in
        (continue [@ocaml.tailcall]) ctxt k ks s stack
    | KSlice_string (_, k) ->
        let offset = accu and (length, (s, stack)) = stack in
        let s_length = Z.of_int (String.length s) in
        let offset = Script_int.to_zint offset in
        let length = Script_int.to_zint length in
        if Compare.Z.(offset < s_length && Z.add offset length <= s_length)
        then
          let s = String.sub s (Z.to_int offset) (Z.to_int length) in
          (continue [@ocaml.tailcall]) ctxt k ks (Some s) stack
        else (continue [@ocaml.tailcall]) ctxt k ks None stack
    | KString_size (_, k) ->
        let s = accu in
        let result = Script_int.(abs (of_int (String.length s))) in
        (continue [@ocaml.tailcall]) ctxt k ks result stack
    (* bytes operations *)
    | KConcat_bytes_pair (_, k) ->
        let x = accu in
        let (y, stack) = stack in
        let s = Bytes.cat x y in
        (continue [@ocaml.tailcall]) ctxt k ks s stack
    | KConcat_bytes (_, k) ->
        let ss = accu in
        (* The cost for this fold_left has been paid upfront *)
        let total_length =
          List.fold_left
            (fun acc s -> S.add acc (S.of_int (Bytes.length s)))
            S.zero
            accu.elements
        in
        Gas.consume ctxt (Interp_costs.concat_string total_length)
        >>?= fun ctxt ->
        let s = Bytes.concat Bytes.empty ss.elements in
        (continue [@ocaml.tailcall]) ctxt k ks s stack
    | KSlice_bytes (_, k) ->
        let offset = accu and (length, (s, stack)) = stack in
        let s_length = Z.of_int (Bytes.length s) in
        let offset = Script_int.to_zint offset in
        let length = Script_int.to_zint length in
        if Compare.Z.(offset < s_length && Z.add offset length <= s_length)
        then
          let s = Bytes.sub s (Z.to_int offset) (Z.to_int length) in
          (continue [@ocaml.tailcall]) ctxt k ks (Some s) stack
        else (continue [@ocaml.tailcall]) ctxt k ks None stack
    | KBytes_size (_, k) ->
        let s = accu in
        let result = Script_int.(abs (of_int (Bytes.length s))) in
        (continue [@ocaml.tailcall]) ctxt k ks result stack
    (* currency operations *)
    | KAdd_tez (_, k) ->
        let x = accu in
        let (y, stack) = stack in
        Tez.(x +? y)
        >>?= fun res -> (continue [@ocaml.tailcall]) ctxt k ks res stack
    | KSub_tez (_, k) ->
        let x = accu in
        let (y, stack) = stack in
        Tez.(x -? y)
        >>?= fun res -> (continue [@ocaml.tailcall]) ctxt k ks res stack
    | KMul_teznat (kinfo, k) -> (
        let x = accu in
        let (y, stack) = stack in
        match Script_int.to_int64 y with
        | None ->
            get_log () >>=? fun log -> fail (Overflow (kinfo.kloc, log))
        | Some y ->
            Tez.(x *? y)
            >>?= fun res -> (continue [@ocaml.tailcall]) ctxt k ks res stack )
    | KMul_nattez (kinfo, k) -> (
        let y = accu in
        let (x, stack) = stack in
        match Script_int.to_int64 y with
        | None ->
            get_log () >>=? fun log -> fail (Overflow (kinfo.kloc, log))
        | Some y ->
            Tez.(x *? y)
            >>?= fun res -> (continue [@ocaml.tailcall]) ctxt k ks res stack )
    (* boolean operations *)
    | KOr (_, k) ->
        let x = accu in
        let (y, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (x || y) stack
    | KAnd (_, k) ->
        let x = accu in
        let (y, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (x && y) stack
    | KXor (_, k) ->
        let x = accu in
        let (y, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks Compare.Bool.(x <> y) stack
    | KNot (_, k) ->
        let x = accu in
        (continue [@ocaml.tailcall]) ctxt k ks (not x) stack
    (* integer operations *)
    | KIs_nat (_, k) ->
        let x = accu in
        (continue [@ocaml.tailcall]) ctxt k ks (Script_int.is_nat x) stack
    | KAbs_int (_, k) ->
        let x = accu in
        (continue [@ocaml.tailcall]) ctxt k ks (Script_int.abs x) stack
    | KInt_nat (_, k) ->
        let x = accu in
        (continue [@ocaml.tailcall]) ctxt k ks (Script_int.int x) stack
    | KNeg_int (_, k) ->
        let x = accu in
        (continue [@ocaml.tailcall]) ctxt k ks (Script_int.neg x) stack
    | KNeg_nat (_, k) ->
        let x = accu in
        (continue [@ocaml.tailcall]) ctxt k ks (Script_int.neg x) stack
    | KAdd_intint (_, k) ->
        let x = accu and (y, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (Script_int.add x y) stack
    | KAdd_intnat (_, k) ->
        let x = accu and (y, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (Script_int.add x y) stack
    | KAdd_natint (_, k) ->
        let x = accu and (y, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (Script_int.add x y) stack
    | KAdd_natnat (_, k) ->
        let x = accu and (y, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (Script_int.add_n x y) stack
    | KSub_int (_, k) ->
        let x = accu and (y, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (Script_int.sub x y) stack
    | KMul_intint (_, k) ->
        let x = accu and (y, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (Script_int.mul x y) stack
    | KMul_intnat (_, k) ->
        let x = accu and (y, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (Script_int.mul x y) stack
    | KMul_natint (_, k) ->
        let x = accu and (y, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (Script_int.mul x y) stack
    | KMul_natnat (_, k) ->
        let x = accu and (y, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (Script_int.mul_n x y) stack
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
        (continue [@ocaml.tailcall]) ctxt k ks result stack
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
        (continue [@ocaml.tailcall]) ctxt k ks result stack
    | KEdiv_intint (_, k) ->
        let x = accu and (y, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (Script_int.ediv x y) stack
    | KEdiv_intnat (_, k) ->
        let x = accu and (y, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (Script_int.ediv x y) stack
    | KEdiv_natint (_, k) ->
        let x = accu and (y, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (Script_int.ediv x y) stack
    | KEdiv_natnat (_, k) ->
        let x = accu and (y, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (Script_int.ediv_n x y) stack
    | KLsl_nat (kinfo, k) -> (
        let x = accu and (y, stack) = stack in
        match Script_int.shift_left_n x y with
        | None ->
            get_log () >>=? fun log -> fail (Overflow (kinfo.kloc, log))
        | Some x ->
            (continue [@ocaml.tailcall]) ctxt k ks x stack )
    | KLsr_nat (kinfo, k) -> (
        let x = accu and (y, stack) = stack in
        match Script_int.shift_right_n x y with
        | None ->
            get_log () >>=? fun log -> fail (Overflow (kinfo.kloc, log))
        | Some r ->
            (continue [@ocaml.tailcall]) ctxt k ks r stack )
    | KOr_nat (_, k) ->
        let x = accu and (y, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (Script_int.logor x y) stack
    | KAnd_nat (_, k) ->
        let x = accu and (y, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (Script_int.logand x y) stack
    | KAnd_int_nat (_, k) ->
        let x = accu and (y, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (Script_int.logand x y) stack
    | KXor_nat (_, k) ->
        let x = accu and (y, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (Script_int.logxor x y) stack
    | KNot_int (_, k) ->
        let x = accu in
        (continue [@ocaml.tailcall]) ctxt k ks (Script_int.lognot x) stack
    | KNot_nat (_, k) ->
        let x = accu in
        (continue [@ocaml.tailcall]) ctxt k ks (Script_int.lognot x) stack
    (* control *)
    | KIf (_, bt, bf) ->
        if accu then
          (continue [@ocaml.tailcall]) ctxt bt ks (fst stack) (snd stack)
        else (continue [@ocaml.tailcall]) ctxt bf ks (fst stack) (snd stack)
    | KLoop (_, body, k) as self ->
        if accu then
          (continue [@ocaml.tailcall])
            ctxt
            body
            (KCons (self, ks))
            (fst stack)
            (snd stack)
        else (continue [@ocaml.tailcall]) ctxt k ks (fst stack) (snd stack)
    | KLoop_left (_, bl, br) as self -> (
      match accu with
      | L v ->
          (continue [@ocaml.tailcall]) ctxt bl (KCons (self, ks)) v stack
      | R v ->
          (continue [@ocaml.tailcall]) ctxt br ks v stack )
    | KDip (_, kinfo_const, b, k) ->
        let ign = accu in
        let ks = KCons (KConst (kinfo_const, ign, k), ks) in
        (continue [@ocaml.tailcall]) ctxt b ks (fst stack) (snd stack)
    | KExec (_, k) ->
        let arg = accu and (code, stack) = stack in
        interp logger ctxt step_constants code arg
        >>=? fun (res, ctxt) ->
        (continue [@ocaml.tailcall]) ctxt k ks res stack
    | KApply (_, capture_ty, k) -> (
        let capture = accu in
        let (lam, stack) = stack in
        let (Lam (descr, expr)) = lam in
        let (Item_t (full_arg_ty, _, _)) = descr.bef in
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
            (continue [@ocaml.tailcall]) ctxt k ks lam' stack
        | _ ->
            assert false )
    | KLambda (_, lam, k) ->
        (continue [@ocaml.tailcall]) ctxt k ks lam (accu, stack)
    | KFailwith (_, kloc, tv, _) ->
        let v = accu in
        trace Cannot_serialize_failure (unparse_data ctxt Optimized tv v)
        >>=? fun (v, _ctxt) ->
        let v = Micheline.strip_locations v in
        get_log () >>=? fun log -> fail (Reject (kloc, v, log))
    | KNop (_, k) ->
        (continue [@ocaml.tailcall]) ctxt k ks accu stack
    (* comparison *)
    | KCompare (_, ty, k) ->
        let a = accu in
        let (b, stack) = stack in
        let r =
          Script_int.of_int @@ Script_ir_translator.compare_comparable ty a b
        in
        (continue [@ocaml.tailcall]) ctxt k ks r stack
    (* comparators *)
    | KEq (_, k) ->
        let cmpres = accu in
        let cmpres = Script_int.compare cmpres Script_int.zero in
        let cmpres = Compare.Int.(cmpres = 0) in
        (continue [@ocaml.tailcall]) ctxt k ks cmpres stack
    | KNeq (_, k) ->
        let cmpres = accu in
        let cmpres = Script_int.compare cmpres Script_int.zero in
        let cmpres = Compare.Int.(cmpres <> 0) in
        (continue [@ocaml.tailcall]) ctxt k ks cmpres stack
    | KLt (_, k) ->
        let cmpres = accu in
        let cmpres = Script_int.compare cmpres Script_int.zero in
        let cmpres = Compare.Int.(cmpres < 0) in
        (continue [@ocaml.tailcall]) ctxt k ks cmpres stack
    | KLe (_, k) ->
        let cmpres = accu in
        let cmpres = Script_int.compare cmpres Script_int.zero in
        let cmpres = Compare.Int.(cmpres <= 0) in
        (continue [@ocaml.tailcall]) ctxt k ks cmpres stack
    | KGt (_, k) ->
        let cmpres = accu in
        let cmpres = Script_int.compare cmpres Script_int.zero in
        let cmpres = Compare.Int.(cmpres > 0) in
        (continue [@ocaml.tailcall]) ctxt k ks cmpres stack
    | KGe (_, k) ->
        let cmpres = accu in
        let cmpres = Script_int.compare cmpres Script_int.zero in
        let cmpres = Compare.Int.(cmpres >= 0) in
        (continue [@ocaml.tailcall]) ctxt k ks cmpres stack
    (* packing *)
    | KPack (_, ty, k) ->
        let value = accu in
        Script_ir_translator.pack_data ctxt ty value
        >>=? fun (bytes, ctxt) ->
        (continue [@ocaml.tailcall]) ctxt k ks bytes stack
    | KUnpack (_, ty, k) ->
        let bytes = accu in
        unpack ctxt ~ty ~bytes
        >>=? fun (opt, ctxt) ->
        (continue [@ocaml.tailcall]) ctxt k ks opt stack
    | KAddress (_, k) ->
        let (_, address) = accu in
        (continue [@ocaml.tailcall]) ctxt k ks address stack
    | KContract (kinfo, t, entrypoint, k) -> (
        let contract = accu in
        match (contract, entrypoint) with
        | ((contract, "default"), entrypoint)
        | ((contract, entrypoint), "default") ->
            Script_ir_translator.parse_contract_for_script
              ~legacy:false
              ctxt
              kinfo.kloc
              t
              contract
              ~entrypoint
            >>=? fun (ctxt, maybe_contract) ->
            (continue [@ocaml.tailcall]) ctxt k ks maybe_contract stack
        | _ ->
            (continue [@ocaml.tailcall]) ctxt k ks None stack )
    | KTransfer_tokens (_, k) ->
        let p = accu in
        let (amount, ((tp, (destination, entrypoint)), stack)) = stack in
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
        (continue [@ocaml.tailcall])
          ctxt
          k
          ks
          ( Internal_operation {source = step_constants.self; operation; nonce},
            lazy_storage_diff )
          stack
    | KImplicit_account (_, k) ->
        let key = accu in
        let contract = Contract.implicit_contract key in
        let res = (Unit_t None, (contract, "default")) in
        (continue [@ocaml.tailcall]) ctxt k ks res stack
    | KCreate_contract
        (_, storage_type, param_type, Lam (_, code), root_name, k) ->
        (* Removed the instruction's arguments manager, spendable and delegatable *)
        let delegate = accu in
        let (credit, (init, stack)) = stack in
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
          ( Internal_operation {source = step_constants.self; operation; nonce},
            lazy_storage_diff )
        in
        (continue [@ocaml.tailcall])
          ctxt
          k
          ks
          res
          ((contract, "default"), stack)
    | KSet_delegate (_, k) ->
        let delegate = accu in
        let operation = Delegation delegate in
        fresh_internal_nonce ctxt
        >>?= fun (ctxt, nonce) ->
        let res =
          ( Internal_operation {source = step_constants.self; operation; nonce},
            None )
        in
        (continue [@ocaml.tailcall]) ctxt k ks res stack
    | KBalance (_, k) ->
        Contract.get_balance_carbonated ctxt step_constants.self
        >>=? fun (ctxt, balance) ->
        (continue [@ocaml.tailcall]) ctxt k ks balance (accu, stack)
    | KLevel (_, k) ->
        let level =
          (Level.current ctxt).level |> Raw_level.to_int32
          |> Script_int.of_int32 |> Script_int.abs
        in
        (continue [@ocaml.tailcall]) ctxt k ks level (accu, stack)
    | KNow (_, k) ->
        let now = Script_timestamp.now ctxt in
        (continue [@ocaml.tailcall]) ctxt k ks now (accu, stack)
    | KCheck_signature (_, k) ->
        let key = accu and (signature, (message, stack)) = stack in
        let res = Signature.check key signature message in
        (continue [@ocaml.tailcall]) ctxt k ks res stack
    | KHash_key (_, k) ->
        let key = accu in
        let res = Signature.Public_key.hash key in
        (continue [@ocaml.tailcall]) ctxt k ks res stack
    | KBlake2b (_, k) ->
        let bytes = accu in
        let hash = Raw_hashes.blake2b bytes in
        (continue [@ocaml.tailcall]) ctxt k ks hash stack
    | KSha256 (_, k) ->
        let bytes = accu in
        let hash = Raw_hashes.sha256 bytes in
        (continue [@ocaml.tailcall]) ctxt k ks hash stack
    | KSha512 (_, k) ->
        let bytes = accu in
        let hash = Raw_hashes.sha512 bytes in
        (continue [@ocaml.tailcall]) ctxt k ks hash stack
    | KSource (_, k) ->
        let res = (step_constants.payer, "default") in
        (continue [@ocaml.tailcall]) ctxt k ks res (accu, stack)
    | KSender (_, k) ->
        let res = (step_constants.source, "default") in
        (continue [@ocaml.tailcall]) ctxt k ks res (accu, stack)
    | KSelf (_, ty, entrypoint, k) ->
        let res = (ty, (step_constants.self, entrypoint)) in
        (continue [@ocaml.tailcall]) ctxt k ks res (accu, stack)
    | KSelf_address (_, k) ->
        let res = (step_constants.self, "default") in
        (continue [@ocaml.tailcall]) ctxt k ks res (accu, stack)
    | KAmount (_, k) ->
        (continue [@ocaml.tailcall])
          ctxt
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
        (continue [@ocaml.tailcall]) ctxt k ks accu stack
    | KDug (_, _n, n', k) ->
        let v = accu in
        let (stack, ()) =
          interp_stack_prefix_preserving_operation
            (fun stack -> ((v, stack), ()))
            n'
            stack
        in
        (continue [@ocaml.tailcall]) ctxt k ks (fst stack) (snd stack)
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
                  ktransfer w stack (ExKInstr (Eq, KConst (kinfo, x, k))) ) )
          | (KRest (_, _), _) ->
              (stack, k)
        in
        match ktransfer n' (accu, stack) (ExKInstr (Eq, k)) with
        | (stack, ExKInstr (Eq, restore_prefix)) ->
            let ks = KCons (restore_prefix, ks) in
            (continue [@ocaml.tailcall]) ctxt b ks (fst stack) (snd stack) )
    | KDropn (_, _n, n', k) ->
        let (_, stack) =
          interp_stack_prefix_preserving_operation
            (fun stack -> (stack, stack))
            n'
            (accu, stack)
        in
        (continue [@ocaml.tailcall]) ctxt k ks (fst stack) (snd stack)
    | KSapling_empty_state (_, memo_size, k) ->
        let state = Sapling.empty_state ~memo_size () in
        (continue [@ocaml.tailcall]) ctxt k ks state (accu, stack)
    | KSapling_verify_update (_, k) -> (
        let transaction = accu in
        let (state, stack) = stack in
        let address = Contract.to_b58check step_constants.self in
        let chain_id = Chain_id.to_b58check step_constants.chain_id in
        let anti_replay = address ^ chain_id in
        Sapling.verify_update ctxt state transaction anti_replay
        >>=? fun (ctxt, balance_state_opt) ->
        match balance_state_opt with
        | Some (balance, state) ->
            let state = Some (Script_int.of_int64 balance, state) in
            (continue [@ocaml.tailcall]) ctxt k ks state stack
        | None ->
            (continue [@ocaml.tailcall]) ctxt k ks None stack )
    | KChainId (_, k) ->
        (continue [@ocaml.tailcall])
          ctxt
          k
          ks
          step_constants.chain_id
          (accu, stack)
    | KNever _ -> (
      match accu with _ -> . )
    | KVoting_power (_, k) ->
        let key_hash = accu in
        Vote.get_voting_power ctxt key_hash
        >>=? fun (ctxt, rolls) ->
        let power = Script_int.(abs (of_int32 rolls)) in
        (continue [@ocaml.tailcall]) ctxt k ks power stack
    | KTotal_voting_power (_, k) ->
        Vote.get_total_voting_power ctxt
        >>=? fun (ctxt, rolls) ->
        let power = Script_int.(abs (of_int32 rolls)) in
        (continue [@ocaml.tailcall]) ctxt k ks power (accu, stack)
    | KKeccak (_, k) ->
        let bytes = accu in
        let hash = Raw_hashes.keccak256 bytes in
        (continue [@ocaml.tailcall]) ctxt k ks hash stack
    | KSha3 (_, k) ->
        let bytes = accu in
        let hash = Raw_hashes.sha3_256 bytes in
        (continue [@ocaml.tailcall]) ctxt k ks hash stack
    | KAdd_bls12_381_g1 (_, k) ->
        let x = accu and (y, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (Bls12_381.G1.add x y) stack
    | KAdd_bls12_381_g2 (_, k) ->
        let x = accu and (y, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (Bls12_381.G2.add x y) stack
    | KAdd_bls12_381_fr (_, k) ->
        let x = accu and (y, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (Bls12_381.Fr.add x y) stack
    | KMul_bls12_381_g1 (_, k) ->
        let x = accu and (y, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (Bls12_381.G1.mul x y) stack
    | KMul_bls12_381_g2 (_, k) ->
        let x = accu and (y, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (Bls12_381.G2.mul x y) stack
    | KMul_bls12_381_fr (_, k) ->
        let x = accu and (y, stack) = stack in
        (continue [@ocaml.tailcall]) ctxt k ks (Bls12_381.Fr.mul x y) stack
    | KMul_bls12_381_fr_z (_, k) ->
        let x = accu and (y, stack) = stack in
        let x = Bls12_381.Fr.of_z (Script_int.to_zint x) in
        let res = Bls12_381.Fr.mul x y in
        (continue [@ocaml.tailcall]) ctxt k ks res stack
    | KMul_bls12_381_z_fr (_, k) ->
        let y = accu and (x, stack) = stack in
        let x = Bls12_381.Fr.of_z (Script_int.to_zint x) in
        let res = Bls12_381.Fr.mul x y in
        (continue [@ocaml.tailcall]) ctxt k ks res stack
    | KInt_bls12_381_fr (_, k) ->
        let x = accu in
        let res = Script_int.of_zint (Bls12_381.Fr.to_z x) in
        (continue [@ocaml.tailcall]) ctxt k ks res stack
    | KNeg_bls12_381_g1 (_, k) ->
        let x = accu in
        (continue [@ocaml.tailcall]) ctxt k ks (Bls12_381.G1.negate x) stack
    | KNeg_bls12_381_g2 (_, k) ->
        let x = accu in
        (continue [@ocaml.tailcall]) ctxt k ks (Bls12_381.G2.negate x) stack
    | KNeg_bls12_381_fr (_, k) ->
        let x = accu in
        (continue [@ocaml.tailcall]) ctxt k ks (Bls12_381.Fr.negate x) stack
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
        (continue [@ocaml.tailcall]) ctxt k ks check stack
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
        (continue [@ocaml.tailcall]) ctxt k ks (fst stack) (snd stack)
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
        (continue [@ocaml.tailcall]) ctxt k ks (fst stack) (snd stack)
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
        (continue [@ocaml.tailcall]) ctxt k ks accu stack
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
        (continue [@ocaml.tailcall]) ctxt k ks accu stack
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
        (continue [@ocaml.tailcall]) ctxt k ks accu stack
    (* Tickets *)
    | KTicket (_, k) ->
        let contents = accu and (amount, stack) = stack in
        let ticketer = (step_constants.self, "default") in
        (continue [@ocaml.tailcall])
          ctxt
          k
          ks
          {ticketer; contents; amount}
          stack
    | KRead_ticket (_, k) ->
        let {ticketer; contents; amount} = accu in
        let stack = (accu, stack) in
        (continue [@ocaml.tailcall])
          ctxt
          k
          ks
          (ticketer, (contents, amount))
          stack
    | KSplit_ticket (_, k) ->
        let ticket = accu and ((amount_a, amount_b), stack) = stack in
        let result =
          if
            Compare.Int.(
              Script_int.(compare (add_n amount_a amount_b) ticket.amount) = 0)
          then
            Some
              ({ticket with amount = amount_a}, {ticket with amount = amount_b})
          else None
        in
        (continue [@ocaml.tailcall]) ctxt k ks result stack
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
        (continue [@ocaml.tailcall]) ctxt k ks result stack
  in
  let (accu, stack) = lift kli stack in
  step ctxt kinstr KNil accu stack
  >>=? fun (accu, stack, ctxt) -> return (unlift klo (accu, stack), ctxt)

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
  let kdescr = translate descr in
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
