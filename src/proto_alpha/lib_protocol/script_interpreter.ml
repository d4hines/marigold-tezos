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

(*

  This module implements an interpreter for Michelson. It takes the
  form of a [step] function that interprets script instructions in a
  dedicated abstract machine.

  The interpreter is written in a small-step style: an execution
  [step] only interprets a single instruction by updating the
  configuration of a dedicated abstract machine.

  This abstract machine has two components:

  - a stack to control which instructions must be executed ; and

  - a stack of values where instructions get their inputs and put
    their outputs.

  In addition, the machine has access to effectful primitives to interact
  with the execution environment (e.g. the tezos node). These primitives
  live in the [Lwt+State+Error] monad. Hence, this interpreter produces
  a computation in the [Lwt+State+Error] monad.

  This interpreter enjoys the following properties:

  - The interpreter is tail-recursive, hence it is robust to stack
    overflow. This property is checked by the compiler thanks to the
    [@ocaml.tailcall] annotation of each recursive calls.

  - The interpreter is type-preserving. Thanks to GADTs, the
    typing rules of Michelson are statically checked by the OCaml
    typechecker: a Michelson program cannot go wrong.

  - The interpreter is tagless. Thanks to GADTs, the exact shape
    of the stack is known statically so the interpreter does not
    have to check that the input stack has the shape expected by
    the instruction to be executed.

  Outline
  =======

  This file is organized as follows:

  1. Runtime errors:
     The standard incantations to register the errors
     that can be produced by this module's functions.

  2. Gas accounting:
     The function [cost_of_instr] assigns a gas consumption
     to an instruction and a stack of values according to
     the cost model. This function is used in the interpretation
     loop. Several auxiliary functions are given to deal with
     gas accounting.

  3. Logging:
     One can instrument the interpreter with logging functions.

  4. Interpretation loop:
     This is the main functionality of this module, aka the
     [step] function.

  5. Interface functions:
     This part of the module builds high-level functions
     on top the more basic [step] function.

  Implementation details are explained along the file.

*)

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
    (fun () -> Cannot_serialize_storage)

(*

   Computing the cost of Michelson instructions
   ============================================

   The function [cost_of_instr] provides a cost model for Michelson
   instructions. It is used by the interpreter to track the
   consumption of gas. This consumption may depend on the values
   on the stack.

 *)

module Interp_costs = Michelson_v1_gas.Cost_of.Interpreter

let cost_of_instr : type a s r f. (a, s, r, f) kinstr -> a -> s -> Gas.cost =
 fun i accu stack ->
  match i with
  | KList_map _ ->
      let list = accu in
      Interp_costs.list_map list
  | KList_iter _ ->
      let list = accu in
      Interp_costs.list_iter list
  | KSet_iter _ ->
      let set = accu in
      Interp_costs.set_iter set
  | KSet_mem _ ->
      let v = accu and (set, _) = stack in
      Interp_costs.set_mem v set
  | KSet_update _ ->
      let v = accu and (_, (set, _)) = stack in
      Interp_costs.set_update v set
  | KMap_map _ ->
      let map = accu in
      Interp_costs.map_map map
  | KMap_iter _ ->
      let map = accu in
      Interp_costs.map_iter map
  | KMap_mem _ ->
      let v = accu and (map, _) = stack in
      Interp_costs.map_mem v map
  | KMap_get _ ->
      let v = accu and (map, _) = stack in
      Interp_costs.map_get v map
  | KMap_update _ ->
      let k = accu and (_, (map, _)) = stack in
      Interp_costs.map_update k map
  | KMap_get_and_update _ ->
      let k = accu and (_, (map, _)) = stack in
      Interp_costs.map_get_and_update k map
  | KBig_map_mem _ ->
      let key = accu and (map, _) = stack in
      Interp_costs.map_mem key map.diff
  | KBig_map_get _ ->
      let key = accu and (map, _) = stack in
      Interp_costs.map_get key map.diff
  | KBig_map_update _ ->
      let key = accu and (_, (map, _)) = stack in
      Interp_costs.map_update key map.diff
  | KBig_map_get_and_update _ ->
      let key = accu and (_, (map, _)) = stack in
      Interp_costs.map_get_and_update key map.diff
  | KAdd_seconds_to_timestamp _ ->
      let n = accu and (t, _) = stack in
      Interp_costs.add_seconds_timestamp n t
  | KAdd_timestamp_to_seconds _ ->
      let t = accu and (n, _) = stack in
      Interp_costs.add_seconds_timestamp n t
  | KSub_timestamp_seconds _ ->
      let t = accu and (n, _) = stack in
      Interp_costs.sub_seconds_timestamp n t
  | KDiff_timestamps _ ->
      let t1 = accu and (t2, _) = stack in
      Interp_costs.diff_timestamps t1 t2
  | KConcat_string_pair _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.concat_string_pair x y
  | KConcat_string _ ->
      let ss = accu in
      Interp_costs.concat_string_precheck ss
  | KSlice_string _ ->
      let (_, (s, _)) = stack in
      Interp_costs.slice_string s
  | KConcat_bytes_pair _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.concat_bytes_pair x y
  | KConcat_bytes _ ->
      let ss = accu in
      Interp_costs.concat_string_precheck ss
  | KSlice_bytes _ ->
      let (_, (s, _)) = stack in
      Interp_costs.slice_bytes s
  | KMul_teznat _ ->
      let (n, _) = stack in
      Interp_costs.mul_teznat n
  | KMul_nattez _ ->
      let n = accu in
      Interp_costs.mul_teznat n
  | KAbs_int _ ->
      let x = accu in
      Interp_costs.abs_int x
  | KNeg_int _ ->
      let x = accu in
      Interp_costs.neg_int x
  | KNeg_nat _ ->
      let x = accu in
      Interp_costs.neg_nat x
  | KAdd_intint _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.add_bigint x y
  | KAdd_intnat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.add_bigint x y
  | KAdd_natint _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.add_bigint x y
  | KAdd_natnat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.add_bigint x y
  | KSub_int _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.sub_bigint x y
  | KMul_intint _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.mul_bigint x y
  | KMul_intnat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.mul_bigint x y
  | KMul_natint _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.mul_bigint x y
  | KMul_natnat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.mul_bigint x y
  | KEdiv_teznat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.ediv_teznat x y
  | KEdiv_intint _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.ediv_bigint x y
  | KEdiv_intnat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.ediv_bigint x y
  | KEdiv_natint _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.ediv_bigint x y
  | KEdiv_natnat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.ediv_bigint x y
  | KLsl_nat _ ->
      let x = accu in
      Interp_costs.lsl_nat x
  | KLsr_nat _ ->
      let x = accu in
      Interp_costs.lsr_nat x
  | KOr_nat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.or_nat x y
  | KAnd_nat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.and_nat x y
  | KAnd_int_nat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.and_nat x y
  | KXor_nat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.xor_nat x y
  | KNot_int _ ->
      let x = accu in
      Interp_costs.not_nat x
  | KNot_nat _ ->
      let x = accu in
      Interp_costs.not_nat x
  | KCompare (_, ty, _) ->
      let a = accu and (b, _) = stack in
      Interp_costs.compare ty a b
  | KCheck_signature _ ->
      let key = accu and (_, (message, _)) = stack in
      Interp_costs.check_signature key message
  | KHash_key _ ->
      let pk = accu in
      Interp_costs.hash_key pk
  | KBlake2b _ ->
      let bytes = accu in
      Interp_costs.blake2b bytes
  | KSha256 _ ->
      let bytes = accu in
      Interp_costs.sha256 bytes
  | KSha512 _ ->
      let bytes = accu in
      Interp_costs.sha512 bytes
  | KKeccak _ ->
      let bytes = accu in
      Interp_costs.keccak bytes
  | KSha3 _ ->
      let bytes = accu in
      Interp_costs.sha3 bytes
  | KPairing_check_bls12_381 _ ->
      let pairs = accu in
      Interp_costs.pairing_check_bls12_381 pairs
  | KSapling_verify_update _ ->
      let tx = accu in
      let inputs = List.length tx.inputs in
      let outputs = List.length tx.outputs in
      Interp_costs.sapling_verify_update ~inputs ~outputs
  | KSplit_ticket _ ->
      let ticket = accu and ((amount_a, amount_b), _) = stack in
      Interp_costs.split_ticket ticket.amount amount_a amount_b
  | KJoin_tickets (_, ty, _) ->
      let (ticket_a, ticket_b) = accu in
      Interp_costs.join_tickets ty ticket_a ticket_b
  | KHalt _ ->
      (* FIXME *)
      Gas.free
  | KDrop _ ->
      Interp_costs.drop
  | KDup _ ->
      Interp_costs.dup
  | KSwap _ ->
      Interp_costs.swap
  | KConst _ ->
      Interp_costs.push
  | KCons_some _ ->
      Interp_costs.cons_some
  | KCons_none _ ->
      Interp_costs.cons_none
  | KIf_none _ ->
      Interp_costs.if_none
  | KCons_pair _ ->
      Interp_costs.cons_pair
  | KUnpair _ ->
      Interp_costs.unpair
  | KCar _ ->
      Interp_costs.car
  | KCdr _ ->
      Interp_costs.cdr
  | KCons_left _ ->
      Interp_costs.cons_left
  | KCons_right _ ->
      Interp_costs.cons_right
  | KIf_left _ ->
      Interp_costs.if_left
  | KCons_list _ ->
      Interp_costs.cons_list
  | KNil _ ->
      Interp_costs.nil
  | KIf_cons _ ->
      Interp_costs.if_cons
  | KList_size _ ->
      Interp_costs.list_size
  | KEmpty_set _ ->
      Interp_costs.empty_set
  | KSet_size _ ->
      Interp_costs.set_size
  | KEmpty_map _ ->
      Interp_costs.empty_map
  | KMap_size _ ->
      Interp_costs.map_size
  | KEmpty_big_map _ ->
      Interp_costs.empty_map
  | KString_size _ ->
      Interp_costs.string_size
  | KBytes_size _ ->
      Interp_costs.bytes_size
  | KAdd_tez _ ->
      Interp_costs.add_tez
  | KSub_tez _ ->
      Interp_costs.sub_tez
  | KOr _ ->
      Interp_costs.bool_or
  | KAnd _ ->
      Interp_costs.bool_and
  | KXor _ ->
      Interp_costs.bool_xor
  | KNot _ ->
      Interp_costs.bool_not
  | KIs_nat _ ->
      Interp_costs.is_nat
  | KInt_nat _ ->
      Interp_costs.int_nat
  | KInt_bls12_381_fr _ ->
      Interp_costs.int_bls12_381_fr
  | KEdiv_tez _ ->
      Interp_costs.ediv_tez
  | KIf _ ->
      Interp_costs.if_
  | KLoop _ ->
      Interp_costs.loop
  | KLoop_left _ ->
      Interp_costs.loop_left
  | KDip _ ->
      Interp_costs.dip
  | KExec _ ->
      Interp_costs.exec
  | KApply _ ->
      Interp_costs.apply
  | KLambda _ ->
      Interp_costs.push
  | KFailwith _ ->
      Gas.free
  | KNop _ ->
      Interp_costs.nop
  | KEq _ ->
      Interp_costs.neq
  | KNeq _ ->
      Interp_costs.neq
  | KLt _ ->
      Interp_costs.neq
  | KLe _ ->
      Interp_costs.neq
  | KGt _ ->
      Interp_costs.neq
  | KGe _ ->
      Interp_costs.neq
  | KPack _ ->
      Gas.free
  | KUnpack _ ->
      Gas.free
  | KAddress _ ->
      Interp_costs.address
  | KContract _ ->
      Interp_costs.contract
  | KTransfer_tokens _ ->
      Interp_costs.transfer_tokens
  | KImplicit_account _ ->
      Interp_costs.implicit_account
  | KSet_delegate _ ->
      Interp_costs.set_delegate
  | KBalance _ ->
      Interp_costs.balance
  | KLevel _ ->
      Interp_costs.level
  | KNow _ ->
      Interp_costs.now
  | KSapling_empty_state _ ->
      Interp_costs.sapling_empty_state
  | KSource _ ->
      Interp_costs.source
  | KSender _ ->
      Interp_costs.source
  | KSelf _ ->
      Interp_costs.self
  | KSelf_address _ ->
      Interp_costs.self
  | KAmount _ ->
      Interp_costs.amount
  | KDig (_, n, _, _) ->
      Interp_costs.dign n
  | KDug (_, n, _, _) ->
      Interp_costs.dugn n
  | KDipn (_, n, _, _, _) ->
      Interp_costs.dipn n
  | KDropn (_, n, _, _) ->
      Interp_costs.dropn n
  | KChainId _ ->
      Interp_costs.chain_id
  | KCreate_contract _ ->
      Interp_costs.create_contract
  | KNever _ ->
      Gas.free
  | KVoting_power _ ->
      Interp_costs.voting_power
  | KTotal_voting_power _ ->
      Interp_costs.total_voting_power
  | KAdd_bls12_381_g1 _ ->
      Interp_costs.add_bls12_381_g1
  | KAdd_bls12_381_g2 _ ->
      Interp_costs.add_bls12_381_g2
  | KAdd_bls12_381_fr _ ->
      Interp_costs.add_bls12_381_fr
  | KMul_bls12_381_g1 _ ->
      Interp_costs.mul_bls12_381_g1
  | KMul_bls12_381_g2 _ ->
      Interp_costs.mul_bls12_381_g2
  | KMul_bls12_381_fr _ ->
      Interp_costs.mul_bls12_381_fr
  | KNeg_bls12_381_g1 _ ->
      Interp_costs.neg_bls12_381_g1
  | KNeg_bls12_381_g2 _ ->
      Interp_costs.neg_bls12_381_g2
  | KNeg_bls12_381_fr _ ->
      Interp_costs.neg_bls12_381_fr
  | KMul_bls12_381_fr_z _ ->
      Interp_costs.mul_bls12_381_fr_z
  | KMul_bls12_381_z_fr _ ->
      Interp_costs.mul_bls12_381_fr_z
  | KDup_n (_, n, _, _) ->
      Interp_costs.dupn n
  | KComb (_, n, _, _) ->
      Interp_costs.comb n
  | KUncomb (_, n, _, _) ->
      Interp_costs.uncomb n
  | KComb_get (_, n, _, _) ->
      Interp_costs.comb_get n
  | KComb_set (_, n, _, _) ->
      Interp_costs.comb_set n
  | KTicket _ ->
      Interp_costs.ticket
  | KRead_ticket _ ->
      Interp_costs.read_ticket
 [@@ocaml.inline always]

(*

   Gas update and check for gas exhaustion
   =======================================

   Each instruction has a cost. The runtime subtracts this cost
   to an amount of gas made available for the script execution.

   Updating the gas counter is a critical aspect to Michelson
   execution because it is done at each execution step.

   For this reason, the interpreter must read and update the
   gas counter as quickly as possible. Hence, the gas counter
   should be stored in a machine register. To motivate the
   OCaml compiler to make that choice, we represent the gas
   counter as a local parameter of the execution [step]
   function.

*)

type local_gas_counter = int

(*

   The gas counter stored in the context is desynchronized with the
   [local_gas_counter] used in the interpretation loop. When we have
   to call a gas-consuming function which lives outside the
   interpreter, we must update the context so that it carries an
   up-to-date gas counter. Similarly, when we return from such a
   function, the [local_gas_counter] must be updated as well.

   To statically track these points where the context's gas counter
   must be updated, we introduce a type for outdated contexts.  The
   [step] function carries an [outdated_context]. When an external
   function needs a [context], the typechecker points out the need for
   a conversion: this forces us to either call [update_context], or
   better, when this is possible, the function
   [use_gas_counter_in_ctxt].

*)
type outdated_context = OutDatedContext of context [@@unboxed]

let update_context local_gas_counter = function
  | OutDatedContext ctxt ->
      Gas.update_gas_counter ctxt (Saturation_repr.of_int local_gas_counter)
  [@@ocaml.inline always]

let update_local_gas_counter ctxt =
  (Gas.gas_counter ctxt :> int)
  [@@ocaml.inline always]

let outdated ctxt = OutDatedContext ctxt [@@ocaml.inline always]

let outdated_context (OutDatedContext ctxt) = ctxt [@@ocaml.inline always]

let use_gas_counter_in_ctxt ctxt local_gas_counter f =
  let ctxt = update_context local_gas_counter ctxt in
  f ctxt
  >>=? fun (y, ctxt) -> return (y, outdated ctxt, update_local_gas_counter ctxt)
  [@@ocaml.inline always]

(*

   [step] calls [consume] at the beginning of each execution step.

   [consume'] is used in the implementation of [KConcat_string]
   and [KConcat_bytes] because in that special cases, the cost
   is expressed with respec to the final result of the concatenation.

*)

let update_and_check gas_counter cost =
  let gas_counter = gas_counter - cost in
  if Compare.Int.(gas_counter < 0) then None else Some gas_counter
  [@@ocaml.inline always]

let consume local_gas_counter k accu stack =
  let cost = cost_of_instr k accu stack in
  update_and_check local_gas_counter (cost :> int)
  [@@ocaml.inline always]

let consume' ctxt local_gas_counter cost =
  match update_and_check local_gas_counter cost with
  | None ->
      Gas.gas_exhausted_error (update_context local_gas_counter ctxt)
  | Some local_gas_counter ->
      Ok local_gas_counter
  [@@ocaml.inline always]

(*

    Execution instrumentation
    =========================

    One can observe the context and the stack at some specific
    points of an execution step. This feature is implemented by
    calling back some [logging_function]s defined in a first
    class module [STEP_LOGGER] passed as argument to the step
    function. The interface documentation describes the points
    where these functions are called.

*)
type ('a, 's, 'b, 'f, 'u) logging_function =
  ('a, 's, 'b, 'f) kinstr ->
  context ->
  Script.location ->
  'u stack_ty ->
  'u ->
  unit

module type STEP_LOGGER = sig
  val log_interp : ('a, 's, 'b, 'f, 'u) logging_function

  val log_entry : ('a, 's, 'b, 'f, 'a * 's) logging_function

  val log_exit : ('a, 's, 'b, 'f, 'u) logging_function

  val get_log : unit -> execution_trace option tzresult Lwt.t
end

type logger = (module STEP_LOGGER)

let log_entry (logger : logger) ctxt gas k accu stack =
  let module Log = (val logger) in
  let kinfo = kinfo_of_kinstr k in
  let ctxt = update_context gas ctxt in
  Log.log_entry k ctxt kinfo.kloc kinfo.kstack_ty (accu, stack)

let log_exit (logger : logger) ctxt gas kprev k accu stack =
  let module Log = (val logger) in
  let ctxt = update_context gas ctxt in
  let kinfo_prev = kinfo_of_kinstr kprev and kinfo = kinfo_of_kinstr k in
  Log.log_exit k ctxt kinfo_prev.kloc kinfo.kstack_ty (accu, stack)

let get_log (logger : logger option) =
  match logger with
  | None ->
      Lwt.return (Ok None)
  | Some logger ->
      let module Log = (val logger) in
      Log.get_log ()
  [@@ocaml.inline always]

(*

  Interpretation loop
  ===================

  The stack of control is a list of [kinstr]. This type is documented
  in the module [Script_typed_cps_ir].

  Since [kinstr] denotes a list  of instructions, the stack of control
  can be seen as a list  of instruction sequences, each representing a
  form of delimited continuation (i.e.  a control stack fragment). The
  [konts] GADT  ensures that the input  and output stack types  of the
  continuations are consistent.

  Loops have a special treatment because their control stack is reused
  as is during the next iteration. This avoids the reallocation of a
  control stack cell at each iteration.

  Higher-order iterators (i.e. MAPs and ITERs) need internal instructions
  to implement [step] as a tail-recursive function. Roughly speaking,
  these instructions help in decomposing the execution of [I f c]
  (where [I] is an higher-order iterator over a container [c]) into
  three phases: to start the iteration, to execute [f] if there are
  elements to be processed in [c], and to loop.

  Dip also has a dedicated constructor in the control stack.  This
  allows the stack prefix to be restored after the execution of the
  [Dip]'s body.

*)
type (_, _, _, _) konts =
  | KNil : ('r, 'f, 'r, 'f) konts
  | KCons :
      ('a, 's, 'b, 't) kinstr * ('b, 't, 'r, 'f) konts
      -> ('a, 's, 'r, 'f) konts
  | KUndip : 'b * ('b, 'a * 's, 'r, 'f) konts -> ('a, 's, 'r, 'f) konts
  | KLoop_in :
      ('a, 's, bool, 'a * 's) kinstr * ('a, 's, 'r, 'f) konts
      -> (bool, 'a * 's, 'r, 'f) konts
  | KLoop_in_left :
      ('a, 's, ('a, 'b) union, 's) kinstr * ('b, 's, 'r, 'f) konts
      -> (('a, 'b) union, 's, 'r, 'f) konts
  | KIter :
      ('a, 'b * 's, 'b, 's) kinstr * 'a list * ('b, 's, 'r, 'f) konts
      -> ('b, 's, 'r, 'f) konts
  | KList_mapping :
      ('a, 'c * 's, 'b, 'c * 's) kinstr
      * 'a list
      * 'b list
      * int
      * ('b boxed_list, 'c * 's, 'r, 'f) konts
      -> ('c, 's, 'r, 'f) konts
  | KList_mapped :
      ('a, 'c * 's, 'b, 'c * 's) kinstr
      * 'a list
      * 'b list
      * int
      * ('b boxed_list, 'c * 's, 'r, 'f) konts
      -> ('b, 'c * 's, 'r, 'f) konts
  | KMap_mapping :
      ('a * 'b, 'd * 's, 'c, 'd * 's) kinstr
      * ('a * 'b) list
      * ('a, 'c) map
      * (('a, 'c) map, 'd * 's, 'r, 'f) konts
      -> ('d, 's, 'r, 'f) konts
  | KMap_mapped :
      ('a * 'b, 'd * 's, 'c, 'd * 's) kinstr
      * ('a * 'b) list
      * ('a, 'c) map
      * 'a
      * (('a, 'c) map, 'd * 's, 'r, 'f) konts
      -> ('c, 'd * 's, 'r, 'f) konts

(*

    The interpreter is parameterized by a small set of values.

*)
type step_constants = {
  source : Contract.t;
  payer : Contract.t;
  self : Contract.t;
  amount : Tez.t;
  chain_id : Chain_id.t;
}

let rec interp_stack_prefix_preserving_operation :
    type fbef bef faft aft result.
    (fbef -> faft * result) ->
    (fbef, faft, bef, aft) stack_prefix_preservation_witness ->
    bef ->
    aft * result =
 fun f n stk ->
  match (n, stk) with
  | (Prefix n, (v, rest)) ->
      let (rest', result) =
        interp_stack_prefix_preserving_operation f n rest
      in
      ((v, rest'), result)
  | (Rest, v) ->
      f v

(*

   As announced earlier, the step function produces a computation in
   the [Lwt+State+Error] monad. The [State] monad is implemented by
   having the [context] passed as input and returned updated as
   output. The [Error] monad is represented by the [tzresult] type
   constructor.

   The [step] function is actually defined as an internal
   tail-recursive routine of the toplevel [step]. It monitors the gas
   level before executing the instruction under focus, once this is
   done, it recursively calls itself on the continuation held by the
   current instruction.

   For each pure instruction (i.e. that is not monadic), the
   interpretation simply updates the input arguments of the [step]
   function. Since these arguments are (most likely) stored in
   hardware registers and since the tail-recursive calls are compiled
   into direct jumps, this interpretation technique offers good
   performances while saving safety thanks to a rich typing.

   For each impure instruction, the interpreter makes use of monadic
   bindings to compose monadic primitives with the [step] function.
   Again, we make sure that the recursive calls to [step] are tail
   calls by annotating them with [@ocaml.tailcall].

*)
let rec run_descr :
    type bef aft.
    logger option ->
    context * step_constants ->
    (bef, aft) kdescr ->
    bef ->
    (aft * context) tzresult Lwt.t =
 fun logger (ctxt, sc) descr stack ->
  let (KDescr {kinstr; kli; klo}) = descr in
  let (accu, stack) = lift kli stack in
  let gas = (Gas.gas_counter ctxt :> int) in
  step logger (outdated ctxt, sc) gas kinstr KNil accu stack
  >>=? fun (accu, stack, ctxt, gas) ->
  return (unlift klo (accu, stack), update_context gas ctxt)

and run :
    type a a' s s' b t b' t' r f.
    logger option ->
    outdated_context * step_constants ->
    local_gas_counter ->
    (a', s', b', t') kinstr ->
    (a, s, b, t) kinstr ->
    (b, t, r, f) konts ->
    a ->
    s ->
    (r * f * outdated_context * local_gas_counter) tzresult Lwt.t =
 fun logger g gas k k' ks accu stack ->
  ( match logger with
  | None ->
      ()
  | Some logger ->
      let (ctxt, _) = g in
      log_exit logger ctxt gas k k' accu stack ) ;
  (step [@ocaml.tailcall]) logger g gas k' ks accu stack
 [@@inline.always]

and next :
    type a s r f.
    logger option ->
    outdated_context * step_constants ->
    local_gas_counter ->
    (a, s, r, f) konts ->
    a ->
    s ->
    (r * f * outdated_context * local_gas_counter) tzresult Lwt.t =
 fun logger ((ctxt, _) as g) gas ks accu stack ->
  match ks with
  | KNil ->
      Lwt.return (Ok (accu, stack, ctxt, gas))
  | KCons (k, ks) ->
      (step [@ocaml.tailcall]) logger g gas k ks accu stack
  | KLoop_in (ki, ks') ->
      let (accu', stack') = stack in
      if accu then (step [@ocaml.tailcall]) logger g gas ki ks accu' stack'
      else (next [@ocaml.tailcall]) logger g gas ks' accu' stack'
  | KLoop_in_left (ki, ks') -> (
    match accu with
    | L v ->
        (step [@ocaml.tailcall]) logger g gas ki ks v stack
    | R v ->
        (next [@ocaml.tailcall]) logger g gas ks' v stack )
  | KUndip (x, ks) ->
      next logger g gas ks x (accu, stack)
  | KIter (body, xs, ks) -> (
    match xs with
    | [] ->
        next logger g gas ks accu stack
    | x :: xs ->
        let ks = KIter (body, xs, ks) in
        (step [@ocaml.tailcall]) logger g gas body ks x (accu, stack) )
  | KList_mapping (body, xs, ys, len, ks) -> (
    match xs with
    | [] ->
        let ys = {elements = List.rev ys; length = len} in
        next logger g gas ks ys (accu, stack)
    | x :: xs ->
        let ks = KList_mapped (body, xs, ys, len, ks) in
        (step [@ocaml.tailcall]) logger g gas body ks x (accu, stack) )
  | KList_mapped (body, xs, ys, len, ks) ->
      let ks = KList_mapping (body, xs, accu :: ys, len, ks) in
      let (accu, stack) = stack in
      next logger g gas ks accu stack
  | KMap_mapping (body, xs, ys, ks) -> (
    match xs with
    | [] ->
        next logger g gas ks ys (accu, stack)
    | (xk, xv) :: xs ->
        let ks = KMap_mapped (body, xs, ys, xk, ks) in
        let res = (xk, xv) in
        let stack = (accu, stack) in
        (step [@ocaml.tailcall]) logger g gas body ks res stack )
  | KMap_mapped (body, xs, ys, yk, ks) ->
      let ys = map_update yk (Some accu) ys in
      let ks = KMap_mapping (body, xs, ys, ks) in
      let (accu, stack) = stack in
      next logger g gas ks accu stack

and step :
    type a s b t r f.
    logger option ->
    outdated_context * step_constants ->
    local_gas_counter ->
    (a, s, b, t) kinstr ->
    (b, t, r, f) konts ->
    a ->
    s ->
    (r * f * outdated_context * local_gas_counter) tzresult Lwt.t =
 fun logger ((ctxt, sc) as g) gas i ks accu stack ->
  match consume gas i accu stack with
  | None ->
      Lwt.return (Gas.gas_exhausted_error (update_context gas ctxt))
  | Some gas -> (
      ( match logger with
      | None ->
          ()
      | Some logger ->
          log_entry logger ctxt gas i accu stack ) ;
      match i with
      | KHalt _ ->
          next logger g gas ks accu stack
      (* stack ops *)
      | KDrop (_, k) ->
          let (accu, stack) = stack in
          (run [@ocaml.tailcall]) logger g gas i k ks accu stack
      | KDup (_, k) ->
          (run [@ocaml.tailcall]) logger g gas i k ks accu (accu, stack)
      | KSwap (_, k) ->
          let (top, stack) = stack in
          (run [@ocaml.tailcall]) logger g gas i k ks top (accu, stack)
      | KConst (_, v, k) ->
          (run [@ocaml.tailcall]) logger g gas i k ks v (accu, stack)
      (* options *)
      | KCons_some (_, k) ->
          (run [@ocaml.tailcall]) logger g gas i k ks (Some accu) stack
      | KCons_none (_, _, k) ->
          (run [@ocaml.tailcall]) logger g gas i k ks None (accu, stack)
      | KIf_none (_, bt, bf) -> (
        match accu with
        | None ->
            let (accu, stack) = stack in
            (run [@ocaml.tailcall]) logger g gas i bt ks accu stack
        | Some v ->
            (run [@ocaml.tailcall]) logger g gas i bf ks v stack )
      (* pairs *)
      | KCons_pair (_, k) ->
          let (b, stack) = stack in
          (run [@ocaml.tailcall]) logger g gas i k ks (accu, b) stack
      | KUnpair (_, k) ->
          let (a, b) = accu in
          (run [@ocaml.tailcall]) logger g gas i k ks a (b, stack)
      | KCar (_, k) ->
          let (a, _) = accu in
          (run [@ocaml.tailcall]) logger g gas i k ks a stack
      | KCdr (_, k) ->
          let (_, b) = accu in
          (run [@ocaml.tailcall]) logger g gas i k ks b stack
      (* unions *)
      | KCons_left (_, k) ->
          (run [@ocaml.tailcall]) logger g gas i k ks (L accu) stack
      | KCons_right (_, k) ->
          (run [@ocaml.tailcall]) logger g gas i k ks (R accu) stack
      | KIf_left (_, bl, br) -> (
        match accu with
        | L v ->
            (run [@ocaml.tailcall]) logger g gas i bl ks v stack
        | R v ->
            (run [@ocaml.tailcall]) logger g gas i br ks v stack )
      (* lists *)
      | KCons_list (_, k) ->
          let (tl, stack) = stack in
          let accu = list_cons accu tl in
          (run [@ocaml.tailcall]) logger g gas i k ks accu stack
      | KNil (_, k) ->
          let stack = (accu, stack) in
          let accu = list_empty in
          (run [@ocaml.tailcall]) logger g gas i k ks accu stack
      | KIf_cons (_, bc, bn) -> (
        match accu.elements with
        | [] ->
            let (accu, stack) = stack in
            (run [@ocaml.tailcall]) logger g gas i bn ks accu stack
        | hd :: tl ->
            let tl = {elements = tl; length = accu.length - 1} in
            (run [@ocaml.tailcall]) logger g gas i bc ks hd (tl, stack) )
      | KList_map (_, body, k) ->
          let xs = accu.elements in
          let ys = [] in
          let len = accu.length in
          let ks = KList_mapping (body, xs, ys, len, KCons (k, ks)) in
          let (accu, stack) = stack in
          (next [@ocaml.tailcall]) logger g gas ks accu stack
      | KList_size (_, k) ->
          let list = accu in
          let len = Script_int.(abs (of_int list.length)) in
          (run [@ocaml.tailcall]) logger g gas i k ks len stack
      | KList_iter (_, body, k) ->
          let xs = accu.elements in
          let ks = KIter (body, xs, KCons (k, ks)) in
          let (accu, stack) = stack in
          (next [@ocaml.tailcall]) logger g gas ks accu stack
      (* sets *)
      | KEmpty_set (_, ty, k) ->
          let res = empty_set ty in
          let stack = (accu, stack) in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KSet_iter (_, body, k) ->
          let set = accu in
          let l = List.rev (set_fold (fun e acc -> e :: acc) set []) in
          let ks = KIter (body, l, KCons (k, ks)) in
          let (accu, stack) = stack in
          (next [@ocaml.tailcall]) logger g gas ks accu stack
      | KSet_mem (_, k) ->
          let (set, stack) = stack in
          let res = set_mem accu set in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KSet_update (_, k) ->
          let (presence, (set, stack)) = stack in
          let res = set_update accu presence set in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KSet_size (_, k) ->
          let res = set_size accu in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      (* maps *)
      | KEmpty_map (_, ty, _, k) ->
          let res = empty_map ty and stack = (accu, stack) in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KMap_map (_, body, k) ->
          let map = accu in
          let xs = List.rev (map_fold (fun k v a -> (k, v) :: a) map []) in
          let ys = empty_map (map_key_ty map) in
          let ks = KMap_mapping (body, xs, ys, KCons (k, ks)) in
          let (accu, stack) = stack in
          (next [@ocaml.tailcall]) logger g gas ks accu stack
      | KMap_iter (_, body, k) ->
          let map = accu in
          let l = List.rev (map_fold (fun k v a -> (k, v) :: a) map []) in
          let ks = KIter (body, l, KCons (k, ks)) in
          let (accu, stack) = stack in
          (next [@ocaml.tailcall]) logger g gas ks accu stack
      | KMap_mem (_, k) ->
          let (map, stack) = stack in
          let res = map_mem accu map in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KMap_get (_, k) ->
          let (map, stack) = stack in
          let res = map_get accu map in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KMap_update (_, k) ->
          let (v, (map, stack)) = stack in
          let key = accu in
          let res = map_update key v map in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KMap_get_and_update (_, k) ->
          let key = accu in
          let (v, (map, rest)) = stack in
          let map' = map_update key v map in
          let v' = map_get key map in
          (run [@ocaml.tailcall]) logger g gas i k ks v' (map', rest)
      | KMap_size (_, k) ->
          let res = map_size accu in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      (* Big map operations *)
      | KEmpty_big_map (_, tk, tv, k) ->
          let ebm = Script_ir_translator.empty_big_map tk tv in
          (run [@ocaml.tailcall]) logger g gas i k ks ebm (accu, stack)
      | KBig_map_mem (_, k) ->
          let (map, stack) = stack in
          let key = accu in
          ( use_gas_counter_in_ctxt ctxt gas
          @@ fun ctxt -> Script_ir_translator.big_map_mem ctxt key map )
          >>=? fun (res, ctxt, gas) ->
          (run [@ocaml.tailcall]) logger (ctxt, sc) gas i k ks res stack
      | KBig_map_get (_, k) ->
          let (map, stack) = stack in
          let key = accu in
          ( use_gas_counter_in_ctxt ctxt gas
          @@ fun ctxt -> Script_ir_translator.big_map_get ctxt key map )
          >>=? fun (res, ctxt, gas) ->
          (run [@ocaml.tailcall]) logger (ctxt, sc) gas i k ks res stack
      | KBig_map_update (_, k) ->
          let key = accu in
          let (maybe_value, (map, stack)) = stack in
          let big_map =
            Script_ir_translator.big_map_update key maybe_value map
          in
          (run [@ocaml.tailcall]) logger g gas i k ks big_map stack
      | KBig_map_get_and_update (_, k) ->
          let key = accu in
          let (v, (map, stack)) = stack in
          let map' = Script_ir_translator.big_map_update key v map in
          ( use_gas_counter_in_ctxt ctxt gas
          @@ fun ctxt -> Script_ir_translator.big_map_get ctxt key map )
          >>=? fun (v', ctxt, gas) ->
          (run [@ocaml.tailcall]) logger (ctxt, sc) gas i k ks v' (map', stack)
      (* timestamp operations *)
      | KAdd_seconds_to_timestamp (_, k) ->
          let n = accu in
          let (t, stack) = stack in
          let result = Script_timestamp.add_delta t n in
          (run [@ocaml.tailcall]) logger g gas i k ks result stack
      | KAdd_timestamp_to_seconds (_, k) ->
          let t = accu in
          let (n, stack) = stack in
          let result = Script_timestamp.add_delta t n in
          (run [@ocaml.tailcall]) logger g gas i k ks result stack
      | KSub_timestamp_seconds (_, k) ->
          let t = accu in
          let (s, stack) = stack in
          let result = Script_timestamp.sub_delta t s in
          (run [@ocaml.tailcall]) logger g gas i k ks result stack
      | KDiff_timestamps (_, k) ->
          let t1 = accu in
          let (t2, stack) = stack in
          let result = Script_timestamp.diff t1 t2 in
          (run [@ocaml.tailcall]) logger g gas i k ks result stack
      (* string operations *)
      | KConcat_string_pair (_, k) ->
          let x = accu in
          let (y, stack) = stack in
          let s = String.concat "" [x; y] in
          (run [@ocaml.tailcall]) logger g gas i k ks s stack
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
          (run [@ocaml.tailcall]) logger g gas i k ks s stack
      | KSlice_string (_, k) ->
          let offset = accu and (length, (s, stack)) = stack in
          let s_length = Z.of_int (String.length s) in
          let offset = Script_int.to_zint offset in
          let length = Script_int.to_zint length in
          if Compare.Z.(offset < s_length && Z.add offset length <= s_length)
          then
            let s = String.sub s (Z.to_int offset) (Z.to_int length) in
            (run [@ocaml.tailcall]) logger g gas i k ks (Some s) stack
          else (run [@ocaml.tailcall]) logger g gas i k ks None stack
      | KString_size (_, k) ->
          let s = accu in
          let result = Script_int.(abs (of_int (String.length s))) in
          (run [@ocaml.tailcall]) logger g gas i k ks result stack
      (* bytes operations *)
      | KConcat_bytes_pair (_, k) ->
          let x = accu in
          let (y, stack) = stack in
          let s = Bytes.cat x y in
          (run [@ocaml.tailcall]) logger g gas i k ks s stack
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
          (run [@ocaml.tailcall]) logger g gas i k ks s stack
      | KSlice_bytes (_, k) ->
          let offset = accu and (length, (s, stack)) = stack in
          let s_length = Z.of_int (Bytes.length s) in
          let offset = Script_int.to_zint offset in
          let length = Script_int.to_zint length in
          if Compare.Z.(offset < s_length && Z.add offset length <= s_length)
          then
            let s = Bytes.sub s (Z.to_int offset) (Z.to_int length) in
            (run [@ocaml.tailcall]) logger g gas i k ks (Some s) stack
          else (run [@ocaml.tailcall]) logger g gas i k ks None stack
      | KBytes_size (_, k) ->
          let s = accu in
          let result = Script_int.(abs (of_int (Bytes.length s))) in
          (run [@ocaml.tailcall]) logger g gas i k ks result stack
      (* currency operations *)
      | KAdd_tez (_, k) ->
          let x = accu in
          let (y, stack) = stack in
          Tez.(x +? y)
          >>?= fun res -> (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KSub_tez (_, k) ->
          let x = accu in
          let (y, stack) = stack in
          Tez.(x -? y)
          >>?= fun res -> (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KMul_teznat (kinfo, k) -> (
          let x = accu in
          let (y, stack) = stack in
          match Script_int.to_int64 y with
          | None ->
              get_log logger >>=? fun log -> fail (Overflow (kinfo.kloc, log))
          | Some y ->
              Tez.(x *? y)
              >>?= fun res ->
              (run [@ocaml.tailcall]) logger g gas i k ks res stack )
      | KMul_nattez (kinfo, k) -> (
          let y = accu in
          let (x, stack) = stack in
          match Script_int.to_int64 y with
          | None ->
              get_log logger >>=? fun log -> fail (Overflow (kinfo.kloc, log))
          | Some y ->
              Tez.(x *? y)
              >>?= fun res ->
              (run [@ocaml.tailcall]) logger g gas i k ks res stack )
      (* boolean operations *)
      | KOr (_, k) ->
          let x = accu in
          let (y, stack) = stack in
          (run [@ocaml.tailcall]) logger g gas i k ks (x || y) stack
      | KAnd (_, k) ->
          let x = accu in
          let (y, stack) = stack in
          (run [@ocaml.tailcall]) logger g gas i k ks (x && y) stack
      | KXor (_, k) ->
          let x = accu in
          let (y, stack) = stack in
          let res = Compare.Bool.(x <> y) in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KNot (_, k) ->
          let x = accu in
          (run [@ocaml.tailcall]) logger g gas i k ks (not x) stack
      (* integer operations *)
      | KIs_nat (_, k) ->
          let x = accu in
          let res = Script_int.is_nat x in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KAbs_int (_, k) ->
          let x = accu in
          let res = Script_int.abs x in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KInt_nat (_, k) ->
          let x = accu in
          let res = Script_int.int x in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KNeg_int (_, k) ->
          let x = accu in
          let res = Script_int.neg x in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KNeg_nat (_, k) ->
          let x = accu in
          let res = Script_int.neg x in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KAdd_intint (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.add x y in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KAdd_intnat (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.add x y in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KAdd_natint (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.add x y in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KAdd_natnat (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.add_n x y in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KSub_int (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.sub x y in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KMul_intint (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.mul x y in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KMul_intnat (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.mul x y in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KMul_natint (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.mul x y in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KMul_natnat (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.mul_n x y in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
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
          (run [@ocaml.tailcall]) logger g gas i k ks result stack
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
          (run [@ocaml.tailcall]) logger g gas i k ks result stack
      | KEdiv_intint (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.ediv x y in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KEdiv_intnat (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.ediv x y in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KEdiv_natint (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.ediv x y in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KEdiv_natnat (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.ediv_n x y in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KLsl_nat (kinfo, k) -> (
          let x = accu and (y, stack) = stack in
          match Script_int.shift_left_n x y with
          | None ->
              get_log logger >>=? fun log -> fail (Overflow (kinfo.kloc, log))
          | Some x ->
              (run [@ocaml.tailcall]) logger g gas i k ks x stack )
      | KLsr_nat (kinfo, k) -> (
          let x = accu and (y, stack) = stack in
          match Script_int.shift_right_n x y with
          | None ->
              get_log logger >>=? fun log -> fail (Overflow (kinfo.kloc, log))
          | Some r ->
              (run [@ocaml.tailcall]) logger g gas i k ks r stack )
      | KOr_nat (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.logor x y in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KAnd_nat (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.logand x y in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KAnd_int_nat (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.logand x y in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KXor_nat (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.logxor x y in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KNot_int (_, k) ->
          let x = accu in
          let res = Script_int.lognot x in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KNot_nat (_, k) ->
          let x = accu in
          let res = Script_int.lognot x in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      (* control *)
      | KIf (_, bt, bf) ->
          let (res, stack) = stack in
          if accu then (run [@ocaml.tailcall]) logger g gas i bt ks res stack
          else (run [@ocaml.tailcall]) logger g gas i bf ks res stack
      | KLoop (_, body, k) ->
          let ks = KLoop_in (body, KCons (k, ks)) in
          (next [@ocaml.tailcall]) logger g gas ks accu stack
      | KLoop_left (_, bl, br) ->
          let ks = KLoop_in_left (bl, KCons (br, ks)) in
          (next [@ocaml.tailcall]) logger g gas ks accu stack
      | KDip (_, _, b, k) ->
          let ign = accu in
          let ks = KUndip (ign, KCons (k, ks)) in
          let (accu, stack) = stack in
          (run [@ocaml.tailcall]) logger g gas i b ks accu stack
      | KExec (_, k) ->
          let arg = accu and (code, stack) = stack in
          ( use_gas_counter_in_ctxt ctxt gas
          @@ fun ctxt -> interp logger (ctxt, sc) code arg )
          >>=? fun (res, ctxt, gas) ->
          (run [@ocaml.tailcall]) logger (ctxt, sc) gas i k ks res stack
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
              let gas = update_local_gas_counter ctxt in
              let ctxt = outdated ctxt in
              (run [@ocaml.tailcall]) logger (ctxt, sc) gas i k ks lam' stack
          | _ ->
              assert false )
      | KLambda (_, lam, k) ->
          (run [@ocaml.tailcall]) logger g gas i k ks lam (accu, stack)
      | KFailwith (_, kloc, tv, _) ->
          let v = accu in
          let ctxt = update_context gas ctxt in
          trace Cannot_serialize_failure (unparse_data ctxt Optimized tv v)
          >>=? fun (v, _ctxt) ->
          let v = Micheline.strip_locations v in
          get_log logger >>=? fun log -> fail (Reject (kloc, v, log))
      | KNop (_, k) ->
          (run [@ocaml.tailcall]) logger g gas i k ks accu stack
      (* comparison *)
      | KCompare (_, ty, k) ->
          let a = accu in
          let (b, stack) = stack in
          let r =
            Script_int.of_int @@ Script_ir_translator.compare_comparable ty a b
          in
          (run [@ocaml.tailcall]) logger g gas i k ks r stack
      (* comparators *)
      | KEq (_, k) ->
          let cmpres = accu in
          let cmpres = Script_int.compare cmpres Script_int.zero in
          let cmpres = Compare.Int.(cmpres = 0) in
          (run [@ocaml.tailcall]) logger g gas i k ks cmpres stack
      | KNeq (_, k) ->
          let cmpres = accu in
          let cmpres = Script_int.compare cmpres Script_int.zero in
          let cmpres = Compare.Int.(cmpres <> 0) in
          (run [@ocaml.tailcall]) logger g gas i k ks cmpres stack
      | KLt (_, k) ->
          let cmpres = accu in
          let cmpres = Script_int.compare cmpres Script_int.zero in
          let cmpres = Compare.Int.(cmpres < 0) in
          (run [@ocaml.tailcall]) logger g gas i k ks cmpres stack
      | KLe (_, k) ->
          let cmpres = accu in
          let cmpres = Script_int.compare cmpres Script_int.zero in
          let cmpres = Compare.Int.(cmpres <= 0) in
          (run [@ocaml.tailcall]) logger g gas i k ks cmpres stack
      | KGt (_, k) ->
          let cmpres = accu in
          let cmpres = Script_int.compare cmpres Script_int.zero in
          let cmpres = Compare.Int.(cmpres > 0) in
          (run [@ocaml.tailcall]) logger g gas i k ks cmpres stack
      | KGe (_, k) ->
          let cmpres = accu in
          let cmpres = Script_int.compare cmpres Script_int.zero in
          let cmpres = Compare.Int.(cmpres >= 0) in
          (run [@ocaml.tailcall]) logger g gas i k ks cmpres stack
      (* packing *)
      | KPack (_, ty, k) ->
          let value = accu in
          ( use_gas_counter_in_ctxt ctxt gas
          @@ fun ctxt -> Script_ir_translator.pack_data ctxt ty value )
          >>=? fun (bytes, ctxt, gas) ->
          (run [@ocaml.tailcall]) logger (ctxt, sc) gas i k ks bytes stack
      | KUnpack (_, ty, k) ->
          let bytes = accu in
          ( use_gas_counter_in_ctxt ctxt gas
          @@ fun ctxt -> unpack ctxt ~ty ~bytes )
          >>=? fun (opt, ctxt, gas) ->
          (run [@ocaml.tailcall]) logger (ctxt, sc) gas i k ks opt stack
      | KAddress (_, k) ->
          let (_, address) = accu in
          (run [@ocaml.tailcall]) logger g gas i k ks address stack
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
              let gas = update_local_gas_counter ctxt in
              let ctxt = outdated ctxt in
              let accu = maybe_contract in
              (run [@ocaml.tailcall]) logger (ctxt, sc) gas i k ks accu stack
          | _ ->
              (run [@ocaml.tailcall]) logger (ctxt, sc) gas i k ks None stack )
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
          let iop = {source = sc.self; operation; nonce} in
          let accu = (Internal_operation iop, lazy_storage_diff) in
          let gas = update_local_gas_counter ctxt in
          let ctxt = outdated ctxt in
          (run [@ocaml.tailcall]) logger (ctxt, sc) gas i k ks accu stack
      | KImplicit_account (_, k) ->
          let key = accu in
          let contract = Contract.implicit_contract key in
          let res = (Unit_t None, (contract, "default")) in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
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
            ( Internal_operation {source = sc.self; operation; nonce},
              lazy_storage_diff )
          in
          let gas = update_local_gas_counter ctxt in
          let ctxt = outdated ctxt in
          let stack = ((contract, "default"), stack) in
          (run [@ocaml.tailcall]) logger (ctxt, sc) gas i k ks res stack
      | KSet_delegate (_, k) ->
          let delegate = accu in
          let operation = Delegation delegate in
          let ctxt = update_context gas ctxt in
          fresh_internal_nonce ctxt
          >>?= fun (ctxt, nonce) ->
          let res =
            (Internal_operation {source = sc.self; operation; nonce}, None)
          in
          let gas = update_local_gas_counter ctxt in
          let ctxt = outdated ctxt in
          (run [@ocaml.tailcall]) logger (ctxt, sc) gas i k ks res stack
      | KBalance (_, k) ->
          let ctxt = update_context gas ctxt in
          Contract.get_balance_carbonated ctxt sc.self
          >>=? fun (ctxt, balance) ->
          let gas = update_local_gas_counter ctxt in
          let ctxt = outdated ctxt in
          let g = (ctxt, sc) in
          (run [@ocaml.tailcall]) logger g gas i k ks balance (accu, stack)
      | KLevel (_, k) ->
          let level =
            (Level.current (outdated_context ctxt)).level |> Raw_level.to_int32
            |> Script_int.of_int32 |> Script_int.abs
          in
          (run [@ocaml.tailcall]) logger g gas i k ks level (accu, stack)
      | KNow (_, k) ->
          let now = Script_timestamp.now (outdated_context ctxt) in
          (run [@ocaml.tailcall]) logger g gas i k ks now (accu, stack)
      | KCheck_signature (_, k) ->
          let key = accu and (signature, (message, stack)) = stack in
          let res = Signature.check key signature message in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KHash_key (_, k) ->
          let key = accu in
          let res = Signature.Public_key.hash key in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KBlake2b (_, k) ->
          let bytes = accu in
          let hash = Raw_hashes.blake2b bytes in
          (run [@ocaml.tailcall]) logger g gas i k ks hash stack
      | KSha256 (_, k) ->
          let bytes = accu in
          let hash = Raw_hashes.sha256 bytes in
          (run [@ocaml.tailcall]) logger g gas i k ks hash stack
      | KSha512 (_, k) ->
          let bytes = accu in
          let hash = Raw_hashes.sha512 bytes in
          (run [@ocaml.tailcall]) logger g gas i k ks hash stack
      | KSource (_, k) ->
          let res = (sc.payer, "default") in
          (run [@ocaml.tailcall]) logger g gas i k ks res (accu, stack)
      | KSender (_, k) ->
          let res = (sc.source, "default") in
          (run [@ocaml.tailcall]) logger g gas i k ks res (accu, stack)
      | KSelf (_, ty, entrypoint, k) ->
          let res = (ty, (sc.self, entrypoint)) in
          (run [@ocaml.tailcall]) logger g gas i k ks res (accu, stack)
      | KSelf_address (_, k) ->
          let res = (sc.self, "default") in
          (run [@ocaml.tailcall]) logger g gas i k ks res (accu, stack)
      | KAmount (_, k) ->
          let accu = sc.amount and stack = (accu, stack) in
          (run [@ocaml.tailcall]) logger g gas i k ks accu stack
      | KDig (_, _n, n', k) ->
          let (stack, accu) =
            interp_stack_prefix_preserving_operation
              (fun (v, stack) -> (stack, v))
              n'
              (accu, stack)
          in
          (run [@ocaml.tailcall]) logger g gas i k ks accu stack
      | KDug (_, _n, n', k) ->
          let v = accu in
          let (stack, ()) =
            interp_stack_prefix_preserving_operation
              (fun stack -> ((v, stack), ()))
              n'
              stack
          in
          let (accu, stack) = stack in
          (run [@ocaml.tailcall]) logger g gas i k ks accu stack
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
              | ExKInstr k -> (
                match inverse_lift lu' with
                | ExLiftInverse Refl ->
                    ktransfer w stack (ExKInstr (KConst (kinfo, x, k))) ) )
            | (KRest (_, _), _) ->
                (stack, k)
          in
          match ktransfer n' (accu, stack) (ExKInstr k) with
          | (stack, ExKInstr restore_prefix) ->
              let ks = KCons (restore_prefix, ks) in
              let (accu, stack) = stack in
              (run [@ocaml.tailcall]) logger g gas i b ks accu stack )
      | KDropn (_, _n, n', k) ->
          let (_, stack) =
            interp_stack_prefix_preserving_operation
              (fun stack -> (stack, stack))
              n'
              (accu, stack)
          in
          let (accu, stack) = stack in
          (run [@ocaml.tailcall]) logger g gas i k ks accu stack
      | KSapling_empty_state (_, memo_size, k) ->
          let state = Sapling.empty_state ~memo_size () in
          (run [@ocaml.tailcall]) logger g gas i k ks state (accu, stack)
      | KSapling_verify_update (_, k) -> (
          let transaction = accu in
          let (state, stack) = stack in
          let address = Contract.to_b58check sc.self in
          let chain_id = Chain_id.to_b58check sc.chain_id in
          let anti_replay = address ^ chain_id in
          let ctxt = update_context gas ctxt in
          Sapling.verify_update ctxt state transaction anti_replay
          >>=? fun (ctxt, balance_state_opt) ->
          let gas = update_local_gas_counter ctxt in
          let ctxt = outdated ctxt in
          match balance_state_opt with
          | Some (balance, state) ->
              let state = Some (Script_int.of_int64 balance, state) in
              (run [@ocaml.tailcall]) logger (ctxt, sc) gas i k ks state stack
          | None ->
              (run [@ocaml.tailcall]) logger (ctxt, sc) gas i k ks None stack )
      | KChainId (_, k) ->
          let accu = sc.chain_id and stack = (accu, stack) in
          (run [@ocaml.tailcall]) logger g gas i k ks accu stack
      | KNever _ -> (
        match accu with _ -> . )
      | KVoting_power (_, k) ->
          let key_hash = accu in
          let ctxt = update_context gas ctxt in
          Vote.get_voting_power ctxt key_hash
          >>=? fun (ctxt, rolls) ->
          let power = Script_int.(abs (of_int32 rolls)) in
          let gas = update_local_gas_counter ctxt in
          let ctxt = outdated ctxt in
          (run [@ocaml.tailcall]) logger (ctxt, sc) gas i k ks power stack
      | KTotal_voting_power (_, k) ->
          let ctxt = update_context gas ctxt in
          Vote.get_total_voting_power ctxt
          >>=? fun (ctxt, rolls) ->
          let power = Script_int.(abs (of_int32 rolls)) in
          let gas = update_local_gas_counter ctxt in
          let ctxt = outdated ctxt in
          let g = (ctxt, sc) in
          (run [@ocaml.tailcall]) logger g gas i k ks power (accu, stack)
      | KKeccak (_, k) ->
          let bytes = accu in
          let hash = Raw_hashes.keccak256 bytes in
          (run [@ocaml.tailcall]) logger g gas i k ks hash stack
      | KSha3 (_, k) ->
          let bytes = accu in
          let hash = Raw_hashes.sha3_256 bytes in
          (run [@ocaml.tailcall]) logger g gas i k ks hash stack
      | KAdd_bls12_381_g1 (_, k) ->
          let x = accu and (y, stack) = stack in
          let accu = Bls12_381.G1.add x y in
          (run [@ocaml.tailcall]) logger g gas i k ks accu stack
      | KAdd_bls12_381_g2 (_, k) ->
          let x = accu and (y, stack) = stack in
          let accu = Bls12_381.G2.add x y in
          (run [@ocaml.tailcall]) logger g gas i k ks accu stack
      | KAdd_bls12_381_fr (_, k) ->
          let x = accu and (y, stack) = stack in
          let accu = Bls12_381.Fr.add x y in
          (run [@ocaml.tailcall]) logger g gas i k ks accu stack
      | KMul_bls12_381_g1 (_, k) ->
          let x = accu and (y, stack) = stack in
          let accu = Bls12_381.G1.mul x y in
          (run [@ocaml.tailcall]) logger g gas i k ks accu stack
      | KMul_bls12_381_g2 (_, k) ->
          let x = accu and (y, stack) = stack in
          let accu = Bls12_381.G2.mul x y in
          (run [@ocaml.tailcall]) logger g gas i k ks accu stack
      | KMul_bls12_381_fr (_, k) ->
          let x = accu and (y, stack) = stack in
          let accu = Bls12_381.Fr.mul x y in
          (run [@ocaml.tailcall]) logger g gas i k ks accu stack
      | KMul_bls12_381_fr_z (_, k) ->
          let x = accu and (y, stack) = stack in
          let x = Bls12_381.Fr.of_z (Script_int.to_zint x) in
          let res = Bls12_381.Fr.mul x y in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KMul_bls12_381_z_fr (_, k) ->
          let y = accu and (x, stack) = stack in
          let x = Bls12_381.Fr.of_z (Script_int.to_zint x) in
          let res = Bls12_381.Fr.mul x y in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KInt_bls12_381_fr (_, k) ->
          let x = accu in
          let res = Script_int.of_zint (Bls12_381.Fr.to_z x) in
          (run [@ocaml.tailcall]) logger g gas i k ks res stack
      | KNeg_bls12_381_g1 (_, k) ->
          let x = accu in
          let accu = Bls12_381.G1.negate x in
          (run [@ocaml.tailcall]) logger g gas i k ks accu stack
      | KNeg_bls12_381_g2 (_, k) ->
          let x = accu in
          let accu = Bls12_381.G2.negate x in
          (run [@ocaml.tailcall]) logger g gas i k ks accu stack
      | KNeg_bls12_381_fr (_, k) ->
          let x = accu in
          let accu = Bls12_381.Fr.negate x in
          (run [@ocaml.tailcall]) logger g gas i k ks accu stack
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
          (run [@ocaml.tailcall]) logger g gas i k ks check stack
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
          let (accu, stack) = stack in
          (run [@ocaml.tailcall]) logger g gas i k ks accu stack
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
          let (accu, stack) = stack in
          (run [@ocaml.tailcall]) logger g gas i k ks accu stack
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
          (run [@ocaml.tailcall]) logger g gas i k ks accu stack
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
          (run [@ocaml.tailcall]) logger g gas i k ks accu stack
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
          (run [@ocaml.tailcall]) logger g gas i k ks accu stack
      (* Tickets *)
      | KTicket (_, k) ->
          let contents = accu and (amount, stack) = stack in
          let ticketer = (sc.self, "default") in
          let accu = {ticketer; contents; amount} in
          (run [@ocaml.tailcall]) logger g gas i k ks accu stack
      | KRead_ticket (_, k) ->
          let {ticketer; contents; amount} = accu in
          let stack = (accu, stack) in
          let accu = (ticketer, (contents, amount)) in
          (run [@ocaml.tailcall]) logger g gas i k ks accu stack
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
          (run [@ocaml.tailcall]) logger g gas i k ks result stack
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
          (run [@ocaml.tailcall]) logger g gas i k ks result stack )

and unpack :
    type a.
    context -> ty:a ty -> bytes:bytes -> (a option * context) tzresult Lwt.t =
 fun ctxt ~ty ~bytes ->
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

(* FIXME: This ugly function will disappear when elaboration is ready. *)
and step_descr :
    type b a.
    bool ->
    logger option ->
    context * step_constants ->
    (b, a) descr ->
    b ->
    (a * context) tzresult Lwt.t =
 fun log_now logger g descr stack ->
  (* FIXME: That's ugly but this is only temporary. *)
  let (KDescr {kinstr} as kdescr) = translate descr in
  ( if log_now then
    match logger with
    | None ->
        ()
    | Some logger ->
        let module Log = (val logger) in
        let kinfo = kinfo_of_kinstr kinstr in
        let ctxt = fst g in
        Log.log_interp kinstr ctxt kinfo.kloc descr.bef stack ) ;
  run_descr logger g kdescr stack

and interp :
    type p r.
    logger option ->
    context * step_constants ->
    (p, r) lambda ->
    p ->
    (r * context) tzresult Lwt.t =
 fun logger g (Lam (code, _)) arg ->
  let stack = (arg, ()) in
  step_descr true logger g code stack >|=? fun ((ret, ()), ctxt) -> (ret, ctxt)

let kstep logger ctxt step_constants kinstr accu stack =
  let gas = (Gas.gas_counter ctxt :> int) in
  step logger (outdated ctxt, step_constants) gas kinstr KNil accu stack
  >>=? fun (accu, stack, ctxt, gas) ->
  return (accu, stack, update_context gas ctxt)

let step logger ctxt step_constants descr stack =
  step_descr false logger (ctxt, step_constants) descr stack

(*

   High-level functions
   ====================

*)
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
    (interp logger (ctxt, step_constants) code (arg, storage))
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
