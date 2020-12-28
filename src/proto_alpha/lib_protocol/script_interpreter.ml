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
      let l = accu in
      Interp_costs.list_iter l
  | KSet_iter _ ->
      let set = accu in
      Interp_costs.set_iter set
  | KSet_mem ->
      let v = accu and (set, _) = stack in
      Interp_costs.set_mem v set
  | KSet_update ->
      let v = accu and (_, (set, _)) = stack in
      Interp_costs.set_update v set
  | KMap_map _ ->
      let map = accu in
      Interp_costs.map_map map
  | KMap_iter _ ->
      let map = accu in
      Interp_costs.map_iter map
  | KMap_mem ->
      let v = accu and (map, _rest) = stack in
      Interp_costs.map_mem v map
  | KMap_get ->
      let v = accu and (map, _rest) = stack in
      Interp_costs.map_get v map
  | KMap_update ->
      let k = accu and (_, (map, _)) = stack in
      Interp_costs.map_update k map
  | KMap_get_and_update ->
      let k = accu and (_, (map, _)) = stack in
      Interp_costs.map_get_and_update k map
  | KBig_map_mem ->
      let key = accu and (map, _) = stack in
      Interp_costs.map_mem key map.diff
  | KBig_map_get ->
      let key = accu and (map, _) = stack in
      Interp_costs.map_get key map.diff
  | KBig_map_update ->
      let key = accu and (_, (map, _)) = stack in
      Interp_costs.map_update key map.diff
  | KBig_map_get_and_update ->
      let key = accu and (_, (map, _)) = stack in
      Interp_costs.map_get_and_update key map.diff
  | KAdd_seconds_to_timestamp ->
      let n = accu and (t, _) = stack in
      Interp_costs.add_seconds_timestamp n t
  | KAdd_timestamp_to_seconds ->
      let t = accu and (n, _) = stack in
      Interp_costs.add_seconds_timestamp n t
  | KSub_timestamp_seconds ->
      let t = accu and (n, _) = stack in
      Interp_costs.sub_seconds_timestamp n t
  | KDiff_timestamps ->
      let t1 = accu and (t2, _) = stack in
      Interp_costs.diff_timestamps t1 t2
  | KConcat_string_pair ->
      let x = accu and (y, _) = stack in
      Interp_costs.concat_string_pair x y
  | KConcat_string ->
      let ss = accu in
      Interp_costs.concat_string_precheck ss
  | KSlice_string ->
      let (_, (s, _)) = stack in
      Interp_costs.slice_string s
  | KConcat_bytes_pair ->
      let x = accu and (y, _) = stack in
      Interp_costs.concat_bytes_pair x y
  | KConcat_bytes ->
      let ss = accu in
      Interp_costs.concat_string_precheck ss
  | KSlice_bytes ->
      let (_, (s, _)) = stack in
      Interp_costs.slice_bytes s
  | KMul_teznat ->
      let (n, _) = stack in
      Interp_costs.mul_teznat n
  | KMul_nattez ->
      let n = accu in
      Interp_costs.mul_teznat n
  | KAbs_int ->
      let x = accu in
      Interp_costs.abs_int x
  | KNeg_int ->
      let x = accu in
      Interp_costs.neg_int x
  | KNeg_nat ->
      let x = accu in
      Interp_costs.neg_nat x
  | KAdd_intint ->
      let x = accu and (y, _) = stack in
      Interp_costs.add_bigint x y
  | KAdd_intnat ->
      let x = accu and (y, _) = stack in
      Interp_costs.add_bigint x y
  | KAdd_natint ->
      let x = accu and (y, _) = stack in
      Interp_costs.add_bigint x y
  | KAdd_natnat ->
      let x = accu and (y, _) = stack in
      Interp_costs.add_bigint x y
  | KSub_int ->
      let x = accu and (y, _) = stack in
      Interp_costs.sub_bigint x y
  | KMul_intint ->
      let x = accu and (y, _) = stack in
      Interp_costs.mul_bigint x y
  | KMul_intnat ->
      let x = accu and (y, _) = stack in
      Interp_costs.mul_bigint x y
  | KMul_natint ->
      let x = accu and (y, _) = stack in
      Interp_costs.mul_bigint x y
  | KMul_natnat ->
      let x = accu and (y, _) = stack in
      Interp_costs.mul_bigint x y
  | KEdiv_teznat ->
      let x = accu and (y, _) = stack in
      Interp_costs.ediv_teznat x y
  | KEdiv_intint ->
      let x = accu and (y, _) = stack in
      Interp_costs.ediv_bigint x y
  | KEdiv_intnat ->
      let x = accu and (y, _) = stack in
      Interp_costs.ediv_bigint x y
  | KEdiv_natint ->
      let x = accu and (y, _) = stack in
      Interp_costs.ediv_bigint x y
  | KEdiv_natnat ->
      let x = accu and (y, _) = stack in
      Interp_costs.ediv_bigint x y
  | KLsl_nat ->
      let x = accu in
      Interp_costs.lsl_nat x
  | KLsr_nat ->
      let x = accu in
      Interp_costs.lsr_nat x
  | KOr_nat ->
      let x = accu and (y, _) = stack in
      Interp_costs.or_nat x y
  | KAnd_nat ->
      let x = accu and (y, _) = stack in
      Interp_costs.and_nat x y
  | KAnd_int_nat ->
      let x = accu and (y, _) = stack in
      Interp_costs.and_nat x y
  | KXor_nat ->
      let x = accu and (y, _) = stack in
      Interp_costs.xor_nat x y
  | KNot_int ->
      let x = accu in
      Interp_costs.not_nat x
  | KNot_nat ->
      let x = accu in
      Interp_costs.not_nat x
  | KCompare ty ->
      let a = accu and (b, _) = stack in
      Interp_costs.compare ty a b
  | KCheck_signature ->
      let key = accu and (_, (message, _)) = stack in
      Interp_costs.check_signature key message
  | KHash_key ->
      let pk = accu in
      Interp_costs.hash_key pk
  | KBlake2b ->
      let bytes = accu in
      Interp_costs.blake2b bytes
  | KSha256 ->
      let bytes = accu in
      Interp_costs.sha256 bytes
  | KSha512 ->
      let bytes = accu in
      Interp_costs.sha512 bytes
  | KKeccak ->
      let bytes = accu in
      Interp_costs.keccak bytes
  | KSha3 ->
      let bytes = accu in
      Interp_costs.sha3 bytes
  | KPairing_check_bls12_381 ->
      let pairs = accu in
      Interp_costs.pairing_check_bls12_381 pairs
  | KSapling_verify_update ->
      let tx = accu in
      let inputs = List.length tx.inputs in
      let outputs = List.length tx.outputs in
      Interp_costs.sapling_verify_update ~inputs ~outputs
  | KSplit_ticket ->
      let ticket = accu and ((amount_a, amount_b), _) = stack in
      Interp_costs.split_ticket ticket.amount amount_a amount_b
  | KJoin_tickets ty ->
      let (ticket_a, ticket_b) = accu in
      Interp_costs.join_tickets ty ticket_a ticket_b
  | KDrop ->
      Interp_costs.drop
  | KDup ->
      Interp_costs.dup
  | KSwap ->
      Interp_costs.swap
  | KConst _ ->
      Interp_costs.push
  | KCons_some ->
      Interp_costs.cons_some
  | KCons_none _ ->
      Interp_costs.cons_none
  | KIf_none _ ->
      Interp_costs.if_none
  | KCons_pair ->
      Interp_costs.cons_pair
  | KUnpair ->
      Interp_costs.unpair
  | KCar ->
      Interp_costs.car
  | KCdr ->
      Interp_costs.cdr
  | KCons_left ->
      Interp_costs.cons_left
  | KCons_right ->
      Interp_costs.cons_right
  | KIf_left _ ->
      Interp_costs.if_left
  | KCons_list ->
      Interp_costs.cons_list
  | KNil ->
      Interp_costs.nil
  | KIf_cons _ ->
      Interp_costs.if_cons
  | KList_size ->
      Interp_costs.list_size
  | KEmpty_set _ ->
      Interp_costs.empty_set
  | KSet_size ->
      Interp_costs.set_size
  | KEmpty_map _ ->
      Interp_costs.empty_map
  | KMap_size ->
      Interp_costs.map_size
  | KEmpty_big_map _ ->
      Interp_costs.empty_map
  | KString_size ->
      Interp_costs.string_size
  | KBytes_size ->
      Interp_costs.bytes_size
  | KAdd_tez ->
      Interp_costs.add_tez
  | KSub_tez ->
      Interp_costs.sub_tez
  | KOr ->
      Interp_costs.bool_or
  | KAnd ->
      Interp_costs.bool_and
  | KXor ->
      Interp_costs.bool_xor
  | KNot ->
      Interp_costs.bool_not
  | KIs_nat ->
      Interp_costs.is_nat
  | KInt_nat ->
      Interp_costs.int_nat
  | KInt_bls12_381_fr ->
      Interp_costs.int_bls12_381_fr
  | KEdiv_tez ->
      Interp_costs.ediv_tez
  | KIf _ ->
      Interp_costs.if_
  | KLoop _ ->
      Interp_costs.loop
  | KLoop_left _ ->
      Interp_costs.loop_left
  | KDip _ ->
      Interp_costs.dip
  | KExec ->
      Interp_costs.exec
  | KApply _ ->
      Interp_costs.apply
  | KLambda _ ->
      Interp_costs.push
  | KFailwith _ ->
      Gas.free
  | KNop ->
      Interp_costs.nop
  | KEq ->
      Interp_costs.neq
  | KNeq ->
      Interp_costs.neq
  | KLt ->
      Interp_costs.neq
  | KLe ->
      Interp_costs.neq
  | KGt ->
      Interp_costs.neq
  | KGe ->
      Interp_costs.neq
  | KPack _ ->
      Gas.free
  | KUnpack _ ->
      Gas.free
  | KAddress ->
      Interp_costs.address
  | KContract _ ->
      Interp_costs.contract
  | KTransfer_tokens ->
      Interp_costs.transfer_tokens
  | KImplicit_account ->
      Interp_costs.implicit_account
  | KSet_delegate ->
      Interp_costs.set_delegate
  | KBalance ->
      Interp_costs.balance
  | KLevel ->
      Interp_costs.level
  | KNow ->
      Interp_costs.now
  | KSapling_empty_state _ ->
      Interp_costs.sapling_empty_state
  | KSource ->
      Interp_costs.source
  | KSender ->
      Interp_costs.source
  | KSelf _ ->
      Interp_costs.self
  | KSelf_address ->
      Interp_costs.self
  | KAmount ->
      Interp_costs.amount
  | KDig (n, _) ->
      Interp_costs.dign n
  | KDug (n, _) ->
      Interp_costs.dugn n
  | KDipn (n, _, _) ->
      Interp_costs.dipn n
  | KDropn (n, _) ->
      Interp_costs.dropn n
  | KChainId ->
      Interp_costs.chain_id
  | KCreate_contract _ ->
      Interp_costs.create_contract
  | KNever ->
      Gas.free
  | KVoting_power ->
      Interp_costs.voting_power
  | KTotal_voting_power ->
      Interp_costs.total_voting_power
  | KAdd_bls12_381_g1 ->
      Interp_costs.add_bls12_381_g1
  | KAdd_bls12_381_g2 ->
      Interp_costs.add_bls12_381_g2
  | KAdd_bls12_381_fr ->
      Interp_costs.add_bls12_381_fr
  | KMul_bls12_381_g1 ->
      Interp_costs.mul_bls12_381_g1
  | KMul_bls12_381_g2 ->
      Interp_costs.mul_bls12_381_g2
  | KMul_bls12_381_fr ->
      Interp_costs.mul_bls12_381_fr
  | KNeg_bls12_381_g1 ->
      Interp_costs.neg_bls12_381_g1
  | KNeg_bls12_381_g2 ->
      Interp_costs.neg_bls12_381_g2
  | KNeg_bls12_381_fr ->
      Interp_costs.neg_bls12_381_fr
  | KMul_bls12_381_fr_z ->
      Interp_costs.mul_bls12_381_fr_z
  | KMul_bls12_381_z_fr ->
      Interp_costs.mul_bls12_381_fr_z
  | KDup_n (n, _) ->
      Interp_costs.dupn n
  | KComb (n, _) ->
      Interp_costs.comb n
  | KUncomb (n, _) ->
      Interp_costs.uncomb n
  | KComb_get (n, _) ->
      Interp_costs.comb_get n
  | KComb_set (n, _) ->
      Interp_costs.comb_set n
  | KTicket ->
      Interp_costs.ticket
  | KRead_ticket ->
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
  ('a, 's, 'b, 'f) knext ->
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
  let kinfo = kinfo_of_knext k in
  let ctxt = update_context gas ctxt in
  Log.log_entry k ctxt kinfo.iloc kinfo.kstack_ty (accu, stack)

let log_exit (logger : logger) ctxt gas kprev k accu stack =
  let module Log = (val logger) in
  let ctxt = update_context gas ctxt in
  let kinfo_prev = kinfo_of_knext kprev and kinfo = kinfo_of_knext k in
  Log.log_exit k ctxt kinfo_prev.iloc kinfo.kstack_ty (accu, stack)

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

  - [KNil] represents the end of execution.

  - [KCons] provides the next continuation to execute.

  - [KReturn] implements stack restoration after a function call
    performed by [KExec].

  - [KUndip] is decidated to [KDip]. This allows the stack prefix to
    be restored after the execution of the [Dip]'s body.

  - Thanks to [KLoop_in] and [KLoop_in_left], loops have a special
    treatment.  The same control stack is reused during each iteration.
    This avoids the reallocation of a control stack cell at each iteration.
    A similar remark applies to higher-order iterators over lists and
    maps (i.e. [KList_*] and [KMap_*]).

*)
type (_, _, _, _) konts =
  | KNil : ('r, 'f, 'r, 'f) konts
  | KCons :
      ('a, 's, 'b, 't) knext * ('b, 't, 'r, 'f) konts
      -> ('a, 's, 'r, 'f) konts
  | KReturn : 's * ('a, 's, 'r, 'f) konts -> ('a, end_of_stack, 'r, 'f) konts
  | KUndip : 'b * ('b, 'a * 's, 'r, 'f) konts -> ('a, 's, 'r, 'f) konts
  | KLoop_in :
      ('a, 's, bool, 'a * 's) knext * ('a, 's, 'r, 'f) konts
      -> (bool, 'a * 's, 'r, 'f) konts
  | KLoop_in_left :
      ('a, 's, ('a, 'b) union, 's) knext * ('b, 's, 'r, 'f) konts
      -> (('a, 'b) union, 's, 'r, 'f) konts
  | KIter :
      ('a, 'b * 's, 'b, 's) knext * 'a list * ('b, 's, 'r, 'f) konts
      -> ('b, 's, 'r, 'f) konts
  | KList_mapping :
      ('a, 'c * 's, 'b, 'c * 's) knext
      * 'a list
      * 'b list
      * int
      * ('b boxed_list, 'c * 's, 'r, 'f) konts
      -> ('c, 's, 'r, 'f) konts
  | KList_mapped :
      ('a, 'c * 's, 'b, 'c * 's) knext
      * 'a list
      * 'b list
      * int
      * ('b boxed_list, 'c * 's, 'r, 'f) konts
      -> ('b, 'c * 's, 'r, 'f) konts
  | KMap_mapping :
      ('a * 'b, 'd * 's, 'c, 'd * 's) knext
      * ('a * 'b) list
      * ('a, 'c) map
      * (('a, 'c) map, 'd * 's, 'r, 'f) konts
      -> ('d, 's, 'r, 'f) konts
  | KMap_mapped :
      ('a * 'b, 'd * 's, 'c, 'd * 's) knext
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
    type a s b t c u d w result.
    (a -> s -> (b * t) * result) ->
    (a, s, b, t, c, u, d, w) stack_prefix_preservation_witness ->
    c ->
    u ->
    (d * w) * result =
 fun f n accu stk ->
  match (n, stk) with
  | (KPrefix (_, n), rest) ->
      interp_stack_prefix_preserving_operation f n (fst rest) (snd rest)
      |> fun ((v, rest'), result) -> ((accu, (v, rest')), result)
  | (KRest, v) ->
      f accu v

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
    type a s r f.
    logger option ->
    context ->
    step_constants ->
    (a, s, r, f) kdescr ->
    a ->
    s ->
    (r * f * context) tzresult Lwt.t =
 fun logger ctxt step_constants descr accu stack ->
  let gas = (Gas.gas_counter ctxt :> int) in
  step logger (outdated ctxt) step_constants gas descr.kinstr KNil accu stack
  >>=? fun (accu, stack, ctxt, gas) ->
  return (accu, stack, update_context gas ctxt)

and run :
    type a a' s s' b t b' t' r f.
    logger option ->
    outdated_context ->
    step_constants ->
    local_gas_counter ->
    (a', s', b', t') knext ->
    (a, s, b, t) knext ->
    (b, t, r, f) konts ->
    a ->
    s ->
    (r * f * outdated_context * local_gas_counter) tzresult Lwt.t =
 fun logger ctxt step_constants gas k k' ks accu stack ->
  ( match logger with
  | None ->
      ()
  | Some logger ->
      log_exit logger ctxt gas k k' accu stack ) ;
  (step [@ocaml.tailcall]) logger ctxt step_constants gas k' ks accu stack
 [@@inline.always]

and next :
    type a s r f.
    logger option ->
    outdated_context ->
    step_constants ->
    local_gas_counter ->
    (a, s, r, f) konts ->
    a ->
    s ->
    (r * f * outdated_context * local_gas_counter) tzresult Lwt.t =
 fun logger ctxt sc gas ks accu stack ->
  match ks with
  | KNil ->
      Lwt.return (Ok (accu, stack, ctxt, gas))
  | KCons (k, ks) ->
      (step [@ocaml.tailcall]) logger ctxt sc gas k ks accu stack
  | KReturn (stack', ks) ->
      next logger ctxt sc gas ks accu stack'
  | KLoop_in (ki, ks') ->
      let (accu', stack') = stack in
      if accu then
        (step [@ocaml.tailcall]) logger ctxt sc gas ki ks accu' stack'
      else (next [@ocaml.tailcall]) logger ctxt sc gas ks' accu' stack'
  | KLoop_in_left (ki, ks') -> (
    match accu with
    | L v ->
        (step [@ocaml.tailcall]) logger ctxt sc gas ki ks v stack
    | R v ->
        (next [@ocaml.tailcall]) logger ctxt sc gas ks' v stack )
  | KUndip (x, ks) ->
      next logger ctxt sc gas ks x (accu, stack)
  | KIter (body, xs, ks) -> (
    match xs with
    | [] ->
        next logger ctxt sc gas ks accu stack
    | x :: xs ->
        let ks = KIter (body, xs, ks) in
        (step [@ocaml.tailcall]) logger ctxt sc gas body ks x (accu, stack) )
  | KList_mapping (body, xs, ys, len, ks) -> (
    match xs with
    | [] ->
        let ys = {elements = List.rev ys; length = len} in
        next logger ctxt sc gas ks ys (accu, stack)
    | x :: xs ->
        let ks = KList_mapped (body, xs, ys, len, ks) in
        (step [@ocaml.tailcall]) logger ctxt sc gas body ks x (accu, stack) )
  | KList_mapped (body, xs, ys, len, ks) ->
      let ks = KList_mapping (body, xs, accu :: ys, len, ks) in
      let (accu, stack) = stack in
      next logger ctxt sc gas ks accu stack
  | KMap_mapping (body, xs, ys, ks) -> (
    match xs with
    | [] ->
        next logger ctxt sc gas ks ys (accu, stack)
    | (xk, xv) :: xs ->
        let ks = KMap_mapped (body, xs, ys, xk, ks) in
        let res = (xk, xv) in
        let stack = (accu, stack) in
        (step [@ocaml.tailcall]) logger ctxt sc gas body ks res stack )
  | KMap_mapped (body, xs, ys, yk, ks) ->
      let ys = map_update yk (Some accu) ys in
      let ks = KMap_mapping (body, xs, ys, ks) in
      let (accu, stack) = stack in
      next logger ctxt sc gas ks accu stack

and step_instr :
    type a s b t b' t' r f.
    logger option ->
    outdated_context ->
    step_constants ->
    local_gas_counter ->
    (a, s) kinfo ->
    (a, s, b, t) kinstr ->
    (* TODO: this seems weird*)
    (a, s, b', t') knext ->
    (b, t, b', t') knext ->
    (b', t', r, f) konts ->
    a ->
    s ->
    (r * f * outdated_context * local_gas_counter) tzresult Lwt.t =
 fun logger ctxt sc gas kinfo i i_k k ks accu stack ->
  match consume gas i accu stack with
  | None ->
      Lwt.return (Gas.gas_exhausted_error (update_context gas ctxt))
  | Some gas -> (
      ( match logger with
      | None ->
          ()
      | Some logger ->
          (* TODO: fix this *)
          log_entry logger ctxt gas i_k accu stack ) ;
      match i with
      (* stack ops *)
      | KDrop ->
          let (accu, stack) = stack in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
      | KDup ->
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu (accu, stack)
      | KSwap ->
          let (top, stack) = stack in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks top (accu, stack)
      | KConst v ->
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks v (accu, stack)
      (* options *)
      | KCons_some ->
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks (Some accu) stack
      | KCons_none _ ->
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks None (accu, stack)
      | KIf_none (bt, bf) -> (
        match accu with
        | None ->
            let (accu, stack) = stack in
            (run [@ocaml.tailcall])
              logger
              ctxt
              sc
              gas
              i_k
              bt
              (KCons (k, ks))
              accu
              stack
        | Some v ->
            (run [@ocaml.tailcall])
              logger
              ctxt
              sc
              gas
              i_k
              bf
              (KCons (k, ks))
              v
              stack )
      (* pairs *)
      | KCons_pair ->
          let (b, stack) = stack in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks (accu, b) stack
      | KUnpair ->
          let (a, b) = accu in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks a (b, stack)
      | KCar ->
          let (a, _) = accu in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks a stack
      | KCdr ->
          let (_, b) = accu in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks b stack
      (* unions *)
      | KCons_left ->
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks (L accu) stack
      | KCons_right ->
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks (R accu) stack
      | KIf_left (bl, br) -> (
        match accu with
        | L v ->
            (run [@ocaml.tailcall])
              logger
              ctxt
              sc
              gas
              i_k
              bl
              (KCons (k, ks))
              v
              stack
        | R v ->
            (run [@ocaml.tailcall])
              logger
              ctxt
              sc
              gas
              i_k
              br
              (KCons (k, ks))
              v
              stack )
      (* lists *)
      | KCons_list ->
          let (tl, stack) = stack in
          let accu = list_cons accu tl in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
      | KNil ->
          let stack = (accu, stack) in
          let accu = list_empty in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
      | KIf_cons (bc, bn) -> (
        match accu.elements with
        | [] ->
            let (accu, stack) = stack in
            (run [@ocaml.tailcall])
              logger
              ctxt
              sc
              gas
              i_k
              bn
              (KCons (k, ks))
              accu
              stack
        | hd :: tl ->
            let tl = {elements = tl; length = accu.length - 1} in
            (run [@ocaml.tailcall])
              logger
              ctxt
              sc
              gas
              i_k
              bc
              (KCons (k, ks))
              hd
              (tl, stack) )
      | KList_map body ->
          let xs = accu.elements in
          let ys = [] in
          let len = accu.length in
          let ks = KList_mapping (body, xs, ys, len, KCons (k, ks)) in
          let (accu, stack) = stack in
          (next [@ocaml.tailcall]) logger ctxt sc gas ks accu stack
      | KList_size ->
          let list = accu in
          let len = Script_int.(abs (of_int list.length)) in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks len stack
      | KList_iter body ->
          let xs = accu.elements in
          let ks = KIter (body, xs, KCons (k, ks)) in
          let (accu, stack) = stack in
          (next [@ocaml.tailcall]) logger ctxt sc gas ks accu stack
      (* sets *)
      | KEmpty_set ty ->
          let res = empty_set ty in
          let stack = (accu, stack) in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KSet_iter body ->
          let set = accu in
          let l = List.rev (set_fold (fun e acc -> e :: acc) set []) in
          let ks = KIter (body, l, KCons (k, ks)) in
          let (accu, stack) = stack in
          (next [@ocaml.tailcall]) logger ctxt sc gas ks accu stack
      | KSet_mem ->
          let (set, stack) = stack in
          let res = set_mem accu set in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KSet_update ->
          let (presence, (set, stack)) = stack in
          let res = set_update accu presence set in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KSet_size ->
          let res = set_size accu in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      (* maps *)
      | KEmpty_map (ty, _) ->
          let res = empty_map ty and stack = (accu, stack) in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KMap_map body ->
          let map = accu in
          let xs = List.rev (map_fold (fun k v a -> (k, v) :: a) map []) in
          let ys = empty_map (map_key_ty map) in
          let ks = KMap_mapping (body, xs, ys, KCons (k, ks)) in
          let (accu, stack) = stack in
          (next [@ocaml.tailcall]) logger ctxt sc gas ks accu stack
      | KMap_iter body ->
          let map = accu in
          let l = List.rev (map_fold (fun k v a -> (k, v) :: a) map []) in
          let ks = KIter (body, l, KCons (k, ks)) in
          let (accu, stack) = stack in
          (next [@ocaml.tailcall]) logger ctxt sc gas ks accu stack
      | KMap_mem ->
          let (map, stack) = stack in
          let res = map_mem accu map in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KMap_get ->
          let (map, stack) = stack in
          let res = map_get accu map in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KMap_update ->
          let (v, (map, stack)) = stack in
          let key = accu in
          let res = map_update key v map in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KMap_get_and_update ->
          let key = accu in
          let (v, (map, rest)) = stack in
          let map' = map_update key v map in
          let v' = map_get key map in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks v' (map', rest)
      | KMap_size ->
          let res = map_size accu in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      (* Big map operations *)
      | KEmpty_big_map (tk, tv) ->
          let ebm = Script_ir_translator.empty_big_map tk tv in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks ebm (accu, stack)
      | KBig_map_mem ->
          let (map, stack) = stack in
          let key = accu in
          ( use_gas_counter_in_ctxt ctxt gas
          @@ fun ctxt -> Script_ir_translator.big_map_mem ctxt key map )
          >>=? fun (res, ctxt, gas) ->
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KBig_map_get ->
          let (map, stack) = stack in
          let key = accu in
          ( use_gas_counter_in_ctxt ctxt gas
          @@ fun ctxt -> Script_ir_translator.big_map_get ctxt key map )
          >>=? fun (res, ctxt, gas) ->
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KBig_map_update ->
          let key = accu in
          let (maybe_value, (map, stack)) = stack in
          let big_map =
            Script_ir_translator.big_map_update key maybe_value map
          in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks big_map stack
      | KBig_map_get_and_update ->
          let key = accu in
          let (v, (map, stack)) = stack in
          let map' = Script_ir_translator.big_map_update key v map in
          ( use_gas_counter_in_ctxt ctxt gas
          @@ fun ctxt -> Script_ir_translator.big_map_get ctxt key map )
          >>=? fun (v', ctxt, gas) ->
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks v' (map', stack)
      (* timestamp operations *)
      | KAdd_seconds_to_timestamp ->
          let n = accu in
          let (t, stack) = stack in
          let result = Script_timestamp.add_delta t n in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks result stack
      | KAdd_timestamp_to_seconds ->
          let t = accu in
          let (n, stack) = stack in
          let result = Script_timestamp.add_delta t n in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks result stack
      | KSub_timestamp_seconds ->
          let t = accu in
          let (s, stack) = stack in
          let result = Script_timestamp.sub_delta t s in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks result stack
      | KDiff_timestamps ->
          let t1 = accu in
          let (t2, stack) = stack in
          let result = Script_timestamp.diff t1 t2 in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks result stack
      (* string operations *)
      | KConcat_string_pair ->
          let x = accu in
          let (y, stack) = stack in
          let s = String.concat "" [x; y] in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks s stack
      | KConcat_string ->
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
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks s stack
      | KSlice_string ->
          let offset = accu and (length, (s, stack)) = stack in
          let s_length = Z.of_int (String.length s) in
          let offset = Script_int.to_zint offset in
          let length = Script_int.to_zint length in
          if Compare.Z.(offset < s_length && Z.add offset length <= s_length)
          then
            let s = String.sub s (Z.to_int offset) (Z.to_int length) in
            (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks (Some s) stack
          else (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks None stack
      | KString_size ->
          let s = accu in
          let result = Script_int.(abs (of_int (String.length s))) in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks result stack
      (* bytes operations *)
      | KConcat_bytes_pair ->
          let x = accu in
          let (y, stack) = stack in
          let s = Bytes.cat x y in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks s stack
      | KConcat_bytes ->
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
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks s stack
      | KSlice_bytes ->
          let offset = accu and (length, (s, stack)) = stack in
          let s_length = Z.of_int (Bytes.length s) in
          let offset = Script_int.to_zint offset in
          let length = Script_int.to_zint length in
          if Compare.Z.(offset < s_length && Z.add offset length <= s_length)
          then
            let s = Bytes.sub s (Z.to_int offset) (Z.to_int length) in
            (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks (Some s) stack
          else (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks None stack
      | KBytes_size ->
          let s = accu in
          let result = Script_int.(abs (of_int (Bytes.length s))) in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks result stack
      (* currency operations *)
      | KAdd_tez ->
          let x = accu in
          let (y, stack) = stack in
          Tez.(x +? y)
          >>?= fun res ->
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KSub_tez ->
          let x = accu in
          let (y, stack) = stack in
          Tez.(x -? y)
          >>?= fun res ->
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KMul_teznat -> (
          let x = accu in
          let (y, stack) = stack in
          match Script_int.to_int64 y with
          | None ->
              get_log logger >>=? fun log -> fail (Overflow (kinfo.iloc, log))
          | Some y ->
              Tez.(x *? y)
              >>?= fun res ->
              (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack )
      | KMul_nattez -> (
          let y = accu in
          let (x, stack) = stack in
          match Script_int.to_int64 y with
          | None ->
              get_log logger >>=? fun log -> fail (Overflow (kinfo.iloc, log))
          | Some y ->
              Tez.(x *? y)
              >>?= fun res ->
              (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack )
      (* boolean operations *)
      | KOr ->
          let x = accu in
          let (y, stack) = stack in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks (x || y) stack
      | KAnd ->
          let x = accu in
          let (y, stack) = stack in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks (x && y) stack
      | KXor ->
          let x = accu in
          let (y, stack) = stack in
          let res = Compare.Bool.(x <> y) in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KNot ->
          let x = accu in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks (not x) stack
      (* integer operations *)
      | KIs_nat ->
          let x = accu in
          let res = Script_int.is_nat x in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KAbs_int ->
          let x = accu in
          let res = Script_int.abs x in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KInt_nat ->
          let x = accu in
          let res = Script_int.int x in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KNeg_int ->
          let x = accu in
          let res = Script_int.neg x in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KNeg_nat ->
          let x = accu in
          let res = Script_int.neg x in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KAdd_intint ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.add x y in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KAdd_intnat ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.add x y in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KAdd_natint ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.add x y in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KAdd_natnat ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.add_n x y in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KSub_int ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.sub x y in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KMul_intint ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.mul x y in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KMul_intnat ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.mul x y in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KMul_natint ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.mul x y in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KMul_natnat ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.mul_n x y in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KEdiv_teznat ->
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
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks result stack
      | KEdiv_tez ->
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
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks result stack
      | KEdiv_intint ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.ediv x y in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KEdiv_intnat ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.ediv x y in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KEdiv_natint ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.ediv x y in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KEdiv_natnat ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.ediv_n x y in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KLsl_nat -> (
          let x = accu and (y, stack) = stack in
          match Script_int.shift_left_n x y with
          | None ->
              get_log logger >>=? fun log -> fail (Overflow (kinfo.iloc, log))
          | Some x ->
              (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks x stack )
      | KLsr_nat -> (
          let x = accu and (y, stack) = stack in
          match Script_int.shift_right_n x y with
          | None ->
              get_log logger >>=? fun log -> fail (Overflow (kinfo.iloc, log))
          | Some r ->
              (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks r stack )
      | KOr_nat ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.logor x y in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KAnd_nat ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.logand x y in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KAnd_int_nat ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.logand x y in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KXor_nat ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.logxor x y in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KNot_int ->
          let x = accu in
          let res = Script_int.lognot x in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KNot_nat ->
          let x = accu in
          let res = Script_int.lognot x in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      (* control *)
      | KIf (bt, bf) ->
          let (res, stack) = stack in
          if accu then
            (run [@ocaml.tailcall])
              logger
              ctxt
              sc
              gas
              i_k
              bt
              (KCons (k, ks))
              res
              stack
          else
            (run [@ocaml.tailcall])
              logger
              ctxt
              sc
              gas
              i_k
              bf
              (KCons (k, ks))
              res
              stack
      | KLoop body ->
          let ks = KLoop_in (body, KCons (k, ks)) in
          (next [@ocaml.tailcall]) logger ctxt sc gas ks accu stack
      | KLoop_left bl ->
          let ks = KLoop_in_left (bl, KCons (k, ks)) in
          (next [@ocaml.tailcall]) logger ctxt sc gas ks accu stack
      | KDip b ->
          let ign = accu in
          let ks = KUndip (ign, KCons (k, ks)) in
          let (accu, stack) = stack in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k b ks accu stack
      | KExec ->
          let arg = accu and (code, stack) = stack in
          let (Lam (code, _)) = code in
          let code = code.kinstr in
          let ks = KReturn (stack, KCons (k, ks)) in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k code ks arg ((), ())
      | KApply capture_ty -> (
          let capture = accu in
          let (lam, stack) = stack in
          let (Lam (descr, expr)) = lam in
          let (Item_t (full_arg_ty, _, _)) = descr.kbef in
          let ctxt = update_context gas ctxt in
          unparse_data ctxt Optimized capture_ty capture
          >>=? fun (const_expr, ctxt) ->
          unparse_ty ctxt capture_ty
          >>?= fun (ty_expr, ctxt) ->
          match full_arg_ty with
          | Pair_t ((capture_ty, _, _), (arg_ty, _, _), _) ->
              let arg_stack_ty =
                Item_t (arg_ty, Item_t (Unit_t None, Empty_t, None), None)
              in
              let full_descr =
                {
                  kloc = descr.kloc;
                  kbef = arg_stack_ty;
                  kaft = descr.kaft;
                  kinstr =
                    (let kinfo_const =
                       {iloc = descr.kloc; kstack_ty = arg_stack_ty}
                     in
                     let kinfo_pair =
                       {
                         iloc = descr.kloc;
                         kstack_ty = Item_t (capture_ty, arg_stack_ty, None);
                       }
                     in
                     KNext
                       ( kinfo_const,
                         KConst capture,
                         KNext (kinfo_pair, KCons_pair, descr.kinstr) ));
                }
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
              (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks lam' stack
          | _ ->
              assert false )
      | KLambda lam ->
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks lam (accu, stack)
      | KFailwith (kloc, tv) ->
          let v = accu in
          let ctxt = update_context gas ctxt in
          trace Cannot_serialize_failure (unparse_data ctxt Optimized tv v)
          >>=? fun (v, _ctxt) ->
          let v = Micheline.strip_locations v in
          get_log logger >>=? fun log -> fail (Reject (kloc, v, log))
      | KNop ->
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
      (* comparison *)
      | KCompare ty ->
          let a = accu in
          let (b, stack) = stack in
          let r =
            Script_int.of_int @@ Script_ir_translator.compare_comparable ty a b
          in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks r stack
      (* comparators *)
      | KEq ->
          let cmpres = accu in
          let cmpres = Script_int.compare cmpres Script_int.zero in
          let cmpres = Compare.Int.(cmpres = 0) in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks cmpres stack
      | KNeq ->
          let cmpres = accu in
          let cmpres = Script_int.compare cmpres Script_int.zero in
          let cmpres = Compare.Int.(cmpres <> 0) in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks cmpres stack
      | KLt ->
          let cmpres = accu in
          let cmpres = Script_int.compare cmpres Script_int.zero in
          let cmpres = Compare.Int.(cmpres < 0) in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks cmpres stack
      | KLe ->
          let cmpres = accu in
          let cmpres = Script_int.compare cmpres Script_int.zero in
          let cmpres = Compare.Int.(cmpres <= 0) in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks cmpres stack
      | KGt ->
          let cmpres = accu in
          let cmpres = Script_int.compare cmpres Script_int.zero in
          let cmpres = Compare.Int.(cmpres > 0) in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks cmpres stack
      | KGe ->
          let cmpres = accu in
          let cmpres = Script_int.compare cmpres Script_int.zero in
          let cmpres = Compare.Int.(cmpres >= 0) in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks cmpres stack
      (* packing *)
      | KPack ty ->
          let value = accu in
          ( use_gas_counter_in_ctxt ctxt gas
          @@ fun ctxt -> Script_ir_translator.pack_data ctxt ty value )
          >>=? fun (bytes, ctxt, gas) ->
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks bytes stack
      | KUnpack ty ->
          let bytes = accu in
          ( use_gas_counter_in_ctxt ctxt gas
          @@ fun ctxt -> unpack ctxt ~ty ~bytes )
          >>=? fun (opt, ctxt, gas) ->
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks opt stack
      | KAddress ->
          let (_, address) = accu in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks address stack
      | KContract (t, entrypoint) -> (
          let contract = accu in
          match (contract, entrypoint) with
          | ((contract, "default"), entrypoint)
          | ((contract, entrypoint), "default") ->
              let ctxt = update_context gas ctxt in
              Script_ir_translator.parse_contract_for_script
                ~legacy:false
                ctxt
                kinfo.iloc
                t
                contract
                ~entrypoint
              >>=? fun (ctxt, maybe_contract) ->
              let gas = update_local_gas_counter ctxt in
              let ctxt = outdated ctxt in
              let accu = maybe_contract in
              (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
          | _ ->
              (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks None stack )
      | KTransfer_tokens ->
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
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
      | KImplicit_account ->
          let key = accu in
          let contract = Contract.implicit_contract key in
          let res = (Unit_t None, (contract, "default")) in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KCreate_contract (storage_type, param_type, Lam (_, code), root_name)
        ->
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
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KSet_delegate ->
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
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KBalance ->
          let ctxt = update_context gas ctxt in
          Contract.get_balance_carbonated ctxt sc.self
          >>=? fun (ctxt, balance) ->
          let gas = update_local_gas_counter ctxt in
          let ctxt = outdated ctxt in
          (run [@ocaml.tailcall])
            logger
            ctxt
            sc
            gas
            i_k
            k
            ks
            balance
            (accu, stack)
      | KLevel ->
          let level =
            (Level.current (outdated_context ctxt)).level |> Raw_level.to_int32
            |> Script_int.of_int32 |> Script_int.abs
          in
          (run [@ocaml.tailcall])
            logger
            ctxt
            sc
            gas
            i_k
            k
            ks
            level
            (accu, stack)
      | KNow ->
          let now = Script_timestamp.now (outdated_context ctxt) in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks now (accu, stack)
      | KCheck_signature ->
          let key = accu and (signature, (message, stack)) = stack in
          let res = Signature.check key signature message in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KHash_key ->
          let key = accu in
          let res = Signature.Public_key.hash key in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KBlake2b ->
          let bytes = accu in
          let hash = Raw_hashes.blake2b bytes in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks hash stack
      | KSha256 ->
          let bytes = accu in
          let hash = Raw_hashes.sha256 bytes in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks hash stack
      | KSha512 ->
          let bytes = accu in
          let hash = Raw_hashes.sha512 bytes in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks hash stack
      | KSource ->
          let res = (sc.payer, "default") in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res (accu, stack)
      | KSender ->
          let res = (sc.source, "default") in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res (accu, stack)
      | KSelf (ty, entrypoint) ->
          let res = (ty, (sc.self, entrypoint)) in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res (accu, stack)
      | KSelf_address ->
          let res = (sc.self, "default") in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res (accu, stack)
      | KAmount ->
          let accu = sc.amount and stack = (accu, stack) in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
      | KDig (_n, n') ->
          let ((accu, stack), x) =
            interp_stack_prefix_preserving_operation
              (fun v stack -> (stack, v))
              n'
              accu
              stack
          in
          let accu = x and stack = (accu, stack) in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
      | KDug (_n, n') ->
          let v = accu in
          let (accu, stack) = stack in
          let ((accu, stack), ()) =
            interp_stack_prefix_preserving_operation
              (fun accu stack -> ((v, (accu, stack)), ()))
              n'
              accu
              stack
          in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
      | KDipn (_n, n', b) ->
          (*

              The following function pops n elements from the stack
              and push their reintroduction in the continuations stack.

          *)
          let rec ktransfer :
              type a s e z c u d w.
              (a, s, e, z, c, u, d, w) stack_prefix_preservation_witness ->
              c ->
              u ->
              (d, w, b', t') knext ->
              a * s * (e, z, b', t') knext =
           fun w accu stack k ->
            match w with
            | KPrefix (kinfo, w) ->
                let k = KNext (kinfo, KConst accu, k) in
                let (accu, stack) = stack in
                ktransfer w accu stack k
            | KRest ->
                (accu, stack, k)
          in
          let (accu, stack, restore_prefix) = ktransfer n' accu stack k in
          let ks = KCons (restore_prefix, ks) in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k b ks accu stack
      | KDropn (_n, n') ->
          let stack =
            let rec aux :
                type a s b t.
                (b, t, b, t, a, s, a, s) stack_prefix_preservation_witness ->
                a ->
                s ->
                b * t =
             fun w accu stack ->
              match w with
              | KRest ->
                  (accu, stack)
              | KPrefix (_, w) ->
                  let (accu, stack) = stack in
                  aux w accu stack
            in
            aux n' accu stack
          in
          let (accu, stack) = stack in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
      | KSapling_empty_state memo_size ->
          let state = Sapling.empty_state ~memo_size () in
          (run [@ocaml.tailcall])
            logger
            ctxt
            sc
            gas
            i_k
            k
            ks
            state
            (accu, stack)
      | KSapling_verify_update -> (
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
              (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks state stack
          | None ->
              (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks None stack )
      | KChainId ->
          let accu = sc.chain_id and stack = (accu, stack) in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
      | KNever -> (
        match accu with _ -> . )
      | KVoting_power ->
          let key_hash = accu in
          let ctxt = update_context gas ctxt in
          Vote.get_voting_power ctxt key_hash
          >>=? fun (ctxt, rolls) ->
          let power = Script_int.(abs (of_int32 rolls)) in
          let gas = update_local_gas_counter ctxt in
          let ctxt = outdated ctxt in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks power stack
      | KTotal_voting_power ->
          let ctxt = update_context gas ctxt in
          Vote.get_total_voting_power ctxt
          >>=? fun (ctxt, rolls) ->
          let power = Script_int.(abs (of_int32 rolls)) in
          let gas = update_local_gas_counter ctxt in
          let ctxt = outdated ctxt in
          (run [@ocaml.tailcall])
            logger
            ctxt
            sc
            gas
            i_k
            k
            ks
            power
            (accu, stack)
      | KKeccak ->
          let bytes = accu in
          let hash = Raw_hashes.keccak256 bytes in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks hash stack
      | KSha3 ->
          let bytes = accu in
          let hash = Raw_hashes.sha3_256 bytes in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks hash stack
      | KAdd_bls12_381_g1 ->
          let x = accu and (y, stack) = stack in
          let accu = Bls12_381.G1.add x y in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
      | KAdd_bls12_381_g2 ->
          let x = accu and (y, stack) = stack in
          let accu = Bls12_381.G2.add x y in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
      | KAdd_bls12_381_fr ->
          let x = accu and (y, stack) = stack in
          let accu = Bls12_381.Fr.add x y in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
      | KMul_bls12_381_g1 ->
          let x = accu and (y, stack) = stack in
          let accu = Bls12_381.G1.mul x y in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
      | KMul_bls12_381_g2 ->
          let x = accu and (y, stack) = stack in
          let accu = Bls12_381.G2.mul x y in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
      | KMul_bls12_381_fr ->
          let x = accu and (y, stack) = stack in
          let accu = Bls12_381.Fr.mul x y in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
      | KMul_bls12_381_fr_z ->
          let x = accu and (y, stack) = stack in
          let x = Bls12_381.Fr.of_z (Script_int.to_zint x) in
          let res = Bls12_381.Fr.mul x y in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KMul_bls12_381_z_fr ->
          let y = accu and (x, stack) = stack in
          let x = Bls12_381.Fr.of_z (Script_int.to_zint x) in
          let res = Bls12_381.Fr.mul x y in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KInt_bls12_381_fr ->
          let x = accu in
          let res = Script_int.of_zint (Bls12_381.Fr.to_z x) in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks res stack
      | KNeg_bls12_381_g1 ->
          let x = accu in
          let accu = Bls12_381.G1.negate x in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
      | KNeg_bls12_381_g2 ->
          let x = accu in
          let accu = Bls12_381.G2.negate x in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
      | KNeg_bls12_381_fr ->
          let x = accu in
          let accu = Bls12_381.Fr.negate x in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
      | KPairing_check_bls12_381 ->
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
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks check stack
      | KComb (_, witness) ->
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
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
      | KUncomb (_, witness) ->
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
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
      | KComb_get (_, witness) ->
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
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
      | KComb_set (_, witness) ->
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
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
      | KDup_n (_, witness) ->
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
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
      (* Tickets *)
      | KTicket ->
          let contents = accu and (amount, stack) = stack in
          let ticketer = (sc.self, "default") in
          let accu = {ticketer; contents; amount} in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
      | KRead_ticket ->
          let {ticketer; contents; amount} = accu in
          let stack = (accu, stack) in
          let accu = (ticketer, (contents, amount)) in
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks accu stack
      | KSplit_ticket ->
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
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks result stack
      | KJoin_tickets contents_ty ->
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
          (run [@ocaml.tailcall]) logger ctxt sc gas i_k k ks result stack )

and step :
    type a s b t r f.
    logger option ->
    outdated_context ->
    step_constants ->
    local_gas_counter ->
    (a, s, b, t) knext ->
    (b, t, r, f) konts ->
    a ->
    s ->
    (r * f * outdated_context * local_gas_counter) tzresult Lwt.t =
 fun logger ctxt sc gas knext ks accu stack ->
  match knext with
  | KHalt _ ->
      next logger ctxt sc gas ks accu stack
  | KNext (kinfo, i, next) ->
      (* TODO: @inlined *)
      step_instr logger ctxt sc gas kinfo i knext next ks accu stack

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
    type a s r f.
    bool ->
    logger option ->
    context ->
    step_constants ->
    (a, s, r, f) kdescr ->
    a ->
    s ->
    (r * f * context) tzresult Lwt.t =
 fun log_now logger ctxt step_constants descr accu stack ->
  ( if log_now then
    match logger with
    | None ->
        ()
    | Some logger ->
        let module Log = (val logger) in
        Log.log_interp descr.kinstr ctxt descr.kloc descr.kbef (accu, stack) ) ;
  run_descr logger ctxt step_constants descr accu stack

and interp :
    type p r.
    logger option ->
    context ->
    step_constants ->
    (p, r) lambda ->
    p ->
    (r * context) tzresult Lwt.t =
 fun logger ctxt step_constants (Lam (code, _)) arg ->
  step_descr true logger ctxt step_constants code arg ((), ())
  >|=? fun (ret, _, ctxt) -> (ret, ctxt)

let kstep logger ctxt step_constants kinstr accu stack =
  let gas = (Gas.gas_counter ctxt :> int) in
  step logger (outdated ctxt) step_constants gas kinstr KNil accu stack
  >>=? fun (accu, stack, ctxt, gas) ->
  return (accu, stack, update_context gas ctxt)

let step logger ctxt step_constants descr stack =
  step_descr false logger ctxt step_constants descr stack

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
