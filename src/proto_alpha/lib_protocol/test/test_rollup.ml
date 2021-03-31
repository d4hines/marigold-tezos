(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Gabriel Alfour <gabriel.alfour@gmail.com>              *)
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

let test msg f = Test_services.tztest msg `Quick f

let (let*) x f = x >>=? f
let (!*) i = Incremental.alpha_ctxt i
let (let**) x f = x >>= fun x -> Lwt.return @@ Environment.wrap_tzresult x >>=? f

module ProtocolCounter = Protocol.Rollup_apply.Counter_rollup
module AlphaRollup = Protocol.Alpha_context.Rollup
module LibCounter = Tezos_rollup_alpha.Counter

let assert_rollup_applied : Protocol.operation_receipt -> (_ -> _ tzresult Lwt.t) -> _ = fun x f ->
  match x with
  | No_operation_metadata -> failwith "operation has no result"
  | Operation_metadata { contents  } -> (
      match contents with
      | Cons_result (_, _) -> failwith "operation has more than one result"
      | Single_result result -> (
          match result with
          | Manager_operation_result manager_result -> (
              match manager_result.operation_result with
              | Applied successful_result -> (
                  match successful_result with
                  | Rollup_result rollup_result -> f rollup_result
                  | _ -> failwith "operation result was not a rollup result"
                )
              | _ -> failwith "operation was not applied"
            )
          | _ -> failwith "operation is not a manager operation"
        )
    )

let assert_fail x f =
  x >>= function
  | Ok _ -> failwith "assert_fail: succeeded"
  | Error x -> (
      let aux (type a) (x : a option) (y : a) =
        match x with
        | Some x -> Some x
        | None -> Some y
      in
      let x' =
        TzTrace.fold aux None x
      in
      match x' with
      | Some x' -> f x'
      | None -> failwith "failed without error"
    )

let assert_raise x f =
  try (
    let _ = x () in
    raise (Failure "assert_raise: succeeded")
  ) with e -> f e

let assert_fail_proto x f =
  assert_fail x @@ function
  | Environment.Ecoproto_error x -> f x
  | _ -> failwith "assert_fail_proto: not a proto error"

let check condition msg =
  if condition then return () else failwith msg

let bake i =
  let* b = Incremental.finalize_block i in
  let* b = Block.bake b in
  let* i = Incremental.begin_construction b in
  return i

let bootstrap2 () =
  let* (bl , bootstrap_contracts) = Context.init 10 in
  let a =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth bootstrap_contracts 0
  in
  let b =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth bootstrap_contracts 1
  in
  let* i = Incremental.begin_construction bl in
  return ((a , b) , i)

open Tezos_rollup_alpha.Counter

let bls_bootstrap2 () = S.Dev.(create_account () , create_account ())

let create_counter_rollup ~rollup_operator i =
  let* i =
    let source = rollup_operator in
    let kind = AlphaRollup.Counter in
    let* op = Op.Rollup.creation ~source ~kind (I i) in
    let* i = Incremental.add_operation i op in
    return i
  in
  let* rollup_id =
    let* result = Incremental.get_last_operation_result i in
    assert_rollup_applied result @@ function
    | Rollup_creation_result { rollup_id ; _ } -> return rollup_id
    | _ -> failwith "expected a rollup creation result"
  in
  return (rollup_id , i)

let alter_block_commitment (alter : [`Root | `Signature | `None]) :
  R.Block_commitment.t -> R.Block_commitment.t = fun bc ->
  let alter_micro_block_commitment alter :
    R.Block_commitment.micro -> R.Block_commitment.micro = fun mbc ->
    match alter with
    | `Root -> (
        let Root hash = mbc.after_root in
        let after_root = R.Root (Bytes.(cat hash @@ of_string "toto")) in
        { mbc with after_root }
      )
    | `Signature -> (
        let parameter = Data_encoding.Binary.of_bytes_exn ProtocolCounter.Parameter.encoding mbc.parameter in
        let Signature s = parameter.aggregated_signature in        
        let aggregated_signature = R.Signature.(Signature (Environment.Bls12_381.G2.(add one s))) in
        let parameter = { parameter with aggregated_signature } in
        let parameter = Data_encoding.Binary.to_bytes_exn ProtocolCounter.Parameter.encoding parameter in
        { mbc with parameter }
      )
    | `None -> mbc
  in
  match bc.micro_block_commitments with
  | [ mbc ] -> { bc with micro_block_commitments = [ alter_micro_block_commitment alter mbc ] }
  | _ -> raise (Failure "expected only one micro block")


let basic_commit ?(alter=`None) ~accounts i o_ctxt =
  let (bls_a , bls_b) = accounts in
  (* Operate some operations Offchain *)
  let o_ctxt =
    let op_a = Client.sign_add_int o_ctxt ~account:bls_a 42 in
    let op_b = Client.sign_add_int o_ctxt ~account:bls_b (-23) in
    let o_ctxt = Operator.process_signed_operation o_ctxt op_a in
    let o_ctxt = Operator.process_signed_operation o_ctxt op_b in
    o_ctxt
  in
  (* Commit the result Onchain *)
  let* (i , o_ctxt) =
    let (block , o_ctxt) = Operator.finish_block o_ctxt in
    let bc = Operator.commit_block o_ctxt block in
    let bc = alter_block_commitment alter bc in
    let* op = Op.Rollup.block_commitment (I i) ~source:o_ctxt.operator bc in
    let* i = Incremental.add_operation i op in
    return (i , o_ctxt)
  in
  (* Need to bake, rollups can not post multiple commitments in the same block *)
  let* block_commitment =
    let* result = Incremental.get_last_operation_result i in
    assert_rollup_applied result @@ function
    | Block_commitment_result { commitment ; level ; _ } -> return (commitment , level)
    | _ -> failwith "expected a rollup block commitment result"
  in
  let* i = bake i in
  return (i , o_ctxt , block_commitment)


let counter_good = test "counter-good" @@ fun () ->
  let* ((rollup_operator , _) , i) = bootstrap2 () in

  (* Create Rollup Onchain *)
  let* (rollup_id , i) = create_counter_rollup ~rollup_operator i in
  (* Need to bake, rollups can start only in next block *)
  let* i = bake i in
  (* Create Rollup Offchain *)
  let o_ctxt = Operator.empty ~rollup_id ~operator:rollup_operator in
  let accounts = bls_bootstrap2 () in

  let basic_commit = basic_commit ~accounts in
  (* Do multiple commitments *)
  let* (i , o_ctxt , (bc_a , _)) = basic_commit i o_ctxt in
  let* (i , o_ctxt , (bc_b , _)) = basic_commit i o_ctxt in
  let* (i , o_ctxt , (bc_c , _)) = basic_commit i o_ctxt in
  ignore i ;
  ignore o_ctxt ;
  (* Have a validator check them *)
  let v_ctxt = Validator.empty ~rollup_id in
  let v_ctxt = Validator.process_block_commitment v_ctxt bc_a in
  let v_ctxt = Validator.process_block_commitment v_ctxt bc_b in
  let v_ctxt = Validator.process_block_commitment v_ctxt bc_c in
  (* Manually check final state *)
  let state = Validator.View.get v_ctxt.state in
  (* 3 * (42 - 23) = 57 *)
  assert (Z.(state = (of_int 57))) ;
  return ()


let counter_rejection = test "counter-rejection" @@ fun () ->
  let* ((rollup_operator , rollup_watcher) , i) = bootstrap2 () in
  
  (* Create Rollup Onchain *)
  let* (rollup_id , i) = create_counter_rollup ~rollup_operator i in
  (* Need to bake, rollups can start only in next block *)
  let* i = bake i in
  (* Create Rollup Offchain *)
  let o_ctxt = Operator.empty ~rollup_id ~operator:rollup_operator in
  let accounts = bls_bootstrap2 () in
  
  (* Need to bake, rollups can start only in next block *)
  let* i = bake i in
  
  (* Try to commit bad content *)
  let* () =
    let* (i , _ , (bc , level)) = basic_commit ~accounts ~alter:`Root i o_ctxt in
    (* Have a validator check them *)
    let v_ctxt = Validator.empty ~rollup_id in
    let { micro_block_index ; state_trace } : Validator.bad =
      try (
        let _ = Validator.process_block_commitment v_ctxt bc in
        raise @@ Failure("unexpected success")
      ) with
      | Validator.Bad x -> x
      | exn -> (
          raise exn
          (* raise @@ Failure("unexpected error") *)
        )
    in
    (* Have the validator do the rejection proof *)
    let* () =
      let br = Validator.invalid_state_hash ~micro_block_index ~level v_ctxt state_trace in
      let* op = Op.Rollup.reject_block (I i) ~source:rollup_watcher br in
      let* i = Incremental.add_operation i op in
      let* result = Incremental.get_last_operation_result i in
      assert_rollup_applied result @@ function
      | Micro_block_rejection_result { removed_rollup_block_indices ; _ } -> (
          assert (removed_rollup_block_indices = [ level ]) ;
          return ()
        )
      | _ -> failwith "expected a rollup micro block rejection result"
    in
    return ()
  in
  (* Try to commit bad signatures *)
  let* () =
    let* (i , _ , (bc , level)) = basic_commit ~alter:`Signature ~accounts i o_ctxt in
    (* Have a validator check them *)
    let v_ctxt = Validator.empty ~rollup_id in
    let { micro_block_index ; state_trace } : Validator.bad =
      try (
        let _ = Validator.process_block_commitment v_ctxt bc in
        raise @@ Failure("unexpected success")
      ) with
      | Validator.Bad x -> x
      | exn ->
        raise exn
        (* raise @@ Failure("unexpected error") *)
    in
    (* Have the validator do the rejection proof *)
    let* () =
      let is = Validator.invalid_signature ~micro_block_index ~level v_ctxt state_trace in
      let* op = Op.Rollup.reject_block (I i) ~source:rollup_watcher is in
      let* i = Incremental.add_operation i op in
      let* result = Incremental.get_last_operation_result i in
      assert_rollup_applied result @@ function
      | Micro_block_rejection_result { removed_rollup_block_indices ; _ } -> (
          assert (removed_rollup_block_indices = [ level ]) ;
          return ()
        )
      | _ -> failwith "expected a rollup micro block rejection result"
    in
    return ()
  in

  return ()

let counter_rejection_too_old = test "counter-rejection-too-old" @@ fun () ->
  let* ((rollup_operator , rollup_watcher) , i) = bootstrap2 () in
  
  (* Create Rollup Onchain *)
  let* (rollup_id , i) = create_counter_rollup ~rollup_operator i in
  (* Need to bake, rollups can start only in next block *)
  let* i = bake i in
  (* Create Rollup Offchain *)
  let o_ctxt = Operator.empty ~rollup_id ~operator:rollup_operator in
  let accounts = bls_bootstrap2 () in
  
  (* Need to bake, rollups can start only in next block *)
  let* i = bake i in
  
  (* Commit bad content *)
  let* (i , _ , (bc , level)) = basic_commit ~accounts ~alter:`Root i o_ctxt in
  (* Bake enough tezos blocks so that the rollup block is final *)
  let* i =
    let aux i _ =
      let* i = i in
      bake i
    in
    let lst = List.repeat (Int32.to_int RA.tezos_level_finality) () in
    List.fold_left aux (return i) lst
  in
  (* Have a validator check them *)
  let v_ctxt = Validator.empty ~rollup_id in
  let { micro_block_index ; state_trace } : Validator.bad =
    try (
      let _ = Validator.process_block_commitment v_ctxt bc in
      raise @@ Failure("unexpected success")
    ) with
    | Validator.Bad x -> x
    | exn -> (
        raise exn
        (* raise @@ Failure("unexpected error") *)
      )
  in
  
  (* Have the validator do the rejection proof, but unfortunately too late *)
  let* () =
    let br = Validator.invalid_state_hash ~micro_block_index ~level v_ctxt state_trace in
    let* op = Op.Rollup.reject_block (I i) ~source:rollup_watcher br in
    assert_fail_proto (Incremental.add_operation i op) @@ function
    | RA.Rollup_rejection_too_old_timestamp _
    | RA.Rollup_rejection_too_old_level _ ->
        return ()
    | _ ->
        failwith "expected a rollup rejection too old error"
  in
  return ()


let counter_invalid_rejection = test "counter-invalid-rejection" @@ fun () ->
  let* ((rollup_operator , rollup_watcher) , i) = bootstrap2 () in
  (* Create Rollup Onchain *)
  let* (rollup_id , i) = create_counter_rollup ~rollup_operator i in
  (* Need to bake, rollups can start only in next block *)
  let* i = bake i in
  (* Create Rollup Offchain *)
  let o_ctxt = Operator.empty ~rollup_id ~operator:rollup_operator in
  let accounts = bls_bootstrap2 () in
  (* Need to bake, rollups can start only in next block *)
  let* i = bake i in
  (* Have the validator do a fake rejection proof *)
  let v_ctxt = Validator.empty ~rollup_id in
  (* Fake commit bad content to get the good rejection *)
  let* (micro_block_index , state_trace) =
    let* (_ , _ , (bc , _)) = basic_commit ~accounts ~alter:`Root i o_ctxt in
    (* Have a validator check them *)
    let v_ctxt = Validator.empty ~rollup_id in
    let { micro_block_index ; state_trace } : Validator.bad =
      try (
        let _ = Validator.process_block_commitment v_ctxt bc in
        raise @@ Failure("unexpected success")
      ) with
      | Validator.Bad x -> x
      | exn -> (
          raise exn
          (* raise @@ Failure("unexpected error") *)
        )
    in
    return (micro_block_index , state_trace)
  in
  (* Commit Good content *)
  let* (i , _ , (_ , level)) = basic_commit ~accounts i o_ctxt in
  let test micro_block_index level state_trace f =
    let br = Validator.invalid_state_hash ~micro_block_index ~level v_ctxt state_trace in
    let* op = Op.Rollup.reject_block (I i) ~source:rollup_watcher br in
    assert_fail_proto (Incremental.add_operation i op) f
  in
  (* Invalid micro block index *)
  let* () =
    test (-1) level state_trace @@ function
    | RA.Rollup_invalid_rejection Rollup_bad_micro_block_index -> return ()
    | _ -> failwith "unexpected error"
  in
  let* () =
    test 42 level state_trace @@ function
    | RA.Rollup_invalid_rejection Rollup_bad_micro_block_index -> return ()
    | _ -> failwith "unexpected error"
  in
  (* Actually good hash *)
  let* () =
    test micro_block_index level state_trace @@ function
    | RA.Rollup_invalid_rejection Rollup_valid -> return ()
    | _ -> failwith "unexpected error"
  in
  return ()

let counter_double_inclusion = test "counter-double-inclusion" @@ fun () ->
  let* ((rollup_operator , rollup_watcher) , i) = bootstrap2 () in
  (* Create Rollup Onchain *)
  let* (rollup_id , i) = create_counter_rollup ~rollup_operator i in
  (* Need to bake, rollups can start only in next block *)
  let* i = bake i in
  (* Create Rollup Offchain *)
  let o_ctxt = Operator.empty ~rollup_id ~operator:rollup_operator in
  let accounts = bls_bootstrap2 () in

  let (bls_a , _) = accounts in
  (* Create same operation offchain *)
  let op_a = GenericClient.sign_add_int ~counter:Z.one ~rollup_id ~account:bls_a
      (Client.signer_storage o_ctxt) 42 in
  let op_b = GenericClient.sign_add_int ~counter:Z.one ~rollup_id ~account:bls_a
      (Client.signer_storage o_ctxt) 23 in
  let o_ctxt = Operator.process_signed_operation o_ctxt op_a in
  (* Have the operator fail *)
  let* () =
    assert_raise (fun () -> Operator.process_signed_operation o_ctxt op_b) @@ function
    | RA.Batcher.Invalid_signature -> return ()
    | exn -> raise exn
  in
  (* Include bad operation *)
  let o_ctxt = Operator.Wrong.include_signed_operation o_ctxt op_b in
  (* Commit the result Onchain *)
  let (block , o_ctxt) = Operator.finish_block o_ctxt in
  let bc = Operator.commit_block o_ctxt block in
  let* op = Op.Rollup.block_commitment (I i) ~source:o_ctxt.operator bc in
  let* i = Incremental.add_operation i op in
  let* (bc , level) =
    let* result = Incremental.get_last_operation_result i in
    assert_rollup_applied result @@ function
    | Block_commitment_result { commitment ; level ; _ } -> return (commitment , level)
    | _ -> failwith "expected a rollup block commitment result"
  in

  (* Need to bake, rollups can not post multiple commitments in the same block *)
  let* i = bake i in
  (* Have a validator check them *)
  let v_ctxt = Validator.empty ~rollup_id in
  let { micro_block_index ; state_trace } : Validator.bad =
    try (
      let _ = Validator.process_block_commitment v_ctxt bc in
      raise @@ Failure "unexpected success"
    ) with
    | Validator.Bad x -> x
    (* | _exn -> raise @@ Failure "unexpected error" *)
    | exn -> raise exn
  in
  let br = Validator.invalid ~micro_block_index ~level v_ctxt state_trace in
  let* op = Op.Rollup.reject_block (I i) ~source:rollup_watcher br in
  let* i = Incremental.add_operation i op in
  let* result = Incremental.get_last_operation_result i in
  let* () =
    assert_rollup_applied result @@ function
    | Micro_block_rejection_result _ -> return ()
    | _ -> failwith "unexpected result"
  in
  
  return ()


let counter_commit_after_rejection = test "counter-commit-after-rejection" @@ fun () ->
  let* ((rollup_operator , rollup_watcher) , i) = bootstrap2 () in
  
  (* Create Rollup Onchain *)
  let* (rollup_id , i) = create_counter_rollup ~rollup_operator i in
  (* Need to bake, rollups can start only in next block *)
  let* i = bake i in
  (* Create Rollup Offchain *)
  let o_ctxt = Operator.empty ~rollup_id ~operator:rollup_operator in
  let accounts = bls_bootstrap2 () in
  
  (* Need to bake, rollups can start only in next block *)
  let* i = bake i in
  
  (* Commit bad content *)
  let* (i , _ , (bc , level)) = basic_commit ~accounts ~alter:`Root i o_ctxt in
  (* Have a validator check them *)
  let v_ctxt = Validator.empty ~rollup_id in
  let { micro_block_index ; state_trace } : Validator.bad =
    try (
      let _ = Validator.process_block_commitment v_ctxt bc in
      raise @@ Failure("unexpected success")
    ) with
    | Validator.Bad x -> x
    | exn -> (
        raise exn
        (* raise @@ Failure("unexpected error") *)
      )
  in
  (* Have the validator do the rejection proof *)
  let br = Validator.invalid_state_hash ~micro_block_index ~level v_ctxt state_trace in
  let* op = Op.Rollup.reject_block (I i) ~source:rollup_watcher br in
  let* i = Incremental.add_operation i op in
  let* result = Incremental.get_last_operation_result i in
  let* () = assert_rollup_applied result @@ function
    | Micro_block_rejection_result { removed_rollup_block_indices ; _ } -> (
        assert (removed_rollup_block_indices = [ level ]) ;
        return ()
      )
    | _ -> failwith "expected a rollup micro block rejection result"
  in
  let* i = bake i in
  (* Resubmit valid block *)
  let* (i , _ , _) = basic_commit ~accounts ~alter:`Root i o_ctxt in
  ignore i ;
  return ()


let counter_prevent_cross_rollup_sign = test "counter-prevent-cross-rollup-sign" @@ fun () ->
  let* ((rollup_operator , rollup_watcher) , i) = bootstrap2 () in
  (* Create Rollup Onchain *)
  let* (rollup_id , i) = create_counter_rollup ~rollup_operator i in
  (* Need to bake, rollups can start only in next block *)
  let* i = bake i in
  (* Create Rollup Offchain *)
  let o_ctxt = Operator.empty ~rollup_id ~operator:rollup_operator in
  let accounts = bls_bootstrap2 () in

  let (bls_a , bls_b) = accounts in
  (* Create same operation offchain *)
  let op_a = GenericClient.sign_add_int ~counter:Z.one ~rollup_id ~account:bls_a
      (Client.signer_storage o_ctxt) 42 in
  let op_b = GenericClient.sign_add_int ~counter:Z.one ~rollup_id:(Z.succ rollup_id) ~account:bls_b
      (Client.signer_storage o_ctxt) 23 in
  let o_ctxt = Operator.process_signed_operation o_ctxt op_a in
  (* Have the operator fail *)
  let* () =
    assert_raise (fun () -> Operator.process_signed_operation o_ctxt op_b) @@ function
    | RA.Batcher.Invalid_signature -> return ()
    | exn -> raise exn
  in
  (* Include bad operation *)
  let o_ctxt = Operator.Wrong.include_signed_operation o_ctxt op_b in
  (* Commit the result Onchain *)
  let (block , o_ctxt) = Operator.finish_block o_ctxt in
  let bc = Operator.commit_block o_ctxt block in
  let* op = Op.Rollup.block_commitment (I i) ~source:o_ctxt.operator bc in
  let* i = Incremental.add_operation i op in
  let* (bc , level) =
    let* result = Incremental.get_last_operation_result i in
    assert_rollup_applied result @@ function
    | Block_commitment_result { commitment ; level ; _ } -> return (commitment , level)
    | _ -> failwith "expected a rollup block commitment result"
  in

  (* Need to bake, rollups can not post multiple commitments in the same block *)
  let* i = bake i in
  (* Have a validator check them *)
  let v_ctxt = Validator.empty ~rollup_id in
  let { micro_block_index ; state_trace } : Validator.bad =
    try (
      let _ = Validator.process_block_commitment v_ctxt bc in
      raise @@ Failure "unexpected success"
    ) with
    | Validator.Bad x -> x
    (* | _exn -> raise @@ Failure "unexpected error" *)
    | exn -> raise exn
  in
  let br = Validator.invalid ~micro_block_index ~level v_ctxt state_trace in
  let* op = Op.Rollup.reject_block (I i) ~source:rollup_watcher br in
  let* i = Incremental.add_operation i op in
  let* result = Incremental.get_last_operation_result i in
  let* () =
    assert_rollup_applied result @@ function
    | Micro_block_rejection_result _ -> return ()
    | _ -> failwith "unexpected result"
  in
  
  return ()


(*
  TODO:
  - account compression for signer (represent accounts as integers)
  - events
  - tx_only rollup
  - rethink all names
  - Test too much gas
  - Test too big leaf
  - Test too much state
  - Meter on chain gas and space
*)


let tests = [
  counter_good ;
  counter_rejection ;
  counter_rejection_too_old ;
  counter_invalid_rejection ;
  counter_double_inclusion ;
  counter_commit_after_rejection ;
  counter_prevent_cross_rollup_sign ;
]
