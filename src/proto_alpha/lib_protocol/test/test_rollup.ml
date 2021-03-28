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

module Rollup = Protocol.Alpha_context.Rollup

let assert_rollup : Protocol.operation_receipt -> (_ -> _ tzresult Lwt.t) -> _ = fun x f ->
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
    let kind = Rollup.Counter in
    let* op = Op.Rollup.creation ~source ~kind (I i) in
    let* i = Incremental.add_operation i op in
    return i
  in
  let* rollup_id =
    let* result = Incremental.get_last_operation_result i in
    assert_rollup result @@ function
    | Rollup_creation_result { rollup_id ; _ } -> return rollup_id
    | _ -> failwith "expected a rollup creation result"
  in
  return (rollup_id , i)

let alter_block_commitment (alter : [`Root | `Signature]) : R.Block_commitment.t -> R.Block_commitment.t = fun bc ->
  let alter_micro_block_commitment alter : R.Block_commitment.micro -> R.Block_commitment.micro = fun mbc ->
    match alter with
    | `Root -> (
        let Root hash = mbc.after_root in
        let after_root = R.Root (Bytes.(cat hash @@ of_string "toto")) in
        { mbc with after_root }
      )
    | `Signature -> (
        let Signature s = mbc.aggregated_signature in
        let aggregated_signature = R.Signature.(Signature (Environment.Bls12_381.G2.(add one s))) in
        { mbc with aggregated_signature }
      )
  in
  match bc.micro_block_commitments with
  | [ mbc ] -> { bc with micro_block_commitments = [ alter_micro_block_commitment alter mbc ] }
  | _ -> raise (Failure "expected only one micro block")


let counter_good = test "counter-good" @@ fun () ->
  let* ((rollup_operator , _) , i) = bootstrap2 () in

  (* Create Rollup Onchain *)
  let* (rollup_id , i) = create_counter_rollup ~rollup_operator i in
  (* Need to bake, rollups can start only in next block *)
  let* i = bake i in
  (* Create Rollup Offchain *)
  let module Operator = MakeOperator(struct let rollup_id = rollup_id end) in
  let (bls_a , bls_b) = bls_bootstrap2 () in
  
  let basic_commit i =
    (* Operate some operations Offchain *)
    let () =
      let block_level = Operator.get_block_level () in
      let op_a = Client.sign_add_int ~block_level ~account:bls_a 42 in
      let op_b = Client.sign_add_int ~block_level ~account:bls_b (-23) in
      Operator.process_signed_operation op_a ;
      Operator.process_signed_operation op_b ;
      ()
    in
    (* Commit the result Onchain *)
    let* i =
      let block = Operator.finish_block () in
      let bc = Operator.commit_block block in
      let* op = Op.Rollup.block_commitment (I i) ~source:rollup_operator bc in
      Incremental.add_operation i op
    in
    (* Need to bake, rollups can not post multiple commitments in the same block *)
    let* block_commitment =
      let* result = Incremental.get_last_operation_result i in
      assert_rollup result @@ function
      | Block_commitment_result { commitment ;  _ } -> return commitment
      | _ -> failwith "expected a rollup block commitment result"
    in
    let* i = bake i in
    return (i , block_commitment)
  in  
  (* Do multiple commitments *)
  let* (i , bc_a) = basic_commit i in
  let* (i , bc_b) = basic_commit i in
  let* (i , bc_c) = basic_commit i in
  ignore i ;
  (* Have a validator check them *)
  let module Validator = MakeValidator(struct let rollup_id = rollup_id end) in
  Validator.process_block_commitment bc_a ;
  Validator.process_block_commitment bc_b ;
  Validator.process_block_commitment bc_c ;
  (* Manually check final state *)
  let state = Validator.Operator.StatefulContext.get () in
  (* 3 * (42 - 23) *)
  assert (Z.(state = (of_int 57))) ;
  return ()



let counter_bad = test "counter-bad" @@ fun () ->
  let* ((rollup_operator , rollup_watcher) , i) = bootstrap2 () in
  
  (* Create Rollup Onchain *)
  let* (rollup_id , i) = create_counter_rollup ~rollup_operator i in
  (* Need to bake, rollups can start only in next block *)
  let* i = bake i in
  (* Create Rollup Offchain *)
  let module Operator = MakeOperator(struct let rollup_id = rollup_id end) in
  let (bls_a , bls_b) = bls_bootstrap2 () in
  
  (* Need to bake, rollups can start only in next block *)
  let* i = bake i in
  
  let basic_commit alter i =
    (* Operate some operations Offchain *)
    let () =
      let block_level = Operator.get_block_level () in
      let op_a = Client.sign_add_int ~block_level ~account:bls_a 42 in
      let op_b = Client.sign_add_int ~block_level ~account:bls_b (-23) in
      Operator.process_signed_operation op_a ;
      Operator.process_signed_operation op_b ;
      ()
    in
    (* Commit the result Onchain *)
    let* i =
      let block = Operator.finish_block () in
      let bc = Operator.commit_block block in
      let bc = alter_block_commitment alter bc in
      let* op = Op.Rollup.block_commitment (I i) ~source:rollup_operator bc in
      Incremental.add_operation i op
    in
    (* Need to bake, rollups can not post multiple commitments in the same block *)
    let* block_commitment =
      let* result = Incremental.get_last_operation_result i in
      assert_rollup result @@ function
      | Block_commitment_result { commitment ; level ; _ } -> return (commitment , level)
      | _ -> failwith "expected a rollup block commitment result"
    in
    let* i = bake i in
    return (i , block_commitment)
  in
  (* Try to commit bad content *)
  let* () =
    let* (i , (bc , level)) = basic_commit `Root i in
    (* Have a validator check them *)
    let module Validator = MakeValidator(struct let rollup_id = rollup_id end) in
    let { micro_block_index ; state_trace } : bad_root =
      try (
        Validator.process_block_commitment bc ;
        raise @@ Failure("unexpected success")
      ) with
      | Bad_root x -> x
      | _exn -> raise @@ Failure("unexpected error")
    in
    (* Have the validator do the rejection proof *)
    let* () =
      let br = Validator.invalid_state_hash ~micro_block_index ~level state_trace in
      let* op = Op.Rollup.reject_block (I i) ~source:rollup_watcher br in
      let* i = Incremental.add_operation i op in
      let* result = Incremental.get_last_operation_result i in
      assert_rollup result @@ function
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
    let* (i , (bc , level)) = basic_commit `Signature i in
    (* Have a validator check them *)
    let module Validator = MakeValidator(struct let rollup_id = rollup_id end) in
    let { micro_block_index } : bad_signature =
      try (
        Validator.process_block_commitment bc ;
        raise @@ Failure("unexpected success")
      ) with
      | Bad_signature x -> x
      | _ -> raise @@ Failure("unexpected error")
    in
    (* Have the validator do the rejection proof *)
    let* () =
      let is = Validator.invalid_signature ~micro_block_index ~level in
      let* op = Op.Rollup.reject_block (I i) ~source:rollup_watcher is in
      let* i = Incremental.add_operation i op in
      let* result = Incremental.get_last_operation_result i in
      assert_rollup result @@ function
      | Micro_block_rejection_result { removed_rollup_block_indices ; _ } -> (
          assert (removed_rollup_block_indices = [ level ]) ;
          return ()
        )
      | _ -> failwith "expected a rollup micro block rejection result"
    in
    return ()
  in

  return ()

(*
  TODO:
  - Test too old rejection
  - Test invalid rejections
  - Test double tx inclusion
  - Test committing after rejection
  - Test sign tx from other rollup
  - account compression (represent accounts as integers)
*)


let tests = [
  counter_good ;
  counter_bad ;
]
