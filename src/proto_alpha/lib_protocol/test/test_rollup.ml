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

let counter_good = test "counter-good" @@ fun () ->
  let* (b , bootstrap_contracts) = Context.init 10 in
  let rollup_operator =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth bootstrap_contracts 0
  in
  let* i = Incremental.begin_construction b in
  let module Counter = Tezos_rollup_alpha.Counter in
  let open Counter in
  
  (* Create Rollup Offchain *)
  let module R = Make() in
  let (bls_a , bls_b) = S.Dev.(
      create_account () , create_account ()
    ) in

  (* Create Rollup Onchain *)
  let* i =
    let source = rollup_operator in
    let kind = Rollup.Counter in
    let* op = Op.rollup_creation ~source ~kind (I i) in
    let* i = Incremental.add_operation i op in
    return i
  in
  let* rollup_id =
    let* result = Incremental.get_last_operation_result i in
    assert_rollup result @@ function
    | Rollup_creation_result { rollup_number ; _ } -> return rollup_number
    | _ -> failwith "expected a rollup creation result"
  in

  (* Need to bake, rollups can start only in next block *)
  let* i =
    let* b = Incremental.finalize_block i in
    let* b = Block.bake b in
    let* i = Incremental.begin_construction b in
    return i
  in

  (* Validate some operations Offchain *)
  let () =
    let op_a =
      let operation =
        let content = Add (Z.of_int 42) in
        Client.make_operation ~content ~block_level
      in
      Client.sign_operation ~account:bls_a ~operation
    in
    let op_b =
      let operation = Add (Z.of_int (-23)) in
      Client.sign_operation ~account:bls_b ~operation
    in
    R.process_signed_operation op_a ;
    R.process_signed_operation op_b ;
    ()
  in
  
  (* Commit the result Onchain *)
  let* _i =
    let { rev_transactions ; after_root ; before_root = _ ; aggregated_signature_opt } = R.finish_micro_block () in
    let transactions =
      let aux { signer ; content ; signature = _ } = Rollup.{ signer ; content } in
      List.map aux @@
      List.rev rev_transactions
    in
    let aggregated_signature = match aggregated_signature_opt with
      | Some x -> x
      | None -> raise (Failure "no aggregated signature")
    in
        
    let bc = Rollup.Block_commitment.{
      transactions ;
      aggregated_signature ;
      after_root ;
      rollup_id ;
    } in
    let* op = Op.rollup_block_commitment (I i) ~source:rollup_operator bc in
    Incremental.add_operation i op
  in

  (* Do the same thing multiple times *)

  (* Withdraw money *)
  
  return ()


let tests = [
  counter_good ;
]



(* let noop = test "noop" @@ fun () ->
 *   let* (b , bootstrap_contracts) = Context.init 10 in
 *   let bootstrap0 =
 *     WithExceptions.Option.get ~loc:__LOC__ @@ List.nth bootstrap_contracts 0
 *   in
 *   let* i = Incremental.begin_construction b in
 * 
 *   let* rollup_block_commitment = Op.rollup_block_commitment (I i) bootstrap0 in
 *   let* i = Incremental.add_operation i rollup_block_commitment in
 *   let* result1 = Incremental.get_last_operation_result i in
 *   let* () =
 *     assert_rollup result1 @@ function
 *     | Block_commitment_result _ -> return ()
 *     | _ -> failwith "expected a block commitment result"
 *   in
 *   
 *   let* rollup_micro_block_rejection = Op.rollup_micro_block_rejection (I i) bootstrap0 in
 *   let* i = Incremental.add_operation i rollup_micro_block_rejection in
 *   let* result2 = Incremental.get_last_operation_result i in
 *   let* () =
 *     assert_rollup result2 @@ function
 *     | Micro_block_rejection_result _ -> return ()
 *     | _ -> failwith "expected a tx rejection result"
 *   in
 * 
 *   let** counter = Protocol.Alpha_context.Rollup.Dev.get_counter !*i in
 *   let* () = check Z.(counter = zero) "rollup counter didn't start at 0" in
 *   let* rollup_creation = Op.rollup_creation (I i) bootstrap0 in
 *   let* i = Incremental.add_operation i rollup_creation in
 *   let* result3 = Incremental.get_last_operation_result i in
 *   let* () =
 *     assert_rollup result3 @@ function
 *     | Rollup_creation_result _ -> return ()
 *     | _ -> failwith "expected a rollup creation result"
 *   in
 *   let** counter = Protocol.Alpha_context.Rollup.Dev.get_counter !*i in
 *   let* () = check Z.(counter = one) "rollup counter didn't increment to 1" in
 *   
 *   return () *)
