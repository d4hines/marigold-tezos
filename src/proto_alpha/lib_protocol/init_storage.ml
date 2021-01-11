(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(* This is the genesis protocol: initialise the state *)
let prepare_first_block ctxt ~typecheck ~level ~timestamp ~fitness =
  Raw_context.prepare_first_block ~level ~timestamp ~fitness ctxt
  >>=? fun (previous_protocol, ctxt) ->
  match previous_protocol with
  | Genesis param ->
      Commitment_storage.init ctxt param.commitments
      >>=? fun ctxt ->
      Roll_storage.init ctxt
      >>=? fun ctxt ->
      Seed_storage.init ctxt
      >>=? fun ctxt ->
      Contract_storage.init ctxt
      >>=? fun ctxt ->
      Bootstrap_storage.init
        ctxt
        ~typecheck
        ?ramp_up_cycles:param.security_deposit_ramp_up_cycles
        ?no_reward_cycles:param.no_reward_cycles
        param.bootstrap_accounts
        param.bootstrap_contracts
      >>=? fun ctxt ->
      Roll_storage.init_first_cycles ctxt
      >>=? fun ctxt ->
      Vote_storage.init
        ctxt
        ~start_position:(Level_storage.current ctxt).level_position
      >>=? fun ctxt ->
      Storage.Block_priority.init ctxt 0
      >>=? fun ctxt -> Vote_storage.update_listings ctxt
  | Edo_008 ->
      return ctxt

let prepare ctxt ~level ~predecessor_timestamp ~timestamp ~fitness =
  Raw_context.prepare ~level ~predecessor_timestamp ~timestamp ~fitness ctxt
  >>=? fun ctxt ->
  Storage.Block_operation_hashes.get ctxt
  >|=? fun pred_operation_hashes ->
  let open Raw_context in
  Raw_context.update_block_operation_hashes
    ctxt
    (List.fold_right
       (fun block_operation_hashes
            (acc : block_operation_hashes_value Block_operation_hashes_map.t) ->
         let hashes =
           Block_operation_hashes_repr.hashes block_operation_hashes
         in
         let level =
           Block_operation_hashes_repr.level block_operation_hashes
         in
         List.fold_right
           (fun hash acc ->
             Block_operation_hashes_map.add
               hash
               (Raw_context.create_block_operation_hashes_value ~level)
               acc)
           hashes
           acc)
       pred_operation_hashes
       (Raw_context.block_operation_hashes ctxt))
