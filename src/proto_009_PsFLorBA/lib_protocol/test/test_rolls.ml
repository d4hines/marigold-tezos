(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Testing
    -------
    Component:  Protocol (rolls)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/main.exe -- test "^rolls$"
    Subject:    On rolls and baking rights.
                A delegate has baking rights provided that it has at least
                more than [token_per_rolls] tz of staking balance. This
                balance corresponds to the quantity of tez that have been
                delegated to it for baking rights. After a given number of
                cycles where it has not made use of its baking rights, its
                account will be deactivated for baker selection. To bake
                again, it will have to re-activate its account.
*)

open Protocol
open Alpha_context
open Test_tez

let account_pair = function [a1; a2] -> (a1, a2) | _ -> assert false

let wrap e = Lwt.return (Environment.wrap_tzresult e)

(** Baking rights consistency. Assert that the number of rolls for
    [account]'s pkh - equals to the number of expected rolls, i.e.,
    staking balance of [account] / (token_per_roll). As of protocol
    version 007, token_per_roll = 8000. Note that the consistency is
    verified against the value in the context, i.e. we are testing
    Storage.Roll.Delegate_roll_list. We do not use RPCs here. *)
let check_rolls (b : Block.t) (baker : baker_hash) =
  Context.get_constants (B b)
  >>=? fun constants ->
  Context.Baker.info (B b) baker
  >>=? fun {staking_balance; _} ->
  let token_per_roll = constants.parametric.tokens_per_roll in
  let expected_rolls =
    Int64.div (Tez.to_mutez staking_balance) (Tez.to_mutez token_per_roll)
  in
  Raw_context.prepare
    b.context
    ~level:b.header.shell.level
    ~predecessor_timestamp:b.header.shell.timestamp
    ~timestamp:b.header.shell.timestamp
    ~fitness:b.header.shell.fitness
  >>= wrap
  >>=? fun ctxt ->
  Roll_storage.count_rolls ctxt baker
  >>= wrap
  >>=? fun rolls ->
  Assert.equal_int ~loc:__LOC__ rolls (Int64.to_int expected_rolls)

let check_no_rolls (b : Block.t) (baker : baker_hash) =
  Raw_context.prepare
    b.context
    ~level:b.header.shell.level
    ~predecessor_timestamp:b.header.shell.timestamp
    ~timestamp:b.header.shell.timestamp
    ~fitness:b.header.shell.fitness
  >>= wrap
  >>=? fun ctxt ->
  Roll_storage.count_rolls ctxt baker
  >>= wrap
  >>=? fun rolls -> Assert.equal_int ~loc:__LOC__ rolls 0

(** Create a block with two initialized baker accounts. Assert
    that the first account has a staking balance that is equal to its
    own balance, and that its staking rights are consistent
    (check_rolls). *)
let test_simple_staking_rights () =
  Context.init 2
  >>=? fun (b, _, bakers) ->
  let (a1, _a2) = account_pair bakers in
  Context.Contract.balance (B b) (Contract.baker_contract a1)
  >>=? fun balance ->
  Context.Baker.info (B b) a1
  >>=? fun info ->
  Assert.equal_tez ~loc:__LOC__ balance info.staking_balance
  >>=? fun () -> check_rolls b a1

(** Create a block with two initialized baker accounts. Bake
    five blocks. Assert that the staking balance of the first account
    equals to its balance. Then both accounts have consistent staking
    rights. *)
let test_simple_staking_rights_after_baking () =
  Context.init 2
  >>=? fun (b, _, bakers) ->
  let (a1, a2) = account_pair bakers in
  Context.Contract.balance (B b) (Contract.baker_contract a1)
  >>=? fun balance ->
  Block.bake_n ~policy:(By_account a2) 5 b
  >>=? fun b ->
  Context.Baker.info (B b) a1
  >>=? fun info ->
  Assert.equal_tez ~loc:__LOC__ balance info.staking_balance
  >>=? fun () -> check_rolls b a1 >>=? fun () -> check_rolls b a2

let frozen_deposit (info : Context.Baker.info) =
  Cycle.Map.fold
    (fun _ {Baker.deposit; _} acc -> Test_tez.Tez.(deposit + acc))
    info.frozen_balance_by_cycle
    Tez.zero

let check_activate_staking_balance ~loc ~deactivated b (baker : baker_hash) =
  Context.Baker.info (B b) baker
  >>=? fun info ->
  Assert.equal_bool ~loc info.deactivated deactivated
  >>=? fun () ->
  Context.Contract.balance (B b) (Contract.baker_contract baker)
  >>=? fun balance ->
  let deposit = frozen_deposit info in
  Assert.equal_tez ~loc Test_tez.Tez.(balance + deposit) info.staking_balance

let run_until_deactivation () =
  Context.init 2
  >>=? fun (b, accounts, bakers) ->
  let (a1, a2) = account_pair accounts in
  Context.Contract.balance (B b) a1
  >>=? fun balance_start ->
  let b1 = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth bakers 0 in
  let b2 = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth bakers 1 in
  check_activate_staking_balance ~loc:__LOC__ ~deactivated:false b b1
  >>=? fun () ->
  Context.Baker.info (B b) b1
  >>=? fun info ->
  Block.bake_until_cycle ~policy:(By_account b2) info.grace_period b
  >>=? fun b ->
  check_activate_staking_balance ~loc:__LOC__ ~deactivated:false b b1
  >>=? fun () ->
  Block.bake_until_cycle_end ~policy:(By_account b2) b
  >>=? fun b ->
  check_activate_staking_balance ~loc:__LOC__ ~deactivated:true b b1
  >|=? fun () -> (b, ((a1, b1), balance_start), (a2, b2))

(** From an initialized block with two contracts/accounts, the first
    one is active then deactivated. After baking, check that the
    account is active again. Baking rights are ensured. *)
let test_deactivation_then_bake () =
  run_until_deactivation ()
  >>=? fun ( b,
             ((_impl_contract, deactivated_account), _start_balance),
             (_a2, _b2) ) ->
  Block.bake ~policy:(By_account deactivated_account) b
  >>=? fun b ->
  check_activate_staking_balance
    ~loc:__LOC__
    ~deactivated:false
    b
    deactivated_account
  >>=? fun () -> check_rolls b deactivated_account

(** A deactivated account, which is emptied (into a newly created sink
    account), then re-activated. Its balance is zero. Baking rights are
    ensured. *)
let test_deactivation_then_empty_then_reactivation () =
  run_until_deactivation ()
  >>=? fun (b, ((impl_contract, deactivated_baker), _start_balance), (_a2, b2)) ->
  (* empty the baker contract *)
  let deactivated_account = Contract.baker_contract deactivated_baker in
  Context.Contract.balance (B b) deactivated_account
  >>=? fun balance ->
  let sink_account = Account.new_account () in
  let sink_contract = Contract.implicit_contract sink_account.pkh in
  Context.get_constants (B b)
  >>=? fun {parametric = {origination_size; cost_per_byte; _}; _} ->
  Tez.(cost_per_byte *? Int64.of_int origination_size)
  >>?= fun origination_burn ->
  let amount =
    match Tez.(balance -? origination_burn) with
    | Ok r ->
        r
    | Error _ ->
        assert false
  in
  Op.transaction (B b) deactivated_account sink_contract amount
  >>=? fun empty_contract ->
  Block.bake ~policy:(By_account b2) ~operation:empty_contract b
  >>=? fun b ->
  (* re-activation *)
  Op.baker_action
    (B b)
    ~action:(Client_proto_baker.Set_active true)
    impl_contract
    deactivated_baker
  >>=? fun reactivation ->
  Block.bake ~policy:(By_account b2) ~operation:reactivation b
  >>=? fun b ->
  check_activate_staking_balance
    ~loc:__LOC__
    ~deactivated:false
    b
    deactivated_baker
  >>=? fun () ->
  Context.Contract.balance (B b) deactivated_account
  >>=? fun balance ->
  Assert.equal_tez ~loc:__LOC__ Tez.zero balance
  >>=? fun () -> check_rolls b deactivated_baker

(** A deactivated account, which is emptied, then re-activated, then
    re-credited of the sunk amount, becomes active again. Staking
    rights remain consistent. *)
let test_deactivation_then_empty_then_reactivation_then_recredit () =
  run_until_deactivation ()
  >>=? fun (b, ((impl_contract, deactivated_baker), balance), (_a2, b2)) ->
  (* empty the baker contract *)
  let deactivated_account = Contract.baker_contract deactivated_baker in
  let sink_account = Account.new_account () in
  let sink_contract = Contract.implicit_contract sink_account.pkh in
  Context.get_constants (B b)
  >>=? fun {parametric = {origination_size; cost_per_byte; _}; _} ->
  Tez.(cost_per_byte *? Int64.of_int origination_size)
  >>?= fun origination_burn ->
  let amount =
    match Tez.(balance -? origination_burn) with
    | Ok r ->
        r
    | Error _ ->
        assert false
  in
  Op.transaction (B b) deactivated_account sink_contract amount
  >>=? fun empty_contract ->
  Block.bake ~policy:(By_account b2) ~operation:empty_contract b
  >>=? fun b ->
  (* re-activation *)
  Op.baker_action
    (B b)
    ~action:(Client_proto_baker.Set_active true)
    impl_contract
    deactivated_baker
  >>=? fun reactivation ->
  Block.bake ~policy:(By_account b2) ~operation:reactivation b
  >>=? fun b ->
  (* recredit *)
  Op.transaction (B b) sink_contract deactivated_account amount
  >>=? fun recredit_contract ->
  Block.bake ~policy:(By_account b2) ~operation:recredit_contract b
  >>=? fun b ->
  check_activate_staking_balance
    ~loc:__LOC__
    ~deactivated:false
    b
    deactivated_baker
  >>=? fun () ->
  Context.Contract.balance (B b) deactivated_account
  >>=? fun balance ->
  Assert.equal_tez ~loc:__LOC__ amount balance
  >>=? fun () -> check_rolls b deactivated_baker

let tests =
  [ Test_services.tztest
      "simple staking rights"
      `Quick
      test_simple_staking_rights;
    Test_services.tztest
      "simple staking rights after baking"
      `Quick
      test_simple_staking_rights_after_baking;
    Test_services.tztest
      "deactivation then bake"
      `Quick
      test_deactivation_then_bake;
    Test_services.tztest
      "deactivation then empty then reactivation"
      `Quick
      test_deactivation_then_empty_then_reactivation;
    Test_services.tztest
      "deactivation then empty then reactivation then recredit"
      `Quick
      test_deactivation_then_empty_then_reactivation_then_recredit ]
