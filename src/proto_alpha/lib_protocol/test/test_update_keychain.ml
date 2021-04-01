(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <contact@marigold.dev>                        *)
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
This testing file consists of two parts for testing.
All operation related test cases are defined in [module Test_Operation]; and,
all storage accessing test cases are written in [module Test_Storage].
*)

open Protocol
open Alpha_context
open Test_tez

open Account.Update_keychain

let pkh_pp = Signature.Public_key_hash.pp

let pk_pp = Signature.Public_key.pp

let keychain_pp = Keychain.pp

(* Utils *)

let wrap x = Lwt.return (Environment.wrap_tzresult x)

let wrap_result x = Lwt.return (Environment.wrap_tztrace x)

let is_none msg k =
  k >>= wrap >>=? function
  | None -> return ()
  | Some _ -> failwith "expect a Some (%s)" msg

let is_some msg k =
  k >>= wrap >>=? function
  | Some x -> return x
  | None -> failwith "expect a None (%s)" msg

(* check if the master key of given key hash in given block
   equals to the given key *)
let checkMasterKey block pkh pk_opt =
  Incremental.begin_construction block
  >>=? fun incr ->
  let ctx = Incremental.alpha_ctxt incr in
  Keychain.get_master_key ctx pkh
  >>= wrap >>=? fun mk_opt ->
  match (mk_opt, pk_opt) with
  | None, None -> return_unit
  | Some mk, Some pk ->
    Assert.equal_pk ~loc:__LOC__ mk pk
    >>=? fun () ->
    return_unit
  | _, _ -> failwith "check master key fails"


module Test_Operation = struct
  let test_sample_update_keychain_op () =
      Context.init 1
      >>=? fun (blk, contracts) ->
      let new_c = WithExceptions.Option.get ~loc:__LOC__ @@ List.hd contracts in
      let kh' = (match Contract.is_implicit new_c with
      | Some kh -> kh
      | None -> Stdlib.failwith "not implicit account")
      in
      let {c_pk; s_pk; _} = new_key_chain kh' Spending_key
      in
      Incremental.begin_construction blk
      >>=? fun incr ->
      let ctxt = Incremental.alpha_ctxt incr in
      Keychain.exists ctxt kh'
      >>= fun (is_exist) ->
      Assert.equal_bool ~loc:__LOC__ is_exist false
      >>=? fun () ->
      Op.update_keychain (B blk) new_c (Some c_pk) (Some s_pk)
      >>=? fun operation ->
      Block.bake blk ~operation
      >>=? fun blk ->
      Incremental.begin_construction blk
      >>=? fun incr ->
      let ctxt = Incremental.alpha_ctxt incr in
      Keychain.find ctxt kh'
      >>= wrap >>=? function
        | Some {master_key; spending_key; _} ->
          (if Signature.Public_key.(master_key<> c_pk) then
             Stdlib.failwith "consensus_key wasn't set correctly."
           else if Signature.Public_key.(spending_key <> s_pk) then
             Stdlib.failwith "spending_key wasn't set correctly."
           else
              return ())
        | None -> Stdlib.failwith "key hash should be found."

  let test_update_keychain_transaction key () =
      Context.init 2
      >>=? fun (blk, accounts) ->
      let src_contract = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth accounts 0 in
      let dst_contract = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth accounts 1 in
      Context.Contract.manager (B blk) src_contract
      >>=? fun src ->
      let ({c_pk; s_pk; _ } as ba) =
      match key with
      | None -> new_key_chain src.pkh Consensus_key
      | Some k -> new_key_chain src.pkh k
      in
      Context.Contract.balance (B blk) dst_contract
      >>=? fun bal_dst ->
      Op.update_keychain (B blk) src_contract (Some c_pk) (Some s_pk)
      >>=? fun op_ba ->
      Block.bake blk ~operation:op_ba
      >>=? fun blk ->
      let kcs = Block.Keychain_list.add ba.ba_pkh ba Block.Keychain_list.empty in
      let amount = Tez.one_mutez in
      (match key with
      | None ->
         Op.transaction_update_keychain (B blk) ba dst_contract amount ~sk:src.sk
      | Some _ ->
         Op.transaction_update_keychain (B blk) ba dst_contract amount)
      >>=? fun op_tx ->
      Block.bake blk ~operation:op_tx ~kcs
      >>= fun res ->
      (match key with
       | None ->
         (Assert.proto_error ~loc:__LOC__ res (function
              | Operation_repr.Invalid_signature -> true
              | _ -> false ))
       | Some _ -> res >>?= fun blk ->
         Assert.balance_was_credited ~loc:__LOC__ (B blk) dst_contract bal_dst amount
      )

  let test_update_keychain_transaction_by_spending_key =
    test_update_keychain_transaction (Some Spending_key)

  let test_update_keychain_transaction_by_consensus_key =
    test_update_keychain_transaction (Some Consensus_key)

  let test_update_keychain_transaction_by_arbitrary_key =
    test_update_keychain_transaction None

  let test_delayed_update () =
    let open Block in
    let policy = By_priority 0 in
    Context.init 1
    >>=? fun (b_pre_init, cs) ->
    Incremental.begin_construction b_pre_init
    >>=? fun incr ->
    let ctx = Incremental.alpha_ctxt incr in
    let acc = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth cs 0 in
    Context.Contract.pkh acc
    >>=? fun pkh ->
    (*
    let (_pkhA, pkA, skA) = Signature.generate_key () in
    let (_pkhB, pkB, _skB) = Signature.generate_key () in
    *)
    let kcA = new_key_chain pkh Consensus_key in
    let pkA = kcA.c_pk in
    let skA = kcA.c_sk in
    let kcB = new_key_chain pkh Consensus_key in
    let pkB = kcB.c_pk in
    let _skB = kcB.c_sk in
    let master_key_delay_cycles =
      (Constants.parametric ctx).master_key_delay_cycles in
    (* check: there is no master key for pkh *)
    checkMasterKey b_pre_init pkh None
    >>=? fun () ->
    (* init keychain with master = pkA *)
    Op.update_keychain (B b_pre_init) acc (Some pkA) (Some pkA)
    >>=? fun operation ->
    bake b_pre_init ~operation
    >>=? fun b_init ->
    (* check: the master of pkh == pkA *)
    checkMasterKey b_init pkh (Some pkA)
    >>=? fun () ->
    let kcs =
      Block.Keychain_list.add kcA.ba_pkh kcA Block.Keychain_list.empty in
    (* ask for update master key *)
    Op.update_keychain (B b_init) ~sk:skA acc (Some pkB) None
    >>=? fun operation ->
    bake b_init ~operation ~kcs
    >>=? fun b ->
    (* check: the master of pkh == pkA (<> pkB) *)
    checkMasterKey b pkh (Some pkA)
    >>=? fun () ->
    (* move cycle to 1 *)
    bake_until_cycle_end ~policy b
    >>=? fun b ->
    (* check: the master of pkh == pkA (<> pkB) *)
    checkMasterKey b pkh (Some pkA)
    >>=? fun () ->
    (* move cycle to n-1 *)
    let delta = master_key_delay_cycles - 2 in
    bake_until_n_cycle_end ~policy delta b
    >>=? fun b ->
    (* check: the master of pkh == pkA (<> pkB) *)
    checkMasterKey b pkh (Some pkA)
    >>=? fun () ->
    (* move cycle to n *)
    bake_until_cycle_end ~policy b
    >>=? fun b ->
    (* check: the master of pkh == pkB *)
    checkMasterKey b pkh (Some pkB)
    >>=? fun () ->
    return_unit

  (** Apply a single endorsement from the slot 0 endorser. *)
  let test_simple_endorsement_with_keychain () =
    Context.init 5
    >>=? fun (b, accounts) ->
    let src_contract = WithExceptions.Option.get ~loc:__LOC__ @@
      List.nth accounts 0 in
    Context.Contract.manager (B b) src_contract
    >>=? fun src ->
    let ({c_pk; s_pk; _ } as ba) =
      new_key_chain src.pkh Consensus_key in
    Op.update_keychain (B b) src_contract (Some c_pk) (Some s_pk)
    >>=? fun op_ba ->
    Block.bake b ~operation:op_ba
    >>=? fun b ->
    let kcs = Block.Keychain_list.add ba.ba_pkh ba Block.Keychain_list.empty in
    Context.get_endorser (B b)
    >>=? fun (delegate, slots) ->
    Op.endorsement_with_slot
      ~delegate:(delegate, slots)
      ~sk:(kc_sign ba)
      (B b)
      ()
    >>=? fun op ->
    Context.Contract.balance (B b) (Contract.implicit_contract delegate)
    >>=? fun initial_balance ->
    let policy = Block.Excluding [delegate] in
    Block.get_next_baker ~policy b
    >>=? fun (_, priority, _) ->
    let () = Format.fprintf Format.std_formatter "---%s\n" __LOC__ in
    (* problemic bake *)
    Block.bake ~policy ~operations:[Operation.pack op] b ~kcs
    >>=? fun b2 ->
    let () = Format.fprintf Format.std_formatter "---%s\n" __LOC__  in
    Test_endorsement.assert_endorser_balance_consistency
      ~loc:__LOC__
      (B b2)
      ~priority
      ~endorsing_power:(List.length slots)
      delegate
      initial_balance

   let test_registered_self_delegate_key_init_delegation () =
     Context.init 5
     >>=? fun (b, bootstrap_contracts) ->
     let bootstrap =
       WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_contracts
     in
     let contract = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth bootstrap_contracts 1 in
     Context.Contract.manager (B b) contract
     >>=? fun src ->
     let ({c_pk; s_pk; _ } as ba) = new_key_chain src.pkh Consensus_key in
     Op.update_keychain (B b) contract (Some c_pk) (Some s_pk)
     >>=? fun op_ba ->
     Block.bake b ~operation:op_ba
     >>=? fun b ->
     let kcs = Block.Keychain_list.add ba.ba_pkh ba Block.Keychain_list.empty in
     let delegate_contract = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth bootstrap_contracts 2 in
     Context.Contract.manager (B b) delegate_contract
     >>=? fun d_src ->
     let d_ba = new_key_chain d_src.pkh Consensus_key in
     Op.update_keychain (B b) delegate_contract (Some d_ba.c_pk) (Some d_ba.s_pk)
     >>=? fun op_ba ->
     Block.bake b ~operation:op_ba ~kcs
     >>=? fun b ->
     Incremental.begin_construction b
     >>=? fun i ->
     let contract = Alpha_context.Contract.implicit_contract src.pkh in
     let delegate_contract =
       Alpha_context.Contract.implicit_contract d_src.pkh
     in
     Op.transaction (I i) bootstrap contract (Tez.of_int 10)
     >>=? fun op ->
     Incremental.add_operation i op
     >>=? fun i ->
     Op.transaction (I i) bootstrap delegate_contract (Tez.of_int 10)
     >>=? fun op ->
     Incremental.add_operation i op
     >>=? fun i ->
     Op.delegation (I i) delegate_contract (Some d_src.pkh) ~sk:d_ba.c_sk ~kc:d_ba
     >>=? fun op ->
     Incremental.add_operation i op
     >>=? fun i ->
     Op.delegation (I i) contract (Some d_src.pkh)
     >>=? fun op ->
     Incremental.add_operation i op
     >>=? fun i ->
     Context.Contract.delegate (I i) contract
     >>=? fun delegate ->
     Assert.equal_pkh ~loc:__LOC__ delegate d_src.pkh
     >>=? fun () -> return_unit

   (* keychain can't be reveal*)
   let test_simple_reveal () =
     Context.init 2
     >>=? fun (blk, contracts) ->
     let c = WithExceptions.Option.get ~loc:__LOC__ @@ List.hd contracts in
     let src_contract = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 1 in 
     Context.Contract.manager (B blk) src_contract
     >>=? fun src ->
     let ({c_pk; s_pk; _ } as ba) = new_key_chain src.pkh Consensus_key in
     Op.update_keychain (B blk) src_contract (Some c_pk) (Some s_pk)
     >>=? fun operation ->
     Block.bake blk ~operation
     >>=? fun blk ->
     let kcs = Block.Keychain_list.add ba.ba_pkh ba Block.Keychain_list.empty in
     Op.transaction (B blk) c src_contract Tez.one
     >>=? fun operation ->
     Block.bake blk ~operation ~kcs
     >>=? fun blk ->
     Op.revelation (B blk) c_pk ~ba
     >>=? fun operation ->
     Block.bake blk ~operation ~kcs
     >>= fun res ->
     (Assert.proto_error ~loc:__LOC__ res (function
          | Contract_storage.Previously_revealed_key _ -> true
          | _ -> false ))

   let test_origination_balances ~loc:_ ?(fee = Tez.zero) ?(credit = Tez.zero) ()
     =
     Context.init 1
     >>=? fun (b, contracts) ->
     let contract = WithExceptions.Option.get ~loc:__LOC__ @@ List.hd contracts in
     Context.Contract.manager (B b) contract
     >>=? fun src ->
     let ({c_pk; s_pk; _ } as ba) = new_key_chain src.pkh Consensus_key in
     Op.update_keychain (B b) contract (Some c_pk) (Some s_pk)
     >>=? fun operation ->
     Block.bake b ~operation
     >>=? fun b ->
     let kcs = Block.Keychain_list.add ba.ba_pkh ba Block.Keychain_list.empty in
     Context.Contract.balance (B b) contract
     >>=? fun balance ->
     Op.origination (B b) contract ~fee ~credit ~script:Op.dummy_script ~sk:ba.c_sk ~kc:ba
     >>=? fun (operation, new_contract) ->
     Context.get_constants (B b)
     >>=? fun { parametric =
                  {origination_size; cost_per_byte; block_security_deposit; _};
                _ } ->
     Tez.(cost_per_byte *? Int64.of_int origination_size)
     >>?= fun origination_burn ->
     Tez.( +? ) credit block_security_deposit
     >>? Tez.( +? ) fee
     >>? Tez.( +? ) origination_burn
     >>? Tez.( +? ) Op.dummy_script_cost
     >>?= fun total_fee ->
     Block.bake ~operation b ~kcs
     >>=? fun b ->
     (* check that after the block has been baked the source contract
        was debited all the fees *)
     Assert.balance_was_debited ~loc:__LOC__ (B b) contract balance total_fee
     >>=? fun _ ->
     (* check the balance of the originate contract is equal to credit *)
     Assert.balance_is ~loc:__LOC__ (B b) new_contract credit

   let test_balances_simple () = test_origination_balances ~loc:__LOC__ ()

   let test_valid_double_endorsement_evidence () =
     Context.init 2
     >>=? fun (b, contract) ->
     let acc = WithExceptions.Option.get ~loc:__LOC__ @@
       List.nth contract 0 in
     Context.Contract.manager (B b) acc
     >>=? fun src ->
     let ({c_pk; s_pk; _ } as ba) =
       new_key_chain src.pkh Consensus_key in
     Op.update_keychain (B b) acc (Some c_pk) (Some s_pk)
     >>=? fun op_ba ->
     Block.bake b ~operation:op_ba
     >>=? fun b ->
     let kcs = Block.Keychain_list.add ba.ba_pkh ba Block.Keychain_list.empty in
     Test_double_endorsement.block_fork b
     >>=? fun (blk_a, blk_b) ->
     Context.get_endorser (B blk_a)
     >>=? fun (delegate, slots) ->
     Op.endorsement ~delegate (B blk_a) () ~sk:ba.c_sk
     >>=? fun endorsement_a ->
     Op.endorsement ~delegate (B blk_b) () ~sk:ba.c_sk
     >>=? fun endorsement_b ->
     Op.endorsement_with_slot ~delegate:(delegate, slots) (B blk_a) () ~sk:ba.c_sk
     >>=? fun endorsement_with_slot_a ->
     Block.bake ~operations:[Operation.pack endorsement_with_slot_a] blk_a ~kcs
     >>=? fun blk_a ->
     (* Block.bake ~operations:[endorsement_b] blk_b >>=? fun _ -> *)
     Op.double_endorsement
       (B blk_a)
       endorsement_a
       endorsement_b
       ~slot:(WithExceptions.Option.get ~loc:__LOC__ (List.hd slots))
     |> fun operation ->
     (* Bake with someone different than the bad endorser *)
     Context.get_bakers (B blk_a)
     >>=? fun bakers ->
     Test_double_endorsement.get_first_different_baker delegate bakers
     |> fun baker ->
     Block.bake ~policy:(By_account baker) ~operation blk_a ~kcs
     >>=? fun blk ->
     (* Check that the frozen deposit, the fees and rewards are removed *)
     List.iter_es
       (fun kind ->
          let contract = Alpha_context.Contract.implicit_contract delegate in
          Assert.balance_is ~loc:__LOC__ (B blk) contract ~kind Tez.zero)
       [Deposit; Fees; Rewards]

end

module Test_Storage = struct
  let init n =
    Context.init n
    >>=? fun (b, cs) ->
    let cs' = List.map Context.Contract.pkh cs in
    all_ep cs'
    >>=? fun pkhs ->
    Incremental.begin_construction b
    >>=? fun incr ->
    let ctx = Incremental.alpha_ctxt incr in
    return (ctx, pkhs)

  let test_create () =
    init 2
    >>=? fun (ctx, pkhs) ->
    let pkh0 =
      WithExceptions.Option.get ~loc:__LOC__ @@ List.nth pkhs 0 in
    let pkh1 =
      WithExceptions.Option.get ~loc:__LOC__ @@ List.nth pkhs 1 in
    (* absence of keychain*)
    Keychain.exists ctx pkh0
    >>= fun existing ->
    Assert.equal_bool ~loc:__LOC__ existing false
    >>=? fun () ->
    Keychain.exists ctx pkh1
    >>= fun existing ->
    Assert.equal_bool ~loc:__LOC__ existing false
    >>=? fun () ->
    (* keychain initializing: first one *)
    let (_pkh, pk, _sk) = Signature.generate_key () in
    Keychain.init ctx pkh0 pk pk
    >>= wrap
    >>=? fun ctx ->
    (* presence and ansence of keychain after init *)
    Keychain.exists ctx pkh0
    >>= fun existing ->
    Assert.equal_bool ~loc:__LOC__ existing true
    >>=? fun () ->
    Keychain.exists ctx pkh1
    >>= fun existing ->
    Assert.equal_bool ~loc:__LOC__ existing false
    >>=? fun () ->
    (* keychain initializing: second one *)
    Keychain.init_with_manager ctx pkh1 None
    >>= wrap
    >>=? fun ctx ->
    (* presence and ansence of keychain after init *)
    Keychain.exists ctx pkh0
    >>= fun existing ->
    Assert.equal_bool ~loc:__LOC__ existing true
    >>=? fun () ->
    Keychain.exists ctx pkh1
    >>= fun existing ->
    Assert.equal_bool ~loc:__LOC__ existing true
    >>=? fun () ->
        (* reinitializing is impossible *)
    Keychain.init_with_manager ctx pkh0 (Some pk)
    >>= function Ok _ -> assert false
      (* [TODO] specify which error *)
      | Error _ -> return_unit
    >>=? fun () ->
    Keychain.init ctx pkh1 pk pk
    >>= function Ok _ -> assert false
      (* [TODO] specify which error *)
      | Error _ -> return_unit
    >>=? fun () ->
    return_unit

  let test_access () =
    init 2
    >>=? fun (ctx, pkhs) ->
    let pkh0 =
      WithExceptions.Option.get ~loc:__LOC__ @@ List.nth pkhs 0 in
    let pkh1 =
      WithExceptions.Option.get ~loc:__LOC__ @@ List.nth pkhs 1 in
    let (_pkh, pkA, _sk) = Signature.generate_key () in
    let (_pkh, pkB, _sk) = Signature.generate_key () in
    Contract.get_manager_key ctx pkh1
    >>= wrap
    >>=? fun mgtk1 ->
    (* init keychain for pkh0 with pkA, pkB *)
    Keychain.init ctx pkh0 pkA pkB
    >>= wrap >>=? fun ctx ->
    (* getting keys on pkhs:
       spending key from pkh0 == pkB
       spending key from pkh1 == None
       master key from pkh0 == pkA
       master key from pkh1 == None *)
    Keychain.get_spending_key ctx pkh0
    |> is_some "get spending key from pkh0"
    >>=? fun sk0 ->
    Assert.equal_pk ~loc:__LOC__ pkB sk0
    >>=? fun () ->
    Keychain.get_spending_key ctx pkh1
    |> is_none "get spending key from pkh1"
    >>=? fun () ->
    Keychain.get_master_key ctx pkh0
    |> is_some "get master key from pkh0"
    >>=? fun mk0 ->
    Assert.equal_pk ~loc:__LOC__ pkA mk0
    >>=? fun () ->
    Keychain.get_master_key ctx pkh1
    |> is_none "get master key from pkh1"
    >>=? fun () ->
    (* init keychain for pkh1 with its manager*)
    Keychain.init_with_manager ctx pkh1 None
    >>= wrap >>=? fun ctx ->
    (* getting keys on pkhs:
       spending key from pkh1 == mgtk1
       master key from pkh1 == mgtk1 *)
    Keychain.get_master_key ctx pkh1
    |> is_some "get master key from pkh1"
    >>=? fun mk1 ->
    Assert.equal_pk ~loc:__LOC__ mk1 mgtk1
    >>=? fun () ->
    Keychain.get_spending_key ctx pkh1
    |> is_some "get spending key from pkh1"
    >>=? fun sk1 ->
    Assert.equal_pk ~loc:__LOC__ sk1 mgtk1
    >>=? fun () ->
    (* setting spending key:
       update spending key for pkh1 with pkB
       spending key from pkh1 == pkB *)
    Keychain.set_spending_key ctx pkh1 pkB
    >>= wrap >>=? fun ctx ->
    Keychain.get_spending_key ctx pkh1
    |> is_some "get spending key from pkh1"
    >>=? fun sk1 ->
    Assert.not_equal_pk ~loc:__LOC__ sk1 mgtk1
    >>=? fun () ->
    Assert.equal_pk ~loc:__LOC__ sk1 pkB
    >>=? fun () ->
    (* setting master key:
       update master key for pkh1 with pkA
       master key from pkh1 == mgtk1 *)
    Keychain.set_master_key ctx pkh1 pkA
    >>= wrap >>=? fun ctx ->
    Keychain.get_master_key ctx pkh1
    |> is_some "get master key from pkh1"
    >>=? fun mk1 ->
    Assert.equal_pk ~loc:__LOC__ mk1 mgtk1
    >>=? fun () ->
    return_unit

  (* check if the master key of given key hash in given block
     equals to the given key *)
  let checkMasterKey block pkh pk_opt =
    Incremental.begin_construction block
    >>=? fun incr ->
    let ctx = Incremental.alpha_ctxt incr in
    Keychain.get_master_key ctx pkh
    >>= wrap >>=? fun mk_opt ->
    match (mk_opt, pk_opt) with
    | None, None -> return_unit
    | Some mk, Some pk ->
      Assert.equal_pk ~loc:__LOC__ mk pk
      >>=? fun () ->
      return_unit
    | _, _ -> failwith "check master key fails"

  let test_delayed_update () =
    let open Block in
    let policy = By_priority 0 in
    Context.init 1
    >>=? fun (b_pre_init, cs) ->
    Incremental.begin_construction b_pre_init
    >>=? fun incr ->
    let ctx = Incremental.alpha_ctxt incr in
    let acc = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth cs 0 in
    Context.Contract.pkh acc
    >>=? fun pkh ->
    let (_pkhA, pkA, skA) = Signature.generate_key () in
    let (_pkhB, pkB, _skB) = Signature.generate_key () in
    let master_key_delay_cycles =
      (Constants.parametric ctx).master_key_delay_cycles in
    (* check: there is no master key for pkh *)
    checkMasterKey b_pre_init pkh None
    >>=? fun () ->
    (* init keychain with master = pkA *)
    Op.update_keychain (B b_pre_init) acc (Some pkA) (Some pkA)
    >>=? fun operation ->
    bake b_pre_init ~operation
    >>=? fun b_init ->
    (* check: the master of pkh == pkA *)
    checkMasterKey b_init pkh (Some pkA)
    >>=? fun () ->
    (* ask for update master key *)
    Op.update_keychain (B b_init) ~sk:skA acc (Some pkB) None
    >>=? fun operation ->
    bake b_init ~operation
    >>=? fun b ->
    (* check: the master of pkh == pkA (<> pkB) *)
    checkMasterKey b pkh (Some pkA)
    >>=? fun () ->
    (* move cycle to 1 *)
    bake_until_cycle_end ~policy b
    >>=? fun b ->
    (* check: the master of pkh == pkA (<> pkB) *)
    checkMasterKey b pkh (Some pkA)
    >>=? fun () ->
    (* move cycle to n-1 *)
    let delta = master_key_delay_cycles - 2 in
    bake_until_n_cycle_end ~policy delta b
    >>=? fun b ->
    (* check: the master of pkh == pkA (<> pkB) *)
    checkMasterKey b pkh (Some pkA)
    >>=? fun () ->
    (* move cycle to n *)
    bake_until_cycle_end ~policy b
    >>=? fun b ->
    (* check: the master of pkh == pkB *)
    checkMasterKey b pkh (Some pkB)
    >>=? fun () ->
    return_unit
end

let tests =
  [ Test_services.tztest
      "keychain: initializing and existence"
      `Quick
      Test_Storage.test_create;
    Test_services.tztest
      "keychain: accessing"
      `Quick
      Test_Storage.test_access;
    Test_services.tztest
      "keychain: delayed updating"
      `Quick
      Test_Operation.test_delayed_update;
    Test_services.tztest
      "baking account test creating keys"
      `Quick
      Test_Operation.test_sample_update_keychain_op;
    Test_services.tztest
      "baking account test transaction by consensus key"
      `Quick
      Test_Operation.test_update_keychain_transaction_by_consensus_key;
    Test_services.tztest
      "baking account test transaction by spending key"
      `Quick
      Test_Operation.test_update_keychain_transaction_by_spending_key;
    Test_services.tztest
      "baking account test transaction by arbitrary key"
      `Quick
      Test_Operation.test_update_keychain_transaction_by_arbitrary_key;
    Test_services.tztest
      "keychain: revelation by consensus key"
      `Quick
      Test_Operation.test_simple_reveal;
    Test_services.tztest "keychain: origination " `Quick Test_Operation.test_balances_simple;
    Test_services.tztest
      "keychain: delegation by consensus key"
      `Quick
      Test_Operation.test_registered_self_delegate_key_init_delegation;
    Test_services.tztest
      "keychain: endorsement by consensus key"
      `Quick
      Test_Operation.test_simple_endorsement_with_keychain;
    Test_services.tztest "keychain: double_endorsement_evidence" `Quick Test_Operation.test_valid_double_endorsement_evidence;
  ]
