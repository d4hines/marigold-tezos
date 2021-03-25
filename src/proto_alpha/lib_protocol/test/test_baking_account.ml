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

open Protocol
open Alpha_context

open Account.Baking_account

let pkh_pp = Signature.Public_key_hash.pp

let pk_pp = Signature.Public_key.pp

let keychain_pp = Keychain.pp

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

module Test_Baking_account = struct
  let test_sample_baking_account_op () =
      Context.init 1
      >>=? fun (blk, contracts) ->
      let new_c = WithExceptions.Option.get ~loc:__LOC__ @@ List.hd contracts in
      let kh' = (match Contract.is_implicit new_c with
      | Some kh -> kh
      | None -> Stdlib.failwith "not implicit account")
      in
      let {c_pk; s_pk; _} = new_baking_account kh' Spending_key
      in
      Incremental.begin_construction blk
      >>=? fun incr ->
      let ctxt = Incremental.alpha_ctxt incr in
      Keychain.exists ctxt kh'
      >>= fun (is_exist) ->
      Assert.equal_bool ~loc:__LOC__ is_exist false
      >>=? fun () ->
      Op.baking_account (B blk) new_c (Some c_pk) (Some s_pk)
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

  let test_baking_account_transaction key () =
      Context.init 2
      >>=? fun (blk, accounts) ->
      let src_contract = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth accounts 0 in
      let dst_contract = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth accounts 1 in
      Context.Contract.manager (B blk) src_contract
      >>=? fun src ->
      let ({c_pk; s_pk; _ } as ba) =
      match key with
      | None -> new_baking_account src.pkh Consensus_key
      | Some k -> new_baking_account src.pkh k
      in
      Context.Contract.balance (B blk) dst_contract
      >>=? fun bal_dst ->
      Op.baking_account (B blk) src_contract (Some c_pk) (Some s_pk)
      >>=? fun op_ba ->
      Block.bake blk ~operation:op_ba
      >>=? fun blk ->
      let amount = Tez.one_mutez in
      (match key with
      | None ->
         Op.transaction_baking_account (B blk) ba dst_contract amount ~sk:src.sk
      | Some _ ->
         Op.transaction_baking_account (B blk) ba dst_contract amount)
      >>=? fun op_tx ->
      Block.bake blk ~operation:op_tx
      >>= fun res ->
      (match key with
       | None ->
         (Assert.proto_error ~loc:__LOC__ res (function
              | Operation_repr.Invalid_signature -> true
              | _ -> false ))
       | Some _ -> res >>?= fun blk ->
         Assert.balance_was_credited ~loc:__LOC__ (B blk) dst_contract bal_dst amount
      )

   let test_baking_account_transaction_by_spending_key =
     test_baking_account_transaction (Some Spending_key)

   let test_baking_account_transaction_by_consensus_key =
     test_baking_account_transaction (Some Consensus_key)

   let test_baking_account_transaction_by_arbitrary_key =
     test_baking_account_transaction None
end

module Test_Keychain = struct
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

  (* pretty print a block info *)
  let pp_intel block idx =
    let open Block in
    let cl = block.header.shell.level |> Int32.to_int in
    current_cycle block
    >>=? fun cc ->
    let () = Format.fprintf
        Format.std_formatter "Block%d: %d\t (%a)\n" idx cl Cycle.pp cc
    in return_unit

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
    let _policy = By_priority 0 in
    Context.init 1
    >>=? fun (b_pre_init, cs) ->
    Incremental.begin_construction b_pre_init
    >>=? fun incr ->
    let ctx = Incremental.alpha_ctxt incr in
    let acc = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth cs 0 in
    Context.Contract.pkh acc
    >>=? fun pkh ->
    let (_pkhA, pkA, _skA) = Signature.generate_key () in
    let (_pkhB, _pkB, _skB) = Signature.generate_key () in
    let _master_key_delay_cycles =
      (Constants.parametric ctx).master_key_delay_cycles in
    (* check: there is no master key for pkh *)
    checkMasterKey b_pre_init pkh None
    >>=? fun () ->
    (* init keychain with master = pkA *)
    Op.baking_account (B b_pre_init) acc (Some pkA) (Some pkA)
    >>=? fun operation ->
    Block.bake b_pre_init ~operation
    >>=? fun b_init ->
    (* check: the master of pkh == pkA *)
    checkMasterKey b_init pkh (Some pkA)
    >>=? fun () ->
    (* [TODO] waiting for counter-pkh-pk problem to be fix *)
    (*
    Op.baking_account (B b_init) ~sk:skA acc (Some pkB) None
    >>=? fun _operation ->
    Block.bake b_init ~operation
    >>=? fun b_updated ->
    checkMasterKey b_updated pkh (Some pkA)
    >>=? fun () ->
    bake_until_n_cycle_end ~policy n b1
    *)
    return_unit

end

let tests =
  [ Test_services.tztest
      "keychain: initializing and existence"
      `Quick
      Test_Keychain.test_create;
    Test_services.tztest
      "keychain: accessing"
      `Quick
      Test_Keychain.test_access;
    Test_services.tztest
      "keychain: delayed updating"
      `Quick
      Test_Keychain.test_delayed_update;
    Test_services.tztest
      "baking account test creating keys"
      `Quick
      Test_Baking_account.test_sample_baking_account_op;
    Test_services.tztest
      "baking account test transaction by consensus key"
      `Quick
      Test_Baking_account.test_baking_account_transaction_by_consensus_key;
    Test_services.tztest
      "baking account test transaction by spending key"
      `Quick
      Test_Baking_account.test_baking_account_transaction_by_spending_key;
    Test_services.tztest
      "baking account test transaction by arbitrary key"
      `Quick
      Test_Baking_account.test_baking_account_transaction_by_arbitrary_key ;
  ]