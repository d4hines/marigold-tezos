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
    let ck = Account.new_account () in
    let sk = Account.new_account () in
    let kh' = (match Contract.is_implicit new_c with
      | Some kh -> kh
      | None -> Stdlib.failwith "not implicit account")
    in
    Incremental.begin_construction blk
    >>=? fun incr ->
    let ctxt = Incremental.alpha_ctxt incr in
    Keychain.exists ctxt kh'
    >>= fun (is_exist) ->
    Assert.equal_bool ~loc:__LOC__ is_exist false
    >>=? fun () ->
    Op.baking_account (B blk) new_c (Some ck.pk) (Some sk.pk)
    >>=? fun operation ->
    Block.bake blk ~operation
    >>=? fun blk ->
    Incremental.begin_construction blk
    >>=? fun incr ->
    let ctxt = Incremental.alpha_ctxt incr in
    Keychain.find ctxt kh'
    >>= wrap >>=? function
    | Some {master_key; spending_key; _} ->
      (if Signature.Public_key.(master_key <> ck.pk) then
         Stdlib.failwith "master_key wasn't set correctly."
       else if Signature.Public_key.(spending_key <> sk.pk) then
         Stdlib.failwith "spending_key wasn't set correctly."
       else
         return ())
    | None -> Stdlib.failwith "key hash should be found."
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
    is_some "get spending key from pkh0"
    @@ Keychain.get_spending_key ctx pkh0
    >>=? fun sk0 ->
    Assert.equal_pk ~loc:__LOC__ pkB sk0
    >>=? fun () ->
    is_none "get spending key from pkh1"
    @@ Keychain.get_spending_key ctx pkh1
    >>=? fun () ->
    is_some "get master key from pkh0"
    @@ Keychain.get_master_key ctx pkh0
    >>=? fun mk0 ->
    Assert.equal_pk ~loc:__LOC__ pkA mk0
    >>=? fun () ->
    is_none "get master key from pkh1"
    @@ Keychain.get_master_key ctx pkh1
    >>=? fun () ->
    (* init keychain for pkh1 with its manager*)
    Keychain.init_with_manager ctx pkh1 None
    >>= wrap >>=? fun ctx ->
    (* getting keys on pkhs:
       spending key from pkh1 == mgtk1
       master key from pkh1 == mgtk1 *)
    is_some "get master key from pkh1"
    @@ Keychain.get_master_key ctx pkh1
    >>=? fun mk1 ->
    Assert.equal_pk ~loc:__LOC__ mk1 mgtk1
    >>=? fun () ->
    is_some "get spending key from pkh1"
    @@ Keychain.get_spending_key ctx pkh1
    >>=? fun sk1 ->
    Assert.equal_pk ~loc:__LOC__ sk1 mgtk1
    >>=? fun () ->
    (* setting spending key:
       update spending key for pkh1 with pkB
       spending key from pkh1 == pkB *)
    Keychain.set_spending_key ctx pkh1 pkB
    >>= wrap >>=? fun ctx ->
    is_some "get spending key from pkh1"
    @@ Keychain.get_spending_key ctx pkh1
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
    is_some "get master key from pkh1"
    @@ Keychain.get_master_key ctx pkh1
    >>=? fun mk1 ->
    Assert.equal_pk ~loc:__LOC__ mk1 mgtk1
    >>=? fun () ->
    return_unit

  (* [TODO] test case for delayed master key update *)
  let test_delayed_update () =
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
  ]
