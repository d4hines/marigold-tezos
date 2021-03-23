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

let wrap e = Lwt.return (Environment.wrap_tzresult e)

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
        | Some {consensus_key; spending_key; _} ->
          (if Signature.Public_key.(consensus_key <> ck.pk) then
             Stdlib.failwith "consensus_key wasn't set correctly."
           else if Signature.Public_key.(spending_key <> sk.pk) then
             Stdlib.failwith "spending_key wasn't set correctly."
           else
              return ())
        | None -> Stdlib.failwith "key hash should be found."
end

module Test_Keychain = struct
  let test_init () =
    (*
    let open Format in
    let () = fprintf std_formatter "test=keychain exist/init\n" in
    Context.init 2
    >>=? fun (b ,bootstrap_contracts) ->
    (* obtain pkhs *)
    let bootstrap0 = WithExceptions.Option.get ~loc:__LOC__ @@
      List.nth bootstrap_contracts 0 in
    let bootstrap1 = WithExceptions.Option.get ~loc:__LOC__ @@
      List.nth bootstrap_contracts 1 in
    Context.Contract.pkh bootstrap0
    >>=? fun pkh0 ->
    Context.Contract.pkh bootstrap1
    >>=? fun pkh1 ->
    let () = fprintf std_formatter "PKH0 = %a\n" pkh_pp pkh0 in
    let () = fprintf std_formatter "PKH1 = %a\n" pkh_pp pkh1 in
    (* obtain context *)
    Incremental.begin_construction b
    >>=? fun incr ->
    let ctx = Incremental.alpha_ctxt incr in
    (* absence of keychain*)
    Keychain.exists ctx pkh0
    >>= fun existing ->
    Assert.equal_bool ~loc:__LOC__ existing false
    >>=? fun () ->
    Keychain.exists ctx pkh1
    >>= fun existing ->
    Assert.equal_bool ~loc:__LOC__ existing false
    >>=? fun () ->
    (* keychain initializing *)
    let (pkh, pk, _sk) = Signature.generate_key () in
    let () = fprintf std_formatter "pkh = %a\n" pkh_pp pkh in
    let () = fprintf std_formatter "pk = %a\n" pk_pp pk in
    Keychain.init ctx pkh0 {consensus_key = pk; spending_key = pk}
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
       *)
    return_unit

end

let test_none () = return_unit

let tests =
  [ Test_services.tztest
      "baking account keychain exist/init"
      `Quick
      Test_Keychain.test_init;
    Test_services.tztest "baking account empty test" `Quick test_none;
    Test_services.tztest "baking account test creating keys" `Quick Test_Baking_account.test_sample_baking_account_op;
  ]
