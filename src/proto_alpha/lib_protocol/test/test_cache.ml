(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Morigold, <contact@marigold.dev>                       *)
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
open Micheline

let wrap e = Lwt.return (Environment.wrap_error e)

let patronum_idx = Obj.magic

let patronum_ctx = Obj.magic

let patronum_gas = Obj.magic

let mockdata (i : int) =
  let str_i = string_of_int i in
  let key = Script_expr_hash.hash_string [str_i] in
  let value = Micheline.strip_locations @@ String (0, "data" ^ str_i) in
  let decost = Script_repr.deserialized_cost value in
  (key, value, decost)

let init (gas_limit : int) =
  Context.init 1
  >>=? fun (b, _) ->
  Incremental.begin_construction b
  >>=? fun incr ->
  let ctx = Incremental.alpha_ctxt incr in
  let gas_limit = Gas.Arith.(fp (integral_of_int gas_limit)) in
  let ctx = Gas.set_limit ctx gas_limit in
  Alpha_context.Big_map.fresh ~temporary:false ctx
  >>= wrap
  >>=? fun x -> return x

(* # cache utilities and wraps *)
module Cache = struct
  let get_list ctx = Raw_context.get_carbonated_cache ctx

  let leng ctx = List.length (get_list ctx)

  let set_option ~loc ctx bmid k v n =
    Storage.Big_map.Contents.set_option
      (patronum_ctx ctx, patronum_idx bmid)
      k
      v
    >>= wrap
    >>=? fun (ctx, _i, _b) ->
    Assert.equal_int ~loc (leng ctx) n >>=? fun () -> return ctx

  let init_set ~loc ctx bmid k v n =
    Storage.Big_map.Contents.init_set (patronum_ctx ctx, patronum_idx bmid) k v
    >>= wrap
    >>=? fun (ctx, _i, _b) ->
    Assert.equal_int ~loc (leng ctx) n >>=? fun () -> return ctx

  let remove ~loc ctx bmid k n =
    Storage.Big_map.Contents.remove (patronum_ctx ctx, patronum_idx bmid) k
    >>= wrap
    >>=? fun (ctx, _i, _b) ->
    Assert.equal_int ~loc (leng ctx) n >>=? fun () -> return ctx

  let mem ~loc ctx bmid k n b0 =
    Storage.Big_map.Contents.mem (patronum_ctx ctx, patronum_idx bmid) k
    >>= wrap
    >>=? fun (ctx, b) ->
    Assert.equal_int ~loc (leng ctx) n
    >>=? fun () -> Assert.equal_bool ~loc b b0 >>=? fun () -> return ctx

  let get_option ~loc ctx bmid k n b0 =
    Storage.Big_map.Contents.get_option (patronum_ctx ctx, patronum_idx bmid) k
    >>= wrap
    >>=? fun (ctx, v_op) ->
    Assert.equal_int ~loc (leng ctx) n
    >>=? fun () ->
    let b = match v_op with None -> false | Some _ -> true in
    Assert.equal_bool ~loc b b0 >>=? fun () -> return ctx

  let init ~loc ctx bmid k v n =
    Storage.Big_map.Contents.init (patronum_ctx ctx, patronum_idx bmid) k v
    >>= wrap
    >>= function
    (* [TODO] separate (Storage_error (Existing_key _)) from others *)
    | Error errs ->
        Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_error errs
    | Ok (ctx, _i) ->
        Assert.equal_int ~loc (leng ctx) n >>=? fun () -> return ctx

  let init_err ~loc ctx bmid k v n =
    Storage.Big_map.Contents.init (patronum_ctx ctx, patronum_idx bmid) k v
    >>= wrap
    >>= function
    | Ok _ ->
        Alcotest.fail "expected an Error from init"
    (* [TODO] separate (Storage_error (Existing_key _)) from others *)
    | Error _ ->
        Assert.equal_int ~loc (leng ctx) n >>=? fun () -> return ctx

  let set ~loc ctx bmid k v n =
    Storage.Big_map.Contents.set (patronum_ctx ctx, patronum_idx bmid) k v
    >>= wrap
    >>= function
    (* [TODO] separate (Storage_error (Missing_key _)) from others *)
    | Error errs ->
        Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_error errs
    | Ok (ctx, _i) ->
        Assert.equal_int ~loc (leng ctx) n >>=? fun () -> return ctx

  let set_err ~loc ctx bmid k v n =
    Storage.Big_map.Contents.set (patronum_ctx ctx, patronum_idx bmid) k v
    >>= wrap
    >>= function
    | Ok _ ->
        Alcotest.fail "expected an Error from set"
    (* [TODO] separate (Storage_error (Missing_key _)) from others *)
    | Error _ ->
        Assert.equal_int ~loc (leng ctx) n >>=? fun () -> return ctx

  let delete ~loc ctx bmid k n =
    Storage.Big_map.Contents.delete (patronum_ctx ctx, patronum_idx bmid) k
    >>= wrap
    >>= function
    (* [TODO] separate (Storage_error (Missing_key _)) from others *)
    | Error errs ->
        Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_error errs
    | Ok (ctx, _i) ->
        Assert.equal_int ~loc (leng ctx) n >>=? fun () -> return ctx

  let delete_err ~loc ctx bmid k n =
    Storage.Big_map.Contents.delete (patronum_ctx ctx, patronum_idx bmid) k
    >>= wrap
    >>= function
    | Ok _ ->
        Alcotest.fail "expected an Error from delete"
    (* [TODO] separate (Storage_error (Missing_key _)) from others *)
    | Error _ ->
        Assert.equal_int ~loc (leng ctx) n >>=? fun () -> return ctx

  let get ~loc ctx bmid k n =
    Storage.Big_map.Contents.get (patronum_ctx ctx, patronum_idx bmid) k
    >>= wrap
    >>= function
    (* [TODO] separate (Storage_error (Missing_key _)) from others *)
    | Error errs ->
        Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_error errs
    | Ok (ctx, _v) ->
        Assert.equal_int ~loc (leng ctx) n >>=? fun () -> return ctx

  let get_err ~loc ctx bmid k n =
    Storage.Big_map.Contents.get (patronum_ctx ctx, patronum_idx bmid) k
    >>= wrap
    >>= function
    | Ok _ ->
        Alcotest.fail "expected an Error from get"
    (* [TODO] separate (Storage_error (Existing_key _)) from others *)
    | Error _ ->
        Assert.equal_int ~loc (leng ctx) n >>=? fun () -> return ctx
end

(* # testing for cache inhabitancy *)
module Operability = struct
  (* ## safe: no exception *)
  (* ### safe_introduce *)
  let safe_introduce () =
    init 100_000_000
    >>=? fun (ctx, bm_id) ->
    let (k1, v1, _decost1) = mockdata 1 in
    let (k2, v2, _decost2) = mockdata 2 in
    Cache.set_option ~loc:__LOC__ ctx bm_id k2 (Some v2) 1
    >>=? fun ctx ->
    Cache.init_set ~loc:__LOC__ ctx bm_id k1 v1 2
    >>=? fun ctx ->
    Cache.init_set ~loc:__LOC__ ctx bm_id k1 v2 2
    >>=? fun ctx ->
    Cache.set_option ~loc:__LOC__ ctx bm_id k1 (Some v1) 2
    >>=? fun _ctx -> return_unit

  (* ### safe_obtain *)
  let safe_obtain () =
    init 100_000_000
    >>=? fun (ctx, bm_id) ->
    let (k1, v1, _decost1) = mockdata 1 in
    let (k2, _v2, _decost2) = mockdata 2 in
    Cache.init_set ~loc:__LOC__ ctx bm_id k1 v1 1
    >>=? fun ctx ->
    Cache.mem ~loc:__LOC__ ctx bm_id k1 1 true
    >>=? fun ctx ->
    Cache.mem ~loc:__LOC__ ctx bm_id k2 1 false
    >>=? fun ctx ->
    Cache.get_option ~loc:__LOC__ ctx bm_id k1 1 true
    >>=? fun ctx ->
    Cache.get_option ~loc:__LOC__ ctx bm_id k2 1 false
    >>=? fun _ctx -> return_unit

  (* ### safe_eliminate *)
  let safe_eliminate () =
    init 100_000_000
    >>=? fun (ctx, bm_id) ->
    let (k1, v1, _decost1) = mockdata 1 in
    let (k2, v2, _decost2) = mockdata 2 in
    let (k3, _v3, _decost3) = mockdata 3 in
    Cache.init_set ~loc:__LOC__ ctx bm_id k1 v1 1
    >>=? fun ctx ->
    Cache.init_set ~loc:__LOC__ ctx bm_id k2 v2 2
    >>=? fun ctx ->
    Cache.remove ~loc:__LOC__ ctx bm_id k3 2
    >>=? fun ctx ->
    Cache.set_option ~loc:__LOC__ ctx bm_id k3 None 2
    >>=? fun ctx ->
    Cache.remove ~loc:__LOC__ ctx bm_id k2 1
    >>=? fun ctx ->
    Cache.set_option ~loc:__LOC__ ctx bm_id k1 None 0
    >>=? fun _ctx -> return_unit

  (* ## unsafe: may raise exception *)
  (* ### unsafe_introduce *)
  let unsafe_introduce () =
    init 100_000_000
    >>=? fun (ctx, bm_id) ->
    let (k1, v1, _decost1) = mockdata 1 in
    let (k2, v2, _decost2) = mockdata 2 in
    let (k3, _v3, _decost3) = mockdata 3 in
    Cache.init ~loc:__LOC__ ctx bm_id k1 v1 1
    >>=? fun ctx ->
    Cache.init_err ~loc:__LOC__ ctx bm_id k1 v1 1
    >>=? fun ctx ->
    Cache.init_err ~loc:__LOC__ ctx bm_id k1 v2 1
    >>=? fun ctx ->
    Cache.init ~loc:__LOC__ ctx bm_id k2 v2 2
    >>=? fun ctx ->
    Cache.set ~loc:__LOC__ ctx bm_id k2 v1 2
    >>=? fun ctx ->
    Cache.set_err ~loc:__LOC__ ctx bm_id k3 v1 2 >>=? fun _ctx -> return_unit

  (* ### unsafe_obtain *)
  let unsafe_obtain () =
    init 100_000_000
    >>=? fun (ctx, bm_id) ->
    let (k1, v1, _decost1) = mockdata 1 in
    let (k2, _v2, _decost2) = mockdata 2 in
    Cache.init ~loc:__LOC__ ctx bm_id k1 v1 1
    >>=? fun ctx ->
    Cache.get_err ~loc:__LOC__ ctx bm_id k2 1
    >>=? fun ctx ->
    Cache.get ~loc:__LOC__ ctx bm_id k1 1 >>=? fun _ctx -> return_unit

  (* ### unsafe_eliminate *)
  let unsafe_eliminate () =
    init 100_000_000
    >>=? fun (ctx, bm_id) ->
    let (k1, v1, _decost1) = mockdata 1 in
    let (k2, _v2, _decost2) = mockdata 2 in
    Cache.init ~loc:__LOC__ ctx bm_id k1 v1 1
    >>=? fun ctx ->
    Cache.delete_err ~loc:__LOC__ ctx bm_id k2 1
    >>=? fun ctx ->
    Cache.delete ~loc:__LOC__ ctx bm_id k1 0 >>=? fun _ctx -> return_unit
end

(* # testing for gas consumption *)
module Reduction = struct
  (* ## safe: no exception *)
  let safe_access () =
    init 100_000_000
    >>=? fun (ctx, bm_id) ->
    let (k1, v1, decost1) = mockdata 1 in
    let (k2, _v2, _decost2) = mockdata 2 in
    Cache.init_set ~loc:__LOC__ ctx bm_id k1 v1 1
    >>=? fun ctx ->
    Cache.mem ~loc:__LOC__ ctx bm_id k1 1 true
    >>=? fun ctx' ->
    let consumed_gas =
      Gas.consumed ~since:(patronum_ctx ctx) ~until:(patronum_ctx ctx')
    in
    Assert.Gas.eq_z ~loc:__LOC__ consumed_gas
    >>=? fun () ->
    Cache.get_option ~loc:__LOC__ ctx' bm_id k1 1 true
    >>=? fun ctx ->
    let consumed_gas =
      Gas.consumed ~since:(patronum_ctx ctx') ~until:(patronum_ctx ctx)
    in
    Assert.Gas.eq ~loc:__LOC__ (patronum_gas decost1) consumed_gas
    >>=? fun () ->
    Cache.get_option ~loc:__LOC__ ctx bm_id k2 1 false
    >>=? fun ctx' ->
    let mem_cost_gas =
      Storage_costs.read_access ~path_length:7 ~read_bytes:0
    in
    let consumed_gas =
      Gas.consumed ~since:(patronum_ctx ctx) ~until:(patronum_ctx ctx')
    in
    Assert.Gas.eq ~loc:__LOC__ (patronum_gas mem_cost_gas) consumed_gas
    >>=? fun () -> return_unit

  (* ## unsafe: may raise exception *)
  let unsafe_access () =
    init 100_000_000
    >>=? fun (ctx, bm_id) ->
    let (k1, v1, decost1) = mockdata 1 in
    let (k2, _v2, _decost2) = mockdata 2 in
    Cache.init_set ~loc:__LOC__ ctx bm_id k1 v1 1
    >>=? fun ctx ->
    Cache.mem ~loc:__LOC__ ctx bm_id k1 1 true
    >>=? fun ctx' ->
    Cache.get ~loc:__LOC__ ctx' bm_id k1 1
    >>=? fun ctx ->
    let consumed_gas =
      Gas.consumed ~since:(patronum_ctx ctx') ~until:(patronum_ctx ctx)
    in
    Assert.Gas.eq ~loc:__LOC__ (patronum_gas decost1) consumed_gas
    >>=? fun () ->
    Cache.get_err ~loc:__LOC__ ctx bm_id k2 1
    >>=? fun ctx' ->
    let consumed_gas =
      Gas.consumed ~since:(patronum_ctx ctx) ~until:(patronum_ctx ctx')
    in
    Assert.Gas.eq_z ~loc:__LOC__ consumed_gas >>=? fun () -> return_unit
end

(* local helper for testing contract cache *)
module ScriptHelper = struct
  let toplevel_from_string str =
    let (ast, errs) = Michelson_v1_parser.parse_toplevel ~check:true str in
    match errs with
    | [] ->
        ast.expanded
    | _ ->
        Stdlib.failwith "parse toplevel"

  let expression_from_string str =
    let (ast, errs) = Michelson_v1_parser.parse_expression ~check:true str in
    match errs with
    | [] ->
        ast.expanded
    | _ ->
        Stdlib.failwith "parse expression"

  let load_script file storage =
    let load_file f =
      let ic = open_in f in
      let res = really_input_string ic (in_channel_length ic) in
      close_in ic ; res
    in
    let contract_string = load_file file in
    let code = toplevel_from_string contract_string in
    let storage = expression_from_string storage in
    Alpha_context.Script.{code = lazy_expr code; storage = lazy_expr storage}

  let getContracts ctx = ok_unit >|?= fun () -> Alpha_context.Contract.list ctx
end

module TestContractCache
    (M : Storage_sigs.Non_iterable_indexed_carbonated_data_storage
           with type key = Contract_repr.t
            and type value = Script_repr.lazy_expr
            and type t := Raw_context.t) =
struct
  let run_test () =
    (* init block with two accounts *)
    Context.init 1
    >>=? fun (b, srcs) ->
    let src0 = Option.get @@ List.nth srcs 0 in
    Incremental.begin_construction b
    >>=? fun inc ->
    (* [check] there are only 1 contracts in alpha_ctxt *)
    let ctx = Incremental.alpha_ctxt inc in
    ScriptHelper.getContracts ctx
    >>=? fun cs ->
    Assert.equal_int ~loc:__LOC__ (List.length cs) 1
    >>=? fun () ->
    (* deploy contract *)
    let script =
      ScriptHelper.load_script "contracts/cache_contract_acc.tz" "0"
    in
    Op.origination (I inc) ~credit:Tez.one src0 ~script
    >>=? fun (op, dst) ->
    match Contract_repr.of_b58check (Contract.to_b58check dst) with
    | Error _ ->
        failwith "contract transformation failure"
    | Ok x ->
        return x
        >>=? fun dst ->
        Incremental.add_operation inc op
        >>=? fun inc ->
        Incremental.finalize_block inc
        >>=? fun b ->
        (* ready for testing *)
        Incremental.begin_construction b
        >>=? fun inc ->
        (* [check] there are only 3 contracts in alpha_ctxt *)
        let ctx = Incremental.alpha_ctxt inc in
        let gas_limit = Gas.Arith.(fp (integral_of_int 100_000_000)) in
        let ctx = Gas.set_limit ctx gas_limit in
        ScriptHelper.getContracts ctx
        >>=? fun cs ->
        Assert.equal_int ~loc:__LOC__ (List.length cs) 2
        >>=? fun () ->
        (* [check] the existency of deployed contract *)
        M.mem (patronum_ctx ctx) dst
        >>= wrap
        >>=? fun (ctx, b) ->
        Assert.equal_bool ~loc:__LOC__ b true
        >>=? fun () ->
        (* [check] contract is non-cached bef get *)
        let cache = Raw_context.get_carbonated_cache ctx in
        Assert.equal_int ~loc:__LOC__ (List.length cache) 0
        >>=? fun () ->
        (* get the deployed contract *)
        let ctx_t1 = ctx in
        M.get (patronum_ctx ctx) dst
        >>= wrap
        >>=? fun (ctx, _v) ->
        let ctx_t2 = ctx in
        (* [check] contract is well-cached aft get *)
        let cache = Raw_context.get_carbonated_cache ctx in
        Assert.equal_int ~loc:__LOC__ (List.length cache) 1
        >>=? fun () ->
        (* get the deployed contract - 2nd *)
        let ctx_t3 = ctx in
        M.get (patronum_ctx ctx) dst
        >>= wrap
        >>=? fun (ctx, _v) ->
        let ctx_t4 = ctx in
        (* [check] get for seconf time is strictly cheaper *)
        let delta_1 =
          Gas.consumed
            ~since:(patronum_ctx ctx_t1)
            ~until:(patronum_ctx ctx_t2)
        in
        let delta_2 =
          Gas.consumed
            ~since:(patronum_ctx ctx_t3)
            ~until:(patronum_ctx ctx_t4)
        in
        Assert.Gas.gt ~loc:__LOC__ delta_1 delta_2 >>=? fun () -> return_unit
end

(* # tztest tasks *)
let operability_safe () =
  Operability.safe_introduce ()
  >>=? fun () ->
  Operability.safe_obtain ()
  >>=? fun () -> Operability.safe_eliminate () >>=? fun () -> return_unit

let operability_unsafe () =
  Operability.unsafe_introduce ()
  >>=? fun () ->
  Operability.unsafe_obtain ()
  >>=? fun () -> Operability.unsafe_eliminate () >>=? fun () -> return_unit

let reduction_safe () = Reduction.safe_access () >>=? fun () -> return_unit

let reduction_unsafe () = Reduction.unsafe_access () >>=? fun () -> return_unit

module M1 = TestContractCache (Storage.Contract.Code)
module M2 = TestContractCache (Storage.Contract.Storage)

let contract_code () = M1.run_test () >>=? fun () -> return_unit

let contract_storage () = M2.run_test () >>=? fun () -> return_unit

let tests =
  [ Test.tztest "test_cache_operability_safe" `Quick operability_safe;
    Test.tztest "test_cache_operability_unsafe" `Quick operability_unsafe;
    Test.tztest "test_cache_reduction_safe" `Quick reduction_safe;
    Test.tztest "test_cache_reduction_unsafe" `Quick reduction_unsafe;
    Test.tztest "test_cache_contract_code" `Quick contract_code;
    Test.tztest "test_cache_contract_storage" `Quick contract_storage ]
