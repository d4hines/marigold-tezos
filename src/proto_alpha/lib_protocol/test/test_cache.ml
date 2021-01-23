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

(* predefined local and/or aux functions *)

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

let init_ctx_with_gas_limit (gas_limit : int) =
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

module BigmapUtil = struct
  let get_list ctx =
    let cache = Raw_context.get_carbonated_cache ctx in
    Raw_context.Cache.get_content_list cache

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

(*****************************************************************************)
(* Test case I : basic cache properties, including:                          *)
(*                                                                           *)
(*   + Operability: cache is working for Creation/Reading/Deleting.          *)
(*   + Reduction: the gas cost of access should be reduced                   *)
(*                                                                           *)
(* All tests are categoried as *safe* and *unsafe*: safe raise no exception  *)
(* yet unsafe might.                                                         *)
(*****************************************************************************)
module Operability = struct
  let safe_introduce () =
    init_ctx_with_gas_limit 100_000_000
    >>=? fun (ctx, bm_id) ->
    let (k1, v1, _decost1) = mockdata 1 in
    let (k2, v2, _decost2) = mockdata 2 in
    BigmapUtil.set_option ~loc:__LOC__ ctx bm_id k2 (Some v2) 1
    >>=? fun ctx ->
    BigmapUtil.init_set ~loc:__LOC__ ctx bm_id k1 v1 2
    >>=? fun ctx ->
    BigmapUtil.init_set ~loc:__LOC__ ctx bm_id k1 v2 2
    >>=? fun ctx ->
    BigmapUtil.set_option ~loc:__LOC__ ctx bm_id k1 (Some v1) 2
    >>=? fun _ctx -> return_unit

  let safe_obtain () =
    init_ctx_with_gas_limit 100_000_000
    >>=? fun (ctx, bm_id) ->
    let (k1, v1, _decost1) = mockdata 1 in
    let (k2, _v2, _decost2) = mockdata 2 in
    BigmapUtil.init_set ~loc:__LOC__ ctx bm_id k1 v1 1
    >>=? fun ctx ->
    BigmapUtil.mem ~loc:__LOC__ ctx bm_id k1 1 true
    >>=? fun ctx ->
    BigmapUtil.mem ~loc:__LOC__ ctx bm_id k2 1 false
    >>=? fun ctx ->
    BigmapUtil.get_option ~loc:__LOC__ ctx bm_id k1 1 true
    >>=? fun ctx ->
    BigmapUtil.get_option ~loc:__LOC__ ctx bm_id k2 1 false
    >>=? fun _ctx -> return_unit

  let safe_eliminate () =
    init_ctx_with_gas_limit 100_000_000
    >>=? fun (ctx, bm_id) ->
    let (k1, v1, _decost1) = mockdata 1 in
    let (k2, v2, _decost2) = mockdata 2 in
    let (k3, _v3, _decost3) = mockdata 3 in
    BigmapUtil.init_set ~loc:__LOC__ ctx bm_id k1 v1 1
    >>=? fun ctx ->
    BigmapUtil.init_set ~loc:__LOC__ ctx bm_id k2 v2 2
    >>=? fun ctx ->
    BigmapUtil.remove ~loc:__LOC__ ctx bm_id k3 2
    >>=? fun ctx ->
    BigmapUtil.set_option ~loc:__LOC__ ctx bm_id k3 None 2
    >>=? fun ctx ->
    BigmapUtil.remove ~loc:__LOC__ ctx bm_id k2 1
    >>=? fun ctx ->
    BigmapUtil.set_option ~loc:__LOC__ ctx bm_id k1 None 0
    >>=? fun _ctx -> return_unit

  let unsafe_introduce () =
    init_ctx_with_gas_limit 100_000_000
    >>=? fun (ctx, bm_id) ->
    let (k1, v1, _decost1) = mockdata 1 in
    let (k2, v2, _decost2) = mockdata 2 in
    let (k3, _v3, _decost3) = mockdata 3 in
    BigmapUtil.init ~loc:__LOC__ ctx bm_id k1 v1 1
    >>=? fun ctx ->
    BigmapUtil.init_err ~loc:__LOC__ ctx bm_id k1 v1 1
    >>=? fun ctx ->
    BigmapUtil.init_err ~loc:__LOC__ ctx bm_id k1 v2 1
    >>=? fun ctx ->
    BigmapUtil.init ~loc:__LOC__ ctx bm_id k2 v2 2
    >>=? fun ctx ->
    BigmapUtil.set ~loc:__LOC__ ctx bm_id k2 v1 2
    >>=? fun ctx ->
    BigmapUtil.set_err ~loc:__LOC__ ctx bm_id k3 v1 2
    >>=? fun _ctx -> return_unit

  let unsafe_obtain () =
    init_ctx_with_gas_limit 100_000_000
    >>=? fun (ctx, bm_id) ->
    let (k1, v1, _decost1) = mockdata 1 in
    let (k2, _v2, _decost2) = mockdata 2 in
    BigmapUtil.init ~loc:__LOC__ ctx bm_id k1 v1 1
    >>=? fun ctx ->
    BigmapUtil.get_err ~loc:__LOC__ ctx bm_id k2 1
    >>=? fun ctx ->
    BigmapUtil.get ~loc:__LOC__ ctx bm_id k1 1 >>=? fun _ctx -> return_unit

  let unsafe_eliminate () =
    init_ctx_with_gas_limit 100_000_000
    >>=? fun (ctx, bm_id) ->
    let (k1, v1, _decost1) = mockdata 1 in
    let (k2, _v2, _decost2) = mockdata 2 in
    BigmapUtil.init ~loc:__LOC__ ctx bm_id k1 v1 1
    >>=? fun ctx ->
    BigmapUtil.delete_err ~loc:__LOC__ ctx bm_id k2 1
    >>=? fun ctx ->
    BigmapUtil.delete ~loc:__LOC__ ctx bm_id k1 0 >>=? fun _ctx -> return_unit
end

module Reduction = struct
  let safe_access () =
    init_ctx_with_gas_limit 100_000_000
    >>=? fun (ctx, bm_id) ->
    let (k1, v1, decost1) = mockdata 1 in
    let (k2, _v2, _decost2) = mockdata 2 in
    BigmapUtil.init_set ~loc:__LOC__ ctx bm_id k1 v1 1
    >>=? fun ctx ->
    BigmapUtil.mem ~loc:__LOC__ ctx bm_id k1 1 true
    >>=? fun ctx' ->
    let consumed_gas =
      Gas.consumed ~since:(patronum_ctx ctx) ~until:(patronum_ctx ctx')
    in
    Assert.Gas.eq_z ~loc:__LOC__ consumed_gas
    >>=? fun () ->
    BigmapUtil.get_option ~loc:__LOC__ ctx' bm_id k1 1 true
    >>=? fun ctx ->
    let consumed_gas =
      Gas.consumed ~since:(patronum_ctx ctx') ~until:(patronum_ctx ctx)
    in
    Assert.Gas.eq ~loc:__LOC__ (patronum_gas decost1) consumed_gas
    >>=? fun () ->
    BigmapUtil.get_option ~loc:__LOC__ ctx bm_id k2 1 false
    >>=? fun ctx' ->
    let mem_cost_gas =
      Storage_costs.read_access ~path_length:7 ~read_bytes:0
    in
    let consumed_gas =
      Gas.consumed ~since:(patronum_ctx ctx) ~until:(patronum_ctx ctx')
    in
    Assert.Gas.eq ~loc:__LOC__ (patronum_gas mem_cost_gas) consumed_gas
    >>=? fun () -> return_unit

  let unsafe_access () =
    init_ctx_with_gas_limit 100_000_000
    >>=? fun (ctx, bm_id) ->
    let (k1, v1, decost1) = mockdata 1 in
    let (k2, _v2, _decost2) = mockdata 2 in
    BigmapUtil.init_set ~loc:__LOC__ ctx bm_id k1 v1 1
    >>=? fun ctx ->
    BigmapUtil.mem ~loc:__LOC__ ctx bm_id k1 1 true
    >>=? fun ctx' ->
    BigmapUtil.get ~loc:__LOC__ ctx' bm_id k1 1
    >>=? fun ctx ->
    let consumed_gas =
      Gas.consumed ~since:(patronum_ctx ctx') ~until:(patronum_ctx ctx)
    in
    Assert.Gas.eq ~loc:__LOC__ (patronum_gas decost1) consumed_gas
    >>=? fun () ->
    BigmapUtil.get_err ~loc:__LOC__ ctx bm_id k2 1
    >>=? fun ctx' ->
    let consumed_gas =
      Gas.consumed ~since:(patronum_ctx ctx) ~until:(patronum_ctx ctx')
    in
    Assert.Gas.eq_z ~loc:__LOC__ consumed_gas >>=? fun () -> return_unit
end

(*****************************************************************************)
(* Test case II : Caching data for Carbonated_Map,                           *)
(*                including Contract.Code and Contract.Storage.              *)
(*****************************************************************************)
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
    (* init block with 1 account *)
    Context.init 1
    >>=? fun (b, srcs) ->
    let src0 = Option.get @@ List.nth srcs 0 in
    Incremental.begin_construction b
    >>=? fun inc ->
    (* [check] there are only 1 contract(account) in alpha_ctxt *)
    let ctx = Incremental.alpha_ctxt inc in
    ScriptHelper.getContracts ctx
    >>=? fun cs ->
    Assert.equal_int ~loc:__LOC__ (List.length cs) 1
    >>=? fun () ->
    (* deploy testing contract *)
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
        let cctnt = Raw_context.Cache.get_content_list cache in
        Assert.equal_int ~loc:__LOC__ (List.length cctnt) 0
        >>=? fun () ->
        (* get the deployed contract *)
        let ctx_t1 = ctx in
        M.get (patronum_ctx ctx) dst
        >>= wrap
        >>=? fun (ctx, _v) ->
        let ctx_t2 = ctx in
        (* [check] contract is well-cached aft get *)
        let cache = Raw_context.get_carbonated_cache ctx in
        let cctnt = Raw_context.Cache.get_content_list cache in
        Assert.equal_int ~loc:__LOC__ (List.length cctnt) 1
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

(*****************************************************************************)
(* Test case III : cache size is restricted within 1MB in testing mode.      *)
(*****************************************************************************)
module Limitation = struct
  let roughly3KBstring (i : int) : string =
    let c = Char.chr i in
    Bytes.to_string (Bytes.make 300_000 c)

  let mock_bigdata (i : int) =
    let str_i = string_of_int i in
    let key = Script_expr_hash.hash_string [str_i] in
    let value = Micheline.strip_locations @@ String (0, roughly3KBstring i) in
    let decost = Script_repr.deserialized_cost value in
    (key, value, decost)

  let get_remnant ctx =
    let cache = Raw_context.get_carbonated_cache ctx in
    Raw_context.Cache.get_remnant cache

  (* ## testing function *)
  let size_limitation () =
    init_ctx_with_gas_limit 100_000_000
    >>=? fun (ctx, bm_id) ->
    (*  *)
    let ctx = Raw_context.set_testing_cache (patronum_ctx ctx) in
    let (k1, v1, _decost1) = mock_bigdata 65 in
    let (k2, v2, _decost2) = mock_bigdata 66 in
    let (k3, v3, _decost3) = mock_bigdata 67 in
    let (k4, v4, _decost3) = mock_bigdata 68 in
    let (k5, v5, _decost5) = mock_bigdata 69 in
    (*  *)
    BigmapUtil.init_set ~loc:__LOC__ ctx bm_id k1 v1 1
    >>=? fun ctx ->
    let cremnent = get_remnant ctx in
    let required_size = 1 * (300_000 + 5) in
    Assert.equal_int ~loc:__LOC__ cremnent (1_000_000 - required_size)
    >>=? fun () ->
    (*  *)
    BigmapUtil.init_set ~loc:__LOC__ ctx bm_id k2 v2 2
    >>=? fun ctx ->
    let cremnent = get_remnant ctx in
    let required_size = 2 * (300_000 + 5) in
    Assert.equal_int ~loc:__LOC__ cremnent (1_000_000 - required_size)
    >>=? fun () ->
    (*  *)
    BigmapUtil.init_set ~loc:__LOC__ ctx bm_id k3 v3 3
    >>=? fun ctx ->
    let cremnent = get_remnant ctx in
    let required_size = 3 * (300_000 + 5) in
    Assert.equal_int ~loc:__LOC__ cremnent (1_000_000 - required_size)
    >>=? fun () ->
    (*  *)
    let cache = Raw_context.get_carbonated_cache ctx in
    let klist = Raw_context.Cache.get_keys_list cache in
    Assert.equal_int ~loc:__LOC__ (List.length klist) 3
    >>=? fun () ->
    (*  *)
    BigmapUtil.init_set ~loc:__LOC__ ctx bm_id k4 v4 3
    >>=? fun ctx ->
    let cache = Raw_context.get_carbonated_cache ctx in
    let klist = Raw_context.Cache.get_keys_list cache in
    Assert.equal_int ~loc:__LOC__ (List.length klist) 3
    >>=? fun () ->
    (*  *)
    BigmapUtil.init_set ~loc:__LOC__ ctx bm_id k5 v5 3
    >>=? fun ctx ->
    let cache = Raw_context.get_carbonated_cache ctx in
    let klist = Raw_context.Cache.get_keys_list cache in
    Assert.equal_int ~loc:__LOC__ (List.length klist) 3
    >>=? fun () ->
    (*  *)
    let cache = Raw_context.get_carbonated_cache ctx in
    let klist = Raw_context.Cache.get_keys_list cache in
    (* the head of queue must be k5 *)
    match List.nth klist 0 with
    | None ->
        failwith "List.nth klist 0 doesn't exist"
    | Some kn -> (
        let k5_prefix = "03170a2e75970big_mapsindex03170a2e75970contents" in
        let k5_str = String.concat "" (Script_expr_hash.to_path k5 ["data"]) in
        Assert.equal
          ~loc:__LOC__
          String.equal
          "aren't equal"
          Format.pp_print_string
          (String.concat "" kn)
          (String.concat "" [k5_prefix; k5_str])
        >>=? fun () ->
        return_unit
        >>=? fun () ->
        (* the last of queue must be k3 *)
        match List.nth klist 2 with
        | None ->
            failwith "List.nth klist 2 doesn't exist"
        | Some kn ->
            let k3_prefix =
              "03170a2e75970big_mapsindex03170a2e75970contents"
            in
            let k3_str =
              String.concat "" (Script_expr_hash.to_path k3 ["data"])
            in
            Assert.equal
              ~loc:__LOC__
              String.equal
              "aren't equal"
              Format.pp_print_string
              (String.concat "" kn)
              (String.concat "" [k3_prefix; k3_str])
            >>=? fun () -> return_unit )
end

(*****************************************************************************)
(* Setup all test cases                                                      *)
(*****************************************************************************)
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

let size_limitation () =
  Limitation.size_limitation () >>=? fun () -> return_unit

let tests =
  [ Test.tztest "test_cache_operability_safe" `Quick operability_safe;
    Test.tztest "test_cache_operability_unsafe" `Quick operability_unsafe;
    Test.tztest "test_cache_reduction_safe" `Quick reduction_safe;
    Test.tztest "test_cache_reduction_unsafe" `Quick reduction_unsafe;
    Test.tztest "test_cache_contract_code" `Quick contract_code;
    Test.tztest "test_cache_contract_storage" `Quick contract_storage;
    Test.tztest "test_cache_limited_size" `Quick size_limitation ]
