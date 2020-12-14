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

let tests =
  [ Test.tztest "test_cache_operability_safe" `Quick operability_safe;
    Test.tztest "test_cache_operability_unsafe" `Quick operability_unsafe;
    Test.tztest "test_cache_reduction_safe" `Quick reduction_safe;
    Test.tztest "test_cache_reduction_unsafe" `Quick reduction_unsafe ]

(*
# note_1

For `Storage_costs.read_access ~path_length:7 ~read_bytes:0`,
the path_length will be determined by

`List.length (data_key index)`

where, the `data_key` calls

`I.to_path i [data_name]`

where, the `I.to_path` will be re-routed to the `to_path` defined in
**src/lib_crypto/bloke2B.ml** which provides a fixed format.

As result, in this test file, an index introduced by
`Script_expr_hash.hash_string` will always have its data key length = 7.
*)
