open Protocol
open Alpha_context

(** Testing
    -------
    Component:    Alpha_context 
    Invocation:   dune build @src/proto_alpha/lib_protocol/test/unit/unittests
    Dependencies: helpers/block.ml
    Subject:      To test the modules (including the top-level)
                  in alpha_context.ml as individual units, particularly
                  failure cases. Superficial goal: increase coverage percentage.
*)

(** Creates and Alpha_context without creating a full-fledged block *)
let create () =
  let accounts = Account.generate_accounts 1 in
  Block.alpha_context accounts

module Test_Script = struct
  (** Force serialise of lazy [Big_map.t] in a give [alpha_context] *)
  let force_bytes_in_context () =
    create ()
    >>=? fun alpha_context ->
    let mbytes_pp ppf t =
      Format.pp_print_string ppf (Environment.Bytes.to_string t)
    in
    let open Alpha_context.Script in
    Environment.wrap_tzresult
    @@ force_bytes_in_context alpha_context
    @@ lazy_expr @@ Micheline.strip_locations
    @@ Prim (0, D_Unit, [], [])
    >>?= fun (bytes, _) ->
    Assert.equal
      ~loc:__LOC__
      Environment.Bytes.equal
      "script serialised incorrectly"
      mbytes_pp
      bytes
      (`Hex "030b" |> Hex.to_bytes)
end

module Test_Global_constants = struct
  (** A default [Alpha_context.t] to work with.  *)
  let context =
    create ()
    >>=? (fun x -> Lwt.return @@ ok @@ Fees.start_counting_storage_fees x)
    |> Util.force_std_lwt

  (** Should be able to store a pair of ints and retrieve
  them successfully. *)
  let round_trip_pair () =
    let expr = Expr.from_string "Pair 3 7" in
    let (context, ty_expr) =
      "(pair int int)" |> Expr.from_string |> Expr.ty_from_expr context
    in
    Global_constants.set context "some constant" ty_expr expr
    >>=? (fun (context, _) -> Global_constants.get_opt context "some constant")
    >|= Environment.wrap_tzresult
    >>=? fun (_, value) ->
    match value with
    | Some (_, value) ->
        assert (value = expr) ;
        return_unit
    | None ->
        failwith "Global_constant round-trip failed."

  (* Should be able to store a string and retrieve it successfully. *)
  let round_trip_string () =
    let expr = Expr.from_string {|"foo"|} in
    let (context, ty_expr) = Expr.ty_from_string context "string" in
    Global_constants.set context "some constant" ty_expr expr
    >>=? (fun (context, _) -> Global_constants.get_opt context "some constant")
    >|= Environment.wrap_tzresult
    >>=? fun (_, value) ->
    match value with
    | Some (_, value) ->
        assert (value = expr) ;
        return_unit
    | None ->
        failwith "Global_constant round-trip failed."

  (* Shouldn't be allowed to set the same constant twice. *)
  let set_twice () =
    let expr = Expr.from_string {|"foo"|} in
    let (context, ty_expr) = Expr.ty_from_string context "string" in
    (* Set the first time *)
    Global_constants.set context "some constant" ty_expr expr
    >>=? (fun (context, _) ->
           (* Try setting a second time. *)
           Global_constants.set context "some constant" ty_expr expr)
    >|= Environment.wrap_tzresult
    >>= fun result ->
    match result with
    | Ok _ ->
        failwith "Should get an error"
    | Error err ->
        Util.assert_error_has_message err "Set on existing global constant" ;
        return_unit
end

module Test_Big_map = struct
  (** Test failure path: look for a non-existent key in a [Big_map] *)
  let mem () =
    create ()
    >>=? (fun alpha_context ->
           Big_map.fresh ~temporary:true alpha_context
           >|= Environment.wrap_tzresult
           >>=? fun (alpha_context, big_map_id) ->
           Big_map.mem
             alpha_context
             big_map_id
             (Script_expr_hash.hash_string ["0"; "0"])
           >|= Environment.wrap_tzresult)
    >>=? fun (_alpha_context, is_member) ->
    Assert.equal_bool ~loc:__LOC__ is_member false

  (** Test failure code path of [get_opt] by looking for missing key in a [Big_map.t] *)
  let get_opt () =
    create ()
    >>=? (fun alpha_context ->
           Big_map.fresh ~temporary:true alpha_context
           >|= Environment.wrap_tzresult
           >>=? fun (alpha_context, big_map_id) ->
           Big_map.get_opt
             alpha_context
             big_map_id
             (Script_expr_hash.hash_string ["0"; "0"])
           >|= Environment.wrap_tzresult)
    >>=? fun (_alpha_context, value) ->
    match value with
    | Some _ ->
        failwith "get_opt should have failed looking for a non-existent key"
    | None ->
        return_unit

  (** Test existence of a non-existent [Big_map] in an [Alpha_context.t] *)
  let exists () =
    create ()
    >>=? (fun alpha_context ->
           Big_map.fresh ~temporary:true alpha_context
           >|= Environment.wrap_tzresult
           >>=? fun (alpha_context, big_map_id) ->
           Big_map.exists alpha_context big_map_id
           >|= Environment.wrap_tzresult)
    >>=? fun (_alpha_context, value) ->
    match value with
    | Some _ ->
        failwith "exists should have failed looking for a non-existent big_map"
    | None ->
        return_unit
end

let tests =
  [ Test.tztest
      "Global_constant get and set round-trip with a pair: happy path."
      `Quick
      Test_Global_constants.round_trip_pair;
    Test.tztest
      "Global_constant get and set round-trip with a string: happy path."
      `Quick
      Test_Global_constants.round_trip_pair;
    Test.tztest
      "Global_constant.set: Test that you can't set the same constant twice."
      `Quick
      Test_Global_constants.set_twice;
    Test.tztest
      "Script.force_bytes_in_context: checks if it serialises a simple \
       michelson expression"
      `Quick
      Test_Script.force_bytes_in_context;
    Test.tztest
      "Big_map.mem: failure case - must return false when starting with an \
       empty map"
      `Quick
      Test_Big_map.mem;
    Test.tztest
      "Big_map.get_opt: failure case - looking up key that doesn't exist"
      `Quick
      Test_Big_map.get_opt;
    Test.tztest
      "Big_map.exists: failure case - looking up big_map that doesn't exist"
      `Quick
      Test_Big_map.exists ]
