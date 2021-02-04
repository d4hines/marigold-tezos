(** Testing
    -------
    Component:    Memory_context
    Invocation:   dune build @unittests src/lib_protocol_environment/test/unit
    Dependencies: --
    Subject:      Unit tests for memory_context.ml
*)

let c = function None -> None | Some s -> Some (Bytes.to_string s)

(** Tests for Memory_context **)
module Test_memory_context = struct
  let basic_ops () =
    let mc = Memory_context.empty in
    let foovalue = Bytes.of_string "foovalue" in
    Context.add mc ["foo"; "bar"] foovalue
    >>= fun mc ->
    Context.find mc ["foo"; "bar"]
    >>= fun value_opt ->
    Assert.equal_string_option (Some "foovalue") (c value_opt) ;
    Context.mem mc ["foo"; "bar"]
    >>= fun is_member ->
    Assert.equal
      true
      is_member
      ~msg:".mem incorrectly returned false for a valid key" ;
    Lwt.return_unit
    >>= fun () ->
    Context.find mc ["foo"; "doesnt exist"]
    >>= (function
          | Some _ ->
              Assert.fail_msg
                ".get should have returned None. Returned something instead"
          | None ->
              Lwt.return_unit)
    >>= fun () ->
    Context.mem mc ["foo"; "doesnt exist"]
    >>= fun is_member ->
    Assert.equal
      false
      is_member
      ~msg:".mem incorrectly returned true for an invalid key" ;
    Context.mem_tree mc ["foo"]
    >>= fun is_dir_member ->
    Assert.equal
      true
      is_dir_member
      ~msg:".mem incorrectly returned false for a valid key" ;
    Context.remove mc ["foo"]
    >>= fun mc ->
    Context.mem mc ["foo"; "bar"]
    >>= fun is_member ->
    Assert.equal
      false
      is_member
      ~msg:"foo/bar is not a memeber anymore, after removing foo" ;
    Context.mem mc ["foo"]
    >>= fun is_member ->
    Assert.equal
      false
      is_member
      ~msg:"foo is not a memeber anymore, after removing foo" ;
    return_unit

  let set_protocol () =
    let mc = Memory_context.empty in
    Context.set_protocol
      mc
      (Protocol_hash.of_string_exn "BLockGenesisGenesisGenesisGenesi")
    >>= fun mc ->
    Context.get_protocol mc
    >|= fun key ->
    Assert.equal
      "BLockGenesisGenesisGenesisGenesi"
      (Protocol_hash.to_string key) ;
    Ok ()

  let fork_test_chain () =
    let mc = Memory_context.empty in
    let protocol =
      Protocol_hash.of_string_exn "BLockGenesisGenesisGenesisGenesi"
    in
    let expiration = Time.Protocol.epoch in
    (* It doesn't matter what expiration is *)
    let value = Bytes.of_string "value" in
    Context.add mc ["foo"; "bar"] value
    >>= fun mc ->
    Context.fork_test_chain ~protocol ~expiration mc
    >>= fun forked_mc ->
    Context.find forked_mc ["foo"; "bar"]
    >|= (function
          | Some v ->
              Assert.equal value v
          | None ->
              Assert.fail_msg
                "Forked chain must have foo/bar in it's context since the \
                 original chain had")
    >>= fun () -> return_unit
end

let tests =
  [ Test.tztest
      "Memory_context.ml: basic operations (get, set, mem, dir_mem)"
      `Quick
      Test_memory_context.basic_ops;
    Test.tztest
      "Memory_context.ml: set_protocol operation"
      `Quick
      Test_memory_context.set_protocol;
    Test.tztest
      "Memory_context.ml: fork_test_chain operation"
      `Quick
      Test_memory_context.fork_test_chain ]
