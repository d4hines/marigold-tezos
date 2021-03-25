open Protocol

(** Testing
    -------
    Component:    Raw_level_repr 
    Invocation:   dune build @src/proto_alpha/lib_protocol/test/unit/unittests
    Dependencies: --
    Subject:      To test the modules (including the top-level)
                  in raw_level_repr.ml as individual units, particularly
                  failure cases. Superficial goal: increase coverage percentage.
*)

module Test_raw_level_repr = struct
  (* NOTE: Avoid assertions against too many functions from Raw_level_repr. For instance,
      Raw_level_repr contains a [compare] function, but while [Assert]'ing, convert them to
      int32 (or any convenient OCaml value) and compare instead of using [Raw_level_repr]'s compare *)

  (** Testing [encoding], int32 underneath, by applying it with Data_encoding *)
  let encoding () =
    let encoding = Raw_level_repr.encoding in
    let bytes = Bytes.make 4 '0' in
    Bytes.set_int32_ne bytes 0 0l ;
    Data_encoding.Binary.of_bytes encoding bytes
    |> (function
         | Ok x ->
             Lwt.return (Ok x)
         | Error e ->
             failwith
               "Data_encoding.Binary.read shouldn't have failed with \
                Raw_level_repr.encoding: %a"
               Data_encoding.Binary.pp_read_error
               e)
    >>=? fun v ->
    Assert.equal_int ~loc:__LOC__ (Int32.to_int (Raw_level_repr.to_int32 v)) 0

  (* TODO rpc_arg. RPC_arg needs to be unit tested separately. Preferably, with a functor *)
  (* let rpc_arg () = () *)

  (** int32 interop tests *)
  let int32_interop () =
    let int32v = 100l in
    Lwt.return (Raw_level_repr.of_int32 int32v)
    >|= Environment.wrap_tzresult
    >>=? fun raw_level ->
    Assert.equal_int32 ~loc:__LOC__ (Raw_level_repr.to_int32 raw_level) int32v
    >>=? fun () ->
    let int32v = -1l in
    Lwt.return (Raw_level_repr.of_int32 int32v)
    >|= Environment.wrap_tzresult
    >>= function
    | Ok _ ->
        failwith "Negative int32s should not be coerced into raw_level"
    | Error _ ->
        return_unit

  (** Asserting [root]'s runtime value. Expected to be [0l] *)
  let root () =
    let root = Raw_level_repr.root in
    Assert.equal_int32 ~loc:__LOC__ (root |> Raw_level_repr.to_int32) 0l

  (** Asserting [succ] which is expected to return successor levels *)
  let succ () =
    let next_raw_level = Raw_level_repr.succ Raw_level_repr.root in
    Assert.equal_int32
      ~loc:__LOC__
      (next_raw_level |> Raw_level_repr.to_int32)
      1l
    >>=? fun () ->
    let arbitrary_next_raw_level =
      Raw_level_repr.succ (Raw_level_repr.of_int32_exn 99l)
    in
    Assert.equal_int32
      ~loc:__LOC__
      (arbitrary_next_raw_level |> Raw_level_repr.to_int32)
      100l

  (** Asserting [pred] which is expected to return predecessor levels *)
  let pred () =
    ( match Raw_level_repr.pred (Raw_level_repr.of_int32_exn 1l) with
    | Some previous_raw_level ->
        Assert.equal_int32
          ~loc:__LOC__
          (previous_raw_level |> Raw_level_repr.to_int32)
          0l
    | None ->
        failwith
          "Raw_level_repr.pred should have successfully returned 0l as the \
           predecessor of 1l" )
    >>=? fun () ->
    Raw_level_repr.pred Raw_level_repr.root
    |> function
    | Some _ ->
        failwith
          "Raw_level_repr.pred should have returned None when asked for \
           predecessor of [root]"
    | None ->
        return_unit

  let skip_succ () =
    let int32_limit = 0x7FFFFFFFl in
    let overflown_next_raw_level =
      Raw_level_repr.succ (Raw_level_repr.of_int32_exn int32_limit)
    in
    if Int32.compare (Raw_level_repr.to_int32 overflown_next_raw_level) 0l >= 0
    then return_unit
    else
      failwith
        "succ of 0x7FFFFFFFl %a was expected to be non-negative"
        Assert.Int32.pp
        (overflown_next_raw_level |> Raw_level_repr.to_int32)
end

let tests =
  [ Test.tztest
      "Raw_level_repr.encoding: checks if encoding is int32 as expected"
      `Quick
      Test_raw_level_repr.encoding;
    Test.tztest
      "Raw_level_repr.root: check if value is 0l"
      `Quick
      Test_raw_level_repr.root;
    Test.tztest
      "Raw_level_repr.succ: basic assertions"
      `Quick
      Test_raw_level_repr.succ;
    Test.tztest
      "Raw_level_repr.pred: basic assertions"
      `Quick
      Test_raw_level_repr.pred;
    Test.tztest
      "Raw_level_repr: int32 interop"
      `Quick
      Test_raw_level_repr.int32_interop ]

let skipped_tests =
  [ Test.tztest
      "Raw_level_repr.succ: overflow"
      `Quick
      Test_raw_level_repr.skip_succ ]
