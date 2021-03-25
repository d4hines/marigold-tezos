(** Testing
    -------
    Component:    Tez_repr 
    Invocation:   dune build @src/proto_alpha/lib_protocol/test/unit/unittests
    Dependencies: --
    Subject:      To test the modules (including the top-level)
                  in tez_repr.ml as individual units, particularly
                  failure cases. Superficial goal: increase coverage percentage.
*)
open Protocol

module Test_tez_repr = struct
  (** Testing predefined units: zero, one_mutez etc *)
  let predefined_values () =
    let zero_int64 = Tez_repr.to_int64 Tez_repr.zero in
    Assert.equal_int64 ~loc:__LOC__ zero_int64 0L
    >>=? fun () ->
    let one_mutez_int64 = Tez_repr.to_int64 Tez_repr.one_mutez in
    Assert.equal_int64 ~loc:__LOC__ one_mutez_int64 1L
    >>=? fun () ->
    let one_cent_int64 = Tez_repr.to_int64 Tez_repr.one_cent in
    Assert.equal_int64 ~loc:__LOC__ one_cent_int64 10000L
    >>=? fun () ->
    let fifty_cents_int64 = Tez_repr.to_int64 Tez_repr.fifty_cents in
    Assert.equal_int64 ~loc:__LOC__ fifty_cents_int64 500000L
    >>=? fun () ->
    let one_int64 = Tez_repr.to_int64 Tez_repr.one in
    Assert.equal_int64 ~loc:__LOC__ one_int64 1000000L

  let subtract () =
    (Lwt.return @@ Tez_repr.(one -? zero))
    >|= Environment.wrap_tzresult
    >>=? fun res ->
    Assert.equal_int64 ~loc:__LOC__ (Tez_repr.to_int64 res) 1000000L

  let substract_underflow () =
    (Lwt.return @@ Tez_repr.(zero -? one))
    >|= Environment.wrap_tzresult
    >>= function
    | Ok _ -> failwith "Expected to underflow" | Error _ -> return_unit

  let addition () =
    (Lwt.return @@ Tez_repr.(one +? zero))
    >|= Environment.wrap_tzresult
    >>=? fun res ->
    Assert.equal_int64 ~loc:__LOC__ (Tez_repr.to_int64 res) 1000000L

  let addition_overflow () =
    (Lwt.return @@ Tez_repr.(of_mutez_exn 0x7fffffffffffffffL +? one))
    >|= Environment.wrap_tzresult
    >>= function
    | Ok _ -> failwith "Expected to overflow" | Error _ -> return_unit

  let mul () =
    (Lwt.return @@ Tez_repr.(zero *? 1L))
    >|= Environment.wrap_tzresult
    >>=? fun res -> Assert.equal_int64 ~loc:__LOC__ (Tez_repr.to_int64 res) 0L

  let mul_overflow () =
    (Lwt.return @@ Tez_repr.(of_mutez_exn 0x7fffffffffffffffL *? 2L))
    >|= Environment.wrap_tzresult
    >>= function
    | Ok _ -> failwith "Expected to overflow" | Error _ -> return_unit

  let div () =
    (Lwt.return @@ Tez_repr.(one *? 1L))
    >|= Environment.wrap_tzresult
    >>=? fun res ->
    Assert.equal_int64 ~loc:__LOC__ (Tez_repr.to_int64 res) 1000000L

  let div_by_zero () =
    (Lwt.return @@ Tez_repr.(one /? 0L))
    >|= Environment.wrap_tzresult
    >>= function
    | Ok _ -> failwith "Expected to overflow" | Error _ -> return_unit

  let to_mutez () =
    let int64v = Tez_repr.(to_mutez one) in
    Assert.equal_int64 ~loc:__LOC__ int64v 1000000L

  let of_mutez_non_negative () =
    match Tez_repr.of_mutez 1000000L with
    | Some tz ->
        Assert.equal_int64
          ~loc:__LOC__
          (Tez_repr.to_int64 tz)
          Tez_repr.(to_int64 one)
    | None ->
        failwith "should have successfully converted 1000000L to tez"

  let of_mutez_negative () =
    match Tez_repr.of_mutez (-1000000L) with
    | Some _ ->
        failwith "should have failed to converted -1000000L to tez"
    | None ->
        return_unit

  let of_mutez_exn () =
    try
      let tz = Tez_repr.of_mutez_exn 1000000L in
      Assert.equal_int64
        ~loc:__LOC__
        (Tez_repr.to_int64 tz)
        Tez_repr.(to_int64 one)
    with e ->
      let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
      failwith "Unexpected exception: %s %s" msg stack

  let of_mutez_exn_negative () =
    try
      let _ = Tez_repr.of_mutez_exn (-1000000L) in
      failwith "should have failed to converted -1000000L to tez"
    with
    | Invalid_argument _ ->
        return_unit
    | e ->
        let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
        failwith "Unexpected exception: %s %s" msg stack

  (* NOTE: Avoid assertions against too many functions from Tez_repr. Convert them to
      int64 and compare instead of using [Tez_repr]'s compare *)

  (** Testing [encoding], int64 underneath, by applying it with Data_encoding *)
  let data_encoding () =
    let encoding = Tez_repr.encoding in
    let bytes =
      Data_encoding.Binary.to_bytes_exn Data_encoding.n (Z.of_int 1000000)
    in
    Data_encoding.Binary.of_bytes encoding bytes
    |> (function
         | Ok x ->
             Lwt.return (Ok x)
         | Error e ->
             failwith
               "Data_encoding.Binary.read shouldn't have failed with \
                Tez_repr.encoding: %a"
               Data_encoding.Binary.pp_read_error
               e)
    >>=? fun v ->
    Assert.equal_int64 ~loc:__LOC__ (Tez_repr.to_mutez v) 1000000L
end

let tests =
  [ Test.tztest
      "Check if predefined values hold expected values"
      `Quick
      Test_tez_repr.predefined_values;
    Test.tztest "Tez.substract: basic behaviour" `Quick Test_tez_repr.subtract;
    Test.tztest
      "Tez.substract: underflow case"
      `Quick
      Test_tez_repr.substract_underflow;
    Test.tztest
      "Tez.add: basic behaviour (one + zero)"
      `Quick
      Test_tez_repr.addition;
    Test.tztest "Tez.add: overflow" `Quick Test_tez_repr.addition_overflow;
    Test.tztest "Tez.mul: basic case" `Quick Test_tez_repr.mul;
    Test.tztest "Tez.mul: overflow case" `Quick Test_tez_repr.mul_overflow;
    Test.tztest "Tez.div: basic case" `Quick Test_tez_repr.div;
    Test.tztest "Tez.div: division by zero" `Quick Test_tez_repr.div_by_zero;
    Test.tztest "Tez.to_mutez: basic assertion" `Quick Test_tez_repr.to_mutez;
    Test.tztest
      "Tez.of_mutez: of non-negative ints"
      `Quick
      Test_tez_repr.of_mutez_non_negative;
    Test.tztest
      "Tez.of_mutez: of non-negative ints"
      `Quick
      Test_tez_repr.of_mutez_non_negative;
    Test.tztest
      "Tez.of_mutez: of negative ints"
      `Quick
      Test_tez_repr.of_mutez_negative;
    Test.tztest
      "Tez.of_mutez_exn: of non-negative ints"
      `Quick
      Test_tez_repr.of_mutez_non_negative;
    Test.tztest
      "Tez.of_mutez_exn: of negative ints"
      `Quick
      Test_tez_repr.of_mutez_negative;
    Test.tztest
      "Tez.data_encoding: must encode tezzies correctly"
      `Quick
      Test_tez_repr.data_encoding ]
