(** Testing
    -------
    Component:    Operation_repr
    Invocation:   dune build @src/proto_alpha/lib_protocol/test/unit/unittests
    Dependencies: --
    Subject:      To test the modules (including the top-level)
                  in operation_repr.ml as individual units, particularly
                  failure cases. Superficial goal: increase coverage percentage.
*)
open Protocol

module Test_operation_repr = struct
  open Operation_repr

  let of_list_single_case () =
    let contents_list =
      of_list
        [ Contents
            (Manager_operation
               {
                 fee = Obj.magic 0;
                 operation = Obj.magic 0;
                 gas_limit = Obj.magic 0;
                 storage_limit = Obj.magic 0;
                 counter = Obj.magic 0;
                 source = Obj.magic 0;
               }) ]
    in
    match contents_list with
    | Contents_list (Single _) ->
        return_unit
    | _ ->
        failwith "Unexpected value"

  let of_list_multiple_case () =
    let contents_list =
      of_list
        [ Contents
            (Manager_operation
               {
                 fee = Obj.magic 0;
                 operation = Obj.magic 0;
                 gas_limit = Obj.magic 0;
                 storage_limit = Obj.magic 0;
                 counter = Obj.magic 0;
                 source = Obj.magic 0;
               });
          Contents
            (Manager_operation
               {
                 fee = Obj.magic 0;
                 operation = Obj.magic 0;
                 gas_limit = Obj.magic 0;
                 storage_limit = Obj.magic 0;
                 counter = Obj.magic 0;
                 source = Obj.magic 0;
               }) ]
    in
    match contents_list with
    | Contents_list (Cons (_, Single _)) ->
        return_unit
    | Contents_list (Single _) | _ ->
        failwith "Unexpected value"

  let of_list_empty_case () =
    try
      let _ = of_list [] in
      failwith "of_list of an empty list was expected to fail"
    with Assert_failure _ -> return_unit
end

let tests =
  [ Test.tztest
      "of_list: single element input list"
      `Quick
      Test_operation_repr.of_list_single_case;
    Test.tztest
      "of_list: multiple element input list"
      `Quick
      Test_operation_repr.of_list_multiple_case;
    Test.tztest
      "of_list: empty input list"
      `Quick
      Test_operation_repr.of_list_empty_case ]
