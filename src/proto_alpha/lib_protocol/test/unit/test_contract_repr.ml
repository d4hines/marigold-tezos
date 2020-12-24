(** Testing
    -------
    Component:    Contract_repr
    Invocation:   dune build @src/proto_alpha/lib_protocol/test/unit/unittests
    Dependencies: contract_hash.ml
    Subject:      To test the modules (including the top-level)
                  in contract_repr.ml as individual units, particularly
                  failure cases. Superficial goal: increase coverage percentage.
*)
open Protocol

(*

  TODO: Remove dependence on contract_hash.ml and mock it

 *)

module Test_contract_repr = struct
  (** Assert if [is_implicit] correctly return the implicit contract *)
  open Contract_repr

  let test_operation_hash =
    Operation_hash.of_bytes_exn
      (Bytes.of_string "test-operation-hash-of-length-32")

  let test_origination_nonce = initial_origination_nonce test_operation_hash

  let test_contract_hash =
    (* WARNING: Uses Contract_repr itself, which is yet to be tested. This happened because Contract_hash wasn't mocked *)
    let data =
      Data_encoding.Binary.to_bytes_exn
        Contract_repr.origination_nonce_encoding
        test_origination_nonce
    in
    Contract_hash.hash_bytes [data]

  let test_implicit_contract = implicit_contract Signature.Public_key_hash.zero

  let test_originated_contract = originated_contract @@ test_origination_nonce

  let implicit () =
    match is_implicit test_implicit_contract with
    | Some _ ->
        return_unit
    | None ->
        failwith
          "must have returned the public key hash of implicit contract. \
           Instead, returned None"

  (** Check if [is_implicit] catches a non-implicit (originated) contract and returns None *)
  let not_implicit () =
    match is_implicit test_originated_contract with
    | None ->
        return_unit
    | Some _ ->
        failwith "must have returned the None. Instead, returned Some _"

  let originated () =
    match is_originated test_originated_contract with
    | Some _ ->
        return_unit
    | None ->
        failwith
          "must have returned the origination nonce correctly. Instead \
           returned None."

  let not_originated () =
    match is_originated test_implicit_contract with
    | None ->
        return_unit
    | Some _ ->
        failwith "must have returned the None. Instead, returned Some _"

  let to_b58check_implicit () =
    Assert.equal
      ~loc:__LOC__
      String.equal
      "%s should have been equal to %"
      Format.pp_print_string
      (to_b58check test_implicit_contract)
      Signature.Public_key_hash.(to_b58check zero)

  let to_b58check_originated () =
    Assert.equal
      ~loc:__LOC__
      String.equal
      "%s should have been equal to %"
      Format.pp_print_string
      (to_b58check test_originated_contract)
      Contract_hash.(to_b58check @@ test_contract_hash)

  let originated_contracts_basic () =
    let since = test_origination_nonce in
    let rec incr_n_times nonce = function
      | 0 ->
          nonce
      | n ->
          incr_n_times (Contract_repr.incr_origination_nonce nonce) (n - 1)
    in
    let until = incr_n_times since 5 in
    let contracts = originated_contracts ~since ~until in
    Assert.equal_int ~loc:__LOC__ (List.length contracts) 5
end

let tests =
  [ Test.tztest
      "Contract_repr.is_implicit: must correctly identify a valid implicit \
       contract"
      `Quick
      Test_contract_repr.implicit;
    Test.tztest
      "Contract_repr.is_implicit: must correctly return None for a originated \
       contract"
      `Quick
      Test_contract_repr.not_implicit;
    Test.tztest
      "Contract_repr.is_originated: must correctly return operation hash of \
       the originated contract"
      `Quick
      Test_contract_repr.originated;
    Test.tztest
      "Contract_repr.is_originated: must correctly return None for an \
       implicit contract contract"
      `Quick
      Test_contract_repr.not_originated;
    Test.tztest
      "Contract_repr.to_b58check: must correctly stringify, b58check encoded, \
       an implicit contract"
      `Quick
      Test_contract_repr.to_b58check_implicit;
    Test.tztest
      "Contract_repr.originated_contract: must correctly create an originated \
       contract"
      `Quick
      Test_contract_repr.originated_contracts_basic;
    Test.tztest
      "Contract_repr.to_b58check: must correctly stringify, b58check encoded, \
       an originated contract"
      `Quick
      Test_contract_repr.to_b58check_originated ]
