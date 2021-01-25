(** Testing
    -------
    Component:    Protocol (interpretation)
    Dependencies: src/proto_alpha/lib_protocol/script_interpreter.ml
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/main.exe -- test "^interpretation$"
    Subject:      Interpretation of Michelson scripts
*)

open Protocol
open Alpha_context
open Interpreter

(** Runs a script with an ill-typed parameter and verifies that a
    Bad_contract_parameter error is returned. *)
let test_bad_contract_parameter () =
  test_context ()
  >>=? fun ctx ->
  (* Run script with a parameter of wrong type *)
  run_script
    ctx
    "{parameter unit; storage unit; code { CAR; NIL operation; PAIR }}"
    ~storage:"Unit"
    ~parameter:"0"
    ()
  >>= function
  | Ok _ ->
      Alcotest.fail "expected an error"
  | Error (Environment.Ecoproto_error (Bad_contract_parameter source') :: _) ->
      Test_services.(check Testable.contract)
        "incorrect field in Bad_contract_parameter"
        default_source
        source' ;
      return_unit
  | Error errs ->
      Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_error errs

(* Check that too many recursive calls of the Michelson interpreter result in an error *)
let test_stack_overflow () =
  test_context ()
  >>=? fun ctxt ->
  let descr instr =
    Script_typed_ir.{loc = 0; bef = Empty_t; aft = Empty_t; instr}
  in
  let enorme_et_seq n =
    let rec aux n acc =
      if n = 0 then acc else aux (n - 1) (descr (Seq (acc, descr Nop)))
    in
    aux n (descr Nop)
  in
  run_step ctxt (enorme_et_seq 10_001) ()
  >>= function
  | Ok _ ->
      Alcotest.fail "expected an error"
  | Error trace ->
      let trace_string =
        Format.asprintf "%a" Environment.Error_monad.pp_trace trace
      in
      let expect =
        "Too many recursive calls were needed for interpretation of a \
         Michelson script"
      in
      if Astring.String.is_infix ~affix:expect trace_string then return_unit
      else Alcotest.failf "Unexpected error (%s) at %s" trace_string __LOC__

(** Test the encoding/decoding of script_interpreter.ml specific errors *)
let test_json_roundtrip name testable enc v =
  let v' =
    Data_encoding.Json.destruct enc (Data_encoding.Json.construct enc v)
  in
  Alcotest.check
    testable
    (Format.asprintf "round trip should not change value of %s" name)
    v
    v' ;
  return_unit

(** Encoding/decoding of script_interpreter.ml specific errors. *)
let test_json_roundtrip_err name e () =
  test_json_roundtrip
    name
    Testable.protocol_error
    Environment.Error_monad.error_encoding
    e

let error_encoding_tests =
  let contract_zero =
    Contract.implicit_contract Signature.Public_key_hash.zero
  in
  let script_expr_int =
    Micheline.strip_locations (Micheline.Int (0, Z.zero))
  in
  List.map
    (fun (name, e) ->
      Test.tztest
        (Format.asprintf "test error encoding: %s" name)
        `Quick
        (test_json_roundtrip_err name e))
    [ ("Reject", Reject (0, script_expr_int, None));
      ("Overflow", Overflow (0, None));
      ( "Runtime_contract_error",
        Runtime_contract_error (contract_zero, script_expr_int) );
      ("Bad_contract_parameter", Bad_contract_parameter contract_zero);
      ( "Cannot_serialize_log",
        Helpers_services.Scripts.Traced_interpreter.Cannot_serialize_log );
      ("Cannot_serialize_failure", Cannot_serialize_failure);
      ("Cannot_serialize_storage", Cannot_serialize_storage) ]

let tests =
  [ Test.tztest "test bad contract error" `Quick test_bad_contract_parameter;
    Test.tztest "test stack overflow error" `Slow test_stack_overflow ]
  @ error_encoding_tests
