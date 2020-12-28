open Protocol
open Alpha_context
open Script_interpreter

let ( >>=?? ) x y =
  x
  >>= function
  | Ok s ->
      y s
  | Error errs ->
      Lwt.return
      @@ Error (List.map (fun x -> Environment.Ecoproto_error x) errs)

let test_context () =
  Context.init 3
  >>=? fun (b, _cs) ->
  Incremental.begin_construction b
  >>=? fun v -> return (Incremental.alpha_ctxt v)

let default_source = Contract.implicit_contract Signature.Public_key_hash.zero

let default_step_constants =
  {
    source = default_source;
    payer = default_source;
    self = default_source;
    amount = Tez.zero;
    chain_id = Chain_id.zero;
  }

(** Helper function that parses and types a script, its initial storage and
   parameters from strings. It then executes the typed script with the storage
   and parameter and returns the result. *)
let run_script ctx ?(step_constants = default_step_constants) contract
    ?(entrypoint = "default") ~storage ~parameter () =
  let contract_expr = Expr.from_string contract in
  let storage_expr = Expr.from_string storage in
  let parameter_expr = Expr.from_string parameter in
  let script =
    Script.{code = lazy_expr contract_expr; storage = lazy_expr storage_expr}
  in
  Script_interpreter.execute
    ctx
    Readable
    step_constants
    ~script
    ~entrypoint
    ~parameter:parameter_expr
    ~internal:false
  >>=?? fun res -> return res

let run_step ctxt code accu stack =
  Script_interpreter.step None ctxt default_step_constants code accu stack

(** Runs a script with an ill-typed parameter and verifies that a
   Bad_contract_parameter error is returned *)
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

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ; s

(* Confront the Michelson interpreter to deep recursions. *)
let test_stack_overflow () =
  let open Script_typed_cps_ir in
  test_context ()
  >>=? fun ctxt ->
  let stack = Script_typed_cps_ir.Item_t (Unit_t None, Empty_t, None) in
  let descr kinstr =
    Script_typed_cps_ir.{kloc = 0; kbef = stack; kaft = stack; kinstr}
  in
  let kinfo = {iloc = -1; kstack_ty = stack} in
  let enorme_et_seq n =
    let rec aux n acc =
      if n = 0 then acc else aux (n - 1) (KNext (kinfo, KNop, acc))
    in
    aux n (KHalt kinfo)
  in
  run_step ctxt (descr (enorme_et_seq 100_000)) () ()
  >>= function
  | Ok _ ->
      return ()
  | Error _ ->
      Alcotest.failf "Unexpected error (%s)" __LOC__

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
