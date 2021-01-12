open Tezos_raw_protocol_alpha
open Tezos_protocol_environment_alpha
open Tezos_alpha_test_helpers
open Tezos_error_monad
open Alpha_context
open Script_interpreter
open Script_typed_ir
open Error_monad
open Util

exception Expression_from_string

(* In this testing module we unit test the interpreter.
The set-up invovled isn't trivial and bears some explanation. *)

(* For unit tests it is enough to create a dummy Logger. *)
module Logger : Script_interpreter.STEP_LOGGER = struct
  let log_interp _ctxt _descr _stack = ()

  let log_entry _ctxt _descr _stack = ()

  let log_exit _ctxt _descr _stack = ()

  let get_log () = Lwt.return (Ok None)
end

let logger = (module Logger : Script_interpreter.STEP_LOGGER)

module Test_script_interpreter = struct
  (* So far it's been enough to have a fixed set of step_constants.
  This will change when we test instructions that depend on the
  step_constants (e.g. AMOUNT). *)
  let default_source =
    Contract.implicit_contract Environment.Signature.Public_key_hash.zero

  let default_step_constants =
    {
      source = default_source;
      payer = default_source;
      self = default_source;
      amount = Tez.zero;
      chain_id = Environment.Chain_id.zero;
    }

  (* We set up a context with some arbitray number
  of implicit accounts (3 in this case). For tests
  that need more elaborate context set-up, see [io_tests]. *)
  let test_context () =
    Context.init 3
    >>=? fun (b, _cs) ->
    Incremental.begin_construction b
    >>=? fun v ->
    return
    @@ (Incremental.alpha_ctxt v (*  *) |> Fees.start_counting_storage_fees)

  let run_script :
      Alpha_context.t ->
      string ->
      string ->
      string ->
      string ->
      unit ->
      (unit, tztrace) result Lwt.t =
   fun context contract parameter storage expected () ->
    (* First, convert all our strings into Micheline values. *)
    let contract_expr = Expr.from_string contract in
    let storage_expr = Expr.from_string storage in
    let parameter_expr = Expr.from_string parameter in
    (* We next need to parse the Micheline into a Michelson script. *)
    (* First, pack it up as a [Script] proper *)
    let script =
      Script.{code = lazy_expr contract_expr; storage = lazy_expr storage_expr}
    in
    (* Then parse/typecheck the script, unpacking the results. *)
    let ( Ex_script {code; arg_type; storage; storage_type; root_name = _},
          context ) =
      force_lwt
      @@ Script_ir_translator.parse_script
           context
           ~legacy:false
           ~allow_forged_in_storage:true
           script
    in
    (* The IR trasnlator always returns a [Lam] for well-typed scripts. *)
    let (Lam (code, _)) = code in
    (* We next parse/typecheck the parameters as Michelson. *)
    let (arg, context) =
      Script_ir_translator.parse_data
        context
        ~legacy:false
        ~allow_forged:false
        arg_type
        (Micheline.root parameter_expr)
      |> force_lwt
    in
    (* A Michelson script always takes a pair of the parameters ([arg])
    and storage. *)
    let stack = ((arg, storage), ()) in
    let results =
      (* [step] will recursively step through the script, returning a pair
      of a list of internal operations and the new storage. *)
      Script_interpreter.step logger context default_step_constants code stack
      >>=? fun (((_, storage), ()), ctx) ->
      (* Using the type determined by type checker, we can unparse the Michelson
      value back into Micheline. *)
      Script_ir_translator.unparse_data ctx Readable storage_type storage
      >>=? fun (micheline, _) ->
      let results =
        Micheline.strip_locations micheline |> micheline_canonical_to_string
      in
      assert_strings_equal expected results ;
      return_unit
    in
    results >|= Environment.wrap_error
    >>= fun x ->
    match x with
    | Ok _ ->
        Error_monad.return ()
    | Error err ->
        (* Getting the data out of a FAILWITH error message is a pain *)
        (* First we convert it a string that has some JSON. *)
        let err_str = Format.asprintf "%a" Error_monad.pp_print_error err in
        print_endline err_str ;
        (* Next we chop off the prefix. *)
        let prefix_len = "Error:\n" |> String.length in
        let err_str =
          String.sub err_str prefix_len (String.length err_str - prefix_len)
        in
        let open Yojson.Basic.Util in
        (* Now we can access it `data.with.string` prop in the JSON. *)
        let failwith_json =
          Yojson.Basic.from_string err_str
          |> member "data" |> member "with" |> member "string"
        in
        let failwith_str =
          match failwith_json with `String str -> str | _ -> assert false
        in
        Util.assert_strings_equal failwith_str expected ;
        Error_monad.return ()
end

(* name * contract * strorage * parameter * expected value *)
let stack_operation_tests =
  [ ( "Identity contract",
      {|{ parameter unit ; storage unit ; code { CDR ; NIL operation ; PAIR } }|},
      "Unit",
      "Unit",
      "Unit" );
    ( "Push int",
      {|{ parameter unit ; storage int ; code { DROP; PUSH int 2; NIL operation ; PAIR } }|},
      "Unit",
      "0",
      "2" );
    ( "Lambda",
      {| {parameter unit; storage int; code {
      DROP;
      LAMBDA unit int { DROP; PUSH int 3 };
      PUSH unit Unit;
      EXEC;
      NIL operation ;
      PAIR }} |},
      "Unit",
      "0",
      "3" );
    ( "FAILWITH string",
      {|{ parameter unit ; storage int ; code { DROP; PUSH string "Some error"; FAILWITH } }|},
      "Unit",
      "0",
      "Some error" ) ]

let stack_operation_tests =
  List.map
    (fun (name, contract, storage, parameter, expected) ->
      Test.tztest
        name
        `Quick
        (Test_script_interpreter.run_script
           (Test_script_interpreter.test_context () |> force_global_lwt)
           contract
           storage
           parameter
           expected))
    stack_operation_tests

type context_transform =
  Alpha_context.t ->
  Alpha_context.t
  Tezos_protocol_environment_alpha.Environment.Error_monad.tzresult
  Lwt.t

(* name * * ctxt_transform * contract * strorage * parameter * expected value
The idea is that for IO tests you might need to modify the context to include
some values in storage, etc. So each test takes an optional function to transform
the initial context however you need.
*)
let io_tests =
  [ ( "GET_GLOBAL string happy path",
      Some
        (fun ctxt ->
          let expr = Expr.from_string {|"bar"|} in
          let (_, ty) = Expr.ty_from_string ctxt "string" in
          Alpha_context.Global_constants.set ctxt "some constant" ty expr
          >>=? fun (context, _) -> Lwt.return_ok context),
      {|{ parameter unit ; storage (option string) ; code {
            DROP; GET_GLOBAL string "some constant"; NIL operation ; PAIR } }|},
      "Unit",
      {|None|},
      {|(Some "bar")|} );
    ( "GET_GLOBAL int happy path",
      Some
        (fun ctxt ->
          let expr = Expr.from_string {|7|} in
          let (_, ty) = Expr.ty_from_string ctxt "int" in
          Alpha_context.Global_constants.set ctxt "some constant" ty expr
          >>=? fun (context, _) -> Lwt.return_ok context),
      {|{ parameter unit ; storage (option int) ; code {
            DROP; GET_GLOBAL int "some constant"; NIL operation ; PAIR } }|},
      "Unit",
      {|None|},
      {|(Some 7)|} );
    (* TODO: Should this fail with a specific error, or just return [None]? *)
    ( "GET_GLOBAL with mismatched types",
      Some
        (fun ctxt ->
          let expr = Expr.from_string {|7|} in
          let (_, ty) = Expr.ty_from_string ctxt "int" in
          Alpha_context.Global_constants.set ctxt "some constant" ty expr
          >>=? fun (context, _) -> Lwt.return_ok context),
      {|{ parameter unit ; storage (option string) ; code {
            DROP; GET_GLOBAL string "some constant"; NIL operation ; PAIR } }|},
      "Unit",
      {|None|},
      {|None|} );
    ( "GET_GLOBAL lambda happy path",
      Some
        (fun ctxt ->
          let expr = Expr.from_string {|{ DROP; PUSH unit Unit }|} in
          let (_, ty) = Expr.ty_from_string ctxt "lambda unit unit" in
          Alpha_context.Global_constants.set ctxt "some constant" ty expr
          >>=? fun (context, _) -> Lwt.return_ok context),
      {|{ parameter unit ; storage (option (lambda unit unit)) ; code {
            DROP; GET_GLOBAL (lambda unit unit) "some constant" ; NIL operation ; PAIR } }|},
      "Unit",
      {|None|},
      {|(Some { DROP ; PUSH unit Unit })|} );
    ( "GET_GLOBAL for nonexistent key",
      None,
      {|{ parameter unit ; storage (option string) ; code {
            DROP; GET_GLOBAL string "some constant"; NIL operation ; PAIR } }|},
      "Unit",
      {|None|},
      {|None|} ) ]

let io_tests =
  List.map
    (fun ( name,
           (fn : context_transform option),
           contract,
           storage,
           parameter,
           expected ) ->
      let context =
        match fn with
        | Some fn ->
            Test_script_interpreter.test_context ()
            |> force_global_lwt |> fn |> force_lwt
        | None ->
            Test_script_interpreter.test_context () |> force_global_lwt
      in
      Test.tztest
        name
        `Quick
        (Test_script_interpreter.run_script
           context
           contract
           storage
           parameter
           expected))
    io_tests

let tests = List.concat [stack_operation_tests; io_tests]
