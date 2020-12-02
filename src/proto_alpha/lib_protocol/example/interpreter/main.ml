open Tezos_raw_protocol_alpha
open Tezos_protocol_environment_alpha
open Tezos_alpha_test_helpers
open Tezos_error_monad

module Dummy = struct
  include Environment
  include Alpha_context
  module Raw_hashes = Raw_hashes
  module Raw_context = Alpha_context

  module Michelson_v1_primitives = struct
    include Michelson_v1_primitives

    let i_push = I_PUSH

    let i_pair = I_PAIR

    let k_parameter = K_parameter

    let k_storage = K_storage

    let k_code = K_code
  end

  module Alpha_context = Alpha_context
  module Script_ir_translator = Script_ir_translator
  module Script_typed_ir = Script_typed_ir
  module Script_interpreter_cost = Script_interpreter_cost
  module Operation = Alpha_context
end

module Error_monad_with_counter = struct
  include Environment.Error_monad

  let calls_counter = ref 0

  let ok_counter = ref 0

  let error_counter = ref 0

  let ( >>=? ) v f =
    calls_counter := !calls_counter + 1 ;
    v
    >>= function
    | Error _ as err ->
        error_counter := !error_counter + 1 ;
        Lwt.return err
    | Ok v ->
        ok_counter := !ok_counter + 1 ;
        f v

  let register_error_kind _ ~id:_ ~title:_ ~description:_
      ?pp:(_ = fun _ _ -> ()) _ _ _ =
    ()
end

module Script_interpreter = Script_interpreter_functor.Make (struct
  include Dummy
  module Error_monad = Error_monad_with_counter
end)

(* copied from lib_protocol/test/interpretation.ml *)
open Alpha_context
open Script_interpreter
open Error_monad

exception Expression_from_string

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

let contract =
  "{ parameter unit ; storage unit ; code { CDR ; NIL operation ; PAIR } }"

let () =
  let promise =
    test_context ()
    >>=? fun ctx ->
    run_script ctx contract ~storage:"Unit" ~parameter:"Unit" ()
  in
  match Lwt_main.run promise with
  | Ok _ ->
      Printf.printf
        "yey, Error_monad.(>>=?) was called %d times, Ok %d, Error %d"
        !Error_monad_with_counter.calls_counter
        !Error_monad_with_counter.ok_counter
        !Error_monad_with_counter.error_counter
  | Error err ->
      Format.printf "%a" Error_monad.pp_print_error err
