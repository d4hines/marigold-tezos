open Tezos_raw_protocol_alpha
open Tezos_protocol_environment_alpha
open Tezos_alpha_test_helpers
open Tezos_error_monad
open Stdlib
module Micheline = Environment.Micheline

(* copied from lib_protocol/test/interpretation.ml *)
open Alpha_context
open Interp.Script_interpreter
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

module No_trace : STEP_LOGGER = struct
  let log_interp _ctxt _descr _stack = ()

  let log_entry _ctxt _descr _stack = ()

  let log_exit _ctxt _descr _stack = ()

  let get_log () = return_none
end

open! Interp

let make_run_script ctx ~parameter ~contract ~storage =
  let parameter_expr = Expr.from_string parameter in
  let contract_expr = Expr.from_string contract in
  let storage_expr = Expr.from_string storage in
  let script =
    Script.{code = lazy_expr contract_expr; storage = lazy_expr storage_expr}
  in
  Interp.run_script
    ctx
    ~step_constants:default_step_constants
    ~parameter_expr
    script
  >>=?? fun res -> return res

let make_run_compiled_script ctx ~parameter ~contract ~storage =
  let parameter_expr = Expr.from_string parameter in
  let contract_expr = Expr.from_string contract in
  (* let x = Show.show_canonical Show.pp_prim contract_expr in *)
  (* print_endline x ; *)
  let storage_expr = Expr.from_string storage in
  let script =
    Script.{code = lazy_expr contract_expr; storage = lazy_expr storage_expr}
  in
  Interp.run_compiled_script
    ctx
    ~step_constants:default_step_constants
    ~parameter_expr
    script
  >>=?? fun res -> return res

let contract =
  "{ parameter int ;\n\
  \  storage int ;\n\
  \  code { PUSH int 1 ;\n\
  \         SWAP ;\n\
  \         CAR ;\n\
  \         PAIR ;\n\
  \         LEFT int ;\n\
  \         LOOP_LEFT\n\
  \           { DUP ;\n\
  \             CAR ;\n\
  \             SWAP ;\n\
  \             CDR ;\n\
  \             PUSH int 0 ;\n\
  \             DIG 2 ;\n\
  \             DUP ;\n\
  \             DUG 3 ;\n\
  \             COMPARE ;\n\
  \             GT ;\n\
  \             IF { SWAP ;\n\
  \                  DUP ;\n\
  \                  DUG 2 ;\n\
  \                  SWAP ;\n\
  \                  MUL ;\n\
  \                  PUSH int 1 ;\n\
  \                  DIG 2 ;\n\
  \                  SUB ;\n\
  \                  PAIR ;\n\
  \                  LEFT int }\n\
  \                { SWAP ; DROP ; RIGHT (pair int int) } } ;\n\
  \         NIL operation ;\n\
  \         PAIR } }\n"

let seq (Script_typed_ir.{bef; _} as left) (Script_typed_ir.{aft; _} as right)
    =
  Script_typed_ir.{loc = 0; bef; aft; instr = Seq (left, right)}

let _x = Script_typed_ir.(Dug (2, Prefix (Prefix Rest)))

let v (a0, (a1, (a2, rest))) = (a2, (a0, (a1, rest)))

let x (a0, (a1, (a2, rest))) = (a0, (a1, (a2, rest)))

open Core
open Core_bench

let () =
  let promise_run_script =
    test_context ()
    >>=? fun ctx ->
    make_run_script ctx ~contract ~storage:"0" ~parameter:"100"
    >>=? fun run_script ->
    let run_script () = run_script () >>=?? fun res -> return res in
    make_run_compiled_script ctx ~contract ~storage:"0" ~parameter:"100"
    >>=? fun run_compiled_script ->
    let run_compiled_script () =
      run_compiled_script () >>=?? fun res -> return res
    in
    return (run_script, run_compiled_script)
  in
  let (run_script, run_compiled_script) =
    match promise_run_script |> Lwt_main.run with
    | Ok v ->
        v
    | Error err ->
        Format.printf "%a" Error_monad.pp_print_error err ;
        assert false
  in
  let () =
    match run_compiled_script () |> Lwt_main.run with
    | Ok (v, _) ->
        let x = Show.show_node Int.pp Show.pp_prim v in
        print_endline x
    | Error _ ->
        assert false
  in
  let () =
    Command.run
      (Bench.make_command
         [ Bench.Test.create ~name:"run_script" (fun () ->
               run_script () |> Lwt_main.run);
           Bench.Test.create ~name:"run_compiled_script" (fun () ->
               run_compiled_script () |> Lwt_main.run) ])
  in
  match Lwt_main.run (run_compiled_script ()) with
  | Ok _ ->
      Printf.printf "yey, it didn't crash"
  | Error err ->
      Format.printf "%a" Error_monad.pp_print_error err
