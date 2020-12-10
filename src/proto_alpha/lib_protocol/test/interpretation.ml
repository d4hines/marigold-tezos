(* Take Smart-Contracts , Sample Inputs , Benchmark interpreters *)
[@@@warning "-33d"]

open Tezos_raw_protocol_alpha
open Alpha_context
open Protocol
open Alpha_context
open Script_interpreter

let force x =
  match x with
  | Ok x ->
      x
  | Error es ->
      Format.printf "Errors :\n" ;
      List.iter (Format.printf "- %a\n" Protocol.Environment.Error_monad.pp) es ;
      raise (Failure "force")

let force_global x =
  match x with
  | Ok x ->
      x
  | Error es ->
      Format.printf "Errors :\n" ;
      List.iter (Format.printf "- %a\n" pp) es ;
      raise (Failure "force")

let force_lwt x = force (Lwt_main.run x)

let force_global_lwt x = force_global (Lwt_main.run x)

(* let smart_contract_names = assert false *)

(* Interpreter_list.names *)

(* let smart_contract_paths : string list =
  let make_path name = "./contracts/interpreter/" ^ name ^ ".tz" in
  List.map make_path smart_contract_names *)

let get_file path =
  let ch = open_in path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ; s

let get_code ~context path =
  let file = get_file path in
  let m_toplevel =
    (* Michelson Toplevel *)
    let open Micheline_parser in
    let (ms_tokens, _errors) = tokenize file in
    let (ms_toplevels, _errors) = parse_toplevel ms_tokens in
    let ms_toplevel = Micheline.Seq (location_zero, ms_toplevels) in
    let (mse_toplevel, _errors) =
      Tezos_client_alpha.Michelson_v1_macros.expand_rec ms_toplevel
    in
    let msc_toplevel = Micheline.strip_locations mse_toplevel in
    let mp_toplevel_r =
      Protocol.Michelson_v1_primitives.prims_of_strings msc_toplevel
    in
    force mp_toplevel_r
  in
  let m_toplevel_lazy = Protocol.Alpha_context.Script.lazy_expr m_toplevel in
  let (ex_code, _ctxt) =
    force_lwt
      (Protocol.Script_ir_translator.parse_code
         context
         ~legacy:false
         ~code:m_toplevel_lazy)
  in
  ex_code

(* module Sampler_parameter : Sampler.Base_samplers_S = struct
  let key_pool_size = 16

  (* samples in [min, max[ *)
  let sample_in_interval (min, max) = min + Random.int (max - min)

  (* ------------------------------------------------------------------------- *)
  (* generic uniform sampling *)
  let bool = Random.bool

  let signature () = Signature.zero (* TODO *)

  let uniform_byte () : char = Char.chr (Random.int 256)

  let uniform_bits (nbytes : int) : string =
    String.init nbytes (fun _ -> uniform_byte ())

  let uniform_nat (nbytes : int) : Z.t = Z.of_bits (uniform_bits nbytes)

  let z ~size =
    let nbytes = sample_in_interval size in
    uniform_nat nbytes

  let int ~size =
    let z = z ~size in
    let s = Random.bool () in
    if s then Protocol.Alpha_context.Script_int.of_zint z
    else Protocol.Alpha_context.Script_int.of_zint Z.(neg z)

  let nat ~size =
    let z = Protocol.Alpha_context.Script_int.of_zint (z ~size) in
    match Protocol.Alpha_context.Script_int.is_nat z with
    | Some res ->
        res
    | None ->
        assert false

  let readable_ascii_string (nbytes : int) : string =
    String.escaped (String.init nbytes (fun _ -> uniform_byte ()))

  let string ~size =
    let len = sample_in_interval size in
    readable_ascii_string len

  let bytes ~size =
    let len = sample_in_interval size in
    Bytes.of_string (uniform_bits len)

  let tez () =
    let i = Random.int64 (Int64.of_int max_int) in
    match Protocol.Alpha_context.Tez.of_mutez i with
    | Some res ->
        res
    | None ->
        assert false

  let timestamp ~size =
    Protocol.Alpha_context.Script_timestamp.of_zint (z ~size)
end *)

let sample_size = 10

(* let bench_path ~context path =
  let (Ex_code code) = get_code ~context path in
  let Protocol.Script_ir_translator.{code; arg_type; storage_type; _} = code in
  let (Lam (descr, _)) = code in
  let (dummy_contract, _) = assert false
  in
  let amount = assert false
  in
  let chain_id = assert false
  in
  let bench =
    Protocol.Script_interpreter.(
      let logger = (module No_trace : STEP_LOGGER) in
      let step_constants =
        {
          self = dummy_contract;
          source = dummy_contract;
          payer = dummy_contract;
          chain_id;
          amount;
      let step_state = Step_state.{context; logger; step_constants} in
      fun () ->
        let values' =
          Protocol.Environment.Error_monad.map_s
            (fun value -> step step_state descr (value, ()))
            values
        in
        ignore @@ Lwt_main.run values')
  in
  bench *)

let run_bench bench =
  let batch_size = 1000 in
  let t1 = Sys.time () in
  for _ = 1 to batch_size do
    bench ()
  done ;
  let t2 = Sys.time () in
  (t2 -. t1) /. float_of_int batch_size

let time_pp ppf time =
  if time < 0.001 then Format.fprintf ppf "%Fµs" (time *. 1000000.)
  else if time < 1. then Format.fprintf ppf "%Fms" (time *. 1000.)
  else Format.fprintf ppf "%Fs" time

(* let () =
  Printexc.record_backtrace true ;
  let (block, _) = force_global_lwt @@ Context.init 1 in
  let i = force_global_lwt @@ Incremental.begin_construction block in
  let context = Incremental.get_context i in
  let named_runs =
    let benches = List.map (bench_path ~context) smart_contract_paths in
    let runs = List.map run_bench benches in
    List.combine smart_contract_names runs
  in
  Format.printf "The following numbers are times it took to run those contracts with %d randomly generated inputs\n" sample_size ;
  List.iter (fun named_run -> Format.printf "%s : %a\n" (fst named_run) time_pp (snd named_run)) named_runs *)
(* /////////////////////////////////////////////////////////////////////////////////////////////////////////// *)
(* /////////////////////////////////////////////////////////////////////////////////////////////////////////// *)
(* /////////////////////////////////////////////////////////////////////////////////////////////////////////// *)
(* /////////////////////////////////////////////////////////////////////////////////////////////////////////// *)
(* /////////////////////////////////////////////////////////////////////////////////////////////////////////// *)

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

let run_step ctxt code param =
  Script_interpreter.step None ctxt default_step_constants code param

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

(* ///////////////////////////////////////////////////////////////////////////////////////////// *)
(* ///////////////////////////////////////////////////////////////////////////////////////////// *)
(* ///////////////////////////////////////////////////////////////////////////////////////////// *)
(* ///////////////////////////////////////////////////////////////////////////////////////////// *)
(* ///////////////////////////////////////////////////////////////////////////////////////////// *)
let parse_ty ctxt node =
  Script_ir_translator.parse_ty
    ctxt
    ~legacy:true
    ~allow_lazy_storage:true
    ~allow_operation:true
    ~allow_contract:true
    ~allow_ticket:true
    node

let path =
  "/workspaces/repos/tezos/src/proto_alpha/lib_protocol/test/factorial.tz"

let micheline_canonical_to_string c =
  Fmt.str
    "%a"
    Micheline_printer.print_expr
    (Micheline_printer.printable Michelson_v1_primitives.string_of_prim c)

module No_trace : STEP_LOGGER = struct
  let log_interp _ctxt _descr _instr _stack= ()

  let log_entry = fun  _ctxt _descr instr _stack -> 
    print_endline @@ Script_typed_cps_ir.my_instr_to_str instr;
    print_endline @@ my_stack_to_string _stack

  let log_exit _ctxt _descr _instr _stack = print_endline @@ my_stack_to_string _stack

  let get_log () = return_none
end
  
    
let run :
    Alpha_context.t ->
    parameter:string ->
    storage:string ->
    contract:string ->
    string =
 fun context ~parameter ~storage ~contract ->
  let parameter_expr = Expr.from_string parameter in
  let contract_expr = Expr.from_string contract in
  let storage_expr = Expr.from_string storage in
  let script =
    Script.{code = lazy_expr contract_expr; storage = lazy_expr storage_expr}
  in
  let c =
    Script_ir_translator.parse_script
      context
      script
      ~legacy:false
      ~allow_forged_in_storage:false
    |> force_lwt
  in
  let (Ex_script {code; arg_type; storage; storage_type; root_name = _}) =
    fst c
  in
  let ctx = snd c in
  let (arg, ctx) =
    Script_ir_translator.parse_data
      ctx
      ~legacy:false
      ~allow_forged:false
      arg_type
      (Micheline.root parameter_expr)
    |> force_lwt
  in
  let (Lam (descr, _)) = code in
  let (((_, storage), ()), ctx) =
    force_lwt
    @@ Script_interpreter.step
         (Some (module No_trace))
         ctx
         default_step_constants
         descr
         ((arg, storage), ())
  in
  let (output, _) =
    force_lwt
    @@ Script_ir_translator.unparse_data
         ctx
         Script_ir_translator.Readable
         storage_type
         storage
  in
  micheline_canonical_to_string @@ Micheline.strip_locations output

(* let ir_input = force_lwt @@ assert false in
  (* parse_data ~ir_input_type ~input in *)
  (* let ir_stack_input_type = 
      Item_t (ir_input_type , Empty_t) in *)
  (* let tc_context : tc_context = Lambda in *)
  (* let (ir_code, ir_stack_output_type) = force_lwt @@ assert false in *)
  (* parse_instr ~context ~ir_input_type in *)
  (* let (a, b) = assert false in *)
  let (Ex_code code) = get_code ~context path in
  (* get_code ~context path in *)
  let Protocol.Script_ir_translator.{code; arg_type; storage_type; _} = code in
  let (Lam (descr, _)) = code in
  let input_type = assert false in
  let ir_input =
    (* assert false in *)
    Script_ir_translator.parse_data context input_type input
  in
  let ir_stack_input = (ir_input, ()) in
  let ir_stack_output =
    force_lwt
    @@ Script_interpreter.step
         None
         context
         default_step_constants
         descr
         ir_stack_input
  in
  let x = assert false in
  let (ir_output_type, (ir_output, _)) =
    match (descr.aft, ir_stack_output) with
    | (Item_t (hd_ty, _, _), (hd, _)) ->
        (hd_ty, hd)
  in
  let (output, _) =
    force_lwt
    @@ Script_ir_translator.unparse_data
         x
         Script_ir_translator.Readable
         ir_output_type
         ir_output
  in
  micheline_canonical_to_string @@ Micheline.strip_locations output *)

(* let contract = *)
(* read_file path *)
(* "{ parameter unit ; storage unit ; code { CDR ; NIL operation ; PAIR } }" *)


let contract =
{|
{
parameter int ;
storage int ;
code { DROP;                          # Ignore the initial store, stack = [n]
       PUSH int 10;
       PUSH @acc int 1;              # We will accumulate the result on top, stack = [1; n]
       SWAP;                         # Put n on top, stack = [n; accu]
       PUSH bool True;               # It is a do-while loop.
       LOOP { DUP;                   # stack = [n; n; accu]
              PUSH int 1;            # stack = [1; n; n; accu]
              SWAP;                  # stack = [n; 1; n; accu]
              SUB;                   # stack = [n - 1; n; accu]
              SWAP;                  # stack = [n; n - 1; accu]
              DIP 1 { SWAP };        # stack = [n; accu; n - 1]
              MUL;                   # stack = [n * accu; n - 1]
              SWAP;                  # stack = [n - 1; n * accu]
              DUP;                   # stack = [n - 1; n - 1; n * accu]
              PUSH int 0;            # stack = [0; n - 1; n - 1; n * accu]
              COMPARE;               # stack = [cmp 0 (n - 1); n - 1; n * accu]
              NEQ };                # stack = [n - 1 <> 0; n - 1; n * accu]
       SWAP;                       # stack = [fact n; 0]
       DIP 1 { DROP };             # stack = [fact n]
       NIL operation ;             # stack = [ []; fact n ]
       PAIR }                      # stack = [ ([], fact n) ]
}
|}

let main () =
  Printexc.record_backtrace true ;
  let context =
    match test_context () |> Lwt_main.run with
    | Ok v ->
        v
    | Error _ ->
        assert false
  in
  run context ~parameter:"3" ~storage:"0" ~contract |> print_endline
