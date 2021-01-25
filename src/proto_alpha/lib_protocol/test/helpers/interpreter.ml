open Protocol
open Alpha_context
include Script_interpreter

exception Expression_from_string

let wrap_error_lwt x = x >>= fun x -> Lwt.return @@ Environment.wrap_error x

let expression_from_string str : Script.expr tzresult Lwt.t =
  let (ast, errs) = Michelson_v1_parser.parse_expression ~check:false str in
  ( match errs with
  | [] ->
      ()
  | lst ->
      Format.printf "expr_from_string: %a\n" Error_monad.pp_print_error lst ;
      raise Expression_from_string ) ;
  return ast.expanded

let expression_to_string (expr : Script.expr) =
  Format.asprintf "%a" Michelson_v1_printer.print_expr expr

let ( >>=?? ) x y =
  x
  >>= function
  | Ok s -> y s | Error _ as err -> Lwt.return @@ Environment.wrap_error err

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
  expression_from_string contract
  >>=? fun contract_expr ->
  expression_from_string storage
  >>=? fun storage_expr ->
  expression_from_string parameter
  >>=? fun parameter_expr ->
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

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ; s

module Logger : STEP_LOGGER = struct
  let log_interp _ctxt _descr _stack = ()

  let log_entry _ctxt _descr _stack = ()

  let log_exit _ctxt _descr _stack = ()

  let get_log () = Lwt.return (Ok None)
end

let run_step ctxt code param =
  Script_interpreter.step
    (module Logger)
    ctxt
    default_step_constants
    code
    param

let init_single () =
  Context.init 1
  >|=? fun (b, srcs) ->
  let src0 = match srcs with [src0] -> src0 | _ -> assert false in
  (b, src0)

let init_pair () =
  Context.init 2
  >|=? fun (b, srcs) ->
  let (src0, src1) =
    match srcs with [src0; src1] -> (src0, src1) | _ -> assert false
  in
  (b, src0, src1)

let apply_operation op b =
  Incremental.begin_construction b
  >>=? fun incr ->
  Incremental.add_operation incr op
  >>=? fun incr -> Incremental.finalize_block incr

(* returns a block in which the contract is originated. *)
let originate_contract ~file ~storage ~src b =
  let code = read_file file in
  expression_from_string code
  >>=? fun code ->
  expression_from_string storage
  >>=? fun storage ->
  let script = Script.{code = lazy_expr code; storage = lazy_expr storage} in
  Op.origination (B b) src ~fee:(Test_tez.Tez.of_int 10) ~script
  >>=? fun (operation, dst) ->
  apply_operation operation b >|=? fun b -> (b, dst)

(* Make a transaction and sync a local client state. [to_exclude] is the list
     of addresses that cannot bake the block*)
let do_transaction ~src ~dst ~parameters ?(amount = 0) ?(fee = 0) ?entrypoint b
    =
  let amount = Test_tez.Tez.of_int amount in
  let fee = Test_tez.Tez.of_int fee in
  expression_from_string parameters
  >>=? fun parameters ->
  let parameters = Script.lazy_expr parameters in
  Op.transaction ~fee ~parameters ?entrypoint (B b) src dst amount
  >>=? fun operation -> apply_operation operation b

let location = function
  | Micheline.Prim (loc, _, _, _)
  | Int (loc, _)
  | String (loc, _)
  | Bytes (loc, _)
  | Seq (loc, _) ->
      loc
