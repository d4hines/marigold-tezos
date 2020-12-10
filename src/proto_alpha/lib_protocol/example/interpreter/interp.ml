open Tezos_protocol_environment_alpha.Environment.Error_monad

open! Tezos_raw_protocol_alpha
open Stdlib
open Migrate_parsetree
open Ast_408
open Script_typed_ir

module Ast_builder = Ppxlib.Ast_builder.Make (struct
  let loc = Location.none
end)

open Ast_builder
module Compare = Tezos_protocol_environment_alpha.Environment.Compare

let print_code code =
  Format.asprintf
    "%a\n%!"
    Pprintast.structure
    (Migrate_408_409.copy_structure code)
  |> print_endline

let to_native_code code = Migrate_408_409.copy_structure code

type ('compiled, 'bef, 'aft) instr_with_code =
  | Eval : ('bef, 'aft) instr -> ('compiled, 'bef, 'aft) instr_with_code
  | Compiled : 'compiled -> ('compiled, 'bef, 'aft) instr_with_code
  | Loop_left :
      ('compiled, 'a * 'rest, ('a, 'b) union * 'rest) descr_with_code
      -> ('compiled, ('a, 'b) union * 'rest, 'b * 'rest) instr_with_code
  | Seq :
      ('compiled, 'bef, 'trans) descr_with_code
      * ('compiled, 'trans, 'aft) descr_with_code
      -> ('compiled, 'bef, 'aft) instr_with_code
  | If :
      ('compiled, 'bef, 'aft) descr_with_code
      * ('compiled, 'bef, 'aft) descr_with_code
      -> ('compiled, bool * 'bef, 'aft) instr_with_code

and ('compiled, 'bef, 'aft) descr_with_code = {
  loc : location;
  bef : 'bef stack_ty;
  aft : 'aft stack_ty;
  instr : ('compiled, 'bef, 'aft) instr_with_code;
}

external magic : 'a -> 'b = "%identity"

let rec merge_while_seq :
    type b a.
    (Parsetree.expression * 'things array, b, 'trans) descr_with_code ->
    (Parsetree.expression * 'things array, 'trans, a) descr_with_code ->
    (Parsetree.expression * 'things array, b, a) descr_with_code =
 fun left right ->
  let instr =
    match (left.instr, right.instr) with
    | (Compiled (left, v_left), Compiled (right, v_right)) ->
        Compiled
          ( [%expr
              let stack = [%e left] in
              [%e right]],
            Array.append v_left v_right )
    | (Compiled _, Seq (({instr = Compiled _; _} as r_left), r_right)) ->
        let left = merge_while_seq left r_left in
        Seq (left, r_right)
    | (Seq (l_left, ({instr = Compiled _; _} as l_right)), Compiled _) ->
        let right = merge_while_seq l_right right in
        Seq (l_left, right)
    | (_, _) ->
        Seq (left, right)
  in
  {loc = left.loc; bef = left.bef; aft = right.aft; instr}

let rec compile_to_ocaml :
    type b a.
    int ->
    (b, a) descr ->
    int
    * (Parsetree.expression * (string * 'things) array, b, a) descr_with_code =
 fun counter descr ->
  let ( counter,
        (instr :
          ( Parsetree.expression * (string * 'things) array,
            b,
            a )
          instr_with_code) ) =
    match descr.instr with
    | Drop ->
        ( counter,
          Compiled
            ( [%expr
                let (_, rest) = stack in
                rest],
              [||] ) )
    | Cons_pair ->
        ( counter,
          Compiled
            ( [%expr
                let (a, (b, rest)) = stack in
                ((a, b), rest)],
              [||] ) )
    | Cdr ->
        ( counter,
          Compiled
            ( [%expr
                let ((_, b), rest) = stack in
                (b, rest)],
              [||] ) )
    | Nil ->
        (counter, Compiled ([%expr {elements = []; length = 0}, stack], [||]))
    | Dup ->
        ( counter,
          Compiled
            ( [%expr
                let (v, rest) = stack in
                (v, (v, rest))],
              [||] ) )
    | Swap ->
        ( counter,
          Compiled
            ( [%expr
                let (vi, (vo, rest)) = stack in
                (vo, (vi, rest))],
              [||] ) )
    | Car ->
        ( counter,
          Compiled
            ( [%expr
                let ((a, _), rest) = stack in
                (a, rest)],
              [||] ) )
    | Cons_left ->
        ( counter,
          Compiled
            ( [%expr
                let (v, rest) = stack in
                (L v, rest)],
              [||] ) )
    | Cons_right ->
        ( counter,
          Compiled
            ( [%expr
                let (v, rest) = stack in
                (R v, rest)],
              [||] ) )
    | Dig (n, _) ->
        let names =
          List.init (n + 1) (fun n -> "a" ^ string_of_int n) |> List.rev
        in
        let pat =
          names
          |> List.fold_left (fun p n -> ppat_tuple [pvar n; p]) (pvar "rest")
        in
        let selected_name = "a" ^ string_of_int n in
        let new_names =
          ( names
          |> List.filter (fun name -> not (String.equal name selected_name)) )
          @ [selected_name]
        in
        let exp =
          new_names
          |> List.fold_left (fun p n -> pexp_tuple [evar n; p]) (evar "rest")
        in
        ( counter,
          Compiled
            ( [%expr
                let [%p pat] = stack in
                [%e exp]],
              [||] ) )
    | Dug (n, _) ->
        let names =
          List.init (n + 1) (fun n -> "i" ^ string_of_int n) |> List.rev
        in
        let pat =
          names
          |> List.fold_left (fun p n -> ppat_tuple [pvar n; p]) (pvar "rest")
        in
        let selected_name = "i0" in
        let new_names =
          selected_name
          :: ( names
             |> List.filter (fun name -> not (String.equal name selected_name))
             )
        in
        let exp =
          new_names
          |> List.fold_left (fun p n -> pexp_tuple [evar n; p]) (evar "rest")
        in
        ( counter,
          Compiled
            ( [%expr
                let [%p pat] = stack in
                [%e exp]],
              [||] ) )
    | Mul_intint | Mul_intnat | Mul_natint ->
        ( counter,
          Compiled
            ( [%expr
                let (x, (y, rest)) = stack in
                (Script_int.mul x y, rest)],
              [||] ) )
    | Mul_natnat ->
        ( counter,
          Compiled
            ( [%expr
                let (x, (y, rest)) = stack in
                (Script_int.mul_n x y, rest)],
              [||] ) )
    | Sub_int ->
        ( counter,
          Compiled
            ( [%expr
                let (x, (y, rest)) = stack in
                (Script_int.sub x y, rest)],
              [||] ) )
    | Gt ->
        ( counter,
          Compiled
            ( [%expr
                let (cmpres, rest) = stack in
                let cmpres = Script_int.compare cmpres Script_int.zero in
                let cmpres = Compare.Int.(cmpres > 0) in
                (cmpres, rest)],
              [||] ) )
    | Const v ->
        let name = "v_" ^ string_of_int counter in
        let counter = counter + 1 in
        ( counter,
          Compiled
            (pexp_tuple [evar name; evar "stack"], [|(name, Obj.magic v)|]) )
    | Compare ty ->
        let name = "v_" ^ string_of_int counter in
        let counter = counter + 1 in
        ( counter,
          Compiled
            ( [%expr
                let (a, (b, stack)) = stack in
                ( Script_int.of_int
                  @@ Script_ir_translator.compare_comparable [%e evar name] a b,
                  stack )],
              [|(name, Obj.magic ty)|] ) )
    | If (left, right) ->
        let (counter, left) = compile_to_ocaml counter left in
        let (counter, right) = compile_to_ocaml counter right in
        let instr =
          match (left.instr, right.instr) with
          | (Compiled (left, v_left), Compiled (right, v_right)) ->
              Compiled
                ( [%expr
                    let (value, stack) = stack in
                    if value then [%e left] else [%e right]],
                  Array.append v_left v_right )
          | _ ->
              If (left, right)
        in
        (counter, instr)
        (* TODO: inline IF *)
    | Seq (left, right) ->
        let (counter, left) = compile_to_ocaml counter left in
        let (counter, right) = compile_to_ocaml counter right in
        (counter, (merge_while_seq left right).instr)
    | Loop_left v ->
        let (counter, v) = compile_to_ocaml counter v in
        (counter, Loop_left v)
    | Nop ->
        ( counter,
          Compiled
            ( [%expr
                let stack = stack in
                stack],
              [||] ) )
    | v ->
        print_endline (Show.get_instr_name v) ;
        assert false
  in
  (counter, {loc = descr.loc; bef = descr.bef; aft = descr.aft; instr})

let header =
  [%str
    type 'a input = Input of 'a

    open Tezos_protocol_environment_alpha.Environment
    open Tezos_raw_protocol_alpha.Script_typed_ir
    open Tezos_raw_protocol_alpha.Alpha_context
    module Script_ir_translator = Tezos_raw_protocol_alpha.Script_ir_translator]

let rec merge_ocaml_code :
    type b a.
    int ->
    (string * Parsetree.structure_item * (string * 'things) array) list ->
    (Parsetree.expression * (string * 'things) array, b, a) descr_with_code ->
    int
    * (string * Parsetree.structure_item * (string * 'things) array) list
    * (int, b, a) descr_with_code =
 fun counter cmms descr ->
  let (counter, cmms, (instr : (int, b, a) instr_with_code)) =
    match descr.instr with
    | Eval eval ->
        (counter, cmms, Eval eval)
    | Compiled (code, values) ->
        let name = "tz_compiled_" ^ string_of_int counter in
        let values_pattern =
          match values |> Array.to_list with
          | [] ->
              punit
          | [(name, _)] ->
              pvar name
          | list ->
              list |> List.map (fun (name, _) -> pvar name) |> ppat_tuple
        in
        let full_code =
          pstr_value
            Ppxlib.Nonrecursive
            [ value_binding
                ~pat:(pvar name)
                ~expr:
                  [%expr fun (Input (stack, [%p values_pattern])) -> [%e code]]
            ]
        in
        let cmms = (name, full_code, values) :: cmms in
        let instr = Compiled counter in
        let counter = counter + 1 in
        (counter, cmms, instr)
    | Seq (left, right) ->
        let (counter, cmms, left) = merge_ocaml_code counter cmms left in
        let (counter, cmms, right) = merge_ocaml_code counter cmms right in
        (counter, cmms, Seq (left, right))
    | Loop_left body ->
        let (counter, cmms, body) = merge_ocaml_code counter cmms body in
        (counter, cmms, Loop_left body)
    | If (left, right) ->
        let (counter, cmms, left) = merge_ocaml_code counter cmms left in
        let (counter, cmms, right) = merge_ocaml_code counter cmms right in
        (counter, cmms, If (left, right))
  in
  (counter, cmms, {descr with instr})

let rec load :
    type b a.
    (int * 'things array) array ->
    (int, b, a) descr_with_code ->
    (int * 'value, b, a) descr_with_code =
 fun address_array descr ->
  let instr : (int * 'value, b, a) instr_with_code =
    match descr.instr with
    | Eval instr ->
        Eval instr
    | Compiled i ->
        let (addr, value) = address_array.(i) in
        let value : 'value =
          match value with
          | [||] ->
              Obj.magic ()
          | [|v|] ->
              Obj.magic v
          | _ ->
              Obj.magic value
        in
        Compiled (addr, value)
    | Seq (left, right) ->
        let left = load address_array left in
        let right = load address_array right in
        Seq (left, right)
    | If (left, right) ->
        let left = load address_array left in
        let right = load address_array right in
        If (left, right)
    | Loop_left body ->
        Loop_left (load address_array body)
  in
  {descr with instr}

let compile_and_load code =
  let (_, code_ocaml) = compile_to_ocaml 0 code in
  let (_, named_ocaml_code, code_ocaml) = merge_ocaml_code 0 [] code_ocaml in
  let names = named_ocaml_code |> List.rev_map (fun (name, _, _) -> name) in
  let full_code =
    header @ (named_ocaml_code |> List.rev_map (fun (_, code, _) -> code))
  in
  let values =
    named_ocaml_code
    |> List.rev_map (fun (_, _, value) -> value |> Array.map snd)
    |> Array.of_list
  in
  print_code full_code ;
  let cmm =
    full_code |> to_native_code
    |> Compile.compile_to_cmm ~ppf_dump:Format.std_formatter
  in
  let names =
    names |> List.map (fun name -> Compile.find_symbol_name_in_cmm name cmm)
  in
  let address_array =
    cmm
    |> Compile.compile_from_cmm names
    |> Array.of_list
    |> Array.mapi (fun i addr -> (addr, values.(i)))
  in
  load address_array code_ocaml

module Dummy = struct
  include Tezos_protocol_environment_alpha.Environment
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

  module Script_interpreter_cost = struct
    let cost_of_instr _ _ = Gas.free

    let unpack_failed _ = Gas.free

    let concat_string _ = Gas.free
  end

  module Operation = Alpha_context

  module Error_monad = struct
    include Tezos_protocol_environment_alpha.Environment.Error_monad

    let register_error_kind _ ~id:_ ~title:_ ~description:_
        ?pp:(_ = fun _ _ -> ()) _ _ _ =
      ()
  end
end

module Script_interpreter =
  Interpreter_example_functor.Script_interpreter_functor.Make (Dummy)

open Script_interpreter

let rec step_compiled :
    type b a.
    logger ->
    stack_depth:int ->
    context ->
    step_constants ->
    (int * 'value, b, a) descr_with_code ->
    b ->
    (a * context) tzresult Lwt.t =
 fun log ~stack_depth ctx const descr stack ->
  let module Log = (val log : STEP_LOGGER) in
  let logged_return : a * context -> (a * context) tzresult Lwt.t =
   fun (ret, ctxt) ->
    Log.log_exit ctxt (Obj.magic descr) ret ;
    return (ret, ctxt)
  in
  let non_terminal_recursion ~ctx ?(stack_depth = stack_depth + 1) descr stack
      =
    if Compare.Int.(stack_depth >= 10_000) then
      fail Michelson_too_many_recursive_calls
    else step_compiled log ~stack_depth ctx const descr stack
  in
  match (descr.instr, stack) with
  | (Eval instr, stack) ->
      Script_interpreter.step
        log
        ctx
        const
        {loc = descr.loc; instr; bef = descr.bef; aft = descr.aft}
        stack
  | (Compiled (addr, value), stack) ->
      let v : a =
        Call_hack.call_pointer (Call_hack.Input (stack, value)) addr
        |> Obj.magic
      in
      logged_return (v, ctx)
  | (Seq (left, right), stack) ->
      step_compiled ~stack_depth log ctx const left stack
      >>=? fun (res, ctx) -> step_compiled log ~stack_depth ctx const right res
  | (If (bt, _), (true, rest)) ->
      step_compiled ~stack_depth log ctx const bt rest
  | (If (_, bf), (false, rest)) ->
      step_compiled ~stack_depth log ctx const bf rest
  | (Loop_left body, (L v, rest)) ->
      non_terminal_recursion ~ctx body (v, rest)
      >>=? fun (trans, ctxt) ->
      step_compiled log ~stack_depth ctxt const descr trans
  | (Loop_left _, (R v, rest)) ->
      logged_return ((v, rest), ctx)

module No_trace : STEP_LOGGER = struct
  let log_interp _ctxt _descr _stack = ()

  let log_entry _ctxt _descr _stack = ()

  let log_exit _ctxt _descr _stack = ()

  let get_log () = return_none
end

let eval_code ctx ~step_constants code input =
  step (module No_trace : STEP_LOGGER) ctx step_constants code input
  >>=? fun _ -> return_unit

let eval_compiled_code ctx ~step_constants code input =
  step_compiled
    (module No_trace : STEP_LOGGER)
    ~stack_depth:0
    ctx
    step_constants
    code
    input
  >>=? fun _ -> return_unit

let run_script ctx ~step_constants ~parameter_expr script =
  Script_ir_translator.parse_script
    ctx
    script
    ~legacy:false
    ~allow_forged_in_storage:false
  >>=? fun ( Script_ir_translator.Ex_script
               {code; arg_type; storage; storage_type; root_name = _},
             ctx ) ->
  let (Lam (code, _)) = code in
  Script_ir_translator.parse_data
    ctx
    ~legacy:false
    ~allow_forged:false
    arg_type
    (Tezos_protocol_environment_alpha.Environment.Micheline.root parameter_expr)
  >>=? fun (arg, ctx) ->
  return (fun () ->
      Script_interpreter.step
        (module No_trace : STEP_LOGGER)
        ctx
        step_constants
        code
        ((arg, storage), ())
      >>=? fun (((_, storage), ()), ctx) ->
      Script_ir_translator.unparse_data ctx Readable storage_type storage)

let run_compiled_script ctx ~step_constants ~parameter_expr script =
  Script_ir_translator.parse_script
    ctx
    script
    ~legacy:false
    ~allow_forged_in_storage:false
  >>=? fun ( Script_ir_translator.Ex_script
               {code; arg_type; storage; storage_type; root_name = _},
             ctx ) ->
  let (Lam (code, _)) = code in
  Script_ir_translator.parse_data
    ctx
    ~legacy:false
    ~allow_forged:false
    arg_type
    (Tezos_protocol_environment_alpha.Environment.Micheline.root parameter_expr)
  >>=? fun (arg, ctx) ->
  let code = compile_and_load code in
  return (fun () ->
      step_compiled
        (module No_trace : STEP_LOGGER)
        ~stack_depth:0
        ctx
        step_constants
        code
        ((arg, storage), ())
      >>=? fun (((_, storage), ()), ctx) ->
      Script_ir_translator.unparse_data ctx Readable storage_type storage)
