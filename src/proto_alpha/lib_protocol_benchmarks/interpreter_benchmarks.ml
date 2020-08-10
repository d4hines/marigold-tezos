(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <nomadic@tezcore.com>                    *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Protocol

module Michelson_parsing_helpers = struct
  let node_from_string str =
    let open Error_monad in
    let module Parser = Tezos_client_alpha.Michelson_v1_parser in
    let (ast, errors) = Parser.parse_expression ~check:false str in
    match errors with
    | [] ->
        return (Environment.Micheline.root ast.Parser.expanded)
    | lst ->
        let res =
          Format.asprintf
            "While parsing expression %s\n%a@"
            str
            pp_print_error
            lst
        in
        Stdlib.failwith res

  (* Extract (typed) IR from Micheline AST, with prescribed expected input
   type. The resulting type is existentially quantified through the
   "judgement" type. *)
  let judgement_from_node (type bef) (ctxt : Alpha_context.t)
      (input_stack_ty : bef Script_typed_ir.stack_ty)
      (instr_node : (int, Alpha_context.Script.prim) Micheline.node) =
    let open Error_monad in
    Script_ir_translator.parse_instr
      ~legacy:false
      Script_ir_translator.(
        Toplevel
          {
            storage_type = Unit_t None;
            param_type = Unit_t None;
            root_name = None;
            legacy_create_contract_literal = false;
          })
      ctxt
      instr_node
      input_stack_ty
    >|= Protocol.Environment.wrap_error
    >>= function
    | Ok _ as ok ->
        Lwt.return ok
    | Error _ as err -> (
      match err with
      | Ok _ ->
          assert false
      | Error errs ->
          let msg =
            Format.asprintf "judgment_from_node: %a@." pp_print_error errs
          in
          Stdlib.failwith msg )
end

module Default_samplers_parameters = struct
  open Base_samplers
  open Michelson_samplers_parameters

  let size = 16

  let algo = `Default

  let parameters =
    {
      int_size = {min = 4; max = 10_000};
      string_size = {min = 1 lsl 10; max = 1 lsl 17};
      bytes_size = {min = 1 lsl 10; max = 1 lsl 17};
      stack_size = {min = 3; max = 3};
      type_depth = {min = 3; max = 3};
      list_size = {min = 10; max = 1000};
      set_size = {min = 10; max = 1000};
      map_size = {min = 10; max = 1000};
    }
end

module Default_config = struct
  type config = Michelson_samplers_parameters.t

  let default_config =
    let open Michelson_samplers_parameters in
    {
      int_size = {min = 8; max = 100_000};
      string_size = {min = 1 lsl 10; max = 1 lsl 17};
      bytes_size = {min = 1 lsl 10; max = 1 lsl 17};
      stack_size = {min = 3; max = 3};
      type_depth = {min = 3; max = 3};
      list_size = {min = 10; max = 1000};
      set_size = {min = 10; max = 1000};
      map_size = {min = 10; max = 1000};
    }

  let config_encoding = Michelson_samplers_parameters.encoding
end

module Default_boilerplate = struct
  type workload = Interpreter_workload.t

  let workload_encoding = Interpreter_workload.encoding

  let workload_to_vector = Interpreter_workload.trace_to_sparse_vec

  let models = Interpreter_model.make_model

  let tags = [Tags.interpreter]
end

module No_trace : Script_interpreter.STEP_LOGGER = struct
  open Environment.Error_monad

  let log_interp _ctxt _descr _stack = ()

  let log_entry _ctxt _descr _stack = ()

  let log_exit _ctxt _descr _stack = ()

  let get_log () = return_none
end

let create_benchmark rng_state instr stack_ty stack =
  let open Error_monad in
  Lwt_main.run
    ( Execution_context.make ~rng_state
    >>=? fun (context, step_constants) ->
    Michelson_parsing_helpers.node_from_string instr
    >>=? fun instr_node ->
    Script_ir_translator.parse_instr
      ~legacy:false
      Script_ir_translator.(
        Toplevel
          {
            storage_type = Unit_t None;
            param_type = Unit_t None;
            root_name = None;
            legacy_create_contract_literal = false;
          })
      context
      instr_node
      stack_ty
    >|= Protocol.Environment.wrap_error
    >>=? fun (jdg, ctxt) ->
    match jdg with
    | Script_ir_translator.Typed descr ->
        let (_, workload, _) =
          Interpreter_workload.extract_deps ctxt step_constants descr stack
        in
        let ctxt = Gas_helpers.set_limit ctxt in
        let closure () =
          ignore @@ Lwt_main.run
          @@ Script_interpreter.step
               (module No_trace)
               ctxt
               step_constants
               descr
               stack
        in
        return (Generator.Plain {workload; closure})
    | _ ->
        assert false )
  |> function
  | Ok closure ->
      closure
  | Error errs ->
      Format.eprintf "%a@." Error_monad.pp_print_error errs ;
      raise (Failure "Interpreter_benchmarks.create_benchmark")

let make ?specialization ~model ~instr ~stack_type ~info ?name () =
  let module B : Benchmark.S = struct
    let name = match name with None -> instr | Some name -> name

    let info = info

    include Default_config
    include Default_boilerplate

    let models = models ?specialization model

    let benchmark rng_state config () =
      let module Samplers = Michelson_samplers.Make (struct
        include Default_samplers_parameters

        let parameters = config

        let state = rng_state
      end) in
      let input_stack = Samplers.Random_value.stack stack_type in
      create_benchmark rng_state instr stack_type input_stack

    let create_benchmarks ~rng_state ~bench_num (config : config) =
      List.repeat bench_num (benchmark rng_state config)
  end in
  Registration_helpers.register (module B) ;
  (module B : Benchmark.S)

(* ------------------------------------------------------------------------- *)

open Michelson_types

module List_size_bench =
( val make
        ~model:(Some N_List_size)
        ~instr:"SIZE"
        ~stack_type:unit_list_stack_ty
        ~info:"Benchmarking the SIZE instruction on random lists of unit."
        ~name:"LIST_SIZE"
        () )

(* CONS *)
module Cons_bench =
( val make
        ~model:(Some N_Cons_list)
        ~instr:"CONS"
        ~stack_type:unit_unit_list_stack_ty
        ~info:"Benchmarking the CONS instruction on random lists of unit."
        () )

(* NIL *)
module Nil_bench =
( val make
        ~model:(Some N_Nil)
        ~instr:"NIL unit"
        ~stack_type:empty_stack_ty
        ~info:"Benchmarking the NIL instruction on an empty stack."
        ~name:"NIL_unit"
        () )

(* IF_CONS *)
module If_cons_bench =
( val make
        ~model:(Some N_If_cons)
        ~instr:"IF_CONS { DROP ; DROP } { }"
        ~stack_type:unit_list_stack_ty
        ~info:"Benchmarking the IF_CONS instruction on an list of units."
        ~name:"IF_CONS"
        () )

(* MAP *)
module List_map_bench =
( val make
        ~model:(Some N_List_map)
        ~instr:"MAP { }"
        ~stack_type:unit_list_stack_ty
        ~info:"Benchmarking the MAP instruction on a list of units."
        ~name:"LIST_MAP"
        () )

(* ITER *)
module List_iter_bench =
( val make
        ~model:(Some N_List_iter)
        ~instr:"ITER { DROP }"
        ~stack_type:unit_list_stack_ty
        ~info:"Benchmarking the ITER instruction on a list of units."
        ~name:"LIST_ITER"
        () )

(* ------------------------------------------------------------------------- *)
(* Option operations *)

(* SOME *)
module Some_bench =
( val make
        ~model:(Some N_Cons_some)
        ~instr:"SOME"
        ~stack_type:unit_stack_ty
        ~info:"Benchmarking the SOME instruction on a unit value."
        () )

(* NONE *)
module None_bench =
( val make
        ~model:(Some N_Cons_none)
        ~instr:"NONE unit"
        ~stack_type:empty_stack_ty
        ~info:"Benchmarking the NONE instruction on an empty stack."
        ~name:"NONE"
        () )

(* IF_NONE *)
module If_none_bench =
( val make
        ~model:(Some N_If_none)
        ~instr:"IF_NONE {} { DROP }"
        ~stack_type:unit_option_stack_ty
        ~info:"Benchmarking the IF_NONE instruction on a unit option."
        ~name:"IF_NONE"
        () )

(* ------------------------------------------------------------------------- *)
(* Stack operations *)

(* Somme dummy micro-bench to give a baseline execution time *)
(* Drop *)
module Drop_bench =
( val make
        ~model:(Some N_Drop)
        ~instr:"DROP"
        ~stack_type:unit_stack_ty
        ~info:"Benchmarking the DROP instruction"
        ~name:"DROP"
        () )

(* Nop *)
module Nop_bench =
( val make
        ~model:(Some N_Nop)
        ~instr:"{ }"
        ~stack_type:unit_stack_ty
        ~info:"Benchmarking the NOP instruction"
        ~name:"NOP"
        () )

(* Seq *)
module Seq_simple_bench =
( val make
        ~model:(Some N_Seq)
        ~instr:"{ { } ; }"
        ~stack_type:unit_stack_ty
        ~info:"benchmarking the SEQ instruction (fixed length)"
        ~name:"SEQ_simple"
        () )

module Seq_bench : Benchmark.S = struct
  type config = {
    sampler_parameters : Default_config.config;
    sequence_length : int;
  }

  let default_config =
    {sampler_parameters = Default_config.default_config; sequence_length = 300}

  let config_encoding =
    let open Data_encoding in
    conv
      (fun {sampler_parameters; sequence_length} ->
        (sampler_parameters, sequence_length))
      (fun (sampler_parameters, sequence_length) ->
        {sampler_parameters; sequence_length})
      (tup2 Default_config.config_encoding int31)

  include Default_boilerplate

  let models = models None

  let name = "SEQ"

  let info = "Benchmarking the SEQ instruction"

  let tags = [Tags.interpreter]

  let rec gen_seq n =
    if n = 0 then "{ }" else Printf.sprintf "{ ; %s }" (gen_seq (n - 1))

  let generate_sequence rng_state cfg =
    let n = Random.State.int rng_state cfg.sequence_length in
    gen_seq n

  let benchmark (config : config) rng_state () =
    let module Samplers = Michelson_samplers.Make (struct
      include Default_samplers_parameters

      let parameters = config.sampler_parameters

      let state = rng_state
    end) in
    let input_stack = Samplers.Random_value.stack unit_stack_ty in
    create_benchmark
      rng_state
      (generate_sequence rng_state config)
      unit_stack_ty
      input_stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (benchmark config rng_state)
end

let () = Registration_helpers.register (module Seq_bench)

(* Dup *)
module Dup_bench =
( val make
        ~model:(Some N_Dup)
        ~instr:"DUP"
        ~stack_type:unit_stack_ty
        ~info:"Benchmarking the DUP instruction"
        ~name:"DUP"
        () )

(* Push nat *)
module Push_bench =
( val make
        ~model:(Some N_Const)
        ~instr:"PUSH nat 3"
        ~stack_type:empty_stack_ty
        ~info:"Benchmarking the PUSH nat instruction"
        ~name:"PUSH_NAT"
        () )

(* Dip *)
module Dip_bench =
( val make
        ~model:(Some N_Dip)
        ~instr:"DIP { }"
        ~stack_type:unit_unit_stack_ty
        ~info:"Benchmarking the DIP instruction"
        ~name:"DIP"
        () )

(* Swap *)
module Swap_bench : Benchmark.S = struct
  type config = {
    sampler_parameters : Default_config.config;
    swaps : Base_samplers.range;
  }

  let default_config =
    {
      sampler_parameters = Default_config.default_config;
      swaps = {Base_samplers.min = 20; max = 4096};
    }

  let config_encoding =
    let open Data_encoding in
    conv
      (fun {sampler_parameters; swaps} -> (sampler_parameters, swaps))
      (fun (sampler_parameters, swaps) -> {sampler_parameters; swaps})
      (tup2 Default_config.config_encoding Base_samplers.range_encoding)

  include Default_boilerplate

  let models = models (Some N_Swap)

  let stack_ty =
    let open Script_typed_ir in
    Item_t (unit_ty, Item_t (unit_ty, Empty_t, None), None)

  let swap n =
    if n <= 0 then invalid_arg "swap: n <= 0"
    else
      let str = "SWAP" in
      let swaps = List.init n (fun _ -> str) in
      "{" ^ String.concat " ; " swaps ^ "}"

  (* let rec seq n =
   *   if n <= 0 then invalid_arg "seq: n <= 0"
   *   else if n = 1 then "{ }"
   *   else "{ " ^ seq (n - 1) ^ " }" *)

  (* a sequence of n swaps *)
  let create_benchmark_nswap rng_state cfg () =
    let module Samplers = Michelson_samplers.Make (struct
      include Default_samplers_parameters

      let parameters = cfg.sampler_parameters

      let state = rng_state
    end) in
    let n = Base_samplers.sample_in_interval rng_state ~range:cfg.swaps in
    let random_stack = Samplers.Random_value.stack stack_ty in
    create_benchmark rng_state (swap n) stack_ty random_stack

  let name = "SWAP"

  let info = "Benchmarking sequences of SWAP instructions of various lengths."

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (create_benchmark_nswap rng_state config)
end

let () = Registration_helpers.register (module Swap_bench)

(* Benchmarking the fancy new instructions *)
type ex_stack = Ex_stack : 'a Script_typed_ir.stack_ty * 'a -> ex_stack

let rec make_stack (depth : int) =
  if depth = 0 then Ex_stack (Script_typed_ir.Empty_t, ())
  else
    let stack = make_stack (depth - 1) in
    match stack with
    | Ex_stack (stack_ty, stack) ->
        Ex_stack (Item_t (unit_ty, stack_ty, None), ((), stack))

module Dig_dug_config = struct
  type config = {max_depth : int}

  let default_config = {max_depth = 1024}

  let config_encoding =
    let open Data_encoding in
    conv
      (fun {max_depth} -> max_depth)
      (fun max_depth -> {max_depth})
      (obj1 (req "max_depth" int31))
end

module Dig_bench : Benchmark.S = struct
  include Dig_dug_config
  include Default_boilerplate

  let name = "DIG"

  let info = "Benchmarking the DIG instruction"

  let models = models (Some N_Dig)

  let make_benchmark rng_state cfg () =
    let depth = 1 + Random.State.int rng_state cfg.max_depth in
    let dig = Random.State.int rng_state depth in
    let stack = make_stack depth in
    match stack with
    | Ex_stack (stack_ty, stack) ->
        create_benchmark rng_state (Printf.sprintf "DIG %d" dig) stack_ty stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_benchmark rng_state config)
end

let () = Registration_helpers.register (module Dig_bench)

module Dug_bench : Benchmark.S = struct
  include Dig_dug_config
  include Default_boilerplate

  let name = "DUG"

  let info = "Benchmarking the DUG instruction"

  let models = models (Some N_Dug)

  let make_benchmark rng_state cfg () =
    let depth = 1 + Random.State.int rng_state cfg.max_depth in
    let dug = Random.State.int rng_state depth in
    let stack = make_stack depth in
    match stack with
    | Ex_stack (stack_ty, stack) ->
        create_benchmark rng_state (Printf.sprintf "DUG %d" dug) stack_ty stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_benchmark rng_state config)
end

let () = Registration_helpers.register (module Dug_bench)

module DipN_bench = struct
  include Dig_dug_config
  include Default_boilerplate

  let name = "DIPN"

  let info = "Benchmarking the DIPN instruction"

  let models = models (Some N_DipN)

  let make_benchmark rng_state cfg () =
    let depth = 1 + Random.State.int rng_state cfg.max_depth in
    let dip = Random.State.int rng_state depth in
    let stack = make_stack depth in
    match stack with
    | Ex_stack (stack_ty, stack) ->
        create_benchmark
          rng_state
          (Printf.sprintf "DIP %d { }" dip)
          stack_ty
          stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_benchmark rng_state config)
end

let () = Registration_helpers.register (module DipN_bench)

module DropN_bench : Benchmark.S = struct
  include Dig_dug_config
  include Default_boilerplate

  let name = "DROPN"

  let info = "Benchmarking the DROPN instruction"

  let models = models (Some N_DropN)

  let make_benchmark rng_state cfg () =
    let depth = 1 + Random.State.int rng_state cfg.max_depth in
    let drop = Random.State.int rng_state depth in
    let stack = make_stack depth in
    match stack with
    | Ex_stack (stack_ty, stack) ->
        create_benchmark
          rng_state
          (Printf.sprintf "DROP %d" drop)
          stack_ty
          stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_benchmark rng_state config)
end

let () = Registration_helpers.register (module DropN_bench)

module DupN_bench : Benchmark.S = struct
  include Dig_dug_config
  include Default_boilerplate

  let name = "DUPN"

  let info = "Benchmarking the DUPN instruction"

  let models = models (Some N_DupN)

  let make_benchmark rng_state cfg () =
    let depth = 1 + Random.State.int rng_state cfg.max_depth in
    let dup = 1 + Random.State.int rng_state depth in
    let stack = make_stack depth in
    match stack with
    | Ex_stack (stack_ty, stack) ->
        create_benchmark rng_state (Printf.sprintf "DUP %d" dup) stack_ty stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_benchmark rng_state config)
end

let () = Registration_helpers.register (module DupN_bench)

(* ------------------------------------------------------------------------- *)
(* Loops *)

module Loop_bench =
( val make
        ~model:(Some N_Loop)
        ~instr:"LOOP { PUSH bool False }"
        ~stack_type:bool_stack_ty
        ~info:"Benchmarking the LOOP instruction"
        ~name:"LOOP"
        () )

module Loop_left =
( val make
        ~model:(Some N_Loop_left)
        ~instr:"LOOP_LEFT { RIGHT unit }"
        ~stack_type:unit_union_stack_ty
        ~info:"Benchmarking the LOOP_LEFT instruction"
        ~name:"LOOP_LEFT"
        () )

(* ------------------------------------------------------------------------- *)
(* Operations on booleans *)

(* OR *)
module Bool_or_bench =
( val make
        ~model:(Some N_Or)
        ~instr:"OR"
        ~stack_type:bool_bool_stack_ty
        ~info:"Benchmarking the OR instruction on booleans."
        ~name:"BOOL_OR"
        () )

(* AND *)
module Bool_and_bench =
( val make
        ~model:(Some N_And)
        ~instr:"AND"
        ~stack_type:bool_bool_stack_ty
        ~info:"Benchmarking the AND instruction on booleans."
        ~name:"BOOL_AND"
        () )

(* XOR *)
module Bool_xor_bench =
( val make
        ~model:(Some N_Xor)
        ~instr:"XOR"
        ~stack_type:bool_bool_stack_ty
        ~info:"Benchmarking the XOR instruction on booleans."
        ~name:"BOOL_XOR"
        () )

(* NOT *)
module Bool_not_bench =
( val make
        ~model:(Some N_Not)
        ~instr:"NOT"
        ~stack_type:bool_stack_ty
        ~info:"Benchmarking the NOT instruction on booleans."
        ~name:"BOOL_NOT"
        () )

(* ------------------------------------------------------------------------- *)
(* Operations on (signed) integers *)

(* Addition of signed integers *)
module Add_bench =
( val make
        ~model:(Some N_Add_intint)
        ~instr:"ADD"
        ~stack_type:int_int_stack_ty
        ~info:"Benchmarking the ADD instruction on signed integers."
        () )

(* Subtraction of signed integers *)
module Sub_bench =
( val make
        ~model:(Some N_Sub_int)
        ~instr:"SUB"
        ~stack_type:int_int_stack_ty
        ~info:"Benchmarking the SUB instruction on signed integers."
        () )

(* Multiplication of signed integers *)
module Mul_bench =
( val make
        ~model:(Some N_Mul_intint)
        ~instr:"MUL"
        ~stack_type:int_int_stack_ty
        ~info:"Benchmarking the MUL instruction on signed integers."
        () )

(* Euclidian division of unsigned integers *)
module Div_bench =
( val make
        ~model:(Some N_Ediv_natnat)
        ~instr:"EDIV"
        ~stack_type:nat_nat_stack_ty
        ~info:"Benchmarking the DIV instruction on natural integers."
        () )

(* Negation of signed integers *)
module Neg_bench =
( val make
        ~model:(Some N_Neg_int)
        ~instr:"NEG"
        ~stack_type:int_stack_ty
        ~info:"Benchmarking the NEG instruction on signed integers."
        () )

(* Absolute value of signed integers *)
module Abs_bench =
( val make
        ~model:(Some N_Abs_int)
        ~instr:"ABS"
        ~stack_type:int_stack_ty
        ~info:"Benchmarking the ABS instruction on signed integers."
        () )

(* OR *)
module Nat_or_bench =
( val make
        ~model:(Some N_Or_nat)
        ~instr:"OR"
        ~stack_type:nat_nat_stack_ty
        ~info:"Benchmarking the OR instruction on unsigned integers."
        ~name:"NAT_OR"
        () )

(* AND *)
module Nat_and_bench =
( val make
        ~model:(Some N_And_nat)
        ~instr:"AND"
        ~stack_type:nat_nat_stack_ty
        ~info:"Benchmarking the AND instruction on unsigned integers."
        ~name:"NAT_AND"
        () )

(* XOR *)
module Nat_xor_bench =
( val make
        ~model:(Some N_Xor_nat)
        ~instr:"XOR"
        ~stack_type:nat_nat_stack_ty
        ~info:"Benchmarking the XOR instruction on unsigned integers."
        ~name:"NAT_XOR"
        () )

(* NOT *)
module Int_not_bench =
( val make
        ~model:(Some N_Not_int)
        ~instr:"NOT"
        ~stack_type:int_stack_ty
        ~info:"Benchmarking the NOT instruction on signed integers."
        ~name:"INT_NOT"
        () )

(* INT *)
module Int_bench =
( val make
        ~model:(Some N_Int_nat)
        ~instr:"INT"
        ~stack_type:nat_stack_ty
        ~info:"Benchmarking the INT instruction."
        ~name:"INT_CAST"
        () )

(* IS_NAT *)
module Is_nat_bench =
( val make
        ~model:(Some N_Is_nat)
        ~instr:"ISNAT"
        ~stack_type:int_stack_ty
        ~info:"Benchmarking the IS_NAT instruction."
        ~name:"ISNAT"
        () )

module Nat_lsl_bench : Benchmark.S = struct
  include Default_config
  include Default_boilerplate

  let stack_type = nat_nat_stack_ty

  let name = "LSL"

  let info = "Benchmarking the LSL instruction on unsigned integers."

  let models = models (Some N_Lsl_nat)

  let make_bench rng_state (cfg : config) () =
    let module Samplers = Michelson_samplers.Make (struct
      include Default_samplers_parameters

      let parameters = cfg

      let state = rng_state
    end) in
    let x = Samplers.Michelson_base.nat () in
    (* shift must be in [0;256]: 1 byte max *)
    let shift =
      Script_int_repr.(abs (of_int (Random.State.int rng_state 256)))
    in
    let stack = (x, (shift, ())) in
    create_benchmark rng_state "LSL" stack_type stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Nat_lsl_bench)

(* LSR *)
module Nat_lsr_bench : Benchmark.S = struct
  include Default_config
  include Default_boilerplate

  let stack_type = nat_nat_stack_ty

  let name = "LSR"

  let info = "Benchmarking the LSR instruction on unsigned integers."

  let models = models (Some N_Lsr_nat)

  let make_bench rng_state (cfg : config) () =
    let module Samplers = Michelson_samplers.Make (struct
      include Default_samplers_parameters

      let parameters = cfg

      let state = rng_state
    end) in
    let x = Samplers.Michelson_base.nat () in
    let shift =
      Script_int_repr.(abs (of_int (Random.State.int rng_state 256)))
    in
    let stack = (x, (shift, ())) in
    create_benchmark rng_state "LSR" stack_type stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Nat_lsr_bench)

(* NEQ *)

module Neq_bench =
( val make
        ~model:(Some N_Neq)
        ~instr:"NEQ"
        ~stack_type:int_int_stack_ty
        ~info:"Benchmarking the NEQ instruction on signed integers."
        ~name:"INT_NEQ"
        () )

module Eq_zero_bench =
( val make
        ~model:(Some N_Eq)
        ~instr:"EQ"
        ~stack_type:int_stack_ty
        ~info:"Benchmarking the EQ instruction."
        ~name:"EQ"
        () )

(* ------------------------------------------------------------------------- *)
(* Tez *)

module Tez_add_bench =
( val make
        ~model:(Some N_Add_tez)
        ~instr:"ADD"
        ~stack_type:mutez_mutez_stack_ty
        ~info:"Benchmarking the addition of mutez"
        ~name:"ADD_MUTEZ"
        () )

module Tez_sub_bench = struct
  include Default_config
  include Default_boilerplate

  let stack_type = mutez_mutez_stack_ty

  let name = "SUB_MUTEZ"

  let info = "Benchmarking subtraction of mutez."

  let models = models (Some N_Sub_tez)

  let make_bench rng_state cfg () =
    let module Samplers = Michelson_samplers.Make (struct
      include Default_samplers_parameters

      let parameters = cfg

      let state = rng_state
    end) in
    let a = Samplers.Random_value.value mutez_ty in
    let b =
      match Alpha_context.Tez.(a /? 2L) with
      | Error _ ->
          assert false
      | Ok x ->
          x
    in
    let stack = (a, (b, ())) in
    create_benchmark rng_state "SUB" stack_type stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Tez_sub_bench)

module Tez_mul_nat_bench : Benchmark.S = struct
  include Default_config
  include Default_boilerplate

  let stack_type = mutez_nat_stack_ty

  let name = "MUL_MUTEZ_NAT"

  let info = "Benchmarking the multiplication of mutez by nat."

  let models = models (Some N_Mul_teznat)

  let make_bench rng_state cfg () =
    let module Samplers = Michelson_samplers.Make (struct
      include Default_samplers_parameters

      let parameters = cfg

      let state = rng_state
    end) in
    let mutez = Samplers.Random_value.value mutez_ty in
    let mutez_int64 = Alpha_context.Tez.to_mutez mutez in
    let int64 = Int64.(div max_int (mul mutez_int64 2L)) in
    let nat =
      match Alpha_context.Script_int.(is_nat (of_int64 int64)) with
      | None ->
          assert false
      | Some nat ->
          nat
    in
    let stack = (mutez, (nat, ())) in
    create_benchmark rng_state "MUL" stack_type stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Tez_mul_nat_bench)

module Tez_div_bench =
( val make
        ~model:(Some N_Ediv_tez)
        ~instr:"EDIV"
        ~stack_type:mutez_mutez_stack_ty
        ~info:"Benchmarking the division of mutez"
        ~name:"MUTEZ_DIV"
        () )

module Tez_div_nat_bench : Benchmark.S = struct
  include Default_config
  include Default_boilerplate

  let stack_type = mutez_nat_stack_ty

  let name = "MUTEZ_DIV_NAT"

  let info = "Benchmarking the multiplication of mutez by nat."

  let models = models (Some N_Ediv_teznat)

  let make_bench rng_state cfg () =
    let module Samplers = Michelson_samplers.Make (struct
      include Default_samplers_parameters

      let parameters = cfg

      let state = rng_state
    end) in
    let mutez = Samplers.Random_value.value mutez_ty in
    let int64 = Random.int64 Int64.max_int in
    let nat =
      match Alpha_context.Script_int.(is_nat (of_int64 int64)) with
      | None ->
          assert false
      | Some nat ->
          nat
    in
    let stack = (mutez, (nat, ())) in
    create_benchmark rng_state "EDIV" stack_type stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Tez_div_nat_bench)

(* ------------------------------------------------------------------------- *)
(* Strings *)

(* CONCAT *)
module Concat_bench =
( val make
        ~model:(Some N_Concat_string)
        ~instr:"CONCAT"
        ~stack_type:string_list_stack_ty
        ~info:"Benchmarking the CONCAT instruction on string lists."
        ()
        ~name:"CONCAT_LIST" )

module Concat_bench_small : Benchmark.S = struct
  include Default_config
  include Default_boilerplate

  let stack_type = string_list_stack_ty

  let name = "CONCAT_SMALL"

  let info = "Benchmarking the CONCAT instruction on lists of small strings."

  let models = models None

  let random_string rng_state =
    String.make 1 (Char.chr (Random.State.int rng_state 256))

  let make_bench rng_state (cfg : config) () =
    let len =
      Base_samplers.sample_in_interval rng_state ~range:cfg.list_size
    in
    let v =
      {
        Script_typed_ir.elements =
          List.init len (fun _ -> random_string rng_state);
        length = len;
      }
    in
    let stack = (v, ()) in
    create_benchmark rng_state "CONCAT" stack_type stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Concat_bench_small)

module Concat_pair_bench =
( val make
        ~model:(Some N_Concat_string_pair)
        ~instr:"CONCAT"
        ~stack_type:string_string_stack_ty
        ~info:"Benchmarking the CONCAT instruction on string pairs."
        ()
        ~name:"CONCAT_PAIR" )

(* SIZE *)
module String_size_bench =
( val make
        ~model:(Some N_String_size)
        ~instr:"SIZE"
        ~stack_type:string_stack_ty
        ~info:"Benchmarking the SIZE instruction on strings."
        ~name:"STRING_SIZE"
        () )

(* SLICE *)
module String_slice_bench : Benchmark.S = struct
  include Default_config
  include Default_boilerplate

  let name = "STRING_SLICE"

  let info = "Benchmarking the SLICE instruction on strings."

  let models = models (Some N_Slice_string)

  let stack_type =
    let open Script_typed_ir in
    Item_t
      (nat_ty, Item_t (nat_ty, Item_t (string_ty, Empty_t, None), None), None)

  let nat_of_positive_int (i : int) =
    let open Alpha_context.Script_int in
    match is_nat (of_int i) with None -> assert false | Some x -> x

  let make_bench rng_state cfg () =
    let module Samplers = Michelson_samplers.Make (struct
      include Default_samplers_parameters

      let parameters = cfg

      let state = rng_state
    end) in
    let string = Samplers.Random_value.value Script_typed_ir.(String_t None) in
    let len = nat_of_positive_int @@ String.length string in
    (* worst case: offset = 0 *)
    let stack = (nat_of_positive_int 0, (len, (string, ()))) in
    create_benchmark rng_state "SLICE" stack_type stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module String_slice_bench)

(* ------------------------------------------------------------------------- *)
(* PAIRS : PAIR/CAR/CDR *)

module Pair_bench =
( val make
        ~model:(Some N_Cons_pair)
        ~instr:"PAIR"
        ~stack_type:unit_unit_stack_ty
        ~info:"Benchmarking the PAIR instruction"
        ~name:"PAIR"
        () )

module Unpair_bench =
( val make
        ~model:(Some N_Unpair)
        ~instr:"UNPAIR"
        ~stack_type:unit_pair_stack_ty
        ~info:"Benchmarking the UNPAIR instruction"
        ~name:"UNPAIR"
        () )

module Car_bench =
( val make
        ~model:(Some N_Car)
        ~instr:"CAR"
        ~stack_type:unit_pair_stack_ty
        ~info:"Benchmarking the CAR instruction"
        ~name:"CAR"
        () )

module Cdr_bench =
( val make
        ~model:(Some N_Cdr)
        ~instr:"CDR"
        ~stack_type:unit_pair_stack_ty
        ~info:"Benchmarking the CDR instruction"
        ~name:"CDR"
        () )

(* ------------------------------------------------------------------------- *)
(* IF *)

module If_bench =
( val make
        ~model:(Some N_If)
        ~instr:"IF { } { }"
        ~stack_type:bool_stack_ty
        ~info:"Benchmarking the IF instruction"
        ~name:"IF"
        () )

(* ------------------------------------------------------------------------- *)
(* UNIONS : LEFT/RIGHT/IF_LEFT/IF_RIGHT *)

module Left_bench =
( val make
        ~model:(Some N_Left)
        ~instr:"LEFT unit"
        ~stack_type:unit_stack_ty
        ~info:"Benchmarking the LEFT instruction"
        ~name:"LEFT"
        () )

(* Needed because referenced in LOOP_LEFT *)
module Right_bench =
( val make
        ~model:(Some N_Right)
        ~instr:"RIGHT unit"
        ~stack_type:unit_stack_ty
        ~info:"Benchmarking the RIGHT instruction"
        ~name:"RIGHT"
        () )

module If_left_bench =
( val make
        ~model:(Some N_If_left)
        ~instr:"IF_LEFT { DROP } { DROP }"
        ~stack_type:unit_union_stack_ty
        ~info:"Benchmarking the IF_LEFT instruction"
        ~name:"IF_LEFT"
        () )

(* ------------------------------------------------------------------------- *)
(* SETS : EMPTY_SET/MEM/UPDATE/ITER/SIZE *)

module Empty_set_bench =
( val make
        ~model:(Some N_Empty_set)
        ~instr:"EMPTY_SET int"
        ~stack_type:empty_stack_ty
        ~info:"Benchmarking the EMPTY_SET instruction"
        ~name:"EMPTY_SET"
        () )

module Int_set_mem_bench =
( val make
        ~model:None
        ~instr:"MEM"
        ~stack_type:int_int_set_stack_ty
        ~info:"Benchmarking the MEM instruction on Z.t sets"
        ~name:"INT_SET_MEM"
        () )

module Int_set_mem_adversarial_bench : Benchmark.S = struct
  include Default_config
  include Default_boilerplate

  let stack_type = int_int_set_stack_ty

  let name = "INT_SET_MEM_ADVERSARIAL"

  let info =
    "Benchmarking the MEM instruction on Z.t sets in an adversarial setting"

  let models = models (Some N_Set_mem)

  let make_bench rng_state (cfg : config) () =
    let set_size =
      Base_samplers.sample_in_interval rng_state ~range:cfg.set_size
    in
    let (_common_prefix, ls) =
      Base_samplers.Adversarial.integers
        rng_state
        ~range:cfg.int_size
        ~n:set_size
    in
    let ls = List.map Script_int_repr.of_zint ls in
    (* add elements into set *)
    let empty_set =
      Script_ir_translator.empty_set (Script_typed_ir.Int_key None)
    in
    let set =
      List.fold_left
        (fun set elt -> Script_ir_translator.set_update elt true set)
        empty_set
        ls
    in
    let elt = List.nth ls (Random.int set_size) in
    let stack = (elt, (set, ())) in
    create_benchmark rng_state "MEM" stack_type stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Int_set_mem_adversarial_bench)

module Int_set_update_adversarial_bench : Benchmark.S = struct
  include Default_config
  include Default_boilerplate

  let stack_type = int_bool_int_set_stack_ty

  let name = "INT_SET_UPDATE_ADVERSARIAL"

  let info =
    "Benchmarking the UPDATE instruction on Z.t sets in an adversarial setting"

  let models = models (Some N_Set_update)

  let make_bench rng_state (cfg : config) () =
    let set_size =
      Base_samplers.sample_in_interval rng_state ~range:cfg.set_size
    in
    let (_common_prefix, ls) =
      Base_samplers.Adversarial.integers
        rng_state
        ~range:cfg.int_size
        ~n:(set_size * 2)
    in
    let (in_set, not_in_set) = List.split_n set_size ls in
    (* add elements into set *)
    let empty_set =
      Script_ir_translator.empty_set (Script_typed_ir.Int_key None)
    in
    let set =
      List.fold_left
        (fun set elt -> Script_ir_translator.set_update elt true set)
        empty_set
        (List.map Script_int_repr.of_zint in_set)
    in
    let stack =
      let flip = Random.bool () in
      if flip then
        (* add an element not in the set *)
        let elt = List.nth not_in_set (Random.int set_size) in
        (Script_int_repr.of_zint elt, (flip, (set, ())))
      else
        (* remove an element in the set *)
        let elt = List.nth in_set (Random.int set_size) in
        (Script_int_repr.of_zint elt, (flip, (set, ())))
    in
    create_benchmark rng_state "UPDATE" stack_type stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Int_set_update_adversarial_bench)

module Int_set_size_bench =
( val make
        ~model:(Some N_Set_size)
        ~instr:"SIZE"
        ~stack_type:int_set_stack_ty
        ~info:"Benchmarking the SIZE instruction on Z.t sets"
        ~name:"INT_SET_SIZE"
        () )

module Int_set_iter_bench =
( val make
        ~model:(Some N_Set_iter)
        ~instr:"ITER { DROP }"
        ~stack_type:int_set_stack_ty
        ~info:"Benchmarking the ITER instruction on Z.t sets"
        ~name:"INT_SET_ITER"
        () )

(* ------------------------------------------------------------------------- *)
(* MAPS : EMPTY_MAP/GET/MEM/UPDATE/MAP/ITER/SIZE *)

module Empty_map_bench =
( val make
        ~model:(Some N_Empty_map)
        ~instr:"EMPTY_MAP int unit"
        ~stack_type:empty_stack_ty
        ~info:"Benchmarking the EMPTY_MAP instruction"
        ~name:"EMPTY_MAP"
        () )

module Int_map_mem_bench =
( val make
        ~model:None
        ~instr:"MEM"
        ~stack_type:int_int_unit_map_stack_ty
        ~info:"Benchmarking the MEM instruction ont Z.t maps"
        ~name:"INT_MAP_MEM"
        () )

module Int_map_mem_adversarial_bench : Benchmark.S = struct
  include Default_config
  include Default_boilerplate

  let stack_type = int_int_unit_map_stack_ty

  let name = "INT_MAP_MEM_ADVERSARIAL"

  let info =
    "Benchmarking the MEM instruction on (Z.t, unit) maps in an adversarial \
     setting"

  let models = models (Some N_Map_mem)

  let make_bench rng_state (cfg : config) () =
    let map_size =
      Base_samplers.sample_in_interval rng_state ~range:cfg.map_size
    in
    let (_common_prefix, ls) =
      Base_samplers.Adversarial.integers
        rng_state
        ~range:cfg.int_size
        ~n:map_size
    in
    let ls = List.map Script_int_repr.of_zint ls in
    (* add elements into set *)
    let empty_map =
      Script_ir_translator.empty_map (Script_typed_ir.Int_key None)
    in
    let map =
      List.fold_left
        (fun map elt -> Script_ir_translator.map_update elt (Some ()) map)
        empty_map
        ls
    in
    let elt = List.nth ls (Random.int map_size) in
    let stack = (elt, (map, ())) in
    create_benchmark rng_state "MEM" stack_type stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Int_map_mem_adversarial_bench)

module Int_map_get_adversarial_bench : Benchmark.S = struct
  include Default_config
  include Default_boilerplate

  let stack_type = int_int_unit_map_stack_ty

  let name = "INT_MAP_GET_ADVERSARIAL"

  let info =
    "Benchmarking the GET instruction on (Z.t, unit) maps in an adversarial \
     setting"

  let models = models (Some N_Map_get)

  let make_bench rng_state (cfg : config) () =
    let map_size =
      Base_samplers.sample_in_interval rng_state ~range:cfg.map_size
    in
    let (_common_prefix, ls) =
      Base_samplers.Adversarial.integers
        rng_state
        ~range:cfg.int_size
        ~n:map_size
    in
    let ls = List.map Script_int_repr.of_zint ls in
    (* add elements into set *)
    let empty_map =
      Script_ir_translator.empty_map (Script_typed_ir.Int_key None)
    in
    let map =
      List.fold_left
        (fun map elt -> Script_ir_translator.map_update elt (Some ()) map)
        empty_map
        ls
    in
    let elt = List.nth ls (Random.int map_size) in
    let stack = (elt, (map, ())) in
    create_benchmark rng_state "GET" stack_type stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Int_map_get_adversarial_bench)

module Int_map_update_adversarial_bench : Benchmark.S = struct
  include Default_config
  include Default_boilerplate

  let stack_type = int_opt_unit_int_unit_map_stack_ty

  let name = "INT_MAP_UPDATE_ADVERSARIAL"

  let info =
    "Benchmarking the GET instruction on (Z.t, unit) maps in an adversarial \
     setting"

  let models = models (Some N_Map_update)

  let make_bench rng_state (cfg : config) () =
    let map_size =
      Base_samplers.sample_in_interval rng_state ~range:cfg.map_size
    in
    let (_common_prefix, ls) =
      Base_samplers.Adversarial.integers
        rng_state
        ~range:cfg.int_size
        ~n:map_size
    in
    let ls = List.map Script_int_repr.of_zint ls in
    (* add elements into set *)
    let empty_map =
      Script_ir_translator.empty_map (Script_typed_ir.Int_key None)
    in
    let map =
      List.fold_left
        (fun map elt -> Script_ir_translator.map_update elt (Some ()) map)
        empty_map
        ls
    in
    let elt = List.nth ls (Random.int map_size) in
    let stack = (elt, (Some (), (map, ()))) in
    create_benchmark rng_state "UPDATE" stack_type stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Int_map_update_adversarial_bench)

module Int_map_size_bench =
( val make
        ~model:(Some N_Map_size)
        ~instr:"SIZE"
        ~stack_type:int_unit_map_stack_ty
        ~info:"Benchmarking the SIZE instruction on Z.t maps"
        ~name:"INT_MAP_SIZE"
        () )

module Int_map_iter_bench =
( val make
        ~model:(Some N_Map_iter)
        ~instr:"ITER { DROP }"
        ~stack_type:int_unit_map_stack_ty
        ~info:"Benchmarking the ITER instruction on Z.t maps"
        ~name:"INT_MAP_ITER"
        () )

module Int_map_map_bench =
( val make
        ~model:(Some N_Map_map)
        ~instr:"MAP { DUP ; DROP }"
        ~stack_type:int_unit_map_stack_ty
        ~info:"Benchmarking the MAP instruction on Z.t maps"
        ~name:"INT_MAP_MAP"
        () )

(* ------------------------------------------------------------------------- *)
(* PACK *)

(* TODO: use lib-snoop-michelson-sampling *)
module Pack_bench : Benchmark.S = struct
  include Default_config
  include Default_boilerplate

  let name = "PACK"

  let info =
    "Benchmarking the PACK instruction on random values of random types"

  let models = models (Some N_Pack)

  let make_bench rng_state (cfg : config) () =
    let module Samplers = Michelson_samplers.Make (struct
      include Default_samplers_parameters

      let parameters = cfg

      let state = rng_state
    end) in
    (* ? *)
    let typ = Samplers.Random_type.m_type ~max_depth:cfg.type_depth.min in
    match typ with
    | Script_ir_translator.Ex_ty ty ->
        let stack_type = Script_typed_ir.Item_t (ty, Empty_t, None) in
        let stack = Samplers.Random_value.stack stack_type in
        create_benchmark rng_state "PACK" stack_type stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Pack_bench)

(* ------------------------------------------------------------------------- *)
(* HASH *)

module Blake2b_bench =
( val make
        ~model:(Some N_Blake2b)
        ~instr:"BLAKE2B"
        ~stack_type:bytes_stack_ty
        ~info:"Benchmarking the BLAKE2B instruction"
        ~name:"BLAKE2B"
        () )

module Sha256_bench =
( val make
        ~model:(Some N_Sha256)
        ~instr:"SHA256"
        ~stack_type:bytes_stack_ty
        ~info:"Benchmarking the SHA256 instruction"
        ~name:"SHA256"
        () )

module Sha512_bench =
( val make
        ~model:(Some N_Sha512)
        ~instr:"SHA512"
        ~stack_type:bytes_stack_ty
        ~info:"Benchmarking the SHA512 instruction"
        ~name:"SHA512"
        () )

module Keccak_bench =
( val make
        ~model:(Some N_Keccak)
        ~instr:"KECCAK"
        ~stack_type:bytes_stack_ty
        ~info:"Benchmarking the KECCAK instruction"
        ~name:"KECCAK"
        () )

module Sha3_bench =
( val make
        ~model:(Some N_Sha3)
        ~instr:"SHA3"
        ~stack_type:bytes_stack_ty
        ~info:"Benchmarking the SHA3 instruction"
        ~name:"SHA3"
        () )

module Add_bls12_381_g1_bench =
( val make
        ~model:(Some N_Add_bls12_381_g1)
        ~instr:"ADD"
        ~stack_type:g1_g1_stack_ty
        ~info:"Benchmarking the ADD instruction on BLS12_381_G1 elements"
        ~name:"ADD_BLS12_381_G1"
        () )

module Add_bls12_381_g2_bench =
( val make
        ~model:(Some N_Add_bls12_381_g2)
        ~instr:"ADD"
        ~stack_type:g2_g2_stack_ty
        ~info:"Benchmarking the ADD instruction on BLS12_381_G2 elements"
        ~name:"ADD_BLS12_381_G2"
        () )

module Add_bls12_381_fr_bench =
( val make
        ~model:(Some N_Add_bls12_381_fr)
        ~instr:"ADD"
        ~stack_type:fr_fr_stack_ty
        ~info:"Benchmarking the ADD instruction on BLS12_381_FR elements"
        ~name:"ADD_BLS12_381_FR"
        () )

module Mul_bls12_381_g1_bench =
( val make
        ~model:(Some N_Mul_bls12_381_g1)
        ~instr:"MUL"
        ~stack_type:g1_fr_stack_ty
        ~info:"Benchmarking the MUL instruction on BLS12_381_G1 elements"
        ~name:"MUL_BLS12_381_G1"
        () )

module Mul_bls12_381_g2_bench =
( val make
        ~model:(Some N_Mul_bls12_381_g2)
        ~instr:"MUL"
        ~stack_type:g2_fr_stack_ty
        ~info:"Benchmarking the MUL instruction  on BLS12_381_G2 elements"
        ~name:"MUL_BLS12_381_G2"
        () )

module Mul_bls12_381_fr_bench =
( val make
        ~model:(Some N_Mul_bls12_381_fr)
        ~instr:"MUL"
        ~stack_type:fr_fr_stack_ty
        ~info:"Benchmarking the MUL instruction on BLS12_381_FR elements"
        ~name:"MUL_BLS12_381_FR"
        () )

module Neg_bls12_381_g1_bench =
( val make
        ~model:(Some N_Neg_bls12_381_g1)
        ~instr:"NEG"
        ~stack_type:g1_stack_ty
        ~info:"Benchmarking the NEG instruction on BLS12_381_G1 elements"
        ~name:"NEG_BLS12_381_G1"
        () )

module Neg_bls12_381_g2_bench =
( val make
        ~model:(Some N_Neg_bls12_381_g2)
        ~instr:"NEG"
        ~stack_type:g2_stack_ty
        ~info:"Benchmarking the NEG instruction on BLS12_381_G2 elements"
        ~name:"NEG_BLS12_381_G2"
        () )

module Neg_bls12_381_fr_bench =
( val make
        ~model:(Some N_Neg_bls12_381_fr)
        ~instr:"NEG"
        ~stack_type:fr_stack_ty
        ~info:"Benchmarking the NEG instruction on BLS12_381_FR elements"
        ~name:"NEG_BLS12_381_FR"
        () )

module Pairing_check_bls12_381_bench =
( val make
        ~model:(Some N_Pairing_check_bls12_381)
        ~instr:"PAIRING_CHECK"
        ~stack_type:pairing_check_stack_ty
        ~info:"Benchmarking the PAIRING_CHECK instruction"
        ~name:"PAIRING_CHECK"
        () )

module Comb_config = struct
  type config = {max_depth : int}

  let default_config = {max_depth = 500}

  let config_encoding =
    let open Data_encoding in
    conv
      (fun {max_depth} -> max_depth)
      (fun max_depth -> {max_depth})
      (obj1 (req "max_depth" int31))

  type stack_spec = {stack_depth : int; comb_depth : int}

  let sample_stack_spec rng_state cfg =
    let stack_depth =
      Base_samplers.sample_in_interval
        rng_state
        ~range:{Base_samplers.min = 2; max = cfg.max_depth}
    in
    let comb_depth =
      Base_samplers.sample_in_interval
        rng_state
        ~range:{Base_samplers.min = 2; max = stack_depth}
    in
    {stack_depth; comb_depth}
end

module Comb_bench : Benchmark.S = struct
  include Comb_config
  include Default_boilerplate

  let default_config = {max_depth = 495}

  let name = "COMB"

  let info = "Benchmarking the PAIR n instruction"

  let models = models (Some N_Comb)

  let make_benchmark rng_state (cfg : config) () =
    let {stack_depth; comb_depth} = sample_stack_spec rng_state cfg in
    let stack = make_stack stack_depth in
    match stack with
    | Ex_stack (stack_type, stack) ->
        let instr = Printf.sprintf "PAIR %d" comb_depth in
        create_benchmark rng_state instr stack_type stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_benchmark rng_state config)
end

let () = Registration_helpers.register (module Comb_bench)

let rec make_comb_stack (depth : int) =
  if depth = 0 then Ex_stack (Item_t (unit_ty, Empty_t, None), ((), ()))
  else
    let comb = make_comb_stack (depth - 1) in
    match comb with
    | Ex_stack (Item_t (comb_ty, Empty_t, None), (comb, ())) ->
        Ex_stack
          ( Item_t
              ( Pair_t ((unit_ty, None, None), (comb_ty, None, None), None),
                Empty_t,
                None ),
            (((), comb), ()) )
    | _ ->
        assert false

module Uncomb_bench : Benchmark.S = struct
  include Comb_config
  include Default_boilerplate

  let name = "UNCOMB"

  let info = "Benchmarking the UNPAIR n instruction"

  let models = models (Some N_Uncomb)

  let make_benchmark rng_state cfg () =
    let {stack_depth; comb_depth} = sample_stack_spec rng_state cfg in
    let stack = make_comb_stack stack_depth in
    match stack with
    | Ex_stack (stack_type, stack) ->
        create_benchmark
          rng_state
          (Printf.sprintf "UNPAIR %d" comb_depth)
          stack_type
          stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_benchmark rng_state config)
end

let () = Registration_helpers.register (module Uncomb_bench)

module Comb_get_bench : Benchmark.S = struct
  include Comb_config
  include Default_boilerplate

  let name = "COMB_GET"

  let info = "Benchmarking the (COMB_)GET n instruction"

  let models = models (Some N_Comb_get)

  let make_benchmark rng_state cfg () =
    let {stack_depth; comb_depth} = sample_stack_spec rng_state cfg in
    let stack = make_comb_stack stack_depth in
    match stack with
    | Ex_stack (stack_type, stack) ->
        create_benchmark
          rng_state
          (Printf.sprintf "GET %d" comb_depth)
          stack_type
          stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_benchmark rng_state config)
end

let () = Registration_helpers.register (module Comb_get_bench)

module Comb_set_bench : Benchmark.S = struct
  include Comb_config
  include Default_boilerplate

  let models = models (Some N_Comb_set)

  let name = "COMB_UPDATE"

  let info = "Benchmarking the (COMB_)UPDATE n instruction"

  let make_benchmark rng_state cfg () =
    let {stack_depth; comb_depth} = sample_stack_spec rng_state cfg in
    let stack = make_comb_stack stack_depth in
    match stack with
    | Ex_stack (stack_type, stack) ->
        create_benchmark
          rng_state
          (Printf.sprintf "UPDATE %d" comb_depth)
          (Item_t (unit_ty, stack_type, None))
          ((), stack)

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_benchmark rng_state config)
end

let () = Registration_helpers.register (module Comb_set_bench)

(* Heavily factorisable. *)
module Check_signature_ed25519_bench : Benchmark.S = struct
  include Default_config
  include Default_boilerplate

  let name = "CHECK_SIGNATURE_ED25519"

  let info =
    "Benchmarking CHECK_SIGNATURE instruction, ed25519 algorithm, valid case"

  let models = models (Some N_Check_signature_ed25519)

  let make_bench rng_state cfg () =
    let module Samplers = Michelson_samplers.Make (struct
      let size = Default_samplers_parameters.size

      let algo = `Algo Signature.Ed25519

      let parameters = cfg

      let state = rng_state
    end) in
    let (_pkh, pk, sk) = Samplers.Crypto_samplers.all () in
    let unsigned_message =
      Samplers.Random_value.value Script_typed_ir.(Bytes_t None)
    in
    let signed_message = Signature.sign sk unsigned_message in
    let stack = (pk, (signed_message, (unsigned_message, ()))) in
    create_benchmark rng_state "CHECK_SIGNATURE" check_signature_stack_ty stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Check_signature_ed25519_bench)

module Check_signature_secp256k1_bench : Benchmark.S = struct
  include Default_config
  include Default_boilerplate

  let name = "CHECK_SIGNATURE_SECP256K1"

  let info =
    "Benchmarking CHECK_SIGNATURE instruction, SECP256K1 algorithm, valid case"

  let models = models (Some N_Check_signature_secp256k1)

  let make_bench rng_state cfg () =
    let module Samplers = Michelson_samplers.Make (struct
      let size = Default_samplers_parameters.size

      let algo = `Algo Signature.Secp256k1

      let parameters = cfg

      let state = rng_state
    end) in
    let (_pkh, pk, sk) = Samplers.Crypto_samplers.all () in
    let unsigned_message =
      Samplers.Random_value.value Script_typed_ir.(Bytes_t None)
    in
    let signed_message = Signature.sign sk unsigned_message in
    let stack = (pk, (signed_message, (unsigned_message, ()))) in
    create_benchmark rng_state "CHECK_SIGNATURE" check_signature_stack_ty stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Check_signature_secp256k1_bench)

module Check_signature_p256_bench : Benchmark.S = struct
  include Default_config
  include Default_boilerplate

  let name = "CHECK_SIGNATURE_P256"

  let info =
    "Benchmarking CHECK_SIGNATURE instruction, P256 algorithm, valid case"

  let models = models (Some N_Check_signature_p256)

  let make_bench rng_state cfg () =
    let module Samplers = Michelson_samplers.Make (struct
      let size = Default_samplers_parameters.size

      let algo = `Algo Signature.P256

      let parameters = cfg

      let state = rng_state
    end) in
    let (_pkh, pk, sk) = Samplers.Crypto_samplers.all () in
    let unsigned_message =
      Samplers.Random_value.value Script_typed_ir.(Bytes_t None)
    in
    let signed_message = Signature.sign sk unsigned_message in
    let stack = (pk, (signed_message, (unsigned_message, ()))) in
    create_benchmark rng_state "CHECK_SIGNATURE" check_signature_stack_ty stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Check_signature_p256_bench)

(* Basic COMPARE (testing equality of equal elements) *)

let make_compare_bench ~name ~spec ~elt_ty =
  let module Bench : Benchmark.S = struct
    include Default_config
    include Default_boilerplate

    let stack_type =
      let open Script_typed_ir in
      Item_t (elt_ty, Item_t (elt_ty, Empty_t, None), None)

    let name = name

    let info =
      Printf.sprintf "Benchmarking the COMPARE instruction on %s." spec

    let models = models ~specialization:spec (Some N_Compare)

    let make_bench rng_state cfg () =
      let module Samplers = Michelson_samplers.Make (struct
        include Default_samplers_parameters

        let parameters = cfg

        let state = rng_state
      end) in
      let x = Samplers.Random_value.value elt_ty in
      let stack = (x, (x, ())) in
      create_benchmark rng_state "COMPARE" stack_type stack

    let create_benchmarks ~rng_state ~bench_num (config : config) =
      List.repeat bench_num (make_bench rng_state config)
  end in
  Registration_helpers.register (module Bench) ;
  (module Bench : Benchmark.S)

module Key_hash_compare_bench =
( val make_compare_bench
        ~name:"KEY_HASH_COMPARE"
        ~spec:"key_hash"
        ~elt_ty:Michelson_types.key_hash_ty )

module Address_compare_bench =
( val make_compare_bench
        ~name:"ADDRESS_COMPARE"
        ~spec:"address"
        ~elt_ty:Michelson_types.address_ty )

module Signature_compare_bench =
( val make_compare_bench
        ~name:"SIGNATURE_COMPARE"
        ~spec:"signature"
        ~elt_ty:Michelson_types.signature_ty )

module Key_compare_bench =
( val make_compare_bench
        ~name:"KEY_COMPARE"
        ~spec:"key"
        ~elt_ty:Michelson_types.public_key_ty )

module Unit_compare_bench =
( val make_compare_bench
        ~name:"UNIT_COMPARE"
        ~spec:"unit"
        ~elt_ty:Michelson_types.unit_ty )

module Chain_id_compare_bench =
( val make_compare_bench
        ~name:"CHAIN_ID_COMPARE"
        ~spec:"chain_id"
        ~elt_ty:Michelson_types.chain_id_ty )

module Bool_compare_bench =
( val make_compare_bench
        ~name:"BOOL_COMPARE"
        ~spec:"bool"
        ~elt_ty:Michelson_types.bool_ty )

module Timestamp_compare_bench =
( val make_compare_bench
        ~name:"TIMESTAMP_COMPARE"
        ~spec:"timestamp"
        ~elt_ty:Michelson_types.timestamp_ty )

module Mutez_compare_bench =
( val make_compare_bench
        ~name:"MUTEZ_COMPARE"
        ~spec:"mutez"
        ~elt_ty:Michelson_types.mutez_ty )

(* 'Adversarial' COMPARE *)

module String_compare_adversarial_bench : Benchmark.S = struct
  include Default_config
  include Default_boilerplate

  let stack_type = string_string_stack_ty

  let name = "STRING_COMPARE_ADVERSARIAL"

  let info =
    "Benchmarking the COMPARE instruction on strings, with adversarsial \
     sampling."

  let models = models ~specialization:"string" (Some N_Compare)

  let make_bench rng_state (cfg : config) () =
    let (_common_prefix, ls) =
      Base_samplers.Adversarial.strings rng_state ~range:cfg.string_size 2
    in
    match ls with
    | [z1; z2] ->
        let stack = (z1, (z2, ())) in
        create_benchmark rng_state "COMPARE" stack_type stack
    | _ ->
        assert false

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module String_compare_adversarial_bench)

module Int_compare_adversarial_bench : Benchmark.S = struct
  include Default_config
  include Default_boilerplate

  let stack_type = int_int_stack_ty

  let name = "INT_COMPARE_ADVERSARIAL"

  let info =
    "Benchmarking the COMPARE instruction on signed integers, with \
     adversarsial sampling."

  let models = models ~specialization:"int" (Some N_Compare)

  let make_bench rng_state (cfg : config) () =
    let (_common_prefix, ls) =
      Base_samplers.Adversarial.integers rng_state ~range:cfg.int_size ~n:2
    in
    match ls with
    | [z1; z2] ->
        let stack =
          (Script_int_repr.of_zint z1, (Script_int_repr.of_zint z2, ()))
        in
        create_benchmark rng_state "COMPARE" stack_type stack
    | _ ->
        assert false

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Int_compare_adversarial_bench)

(* ------------------------------------------------------------------------- *)
(* 'big loop' micro-bench *)

module Big_loop : Benchmark.S = struct
  type config = {iterations : int}

  let default_config = {iterations = 5000}

  let config_encoding =
    let open Data_encoding in
    conv
      (fun {iterations} -> iterations)
      (fun iterations -> {iterations})
      (obj1 (req "iterations" int31))

  include Default_boilerplate

  let models = models None

  let stack_type = int_stack_ty

  let program = "{ DUP ; NEQ ; LOOP { PUSH int 1 ; SWAP ; SUB ; DUP ; NEQ } }"

  let name = "BIG_LOOP"

  let info = "Benchmarking a big loop"

  let make_bench rng_state cfg () =
    let iter =
      Alpha_context.Script_int.of_int
        (Random.State.int rng_state cfg.iterations)
    in
    let stack = (iter, ()) in
    create_benchmark rng_state program stack_type stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Big_loop)

(* ------------------------------------------------------------------------- *)
(* Voting power *)

module Voting_power : Benchmark.S = struct
  type config = unit

  let default_config = ()

  let config_encoding = Data_encoding.unit

  include Default_boilerplate

  let stack_type = keyhash_stack_ty

  let program = "VOTING_POWER"

  let name = "VOTING_POWER"

  let info = "Benchmarking VOTING_POWER"

  let models = models (Some N_Voting_power)

  let make_bench rng_state _config () =
    let key_hash = Tezos_crypto.Signature.Public_key_hash.zero in
    let stack = (key_hash, ()) in
    create_benchmark rng_state program stack_type stack

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Voting_power)

module Total_voting_power : Benchmark.S = struct
  type config = unit

  let default_config = ()

  let config_encoding = Data_encoding.unit

  include Default_boilerplate

  let stack_type = empty_stack_ty

  let program = "TOTAL_VOTING_POWER"

  let name = "TOTAL_VOTING_POWER"

  let info = "Benchmarking TOTAL_VOTING_POWER"

  let models = models (Some N_Total_voting_power)

  let make_bench rng_state _config () =
    create_benchmark rng_state program stack_type ()

  let create_benchmarks ~rng_state ~bench_num (config : config) =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Total_voting_power)

(* ------------------------------------------------------------------------- *)
(* Tickets *)

module Ticket =
( val make
        ~model:(Some N_Ticket)
        ~instr:"TICKET"
        ~stack_type:Michelson_types.ticket_stack_ty
        ~info:"Benchmarking the TICKET instruction"
        ~name:"TICKET"
        () )

module Read_ticket =
( val make
        ~model:(Some N_Read_ticket)
        ~instr:"READ_TICKET"
        ~stack_type:Michelson_types.(read_ticket_stack_ty unit_cmp_ty)
        ~info:"Benchmarking the READ_TICKET instruction"
        ~name:"READ_TICKET"
        () )
