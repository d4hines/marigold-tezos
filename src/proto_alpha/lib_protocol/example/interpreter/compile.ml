open Stdlib

let filename = "Tuturu.ml"

let modname = "Tuturu"

let env = Compmisc.init_path () ; Compmisc.initial_env ()

let () = Clflags.native_code := true

let () = Clflags.opaque := false

let () = Compilenv.reset modname

let load_paths_from_cmt cmt =
  cmt.Cmt_format.cmt_loadpath
  |> List.map (fun fpath ->
         match Filename.is_relative fpath with
         | true ->
             Filename.concat cmt.Cmt_format.cmt_builddir fpath
         | false ->
             fpath)
  |> List.rev
  |> List.iter Load_path.add_dir

let interpreter_cmt =
  "/home/eduardo/tezos/_build/default/src/proto_alpha/lib_protocol/.tezos_raw_protocol_alpha.objs/byte/tezos_raw_protocol_alpha__Script_interpreter_functor.cmt"

let x =
  Load_path.add_dir
    "/home/eduardo/tezos/_build/default/src/proto_alpha/lib_protocol/.tezos_raw_protocol_alpha.objs/native"

let cmt = Cmt_format.read_cmt interpreter_cmt

let () = load_paths_from_cmt cmt

let simplf_lambda lambda =
  let open Lambda in
  {lambda with code = Simplif.simplify_lambda lambda.code}

let lambda_to_clambda lambda ~ppf_dump =
  let module Backend = struct
    let symbol_for_global' = Compilenv.symbol_for_global'

    let closure_symbol = Compilenv.closure_symbol

    let really_import_approx = Import_approx.really_import_approx

    let import_symbol = Import_approx.import_symbol

    let size_int = Arch.size_int

    let big_endian = Arch.big_endian

    let max_sensible_number_of_arguments = Proc.max_arguments_for_tailcalls - 1
  end in
  let backend : (module Backend_intf.S) = (module Backend) in
  let open Lambda in
  let program =
    Flambda_middle_end.middle_end
      ~ppf_dump
      ~backend
      ~filename
      ~prefixname:".cmx"
      ~module_ident:lambda.module_ident
      ~module_initializer:lambda.code
      ~size:lambda.main_module_block_size
  in
  let export = Build_export_info.build_transient ~backend program in
  let { Flambda_to_clambda.expr;
        preallocated_blocks;
        structured_constants;
        exported } =
    Flambda_to_clambda.convert (program, export)
  in
  let constants =
    List.map
      (fun (symbol, definition) ->
        {
          Clambda.symbol = Linkage_name.to_string (Symbol.label symbol);
          exported = true;
          definition;
          provenance = None;
        })
      (Symbol.Map.bindings structured_constants)
  in
  let clambda = Un_anf.apply ~ppf_dump expr ~what:"init_code" in
  let () = Compilenv.set_export_info exported in
  (clambda, preallocated_blocks, constants)

let compile_to_cmm code ~ppf_dump =
  code
  |> Typemod.type_implementation filename ".cmo" modname env
  |> Translmod.transl_implementation_flambda modname
  |> simplf_lambda
  |> lambda_to_clambda ~ppf_dump
  |> Cmmgen.compunit ~ppf_dump

let compile_to_cmm code ~ppf_dump =
  try compile_to_cmm code ~ppf_dump
  with Typetexp.Error (_, env, err) as exn ->
    Format.asprintf "%a\n%!" (Typetexp.report_error env) err |> print_endline ;
    raise exn

let find_symbol_name_in_cmm name cmm =
  let name = "camlTuturu__" ^ name in
  let name_length = String.length name in
  cmm
  |> List.find (function
         | Cmm.Cfunction fundecl ->
             String.length fundecl.Cmm.fun_name >= name_length
             && String.sub fundecl.Cmm.fun_name 0 name_length = name
         | _ ->
             false)
  |> function
  | Cmm.Cfunction fundecl -> fundecl.Cmm.fun_name | _ -> assert false

let load_library () =
  Dl.dlopen
    ~filename:(Unix.getcwd () ^ "/tuturu.so")
    ~flags:[RTLD_LOCAL; RTLD_LAZY]

external register_module :
  frametable:nativeint ->
  gc_roots:nativeint ->
  data:nativeint * nativeint ->
  code:nativeint * nativeint ->
  unit = "interpreter_register_module"

let register_module library =
  let frametable = Dl.dlsym ~handle:library ~symbol:"camlTuturu__frametable" in
  let gc_roots = Dl.dlsym ~handle:library ~symbol:"camlTuturu__gc_roots" in
  let data_begin = Dl.dlsym ~handle:library ~symbol:"camlTuturu__data_begin" in
  let data_end = Dl.dlsym ~handle:library ~symbol:"camlTuturu__data_end" in
  let code_begin = Dl.dlsym ~handle:library ~symbol:"camlTuturu__code_begin" in
  let code_end = Dl.dlsym ~handle:library ~symbol:"camlTuturu__code_end" in
  register_module
    ~frametable
    ~gc_roots
    ~data:(data_begin, data_end)
    ~code:(code_begin, code_end)

let cmm_to_asm ~file cmm =
  let oc = open_out file in
  Emitaux.output_channel := oc ;
  Emit.begin_assembly () ;
  List.iter (Asmgen.compile_phrase ~ppf_dump:Format.std_formatter) cmm ;
  Asmgen.compile_phrase
    ~ppf_dump:Format.std_formatter
    (Cmmgen.reference_symbols
       (List.filter
          (fun s -> s <> "" && s.[0] <> '%')
          (List.map Primitive.native_name !Translmod.primitive_declarations))) ;
  Emit.end_assembly () ;
  close_out oc

let load library symbol =
  let symbol : int =
    Dl.dlsym ~handle:library ~symbol
    |> Obj.repr
    |> (fun x -> Obj.field x 1)
    |> Obj.obj
  in
  symbol

(* TODO: callback when falling back to interpreter *)

let compile_from_cmm names cmm =
  let () = cmm |> cmm_to_asm ~file:"tuturu.s" in
  let _ = Unix.system "as tuturu.s -o tuturu.o" in
  let _ = Unix.system "ld -shared tuturu.o -o tuturu.so" in
  let library = load_library () in
  let () = register_module library in
  names |> List.map (load library)
