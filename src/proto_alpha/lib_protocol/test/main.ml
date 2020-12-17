[@@@warning "-21"]

[@@@warning "-20"]

[@@@warning "-26"]

open Tezos_raw_protocol_alpha
open Alpha_context
open Script
open Printf
open Show

let () = Printexc.record_backtrace true

let ( >>=?? ) x k = x >>= fun x -> Lwt.return (Environment.wrap_error x) >>=? k

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

let type_script context script =
  Script_ir_translator.parse_script
    context
    ~legacy:false
    ~allow_forged_in_storage:false
    script

let micheline_canonical_to_string c =
  Fmt.str
    "%a"
    Micheline_printer.print_expr
    (Micheline_printer.printable Michelson_v1_primitives.string_of_prim c)

let register_two_contracts () =
  Context.init ~initial_balances:[1_000_000_000_000L; 1_000_000_000_000L] 2
  >|=? fun (b, contracts) ->
  let contract_1 = List.nth contracts 0 in
  let contract_2 = List.nth contracts 1 in
  (b, contract_1, contract_2)

let make_contract ~script ~originator ~amount b =
  let amount = Tez.of_mutez amount |> Option.get in
  Incremental.begin_construction b
  >>=? fun b ->
  Op.origination (I b) originator ~script ~credit:amount
  >>=? fun (op, originated_contract) ->
  Incremental.add_operation b op
  >>=? fun b ->
  Incremental.finalize_block b >>=? fun b -> return (b, originated_contract)

let get_pkh_from_contract x = Contract.is_implicit x |> Option.get

let logid label x =
  print_endline (label ^ ": " ^ x) ;
  x

let get_next_context b =
  Incremental.begin_construction b
  >>=? fun b -> return (Incremental.alpha_ctxt b)

let fa12_transfer ~token ~from ~to_ ~amount b =
  let from_address = Contract.is_implicit from |> Option.get in
  let to_address = Contract.is_implicit to_ |> Option.get in
  let parameters =
    Printf.sprintf
      {|Pair "%s" (Pair "%s" %Ld)|}
      (Signature.Public_key_hash.to_b58check from_address)
      (Signature.Public_key_hash.to_b58check to_address)
      amount
    |> Expr.from_string |> lazy_expr
  in
  Incremental.begin_construction b
  >>=? fun b ->
  Op.transaction
    (I b)
    ~entrypoint:"transfer"
    ~parameters
    ~fee:Tez.zero
    from
    token
    Tez.zero
  >>=? fun op ->
  Incremental.add_operation b op >>=? fun b -> Incremental.finalize_block b

let make_run_fa12_transfer script context token alice bob =
  let module No_trace : Script_interpreter.STEP_LOGGER = struct
    let log_interp _ctxt _descr _stack = ()

    let log_entry _ctxt _descr _stack = ()

    let log_exit _ctxt _descr _stack = ()

    let get_log () = return_none
  end in
  let step_constants : Script_interpreter.step_constants =
    {
      source = alice;
      payer = alice;
      self = token;
      amount = Tez.zero;
      chain_id = Chain_id.zero;
    }
  in
  type_script context script
  >>=?? fun ( Script_ir_translator.Ex_script
                {code; arg_type; storage; storage_type; root_name = _},
              context ) ->
  let parameters =
    Printf.sprintf
      {|Left (Right (Pair "%s" (Pair "%s" %Ld)))|}
      (Signature.Public_key_hash.to_b58check (get_pkh_from_contract alice))
      (Signature.Public_key_hash.to_b58check (get_pkh_from_contract bob))
      1_000L
    |> Expr.from_string
  in
  let (Lam (code, _)) = code in
  Script_ir_translator.parse_data
    context
    ~legacy:false
    ~allow_forged:false
    arg_type
    (Micheline.root parameters)
  >>=?? fun (arg, context) ->
  let logger = (module No_trace : Script_interpreter.STEP_LOGGER) in
  let input = ((arg, storage), ()) in
  let run_script () =
    Script_interpreter.step logger context step_constants code input
  in
  let eval_script () =
    run_script ()
    >>=? fun (((_, storage), ()), ctx) ->
    Script_ir_translator.unparse_data ctx Readable storage_type storage
  in
  let run_script () = run_script() |> Lwt.map (fun _ -> ()) in
  return (run_script, eval_script)

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ; s

let fa12_contract =
  sprintf "{%s}" (read_file "/workspaces/repos/fa1.2/morley/fa1.2.tz")
  |> Expr.from_string |> lazy_expr

(* let dexter_contract =
  sprintf "{%s}" (read_file "/workspaces/repos/joseph/dexter/ligo/dexter.tz")
  |> Expr.from_string |> lazy_expr *)

(* (pair
  (big_map %accounts address
    (pair (nat :balance)
          (map :approvals address
                          nat)))
  (nat %fields))

Pair
  {Elt "tz1KtBg8nLf3bizWRjaCGLtz46Ls5vyXwTFa" (Pair 100000000000000 {})}
  100000000000000 *)
let make_initial_storage address =
  let address = Signature.Public_key_hash.to_b58check address in
  sprintf {|Pair {Elt "%s" (Pair 100000000000000 {})} 100000000000000|} address
  |> Expr.from_string |> lazy_expr

let setup_fa12_token () =
  (* Register two contracts, bob and alie, each with 
  a million tez  *)
  register_two_contracts ()
  >>=? fun (b, alice, bob) ->
  (* Initially, our storage is going to have a ton of of craft tokens,
  all in Alice's custody *)
  let alice_address =
    Signature.Public_key_hash.to_b58check (get_pkh_from_contract alice)
  in
  let storage = make_initial_storage @@ get_pkh_from_contract alice in
  let script = Script.{code = fa12_contract; storage} in
  (* Originate the contract with 1000 tez in its account *)
  make_contract ~script ~originator:alice ~amount:500L b
  (* this block has the contract ready to be called *)
  >>=? fun (b, token) ->
  get_next_context b >>=? fun ctx -> return (script, ctx, token, alice, bob)

let (script, context, token, alice, bob) =
  setup_fa12_token () |> force_global_lwt

let (run_script, eval_script) =
  make_run_fa12_transfer script context token alice bob |> force_global_lwt

let print_alice_balance () =
  let alice_address =
    Signature.Public_key_hash.to_b58check (get_pkh_from_contract alice)
  in
  let (_, storage_opt) =
    Alpha_context.Contract.get_storage context token |> force_lwt
  in
  let storage = Option.get storage_opt in
  Show.show_canonical Show.pp_prim storage |> print_endline ;
  print_endline @@ micheline_canonical_to_string storage ;
  let big_map_id =
    match storage |> Obj.magic with
    | Canonical (Prim (0, D_Pair, [Int (1, x); _], [])) ->
        x
    | _ ->
        assert false
  in
  let big_map_id : Big_map.Id.t = Obj.magic big_map_id in
  let comparable_ty = Script_typed_ir.Address_key None in
  let z = Expr.from_string (sprintf {|"%s"|} alice_address) in
  Script_ir_translator.parse_comparable_data
    context
    comparable_ty
    (Micheline.root z)
  >>=?? fun (address, context) ->
  Script_ir_translator.hash_comparable_data context comparable_ty address
  >>=?? fun (hash, context) ->
  Big_map.get_opt context big_map_id hash
  >>=?? fun (_ctx, foo) ->
  (* let (left, right) = foo |> Option.get in *)
  print_endline "Hello" ;
  print_endline @@ micheline_canonical_to_string (Option.get foo) ;
  Printf.printf "exists: %b\n%!" (foo |> Option.is_some) ;
  return ()

let _ =
  Lwt_main.run
    ( eval_script ()
    >>=? fun (storage, _context) ->
    print_endline "======== Storage after transfer =========" ;
    Micheline.strip_locations storage
    |> micheline_canonical_to_string |> print_endline ;
    return () )

let () =
  let open Core in
  let open Core_bench in
  Command.run
    (Bench.make_command
        [ Bench.Test.create ~name:"run_script" (fun () ->
              run_script () |> Lwt_main.run) ])
