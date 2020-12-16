[@@@warning "-21"]
[@@@warning "-26"]

open Tezos_raw_protocol_alpha
open Alpha_context
open Script
open Printf

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

let logid label x = print_endline (label ^ ": " ^ x); x

let fa12_transfer ~token ~from ~to_ ~amount b =
  let from_address = Contract.is_implicit from |> Option.get in
  let to_address = Contract.is_implicit to_ |> Option.get in
  let parameters =
    Printf.sprintf
      {|Pair "%s" (Pair "%s" %Ld)|}
      (Signature.Public_key_hash.to_b58check from_address)
      (Signature.Public_key_hash.to_b58check to_address)
      amount
    |> logid "params" |> Expr.from_string |> lazy_expr
  in
  let b = Incremental.begin_construction b |> force_global_lwt in
  (* let op =
    Op.transaction
      (I b)
      ~entrypoint:"transfer"
      ~parameters
      ~fee:Tez.zero
      from
      token
      Tez.zero
    |> force_global_lwt
  in
  let b = Incremental.add_operation b op |> force_global_lwt in *)
  (* let b = Incremental.finalize_block b |> force_global_lwt in *)
  let context = Incremental.alpha_ctxt b in
  let (_, storage_opt) = Alpha_context.Contract.get_storage context token |> force_lwt in
  let storage = Option.get storage_opt in
  storage
  

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ; s

let fa12_contract =
  sprintf "{%s}" (read_file "/workspaces/repos/fa1.2/morley/fa1.2.tz")
  |> Expr.from_string |> lazy_expr

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
  |> logid "initial storage" |> Expr.from_string |> lazy_expr


let main () =
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
  >>=? fun (b, craft_token_contract) ->
  (* Call the contract *)
  let x = fa12_transfer ~token:craft_token_contract ~from:alice ~to_:bob ~amount:400L b in
  print_endline "amounts";
  print_endline @@ micheline_canonical_to_string x ;
  return ()

let _ =
  match main () |> Lwt_main.run with
  | Ok _ ->
      ()
  | Error err ->
      Format.printf "%a" Error_monad.pp_print_error err
