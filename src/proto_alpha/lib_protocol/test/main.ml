[@@@warning "-21"]

open Tezos_raw_protocol_alpha
open Alpha_context
open Script
open Printf

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
  |> Expr.from_string |> lazy_expr

let get_pkh_from_contract x = Contract.is_implicit x |> Option.get

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
  print_endline "here2" ;
  (* Call the contract *)
  fa12_transfer ~token:craft_token_contract ~from:alice ~to_:bob ~amount:400L b
  >>=? (fun b ->
         let open Environment_context in
         let (Context ctx) = b.context in
         ( match ctx.kind with
         | Memory_context.Memory ->
             ()
         | Shell_context.Shell ->
             () ) ;
         Environment_context.Context.get b.context [alice_address]
         >|= fun b -> Ok (b |> Option.get))
  >>=? fun value ->
  print_endline "something" ;
  Bytes.to_string value |> print_endline ;
  return ()

let _ =
  match main () |> Lwt_main.run with
  | Ok _ ->
      ()
  | Error err ->
      Format.printf "%a" Error_monad.pp_print_error err
