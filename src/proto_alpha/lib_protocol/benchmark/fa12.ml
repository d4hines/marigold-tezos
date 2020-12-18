[@@@warning "-21"]

[@@@warning "-20"]

[@@@warning "-27"]

[@@@warning "-26"]

open Tezos_raw_protocol_alpha
open Alpha_context
open Script
open Printf
open Show
open Util
open Block_setup

let contract =
  sprintf "{%s}" (read_file "/workspaces/repos/fa1.2/morley/fa1.2.tz")
  |> Expr.from_string |> lazy_expr

let make_initial_storage address =
  let address = Signature.Public_key_hash.to_b58check address in
  sprintf {|Pair {Elt "%s" (Pair 100000000000000 {})} 100000000000000|} address
  |> Expr.from_string |> lazy_expr

let setup_token () =
  (* Register two contracts, bob and alie, each with 
      a million tez  *)
  Context.init 2
  >>=? fun (b, contracts) ->
  let alice = List.nth contracts 0 in
  let bob = List.nth contracts 1 in
  (* Initially, our storage is going to have a ton of of craft tokens,
      all in Alice's custody *)
  let alice_address =
    Signature.Public_key_hash.to_b58check (get_pkh_from_contract alice)
  in
  let storage = make_initial_storage @@ get_pkh_from_contract alice in
  let script = Script.{code = contract; storage} in
  (* Originate the contract with 1000 tez in its account *)
  Block_setup.make_contract ~script ~originator:alice ~amount:500 b
  (* this block has the contract ready to be called *)
  >>=? fun (b, token) ->
  get_next_context b >>=? fun ctx -> return (script, ctx, token, alice, bob)

let make_run_transfer script context token alice bob =
  let parameters =
    Printf.sprintf
      {|Left (Right (Pair "%s" (Pair "%s" %Ld)))|}
      (Signature.Public_key_hash.to_b58check (get_pkh_from_contract alice))
      (Signature.Public_key_hash.to_b58check (get_pkh_from_contract bob))
      1_000L
    |> Expr.from_string
  in
  make_benchmark
    ~source:alice
    ~payer:alice
    ~self:token
    script
    parameters
    context

(* tezos-client transfer 0 \ 
    from alice \
    to tezosGold 
    --entrypoint 'approve' \
    --arg 'Pair "KT1VbT8n6YbrzPSjdAscKfJGDDNafB5yHn1H" 200' \
    --burn-cap 1 *)
let approve_spend ~owner ~token ~approved ~amount b =
  let approved_spender_str =
    Signature.Public_key_hash.to_b58check (get_pkh_from_contract approved)
  in
  let parameters = sprintf {|Pair "%s" %d|} approved_spender_str amount in
  do_transaction b owner token amount "transfer" parameters
