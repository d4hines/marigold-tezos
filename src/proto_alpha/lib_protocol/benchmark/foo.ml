[@@@warning "-21"]

[@@@warning "-20"]

[@@@warning "-27"]

[@@@warning "-26"]

open Tezos_raw_protocol_alpha
open Alpha_context
open Script
open Printf
open Show

let make_benchmark ~source ~payer ~self script parameters context =
  let step_constants : Script_interpreter.step_constants =
    {source; payer; self; amount = Tez.zero; chain_id = Chain_id.zero}
  in
  type_script context script
  >>=?? fun ( Script_ir_translator.Ex_script
                {code; arg_type; storage; storage_type; root_name = _},
              context ) ->
  let (Lam (code, _)) = code in
  Script_ir_translator.parse_data
    context
    ~legacy:false
    ~allow_forged:false
    arg_type
    (Micheline.root parameters)
  >>=?? fun (arg, context) ->
  let input = (arg, storage) in
  let run_script () =
    Script_interpreter.step None context step_constants code input ((), ())
  in
  let eval_script () =
    run_script ()
    >>=? fun ((ops, storage), _, ctx) ->
    Script_ir_translator.unparse_data ctx Readable storage_type storage
  in
  let run_script () = run_script () |> Lwt.map (fun _ -> ()) in
  return (run_script, eval_script)

module Dexter = struct
  let contract =
    sprintf
      "{%s}"
      (read_file "/workspaces/repos/joseph/dexter/ligo/contracts/dexter.tz")
    |> Expr.from_string |> lazy_expr

  (* tezos-client transfer 1000 
    from alice \
    to tezosGoldExchange \
    --entrypoint 'addLiquidity' \
    --arg 'Pair (Pair "tz1MQehPikysuVYN5hTiKTnrsFidAww7rv3z" 1) (Pair 100 "2030-01-01T12:00:00Z")' \
    --burn-cap 1 *)
  let add_liquidity _b _liquidity_provider = assert false

  let make_initial_storage ~owner ~token =
    let owner_address =
      Signature.Public_key_hash.to_b58check (get_pkh_from_contract owner)
    in
    let token_address =
      Contract_hash.to_b58check (get_hash_from_contract token)
    in
    sprintf
      {|Pair {} (Pair (Pair False (Pair False 0)) (Pair (Pair "%s" "%s") (Pair 0 0)))|}
      owner_address
      token_address
    |> Expr.from_string |> lazy_expr

  let setup_exchange ~owner ~token ~amount b =
    let storage = make_initial_storage ~owner ~token in
    let script = Script.{code = contract; storage} in
    (* Originate the contract with 1000 tez in its account *)
    make_contract ~script ~originator:owner ~amount b
    (* this block has the contract ready to be called *)
    >>=? fun (b, exchange) ->
    get_next_context b >>=? fun context -> return (script, context, exchange)
end

(* (pair
  (big_map %accounts address
    (pair (nat :balance)
          (map :approvals address
                          nat)))
  (nat %fields))

Pair
  {Elt "tz1KtBg8nLf3bizWRjaCGLtz46Ls5vyXwTFa" (Pair 100000000000000 {})}
  100000000000000 *)

(* tezos-client transfer 5 \
             from simon \
             to tezosGoldExchange \
             --entrypoint 'xtzToToken' \
             --arg 'Pair "tz1bqV1wz5mJmBRSDkYiRZxX7yBDLKV7HKo3" (Pair 1 "2030-01-29T18:00:00Z")' \
             --burn-cap 1 *)

let (script, context, token, alice, bob) =
  Fa12.setup_token () |> force_global_lwt

let (run_script, eval_script) =
  Fa12.make_run_transfer script context token alice bob |> force_global_lwt

let run_script_batch () = List.init 100 (fun _ -> run_script ()) |> Lwt.all
