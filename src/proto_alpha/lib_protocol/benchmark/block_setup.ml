open Tezos_raw_protocol_alpha
open Alpha_context
open Script
open Util

let register_two_contracts () =
  Context.init ~initial_balances:[1_000_000_000_000L; 1_000_000_000_000L] 2
  >|=? fun (b, contracts) ->
  let contract_1 = List.nth contracts 0 in
  let contract_2 = List.nth contracts 1 in
  (b, contract_1, contract_2)

let get_pkh_from_contract x = Contract.is_implicit x |> Option.get

let get_hash_from_contract x = Contract.is_originated x |> Option.get

let make_contract ~script ~originator ~amount b =
  let amount = Util.of_mutez amount in
  Incremental.begin_construction b
  >>=? fun b ->
  Op.origination (I b) originator ~script ~credit:amount
  >>=? fun (op, originated_contract) ->
  Incremental.add_operation b op
  >>=? fun b ->
  Incremental.finalize_block b >>=? fun b -> return (b, originated_contract)


let get_next_context b =
  Incremental.begin_construction b
  >>=? fun b -> return (Incremental.alpha_ctxt b)


let do_transaction block sender recipient (amount : Int.t) entrypoint args =
  let parameters = args |> Expr.from_string |> lazy_expr in
  Incremental.begin_construction block
  >>=? fun b ->
  Op.transaction
    (I b)
    ~entrypoint
    ~parameters
    ~fee:Tez.zero
    sender
    recipient
    (of_mutez amount)
  >>=? fun op ->
  Incremental.add_operation b op >>=? fun b -> Incremental.finalize_block b

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

let transfer ~token ~from ~to_ ~amount b =
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
