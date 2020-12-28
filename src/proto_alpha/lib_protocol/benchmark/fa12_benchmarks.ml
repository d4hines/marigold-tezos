open Tezos_raw_protocol_alpha
open Alpha_context
open Printf
open Util
open Pipeline

let set_up_fa12 :
    unit -> Block.t * (Contract.t * Contract.t * Contract.t) Pipeline.operation
    =
 fun () ->
  let (b, contracts) = Context.init 2 |> force_global_lwt in
  let alice = List.nth contracts 0 |> Option.get in
  let bob = List.nth contracts 1 |> Option.get in
  let initial_storage =
    sprintf
      {|Pair {Elt "%s" (Pair {} 100000000000000)} 100000000000000|}
      (contract_to_pkh alice)
  in
  let pipeline =
    Origination
      {
        originator = alice;
        amount = 100;
        contract = read_file "./contracts/fa1.2.tz";
        initial_storage;
      }
    >>| fun (_, fa12) -> (fa12, alice, bob)
  in
  (b, pipeline)

let approve_fa12 token approver spender amount =
  let parameters =
    sprintf {|
        Pair "%s" %d
    |} (contract_to_pkh spender) amount
  in
  Transfer
    {
      sender = approver;
      recipient = token;
      entrypoint = "approve";
      amount = 0;
      parameters;
    }

let approve_fa12_benchmark () =
  set_up_fa12 ()
  >>=! fun (_, (token, alice, bob)) ->
  let parameters =
    sprintf {|Left (Left (Left (Pair "%s" 100)))|} (contract_to_pkh bob)
  in
  Transfer
    {
      sender = alice;
      recipient = token;
      entrypoint = "approve";
      amount = 0;
      parameters;
    }

let transfer_benchmark : unit -> Pipeline.goal =
 fun () ->
  set_up_fa12 ()
  >>=! fun (_, (token, alice, bob)) ->
  let parameters =
    sprintf
      {|Right (Pair "%s" (Pair "%s" 10))|}
      (contract_to_pkh alice)
      (contract_to_pkh bob)
  in
  Transfer
    {
      sender = alice;
      recipient = token;
      entrypoint = "transfer";
      amount = 0;
      parameters;
    }
