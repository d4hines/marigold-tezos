open Tezos_raw_protocol_alpha
open Printf
open Util
open Pipeline

[@@@warning "-21"]

let xtzToToken_benchmark : unit -> Pipeline.goal =
 fun () ->
  let (b, op) = Fa12_benchmarks.set_up_fa12 () in
  ( b,
    op >>= fun (_b2, (token, alice, bob)) ->
    (* Util.alice := alice ;
       ( Obj.magic Script_interpreter.print_balance
         : (string -> Alpha_context.t -> unit) ref )
       := print_alice_token_balance ; *)

    (* print_token_balance_from_block "1 dexter" b2 0 alice ; *)
    (* Originate the Dexter Contract *)
    let dexter_contract = read_file "./contracts/dexter.tz" in
    let initial_storage =
      sprintf
        {|Pair {}
            (Pair
             (Pair (Pair False 0)
                   (Pair "%s" False))
             (Pair (Pair "%s" 0) 0))
          |}
        (contract_to_pkh alice)
        (contract_to_pkh token)
    in
    Origination
      {
        originator = alice;
        amount = 0;
        contract = dexter_contract;
        initial_storage;
      }
    (* Approve the spend *)
    >>= fun (_b2, dexter) ->
    (* print_token_balance_from_block "2 dexter" b2 0 alice ; *)
    Fa12_benchmarks.approve_fa12 token alice dexter 1_000_000
    (* Add Liquidity *) >>= fun (_b2, _) ->
    (* print_token_balance_from_block "3 dexter" b2 0 alice ; *)
    Obj.magic Script_interpreter.entrypoint_running := "add liquidity" ;
    let parameters =
      sprintf
        {|
                Pair
                  (Pair "%s" 1)
                  (Pair 100000 "2030-01-01T12:00:00Z")
            |}
        (contract_to_pkh alice)
    in
    Transfer
      {
        sender = alice;
        recipient = dexter;
        entrypoint = "addLiquidity";
        amount = 5_000_000;
        parameters;
      }
    >>| fun (b, _) -> (b, bob, dexter) )
  >>=! fun (_, (_, bob, dexter)) ->
  Obj.magic Script_interpreter.entrypoint_running := "xtzToToken" ;
  (* Do xtzToToken, exchanging 1 of bob's tez for token,
     sending the results to bob *)
  let parameters =
    sprintf
      {|
    Right
      (Right
        (Pair
          "%s"
          (Pair 1 "2050-01-29T18:00:00Z")))|}
      (contract_to_pkh bob)
  in
  Transfer
    {
      sender = bob;
      recipient = dexter;
      entrypoint = "xtzToToken";
      amount = 1_000_000;
      parameters;
    }
