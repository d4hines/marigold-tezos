open Util
open Pipeline

let fact_benchmark : unit -> Pipeline.goal =
 fun () ->
  let (b, op) = Fa12_benchmarks.set_up_fa12 () in
  let (b, x) =
    ( b,
      op
      >>= fun (_, (_, alice, _)) ->
      (* Originate the Fact Contract *)
      let fact_contract = read_file "./contracts/fact.tz" in
      let initial_storage = "0" in
      Origination
        {
          originator = alice;
          amount = 0;
          contract = fact_contract;
          initial_storage;
        }
      >>| fun (b, fact) -> (b, alice, fact) )
  in
  (b, x)
  >>=! fun (_, (_, alice, fact)) ->
  let parameters = "100" in
  Transfer
    {
      sender = alice;
      recipient = fact;
      entrypoint = "default";
      amount = 1_000_000;
      parameters;
    }
