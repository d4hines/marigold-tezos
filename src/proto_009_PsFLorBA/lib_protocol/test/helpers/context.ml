(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Alpha_context

type t = B of Block.t | I of Incremental.t

let branch = function B b -> b.hash | I i -> (Incremental.predecessor i).hash

let level = function B b -> b.header.shell.level | I i -> Incremental.level i

let get_level ctxt =
  level ctxt |> Raw_level.of_int32 |> Environment.wrap_tzresult

let rpc_ctxt =
  object
    method call_proto_service0
        : 'm 'q 'i 'o.
          ( ([< RPC_service.meth] as 'm),
            Environment.RPC_context.t,
            Environment.RPC_context.t,
            'q,
            'i,
            'o )
          RPC_service.t -> t -> 'q -> 'i -> 'o tzresult Lwt.t =
      fun s pr q i ->
        match pr with
        | B b ->
            Block.rpc_ctxt#call_proto_service0 s b q i
        | I b ->
            Incremental.rpc_ctxt#call_proto_service0 s b q i

    method call_proto_service1
        : 'm 'a 'q 'i 'o.
          ( ([< RPC_service.meth] as 'm),
            Environment.RPC_context.t,
            Environment.RPC_context.t * 'a,
            'q,
            'i,
            'o )
          RPC_service.t -> t -> 'a -> 'q -> 'i -> 'o tzresult Lwt.t =
      fun s pr a q i ->
        match pr with
        | B bl ->
            Block.rpc_ctxt#call_proto_service1 s bl a q i
        | I bl ->
            Incremental.rpc_ctxt#call_proto_service1 s bl a q i

    method call_proto_service2
        : 'm 'a 'b 'q 'i 'o.
          ( ([< RPC_service.meth] as 'm),
            Environment.RPC_context.t,
            (Environment.RPC_context.t * 'a) * 'b,
            'q,
            'i,
            'o )
          RPC_service.t -> t -> 'a -> 'b -> 'q -> 'i -> 'o tzresult Lwt.t =
      fun s pr a b q i ->
        match pr with
        | B bl ->
            Block.rpc_ctxt#call_proto_service2 s bl a b q i
        | I bl ->
            Incremental.rpc_ctxt#call_proto_service2 s bl a b q i

    method call_proto_service3
        : 'm 'a 'b 'c 'q 'i 'o.
          ( ([< RPC_service.meth] as 'm),
            Environment.RPC_context.t,
            ((Environment.RPC_context.t * 'a) * 'b) * 'c,
            'q,
            'i,
            'o )
          RPC_service.t -> t -> 'a -> 'b -> 'c -> 'q -> 'i -> 'o tzresult Lwt.t
        =
      fun s pr a b c q i ->
        match pr with
        | B bl ->
            Block.rpc_ctxt#call_proto_service3 s bl a b c q i
        | I bl ->
            Incremental.rpc_ctxt#call_proto_service3 s bl a b c q i
  end

let get_endorsers ctxt =
  Alpha_services.Baker.Endorsing_rights.get rpc_ctxt ctxt

let get_endorser ctxt =
  Alpha_services.Baker.Endorsing_rights.get rpc_ctxt ctxt
  >|=? fun endorsers ->
  let endorser = WithExceptions.Option.get ~loc:__LOC__ @@ List.hd endorsers in
  (endorser.baker, endorser.slots)

let get_voting_power = Alpha_services.Baker.voting_power rpc_ctxt

let get_total_voting_power = Alpha_services.Voting.total_voting_power rpc_ctxt

let get_bakers ctxt =
  Alpha_services.Baker.Baking_rights.get ~max_priority:256 rpc_ctxt ctxt
  >|=? fun bakers ->
  List.map (fun p -> p.Alpha_services.Baker.Baking_rights.baker) bakers

let get_seed_nonce_hash ctxt =
  let header =
    match ctxt with B {header; _} -> header | I i -> Incremental.header i
  in
  match header.protocol_data.contents.seed_nonce_hash with
  | None ->
      failwith "No committed nonce"
  | Some hash ->
      return hash

let get_seed ctxt = Alpha_services.Seed.get rpc_ctxt ctxt

let get_constants ctxt = Alpha_services.Constants.all rpc_ctxt ctxt

let get_minimal_valid_time ctxt ~priority ~endorsing_power =
  Alpha_services.Baker.Minimal_valid_time.get
    rpc_ctxt
    ctxt
    priority
    endorsing_power

let rec reward_for_priority reward_per_prio prio =
  match reward_per_prio with
  | [] ->
      (* Empty reward list in parameters means no rewards *)
      Tez.zero
  | [last] ->
      last
  | first :: rest ->
      if Compare.Int.(prio <= 0) then first
      else reward_for_priority rest (pred prio)

let get_baking_reward ctxt ~priority ~endorsing_power =
  get_constants ctxt
  >>=? fun {Constants.parametric = {baking_reward_per_endorsement; _}; _} ->
  let reward_per_endorsement =
    reward_for_priority baking_reward_per_endorsement priority
  in
  Lwt.return
    (Environment.wrap_tzresult
       Tez.(reward_per_endorsement *? Int64.of_int endorsing_power))

let get_endorsing_reward ctxt ~priority ~endorsing_power =
  get_constants ctxt
  >>=? fun {Constants.parametric = {endorsement_reward; _}; _} ->
  let reward_per_endorsement =
    reward_for_priority endorsement_reward priority
  in
  Lwt.return
    (Environment.wrap_tzresult
       Tez.(reward_per_endorsement *? Int64.of_int endorsing_power))

(* Voting *)

module Vote = struct
  let get_ballots ctxt = Alpha_services.Voting.ballots rpc_ctxt ctxt

  let get_ballot_list ctxt = Alpha_services.Voting.ballot_list rpc_ctxt ctxt

  let get_current_period ctxt =
    Alpha_services.Voting.current_period rpc_ctxt ctxt

  let get_current_quorum ctxt =
    Alpha_services.Voting.current_quorum rpc_ctxt ctxt

  let get_listings ctxt = Alpha_services.Voting.listings rpc_ctxt ctxt

  let get_proposals ctxt = Alpha_services.Voting.proposals rpc_ctxt ctxt

  let get_current_proposal ctxt =
    Alpha_services.Voting.current_proposal rpc_ctxt ctxt

  let get_protocol (b : Block.t) =
    Tezos_protocol_environment.Context.get_protocol b.context

  let get_participation_ema (b : Block.t) =
    Environment.Context.find b.context ["votes"; "participation_ema"]
    >|= function
    | None -> assert false | Some bytes -> ok (TzEndian.get_int32 bytes 0)

  let set_participation_ema (b : Block.t) ema =
    let bytes = Bytes.make 4 '\000' in
    TzEndian.set_int32 bytes 0 ema ;
    Environment.Context.add b.context ["votes"; "participation_ema"] bytes
    >|= fun context -> {b with context}
end

module Contract = struct
  let pp = Alpha_context.Contract.pp

  let pkh c =
    Alpha_context.Contract.is_implicit c
    |> function
    | Some p -> return p | None -> failwith "pkh: only for implicit contracts"

  let baker c =
    match Alpha_context.Contract.is_baker c with
    | Some baker ->
        return baker
    | None ->
        failwith "contract %a is not a baker" Contract.pp c

  type balance_kind = Main | Deposit | Fees | Rewards

  let balance ?(kind = Main) ctxt contract =
    match kind with
    | Main ->
        Alpha_services.Contract.balance rpc_ctxt ctxt contract
    | _ -> (
      match Alpha_context.Contract.is_baker contract with
      | None ->
          invalid_arg
            "get_balance: no frozen accounts for a non-baking contract."
      | Some baker ->
          Alpha_services.Baker.frozen_balance_by_cycle rpc_ctxt ctxt baker
          >>=? fun map ->
          Lwt.return
          @@ Cycle.Map.fold
               (fun _cycle {Baker.deposit; fees; rewards} acc ->
                 acc
                 >>? fun acc ->
                 match kind with
                 | Deposit ->
                     Test_tez.Tez.(acc +? deposit)
                 | Fees ->
                     Test_tez.Tez.(acc +? fees)
                 | Rewards ->
                     Test_tez.Tez.(acc +? rewards)
                 | Main ->
                     assert false)
               map
               (Ok Tez.zero) )

  let counter ctxt contract =
    match Contract.is_implicit contract with
    | None ->
        invalid_arg "Helpers.Context.counter"
    | Some mgr ->
        Alpha_services.Contract.counter rpc_ctxt ctxt mgr

  let find_account _ contract =
    match Contract.is_implicit contract with
    | None ->
        invalid_arg "Helpers.Context.manager"
    | Some pkh ->
        Account.find pkh

  let is_public_key_revealed ctxt contract =
    Alpha_services.Contract.public_key rpc_ctxt ctxt contract
    >|=? fun res -> res <> None

  let delegate = Alpha_services.Contract.delegate rpc_ctxt

  let delegate_opt = Alpha_services.Contract.delegate_opt rpc_ctxt

  let storage = Alpha_services.Contract.storage rpc_ctxt
end

module Baker = struct
  type info = Baker_services.info = {
    balance : Tez.t;
    frozen_balance : Tez.t;
    frozen_balance_by_cycle : Baker.frozen_balance Cycle.Map.t;
    staking_balance : Tez.t;
    delegated_contracts : Alpha_context.Contract.t list;
    delegated_balance : Tez.t;
    deactivated : bool;
    grace_period : Cycle.t;
    consensus_key : Signature.Public_key.t;
    pending_consensus_key : (Signature.Public_key.t * Cycle.t) option;
    voting_power : int32;
  }

  let info = Alpha_services.Baker.info rpc_ctxt

  let consensus_key ?level ?offset =
    Alpha_services.Baker.consensus_key rpc_ctxt ?level ?offset
end

let init ?endorsers_per_block ?with_commitments
    ?(initial_implicit_balances = []) ?(initial_baker_balances = [])
    ?initial_endorsers ?min_proposal_quorum n =
  let (accounts, baker_accounts, origination_nonce) =
    Account.generate_accounts
      ~initial_implicit_balances
      ~initial_baker_balances
      n
  in
  let contracts =
    List.map
      (fun (a, _) -> Alpha_context.Contract.implicit_contract Account.(a.pkh))
      accounts
  in
  let bakers = List.map (fun (a, _) -> Account.(a.baker)) baker_accounts in
  Block.genesis
    ?endorsers_per_block
    ?with_commitments
    ?initial_endorsers
    ?min_proposal_quorum
    accounts
    baker_accounts
    origination_nonce
  >|=? fun blk -> (blk, contracts, bakers)
