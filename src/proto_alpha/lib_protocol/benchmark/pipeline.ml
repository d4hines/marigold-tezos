open Tezos_raw_protocol_alpha
open Alpha_context
open Script
open Util

let logger = None

let make_contract :
    script:Script.t ->
    originator:Contract.t ->
    amount:Int.t ->
    b:Block.t ->
    (Block.t * Contract.t, tztrace) result Lwt.t =
 fun ~script ~originator ~amount ~b ->
  let amount = Util.of_mutez amount in
  let x =
    Incremental.begin_construction b
    >>=? fun b ->
    Op.origination (I b) originator ~script ~credit:amount
    >>=? fun (op, originated_contract) ->
    Incremental.add_operation b op
    >>=? fun b ->
    Incremental.finalize_block b
    >>=? fun b -> Error_monad.return (b, originated_contract)
  in
  x

let get_next_context b =
  force_global_lwt
    ( Incremental.begin_construction b
    >>=? fun b -> return (Incremental.alpha_ctxt b) )

let do_transaction block ~sender ~recipient ~amount ~entrypoint ~parameters =
  let parameters = parameters |> Expr.from_string in
  let parameters = lazy_expr parameters in
  Incremental.begin_construction block
  >>=? fun b ->
  Op.transaction
    (I b)
    ~parameters
    ~entrypoint
    ~fee:Tez.zero
    sender
    recipient
    (of_mutez amount)
  >>=? fun op ->
  Incremental.add_operation b op >>=? fun b -> Incremental.finalize_block b

let type_script context script =
  force_lwt
  @@ Script_ir_translator.parse_script
       context
       ~legacy:false
       ~allow_forged_in_storage:true
       script

module Transfer : sig
  type t = {
    sender : Contract.t;
    recipient : Contract.t;
    entrypoint : String.t;
    amount : Int.t;
    parameters : String.t;
  }
end = struct
  type t = {
    sender : Contract.t;
    recipient : Contract.t;
    entrypoint : String.t;
    amount : Int.t;
    parameters : String.t;
  }
end

type goal = (unit -> string Lwt.t) * (unit -> unit Lwt.t)

module Finally : sig
  type t = goal
end = struct
  type t = goal
end

(* let return =  *)
type 'a operation =
  | Origination : {
      originator : Contract.t;
      amount : Int.t;
      contract : String.t;
      initial_storage : String.t;
    }
      -> Contract.t operation
  | Transfer : Transfer.t -> Transfer.t operation
  | Pending : ('a operation * (Block.t * 'a -> 'b operation)) -> 'b operation
  (* | Finally : Finally.t -> (Block.t * Finally.t) operation *)
  | Return : 'a -> 'a operation

let op_to_string : type a. a operation -> string =
 fun op ->
  match op with
  | Transfer _ ->
      "Transfer"
  | Origination _ ->
      "Origination"
  | Pending _ ->
      "Pending"
  | Return _ ->
      "Return"

let return v = Return v

let bind : type a b. a operation -> (Block.t * a -> b operation) -> b operation
    =
 fun op f -> Pending (op, f)

let ( >>= ) = bind

let map op f = bind op (fun (b, v) -> return (f (b, v)))

let ( >>| ) = map

let rec run : type a. a operation -> Block.t -> (Block.t * a) tzresult Lwt.t =
 fun op b ->
  match op with
  | Origination {originator; amount; contract; initial_storage} ->
      let contract = contract |> Expr.from_string |> lazy_expr in
      let storage = initial_storage |> Expr.from_string |> lazy_expr in
      let script = Script.{code = contract; storage} in
      let x =
        make_contract ~script ~originator ~amount ~b |> force_global_lwt
      in
      Error_monad.return x
  | Transfer x ->
      let b =
        do_transaction
          b
          ~sender:x.sender
          ~recipient:x.recipient
          ~parameters:x.parameters
          ~amount:x.amount
          ~entrypoint:x.entrypoint
        |> force_global_lwt
      in
      Error_monad.return (b, x)
  | Pending (op, f) ->
      run op b >>=? fun (b, x) -> run (f (b, x)) b
  | Return v ->
      Error_monad.return (b, v)

let get_transfer : Transfer.t operation -> Transfer.t =
 fun op -> match op with Transfer x -> x | _ -> assert false

let finally :
    type a.
    Block.t * a operation -> (Block.t * a -> Transfer.t operation) -> goal =
 fun (b, op) f ->
  let (b, x) = run op b |> force_global_lwt in
  let transfer = f (b, x) |> get_transfer in
  let step_constants : Script_interpreter.step_constants =
    {
      source = transfer.sender;
      payer = transfer.sender;
      self = transfer.recipient;
      amount = Util.of_mutez transfer.amount;
      chain_id = Chain_id.zero;
    }
  in
  let context = get_next_context b in
  let (context, script) =
    Alpha_context.Contract.get_script context transfer.recipient |> force_lwt
  in
  let script = script |> Option.get in
  let ( Ex_script {code; arg_type; storage; storage_type; root_name = _},
        context ) =
    type_script context script
  in
  let (Lam (code, _)) = code in
  let (arg, context) =
    Script_ir_translator.parse_data
      context
      ~legacy:false
      ~allow_forged:false
      arg_type
      (Micheline.root (Expr.from_string transfer.parameters))
    |> force_lwt
  in
  let reg = (arg, storage) in
  let stack = ((), ()) in
  let eval_script () =
    Script_interpreter.step logger context step_constants code reg stack
  in
  let run_script () = eval_script () |> Lwt.map (fun _ -> ()) in
  let eval_script () =
    let output =
      eval_script ()
      >>=? fun ((_, storage), _, ctx) ->
      Script_ir_translator.unparse_data ctx Readable storage_type storage
      >>=? fun (micheline, _) ->
      Micheline.strip_locations micheline
      |> micheline_canonical_to_string |> Error_monad.return
    in
    output |> Lwt.map (fun v -> force v)
  in
  (eval_script, run_script)

let ( >>=! ) = finally