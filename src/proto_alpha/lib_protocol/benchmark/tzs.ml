open Util
open Pipeline
open Micheline
open Tezos_raw_protocol_alpha
open Alpha_context

type input =
  | InContract : { address : string; code : string; storage : string } -> input

type output = OutContract : { address : string; storage : string } -> output

type tzs_test =
  | TzsTest :
      { name : string;
        input : input list;
        operations : 'a Pipeline.operation list;
        output : output list
      }
      -> tzs_test

let parse_str : string -> (string Micheline.canonical list, 'a) result =
 fun source ->
  match Micheline_parser.tokenize source with
  | (_, e :: _e') -> error e
  | (tokens, []) -> (
      match Micheline_parser.parse_toplevel ?check:(Some false) tokens with
      | (_, e :: _e') -> error e
      | (ast, []) -> (
          match ast with
          | Seq _ :: _ -> ok @@ List.map Micheline.strip_locations ast
          | Prim _ :: _ ->
              ok
              @@ [ Micheline.strip_locations
                   @@ Seq (Micheline_parser.location_zero, ast) ]
          | _ -> assert false ) )

let find :
    string -> ('a, string) node list -> ('a, string) Micheline.node option =
 fun prop props ->
  List.fold_left
    (fun acc curr ->
      match acc with
      | Some x -> Some x
      | None -> (
          match curr with
          | Prim (_, prim_name, args, _) ->
              if prim_name = prop then Some (Option.get @@ List.hd args)
              else None
          | _ -> None ))
    None
    props

let member :
    string -> ('a, string) Micheline.node -> ('a, string) Micheline.node option
    =
 fun prop node ->
  match node with
  | Seq (_, props) -> find prop props
  | _ -> None

let ( .@!{} ) x y = member y x |> Option.get

let as_string node =
  match node with
  | Prim (_, x, _, _) -> x
  | String (_, x) -> x
  | _ -> assert false

let as_list node =
  match node with
  | Seq (_, x) -> x
  | _ -> assert false

let prim node =
  match node with
  | Prim (_, name, args, _) -> (name, args)
  | _ -> assert false

let map f x = List.map f (as_list x)

let extract_incontract contract =
  let address = contract.@!{"address"} |> as_string in
  let storage =
    contract.@!{"storage"} |> strip_locations
    |> micheline_canonical_string_to_string
  in
  let code =
    contract.@!{"code"} |> fun x ->
    match x with
    | String (_, loc) -> read_file loc
    | x -> x |> strip_locations |> micheline_canonical_string_to_string
  in
  InContract { address; storage; code }

let extract_outcontract contract =
  let address = contract.@!{"address"} |> as_string in
  let storage =
    contract.@!{"storage"} |> strip_locations
    |> micheline_canonical_string_to_string
  in
  OutContract { address; storage }

let extract_transfer : (int, string) node -> 'a Pipeline.operation =
 fun args ->
  let sender : Contract.t =
    args.@!{"sender"} |> as_string |> fun x ->
    Contract_repr.of_b58check x |> Obj.magic |> force_global
  in
  let recipient : Contract.t =
    args.@!{"recipient"} |> as_string |> fun x ->
    Contract_repr.of_b58check x |> Obj.magic |> force_global
  in
  let entrypoint = args.@!{"entrypoint"} |> as_string in
  let parameters =
    args.@!{"parameters"} |> strip_locations
    |> Util.micheline_canonical_string_to_string
  in
  Transfer { sender; recipient; entrypoint; parameters; amount = 0 }

let parse_micheline test =
  let name = test.@!{"name"} |> as_string in
  let input =
    map
      (fun input ->
        match prim input with
        | ("contract", [contract]) -> extract_incontract contract
        | _ -> assert false)
      test.@!{"input"}
  in
  let operations =
    map
      (fun op ->
        match prim op with
        | ("transfer", [transfer]) -> extract_transfer transfer
        | _ -> assert false)
      test.@!{"operations"}
  in
  let output =
    map
      (fun output ->
        match prim output with
        | ("contract", [contract]) -> extract_outcontract contract
        | _ -> assert false)
      test.@!{"output"}
  in

  TzsTest { name; input; operations; output }

(* TODO: fix GADT if that's possible.*)
let interpret block default_contract (TzsTest test) =
  let originations =
    List.map
      (fun (InContract x) ->
        Pipeline.Origination
          { originator = default_contract;
            amount = 0;
            contract = x.code;
            initial_storage = x.storage
          })
      test.input
  in
  let rec make_pipeline :
      type a b c.
      a Pipeline.operation list -> b Pipeline.operation -> c Pipeline.operation
      =
   fun l pipeline ->
    let open Pipeline in
    match l with
    | [] -> Obj.magic pipeline
    | x :: s -> pipeline >>= fun _ -> make_pipeline s x
  in
  let ops = originations @ Obj.magic test.operations in
  let (first, last) = List.split_n (List.length ops - 1) ops in
  match first with
  (* Should contain at least one origination *)
  | [] -> assert false
  | x :: s ->
      (block, make_pipeline s x) >>=! fun _ ->
      List.hd last |> Option.get |> Obj.magic

let run_test () =
  let unparsed =
    read_file "src/proto_alpha/lib_protocol/benchmark/example.tzs"
    |> parse_str |> force_global |> List.hd |> Option.get
  in
  let parsed = parse_micheline @@ root unparsed in
  let (b, contracts) = Context.init 10 |> force_global_lwt in
  let (eval, _) = interpret b (first contracts) parsed in
  print_endline @@ Lwt_main.run @@ eval ()

let () = run_test ()
