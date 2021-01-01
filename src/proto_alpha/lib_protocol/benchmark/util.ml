open Tezos_raw_protocol_alpha
open Alpha_context

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ;
  s

let write_file filename str =
  let ch = open_out_gen [ Open_creat; Open_wronly ] 0o666 filename in
  output_string ch str ;
  close_out_noerr ch

let force x =
  match x with
  | Ok x -> x
  | Error es ->
      Format.printf "Errors :\n" ;
      List.iter (Format.printf "- %a\n" Protocol.Environment.Error_monad.pp) es ;
      raise (Failure "force")

let force_global x =
  match x with
  | Ok x -> x
  | Error es ->
      Format.printf "Errors :\n" ;
      List.iter (Format.printf "- %a\n" pp) es ;
      raise (Failure "force")

let force_global_lwt x = force_global (Lwt_main.run x)

let force_lwt x = force (Lwt_main.run x)

let ( >>=?? ) x k = x >>= fun x -> Lwt.return (Environment.wrap_error x) >>=? k

let of_mutez x = Tez.of_mutez (Int64.of_int x) |> Option.get

let micheline_canonical_to_string c =
  Fmt.str
    "%a"
    Micheline_printer.print_expr
    (Micheline_printer.printable Michelson_v1_primitives.string_of_prim c)

let contract_to_pkh = Contract.to_b58check

open Script_typed_ir

let get_big_map_by_id context id key =
  let key_type = Address_key None in
  let value_type =
    Pair_t
      ( (Map_t (Address_key None, Nat_t None, None), None, None),
        (Nat_t None, None, None),
        None )
  in
  let map =
    {
      id = Some (Obj.magic (Z.of_int id));
      diff = Script_ir_translator.empty_map key_type;
      key_type;
      value_type;
    }
  in
  Script_ir_translator.big_map_get context key map |> force_lwt

let show_map map =
  let module Boxed_map = ( val map : Script_typed_ir.Boxed_map
                             with type key = address
                              and type value = Script_int_repr.n
                                               Script_int_repr.num )
  in
  let (x, _) = Boxed_map.boxed in
  Format.sprintf
    "map {\n%s\n}"
    ( Boxed_map.OPS.bindings x
    |> List.map (fun ((contract, entrypoint), v) ->
           Format.sprintf
             "%s+%s = %s ;"
             (contract_to_pkh contract)
             entrypoint
             (Script_int_repr.to_string v))
    |> String.concat "\n" )

let get_next_context b =
  force_global_lwt
    ( Incremental.begin_construction b >>=? fun b ->
      return (Incremental.alpha_ctxt b) )

let get_balance : Block.t -> Contract.t -> Alpha_context.Tez.t =
 fun b c ->
  let context = get_next_context b in
  Contract_storage.get_balance (Obj.magic context) (Obj.magic c)
  |> force_lwt |> Obj.magic

let short c = String.sub (Contract.to_b58check c) 2 10

let contract_to_name sender =
  match short sender with
  | "1TTAuzs8A1" -> "alice"
  | "1TAN9oBiGe" -> "bob"
  | _ -> assert false

let print_balance prefix block sender =
  let person = contract_to_name sender in
  let contract_tez = get_balance block sender |> Tez.to_string in
  print_endline @@ prefix ^ person ^ " has " ^ contract_tez ^ "tz"

let print_token_balance prefix context id contract =
  let key = (contract, "default") in
  let key_type = Address_key None in
  let value_type =
    Pair_t
      ( (Map_t (Address_key None, Nat_t None, None), None, None),
        (Nat_t None, None, None),
        None )
  in
  let map =
    {
      id = Some (Obj.magic (Z.of_int id));
      diff = Script_ir_translator.empty_map key_type;
      key_type;
      value_type;
    }
  in
  let (x, _) = Script_ir_translator.big_map_get context key map |> force_lwt in
  let x = Option.get x |> snd |> Script_int.to_string in
  print_endline @@  prefix ^ ": " ^ contract_to_name contract
  ^ " has a token balance of " ^ x

let print_token_balance_from_block prefix b id contract =
  let context = get_next_context b in
  print_token_balance prefix context id contract

let alice : Contract.t ref = ref @@ Obj.magic ()

let print_alice_token_balance prefix context = 
  print_token_balance prefix context 0 !alice
