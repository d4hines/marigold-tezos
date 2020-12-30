open Tezos_raw_protocol_alpha
open Alpha_context

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ; s

let write_file  filename str  =
  let ch = open_out_gen [Open_creat; Open_wronly] 0o666  filename in
  output_string ch str;
  close_out_noerr ch

let force x =
  match x with
  | Ok x ->
      x
  | Error es ->
      Format.printf "Errors :\n" ;
      List.iter (Format.printf "- %a\n" Protocol.Environment.Error_monad.pp) es ;
      raise (Failure "force")

let force_global x =
  match x with
  | Ok x ->
      x
  | Error es ->
      Format.printf "Errors :\n" ;
      List.iter (Format.printf "- %a\n" pp) es ;
      raise (Failure "force")
let force_global_lwt x = force_global (Lwt_main.run x)
let force_lwt x = force (Lwt_main.run x)

let ( >>=?? ) x k = x >>= fun x -> Lwt.return (Environment.wrap_error x) >>=? k

let of_mutez x = Tez.of_mutez (Int64.of_int x) |> Option.get

let logid label x =
  print_endline (label ^ ": " ^ x) ;
  x

let micheline_canonical_to_string c =
  Fmt.str
    "%a"
    Micheline_printer.print_expr
    (Micheline_printer.printable Michelson_v1_primitives.string_of_prim c)
  
let contract_to_pkh = Contract.to_b58check
