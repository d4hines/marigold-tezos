open Tezos_raw_protocol_alpha
open Alpha_context

module Option = struct
  include Option

  let get o =
    match o with
    | Some x ->
        x
    | None ->
        raise @@ Invalid_argument "Option.get called on None"
end

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ; s

let write_file filename str =
  let ch = open_out_gen [Open_creat; Open_wronly] 0o666 filename in
  output_string ch str ;
  close_out_noerr ch ;
  if not @@ Sys.file_exists filename then
    raise @@ Failure "Something went wrong creating benchmark regression"

let force x =
  match x with
  | Ok x ->
      x
  | Error es ->
      Format.printf "Errors :\n" ;
      Format.printf "- %a\n" Protocol.Environment.Error_monad.pp_trace es;
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

let assert_strings_equal : string -> string -> unit =
 fun expected actual ->
  if String.equal expected actual then ()
  else (
    print_endline "Strings not equal." ;
    print_endline "-------- Expected ---------" ;
    print_endline expected ;
    print_endline "-------- Actual ---------" ;
    print_endline actual ;
    assert false )

let assert_error_has_message err message =
  let err_str =
    Format.asprintf "%a" Error_monad.pp_print_error err |> String.split '\n'
  in
  let err_str = List.nth err_str 1 |> Option.get |> String.trim in
  assert_strings_equal err_str message
