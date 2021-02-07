(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <team@marigold.dev>                           *)
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

open Tezos_raw_protocol_alpha
open Alpha_context

module Option = struct
  include Option

  let get opt =
    match opt with
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

let of_mutez x = Tez.of_mutez_exn (Int64.of_int x)

let logid label x =
  print_endline (label ^ ": " ^ x) ;
  x

let micheline_canonical_to_string c =
  Fmt.str
    "%a"
    Micheline_printer.print_expr
    (Micheline_printer.printable Michelson_v1_primitives.string_of_prim c)
