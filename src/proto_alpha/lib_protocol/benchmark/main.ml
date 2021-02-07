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

open Util

let () = Printexc.record_backtrace true

exception RegressionDifference of string * string * string

let create_benchmark name f =
  try
    let (eval, closure) = f () in
    let new_results = Lwt_main.run @@ eval () |> String.trim in
    let path = Sys.getcwd () in
    let path = Str.replace_first (Str.regexp "_build/default/") "" path in
    let path = path ^ "/regression_results/" ^ name in
    let () =
      match Sys.file_exists path with
      | true -> (
          let old_results = read_file path |> String.trim in
          match old_results <> new_results with
          | true ->
              raise @@ RegressionDifference (name, old_results, new_results)
          | false ->
              () )
      | false ->
          write_file path new_results
    in
    Core_bench.Bench.Test.create ~name (fun () -> Lwt_main.run @@ closure ())
  with RegressionDifference (name, old_results, new_results) ->
    print_endline
      ( "Evaluation of " ^ name
      ^ " produced different results than regression records."
      ^ "\n========== Old results ==========\n" ^ old_results
      ^ "\n========== New results ==========\n" ^ new_results ) ;
    assert false

let benchmarks =
  [ create_benchmark "Fact" Fact_benchmarks.fact_benchmark;
    create_benchmark "FA1.2_Approve" Fa12_benchmarks.approve_fa12_benchmark;
    create_benchmark "FA1.2_Transfer" Fa12_benchmarks.transfer_benchmark;
    create_benchmark "Dexter_xtzToToken" Dexter_benchmarks.xtzToToken_benchmark
  ]

let () =
  let devmode =
    Array.length Sys.argv >= 2 && String.equal Sys.argv.(1) "--devmode"
  in
  if not devmode then
    Core.Command.run (Core_bench.Bench.make_command benchmarks)
  else print_endline "👍 All regression tests pass 👍"
