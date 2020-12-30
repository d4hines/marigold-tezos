[@@@warning "-33"]

open Util
open Unit_tests
let x = (==)

let () = Printexc.record_backtrace true

exception RegressionDifference of string * string * string

(* let create_benchmark name f =
  try
    let (eval, closure) = f () in
    let new_results = Lwt_main.run @@ eval () |> String.trim in
    let path = "./regression_results/" ^ name in
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
  with
  | RegressionDifference (name, old_results, new_results) ->
      print_endline
        ( "Evaluation of " ^ name
        ^ " produced different results than regression records."
        ^ "\n========== Old results ==========\n" ^ old_results
        ^ "\n========== New results ==========\n" ^ new_results ) ;
      assert false
  | Tezos_raw_protocol_alpha.Script_tagged_ir.Micheline_exception micheline ->
    let foo = (Micheline.strip_locations micheline) in
    print_endline @@ Show.show_canonical (Show.pp_prim) foo ;
      assert false

let benchmarks =
  [ (* create_benchmark "FA1.2_Approve" Fa12_benchmarks.approve_fa12_benchmark; *)
    (* create_benchmark "FA1.2_Transfer" Fa12_benchmarks.transfer_benchmark; *)
    create_benchmark "Dexter_xtzToToken" Dexter_benchmarks.xtzToToken_benchmark
  ] *)

(* let () = Core.Command.run (Core_bench.Bench.make_command benchmarks) *)
