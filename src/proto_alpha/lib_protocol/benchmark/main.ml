open Util

let () = Printexc.record_backtrace true

exception RegressionDifference of string * string * string

let create_benchmark name f =
  try
    let (eval, closure) = f () in
    let new_results = Lwt_main.run @@ eval () |> String.trim in
    let path = Sys.getcwd () ^ "/regression_results/" ^ name in
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
  [ create_benchmark "FA1.2_Approve" Fa12_benchmarks.approve_fa12_benchmark;
    create_benchmark "FA1.2_Transfer" Fa12_benchmarks.transfer_benchmark;
    create_benchmark "Dexter_xtzToToken" Dexter_benchmarks.xtzToToken_benchmark
  ]

let () = Core.Command.run (Core_bench.Bench.make_command benchmarks)
