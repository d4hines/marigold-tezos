[@@@warning "-21"]

[@@@warning "-20"]

[@@@warning "-27"]

[@@@warning "-26"]

open Tezos_raw_protocol_alpha
open Alpha_context
open Script
open Printf
open Show
open Foo

let () = Printexc.record_backtrace true

let _ =
  Lwt_main.run
    ( eval_script ()
    >>=? fun (storage, _context) ->
    print_endline "======== Storage after transfer =========" ;
    Micheline.strip_locations storage
    |> micheline_canonical_to_string |> print_endline ;
    return () )

(* let () =
  let open Core in
  let open Core_bench in
  Command.run
    (Bench.make_command
       [ Bench.Test.create ~name:"run_script" (fun () ->
             run_script () |> Lwt_main.run);
         Bench.Test.create ~name:"run_script_batch" (fun () ->
             run_script_batch () |> Lwt_main.run) ]) *)

