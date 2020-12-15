(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs <contact@tezos.com>                       *)
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

(* Input parameter parsing *)

let _ =
  if Array.length Sys.argv < 2 then (
    Format.eprintf "Executable expects random seed on input\n%!" ;
    exit 1 )
  else Random.init (int_of_string Sys.argv.(1))

(* ------------------------------------------------------------------------- *)
(* Base sampler parameters *)

let sampling_parameters =
  let open Michelson_samplers_parameters in
  let size = {Tezos_benchmark.Base_samplers.min = 4; max = 32} in
  {
    int_size = size;
    string_size = size;
    bytes_size = size;
    stack_size = size;
    type_depth = size;
    list_size = size;
    set_size = size;
    map_size = size;
  }

module Full = Michelson_samplers_base.Make_full (struct
  let parameters = sampling_parameters

  let state = Random.State.make [|42; 987897; 54120|]

  let algo = `Default

  let size = 16
end)

(* ------------------------------------------------------------------------- *)
(* Test code autocompletion *)

module Autocomp = Autocomp.Make (Full)

let () = Format.eprintf "============================@.%!"

let () = Format.eprintf "Testing code autocompletion@.%!"

let bef =
  Tezos_benchmark_type_inference.Type.(item unit (item unit (item unit empty)))

let aft =
  Tezos_benchmark_type_inference.Type.(
    item int (item unit (item (pair nat nat) empty)))

module Samplers = Michelson_samplers_base.Make_full

let (code, _) =
  let term = Autocomp.invent_term bef aft in
  let run x = x (Tezos_benchmark_type_inference.Inference.M.empty ()) in
  run term

let _ =
  Format.eprintf "type: %a => %a@." Type.Stack.pp bef Type.Stack.pp aft ;
  Format.eprintf "%a@." Mikhailsky.pp (Mikhailsky.seq code)

let () = Format.eprintf "============================@.%!"

(* ------------------------------------------------------------------------- *)
(* MCMC instantiation *)

module Gen = Generators.Code (struct
  module Samplers = Full

  let target_size = 500

  let verbosity = `Trace
end)

let start = Unix.gettimeofday ()

let generator = Gen.generator ~burn_in:(500 * 7)

let stop = Unix.gettimeofday ()

let () = Format.printf "Burn in time: %f seconds@." (stop -. start)

let _ =
  for i = 1 to 1000 do
    let (michelson, (bef, aft)) = StaTz.Stats.sample_gen generator in
    let printable =
      Micheline_printer.printable
        Tezos_protocol_alpha.Protocol.Michelson_v1_primitives.string_of_prim
        michelson
    in
    Format.eprintf "result %d/1000:@." i ;
    Format.eprintf "type: %a => %a@." Type.Stack.pp bef Type.Stack.pp aft ;
    Format.eprintf "%a@." Micheline_printer.print_expr printable
  done
