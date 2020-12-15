(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@tezos.com>                       *)
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

open Protocol

(* Samplers for basic Michelson types. *)

module type Base_S = sig
  val int : unit -> Alpha_context.Script_int.z Alpha_context.Script_int.num

  val nat : unit -> Alpha_context.Script_int.n Alpha_context.Script_int.num

  val signature : unit -> Tezos_crypto.Signature.t

  val string : unit -> string

  val bytes : unit -> bytes

  val tez : unit -> Alpha_context.Tez.tez

  val timestamp : unit -> Alpha_context.Script_timestamp.t

  val bool : unit -> bool
end

module type Full_S = sig
  val state : Random.State.t

  val sampling_parameters : Michelson_samplers_parameters.t

  module Crypto_samplers : Crypto_samplers.Finite_key_pool_S

  module Michelson_base : Base_S
end

module Make_base (P : Michelson_samplers_parameters.S) : Base_S = struct
  let int () =
    let i = Base_samplers.int P.state ~range:P.parameters.int_size in
    Alpha_context.Script_int.of_zint i

  let nat () =
    let i = Base_samplers.nat P.state ~range:P.parameters.int_size in
    match
      Alpha_context.Script_int.is_nat (Alpha_context.Script_int.of_zint i)
    with
    | None ->
        assert false
    | Some n ->
        n

  (* We ought to do better *)
  let signature () = Signature.zero

  let string () = Base_samplers.string P.state ~range:P.parameters.string_size

  let bytes () = Base_samplers.bytes P.state ~range:P.parameters.bytes_size

  let tez () =
    let i = Random.State.int64 P.state (Int64.of_int max_int) in
    match Protocol.Alpha_context.Tez.of_mutez i with
    | Some res ->
        res
    | None ->
        assert false

  let timestamp () =
    let i = Base_samplers.int P.state ~range:P.parameters.int_size in
    Protocol.Alpha_context.Script_timestamp.of_zint i

  let bool () = Base_samplers.uniform_bool P.state
end

module Make_full (P : Michelson_samplers_parameters.S) : Full_S = struct
  let state = P.state

  let sampling_parameters = P.parameters

  module Crypto_samplers = Crypto_samplers.Make_finite_key_pool (P)
  module Michelson_base = Make_base (P)
end
