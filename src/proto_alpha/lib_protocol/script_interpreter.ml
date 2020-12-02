(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

include Script_interpreter_functor.Make (struct
  include Alpha_context
  module Pervasives = Pervasives
  module Z = Z
  module Signature = Signature
  module Bls12_381 = Bls12_381
  module Chain_id = Chain_id
  module Compare = Compare
  module Data_encoding = Data_encoding
  module Bytes = Bytes
  module TzEndian = TzEndian
  module String = String
  module Raw_hashes = Raw_hashes
  module Option = Option
  module Micheline = Micheline
  module Lwt = Lwt
  module List = List
  module Format = Format
  module Raw_context = Alpha_context
  module Error_monad = Error_monad

  module Michelson_v1_primitives = struct
    include Michelson_v1_primitives

    let i_push = I_PUSH

    let i_pair = I_PAIR

    let k_parameter = K_parameter

    let k_storage = K_storage

    let k_code = K_code
  end

  module Alpha_context = Alpha_context
  module Operation = Alpha_context
  module Script_ir_translator = Script_ir_translator
  module Script_typed_ir = Script_typed_ir
  module Script_interpreter_cost = Script_interpreter_cost
end)
