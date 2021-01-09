(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020-2021 Marigold <contact@marigold.dev>                   *)
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

type t = {level : Raw_level_repr.t; hashes : Operation_hash.t list}

let encoding =
  let open Data_encoding in
  conv
    (fun {level; hashes} -> (level, hashes))
    (fun (level, hashes) -> {level; hashes})
    (obj2
       (req "level" Raw_level_repr.encoding)
       (req "hashes" (list Operation_hash.encoding)))

let rpc_arg =
  let construct v =
    Ezjsonm.to_string @@ Ezjsonm.wrap
    @@ Data_encoding.Json.construct encoding v
  in
  let destruct v =
    Ok (Data_encoding.Json.destruct encoding @@ Ezjsonm.from_string v)
  in
  let name = "block_operation_hashes" in
  let description = "Operation hashes in a block" in
  RPC_arg.make ~descr:description ~name ~construct ~destruct ()

let compare x y = Raw_level_repr.compare x.level y.level

let path_length = 1

let to_path k l =
  let open Data_encoding in
  (k |> Binary.to_bytes_exn encoding |> Bytes.to_string) :: l

let of_path = function
  | [s] ->
      s |> Bytes.of_string |> Data_encoding.Binary.of_bytes encoding
  | _ ->
      None

let get_level v = v.level

let get_operation_hashes v = v.hashes

let make ~level ~hashes = {level; hashes}
