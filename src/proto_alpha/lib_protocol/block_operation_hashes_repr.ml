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

type single_block_operation_hashes = {
  level : Raw_level_repr.t;
  hashes : Operation_hash.t list;
}

type t = single_block_operation_hashes list

let encoding_single_block_operations_hashes =
  let open Data_encoding in
  conv
    (fun {level; hashes} -> (level, hashes))
    (fun (level, hashes) -> {level; hashes})
    (obj2
       (req "level" Raw_level_repr.encoding)
       (req "hashes" (list Operation_hash.encoding)))

let encoding =
  let open Data_encoding in
  list encoding_single_block_operations_hashes

let to_list x = x

let of_list x = x

let compare x y = Raw_level_repr.compare x.level y.level

let path_length = 1

let get_level v = v.level

let get_operation_hashes v = List.map (fun v -> v.hashes) v

let make ~level ~hashes = {level; hashes}
