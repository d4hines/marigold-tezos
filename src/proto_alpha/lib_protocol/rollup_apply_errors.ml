(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Gabriel Alfour <gabriel.alfour@gmail.com>              *)
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

type error += Rollup_rejection_too_old_level of {
    rollup_block_tezos_level : Level_repr.t ;
    current_tezos_level : Level_repr.t ;
  }

let () =
  let open Data_encoding in
  register_error_kind
    `Temporary
    ~id:"rollup.rejection_too_old.level"
    ~title:"Rejection of a block too old (level)"
    ~description:"The tezos level of the rejected rollup block is too far in the past"
    (obj2
       (req "rollup_block_level" Level_repr.encoding)
       (req "current_level" Level_repr.encoding))
    (function
      | Rollup_rejection_too_old_level { rollup_block_tezos_level = r ; current_tezos_level = c } ->
          Some (r , c)
      | _ ->
          None)
    (fun (r , c) -> Rollup_rejection_too_old_level { rollup_block_tezos_level = r ; current_tezos_level = c } )

type error += Rollup_rejection_too_old_timestamp of {
    rollup_block_timestamp : Time.t ;
    current_timestamp : Time.t ;
  }

let () =
  let open Data_encoding in
  register_error_kind
    `Temporary
    ~id:"rollup.rejection_too_old.timestamp"
    ~title:"Rejection of a block too old (timestamp)"
    ~description:"The tezos timestamp of the rejected rollup block is too far in the past"
    (obj2
       (req "rollup_block_timestamp" Time.encoding)
       (req "current_timestamp" Time.encoding))
    (function
      | Rollup_rejection_too_old_timestamp { rollup_block_timestamp = r ; current_timestamp = c } ->
          Some (r , c)
      | _ ->
          None)
    (fun (r , c) -> Rollup_rejection_too_old_timestamp { rollup_block_timestamp = r ; current_timestamp = c } )

type rollup_invalid_rejection =
  | Rollup_bad_micro_block_index
  | Rollup_valid
  | Rollup_unexpected_error

let rollup_invalid_rejection_encoding : rollup_invalid_rejection Data_encoding.t =
  let open Data_encoding in
  union [
    case ~title:"bad_micro_block_index" (Tag 0) unit
      (function Rollup_bad_micro_block_index -> Some () | _ -> None)
      (fun () -> Rollup_bad_micro_block_index) ;
    case ~title:"valid_state_hash" (Tag 1) unit
      (function Rollup_valid -> Some () | _ -> None)
      (fun () -> Rollup_valid) ;
    case ~title:"unexpected_error" (Tag 2) unit
      (function Rollup_unexpected_error -> Some () | _ -> None)
      (fun () -> Rollup_unexpected_error) ;
  ]

type error += Rollup_invalid_rejection of rollup_invalid_rejection

let () =
  register_error_kind
    `Temporary
    ~id:"rollup.invalid_rejection"
    ~title:"Invalid rejection of a block"
    ~description:"A rollup block rejection was submitted, but the rejection is invalid"
    rollup_invalid_rejection_encoding
    (function Rollup_invalid_rejection x -> Some x | _ -> None)
    (fun x -> Rollup_invalid_rejection x)
