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

open Alpha_context
open Apply_results
open Rollup

module Full_tx_rollup = struct
  (*
    Used for testing purposes, and trying out new abstractions.
    Only tz1 txs.
    State is `encoding(keys) -> encoding(balances)`.
  *)
  
  type id = Signature.public_key
  type balance = Z.t

  type transaction = {
    destination : id ;
    amount : Z.t ;
  }

  type freeze = {
    beneficiary : Contract.t ;
    amount : Z.t ;
  }
  
  type operation =
    | Transaction of transaction
    | Freeze of freeze

  let encode_id : id -> bytes =
    fun (Public_key pk) -> Bls12_381.G1.to_bytes pk

  let decode_id_opt : bytes -> id option =
    fun bytes -> match Bls12_381.G1.of_bytes_opt bytes with
      | Some pk -> Some (Public_key pk)
      | None -> None

  let encode_balance_opt : balance -> bytes option = fun balance ->
    Data_encoding.(Binary.to_bytes z balance)

  let decode_balance_opt : bytes -> balance option = fun bytes ->
    match Data_encoding.Binary.of_bytes Data_encoding.z bytes with
    | Some balance -> Some balance
    | None -> None

  let force = function (* TODO: remove all uses. what to do when encoding fail? *)
    | Some x -> x
    | None -> raise (Failure "force")

  let tx_encoding = Data_encoding.(
      tup2
        (conv (encode_id) (fun x -> force @@ decode_id_opt x) bytes)
        z
    )

  let freeze_encoding = Data_encoding.(
      tup2
        Contract.encoding
        z
    )  
  
  let encoding : operation Data_encoding.t = Data_encoding.(
      union [
        case (Tag 0) ~title:"transaction" tx_encoding
          (function Transaction { destination = d ; amount = a } -> Some (d , a) | _ -> None)
          (fun (d , a) -> Transaction { destination = d ; amount = a })
        ;
        case (Tag 1) ~title:"freeze" freeze_encoding
          (function Freeze { beneficiary = b ; amount = a } -> Some (b , a) | _ -> None)
          (fun (b , a) -> Freeze { beneficiary = b ; amount = a })
        ;
      ]
    )
  
  let decode_opt : bytes -> operation option = fun bytes ->
    Data_encoding.Binary.of_bytes encoding bytes

  let encode_opt : operation -> bytes option = Data_encoding.Binary.to_bytes encoding

  open New_storage
  
  module Transition = struct

    let off_chain : source:Signature.public_key -> bytes -> M.t -> M.t tzresult =
      fun ~source raw_message ctxt ->
        let*? message =
          match decode_opt raw_message with
          | Some tx -> ok tx
          | None -> error (raise (Failure "bad decode")) (* TODO: add tzresult error *)
        in
        match message with
        | Transaction { destination ; amount } -> (
            let*? () =
              if Z.(geq amount zero)
              then ok ()
              else error (raise (Failure "amount < 0")) (* TODO: add tzresult error *)
            in
            let raw_source = encode_id source in
            let*? source_balance = try
                let raw_balance = M.get ctxt (key_of_bytes raw_source) in
                  match decode_balance_opt raw_balance with
                  | Some balance -> ok balance
                  | None -> error (raise (Failure "balance bad decode")) (* TODO: add tzresult error *)                
              with _ -> ok Z.zero
            in
            let*? () =
              if Z.(geq source_balance amount)
              then ok ()
              else error (raise (Failure "source_balance < amount")) (* TODO: add tzresult error *)
            in
            let raw_destination = encode_id destination in
            let*? destination_balance = try
                let raw_balance = M.get ctxt (key_of_bytes raw_destination) in
                  match decode_balance_opt raw_balance with
                  | Some balance -> ok balance
                  | None -> error (raise (Failure "balance bad decode")) (* TODO: add tzresult error *)
              with _ -> ok Z.zero
            in
            let new_destination_balance = Z.add destination_balance amount in
            let*? raw_new_destination_balance = match encode_balance_opt new_destination_balance with
              | Some bytes -> ok bytes
              | None -> error (raise (Failure "balance bad encode")) (* TODO: add tzresult error *)
            in
            let new_source_balance = Z.sub source_balance amount in
            let*? raw_new_source_balance = match encode_balance_opt new_source_balance with
              | Some bytes -> ok bytes
              | None -> error (raise (Failure "balance bad encode")) (* TODO: add tzresult error *)
            in
            let ctxt = M.set ctxt (key_of_bytes raw_source) raw_new_source_balance in
            let ctxt = M.set ctxt (key_of_bytes raw_destination) raw_new_destination_balance in
            ok ctxt
          )
        | Freeze { beneficiary = _ ; amount } -> (
            let*? () =
              if Z.(geq amount zero)
              then ok ()
              else error (raise (Failure "amount < 0")) (* TODO: add tzresult error *)
            in
            let raw_source = encode_id source in
            let*? source_balance = try
                let raw_balance = M.get ctxt (key_of_bytes raw_source) in
                match decode_balance_opt raw_balance with
                | Some balance -> ok balance
                | None -> error (raise (Failure "balance bad decode")) (* TODO: add tzresult error *)
              with _ -> ok Z.zero
            in
            let*? () =
              if Z.(geq source_balance amount)
              then ok ()
              else error (raise (Failure "source_balance < amount")) (* TODO: add tzresult error *)
            in
            let new_source_balance = Z.sub source_balance amount in
            let*? raw_new_source_balance = match encode_balance_opt new_source_balance with
              | Some bytes -> ok bytes
              | None -> error (raise (Failure "balance bad encode")) (* TODO: add tzresult error *)
            in
            let ctxt = M.set ctxt (key_of_bytes raw_source) raw_new_source_balance in
            ok ctxt
          )

  end
  
end
