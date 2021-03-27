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

module Signature = struct
  include Bls_signature
  include Make(struct
    let hash_function = Raw_hashes.sha256
  end)

end

open Signature

module Rollup_level = struct
  type t = Z.t
  let encoding : t Data_encoding.t = Data_encoding.z
end

module Signature_encoding = struct

  let encode_public_key : public_key -> bytes =
    fun (Public_key pk) -> Bls12_381.G1.to_bytes pk

  let decode_public_key_opt : bytes -> public_key option =
    fun bytes -> match Bls12_381.G1.of_bytes_opt bytes with
      | Some pk -> Some (Public_key pk)
      | None -> None

  let public_key_encoding : public_key Data_encoding.t =
    Data_encoding.(
      conv
        (fun (Public_key pk) -> Bls12_381.G1.to_bytes pk)
        (fun bytes -> match Bls12_381.G1.of_bytes_opt bytes with
           | Some pk -> Public_key pk
           | None -> raise (Failure "bad public_key encoding")
        )
        bytes
    )

  let encoding : signature Data_encoding.t =
    Data_encoding.(
      conv
        (fun (Signature s) -> Bls12_381.G2.to_bytes s)
        (fun bytes -> match Bls12_381.G2.of_bytes_opt bytes with
           | Some s -> Signature s
           | None -> raise (Failure "bad signature encoding")
        )
        bytes
    )

end

type transaction = {
  content : bytes ;
  signer : Signature.public_key ;
}

let transaction_encoding : transaction Data_encoding.t =
  Data_encoding.(
    conv
      (fun { content = c ; signer = s } -> (c , s))
      (fun (c , s) -> { content = c ; signer = s })
    @@ tup2 bytes Signature_encoding.public_key_encoding
  )

type root = Root of bytes

let root_encoding : root Data_encoding.t =
  Data_encoding.(
    conv (fun (Root r) -> r) (fun r -> Root r) bytes
  )

type rollup_kind =
  | Counter
let rollup_kind_encoding : rollup_kind Data_encoding.t = Data_encoding.(
    union [
      case ~title:"counter" (Tag 0) unit (function Counter -> Some ()) (fun () -> Counter) ;
    ]
  )

module Event = struct

  type freeze = {
    amount : Z.t ;
    beneficiary : Contract_repr.t ;
  }

  type message = bytes

  type t =
    | Freeze of freeze
    | Message of message

  let freeze_encoding : freeze Data_encoding.t = Data_encoding.(
      conv
        (fun { amount ; beneficiary } -> ( amount , beneficiary ))
        (fun ( amount , beneficiary ) -> { amount ; beneficiary })
      @@ tup2 z Contract_repr.encoding
    )
    
  let message_encoding : message Data_encoding.t = Data_encoding.bytes
  
  let encoding : t Data_encoding.t = Data_encoding.(
      union [
        case (Tag 0) ~title:"event_freeze" freeze_encoding
          (function Freeze f -> Some f | _ -> None)
          (fun f -> Freeze f) ;
        case (Tag 1) ~title:"event_message" message_encoding
          (function Message m -> Some m | _ -> None)
          (fun m -> Message m) ;
      ]
    )
  
end

module Rollup_onchain_content = struct
  type t = {
    operator : Contract_repr.t ;
    level : Rollup_level.t ;
    kind : rollup_kind ;
  }

  let encoding : t Data_encoding.t = Data_encoding.(
      conv
        ( fun { operator = o ; level = l ; kind = r } -> (o , l , r))
        ( fun (o , l , r) -> { operator = o ; level = l ; kind = r })
      @@ tup3 Contract_repr.encoding z rollup_kind_encoding
    )
end

module Block_onchain_content = struct
  
  type micro = {
    transactions : transaction list ;
    aggregated_signature : Signature.signature ;
    events : Event.t list ;
    before_root : root ;
    after_root : root ;
  }
  
  type t = {
    micro_blocks : micro list ;
    tezos_level : Level_repr.t ;
  }

  let micro_encoding : micro Data_encoding.t = Data_encoding.(
      conv
        ( fun { transactions = t ; aggregated_signature = a ; events = e ; before_root = br ; after_root = ar } ->
            ( t , a , e , br , ar) )
        ( fun ( t , a , e , br , ar) ->
            { transactions = t ; aggregated_signature = a ; events = e ; before_root = br ; after_root = ar})
      @@ tup5 (list transaction_encoding) Signature_encoding.encoding (list Event.encoding) root_encoding root_encoding
    )

  let encoding : t Data_encoding.t = Data_encoding.(
      conv
        ( fun { micro_blocks = mb ; tezos_level = tl } ->
            ( mb , tl) )
        ( fun ( mb , tl) ->
            { micro_blocks = mb ; tezos_level = tl })
      @@ tup2 (list micro_encoding) Level_repr.encoding
    )

end

module Rollup_creation = struct
  type t = {
    operator : Contract_repr.t ;
    kind : rollup_kind ;
  }

  let encoding : t Data_encoding.t = Data_encoding.(
      conv
        (fun { operator = o ; kind = k } -> ( o , k ))
        (fun ( o , k ) -> { operator = o ; kind = k })
        @@ tup2 Contract_repr.encoding rollup_kind_encoding
    )
end

module Block_commitment = struct
  type micro = {
    transactions : transaction list ;
    aggregated_signature : Signature.signature ;
    after_root : root ;
  }    
  
  type t = {
    micro_block_commitments : micro list ;
    rollup_id : Z.t ;
  }

  let micro_encoding : micro Data_encoding.t = Data_encoding.(
      conv
        (fun { transactions = t ; aggregated_signature = a ; after_root = r } -> (t , a , r))
        (fun (t , a , r) -> { transactions = t ; aggregated_signature = a ; after_root = r })
      @@ tup3 (list transaction_encoding) Signature_encoding.encoding root_encoding
    )

  let encoding : t Data_encoding.t = Data_encoding.(
      conv
        (fun { micro_block_commitments = m ; rollup_id = i } -> (m , i))
        (fun (m , i) -> { micro_block_commitments = m ; rollup_id = i })
      @@ tup2 (list micro_encoding) z
    )
end

type micro_block_index = int
let micro_block_index_encoding : micro_block_index Data_encoding.t = Data_encoding.int31

type transaction_index = int
let transaction_index_encoding : transaction_index Data_encoding.t = Data_encoding.int31


type state_trace = New_storage.stream
let state_trace_encoding : state_trace Data_encoding.t = Data_encoding.(list bytes)

module Micro_block_rejection = struct
  
  type invalid_signature = unit
  let invalid_signature_encoding : invalid_signature Data_encoding.t =
    Data_encoding.unit

  type gas_overflow = state_trace
  let gas_overflow_encoding : gas_overflow Data_encoding.t = state_trace_encoding

  type state_overflow = state_trace
  let state_overflow_encoding : state_overflow Data_encoding.t = state_trace_encoding
  
  type invalid_block_id = transaction_index
  let invalid_block_id_encoding : invalid_block_id Data_encoding.t =
    transaction_index_encoding

  type rejection_content =
    | Gas_overflow of gas_overflow (* A micro block goes over the gas limit *)
    | State_overflow of state_overflow (* A micro block requires too much state to check *)
    | Invalid_signature of invalid_signature (* A tx has an invalid signature (public key, tx and sig don't match) *)
    | Invalid_state_hash of state_trace   (* The resulting state hash is not correct *)
    | Persisting_state_too_big of state_trace (* Evaluation results in persisting a leaf too big in the store *)
    | Invalid_block_id of invalid_block_id (* A tx signed a different rollup block id *)
    (* TODO: | Invalid_event_hash *) 

  let rejection_content_encoding : rejection_content Data_encoding.t = Data_encoding.(
      union [
        case (Tag 0) ~title:"gas_overflow"
          gas_overflow_encoding
          (function Gas_overflow go -> Some go | _ -> None)
          (fun go -> Gas_overflow go) ;
        case (Tag 1) ~title:"state_overflow"
          state_overflow_encoding
          (function State_overflow so -> Some so | _ -> None)
          (fun so -> State_overflow so) ;
        case (Tag 2) ~title:"invalid_signature"
          invalid_signature_encoding
          (function Invalid_signature is -> Some is | _ -> None)
          (fun is -> Invalid_signature is) ;
        case (Tag 3) ~title:"invalid_state_hash"
          state_trace_encoding
          (function Invalid_state_hash st -> Some st | _ -> None)
          (fun st -> Invalid_state_hash st) ;
        case (Tag 4) ~title:"persisting_state_too_big"
          state_trace_encoding
          (function Persisting_state_too_big st -> Some st | _ -> None)
          (fun st -> Persisting_state_too_big st) ;
        case (Tag 5) ~title:"invalid_block_id"
          invalid_block_id_encoding
          (function Invalid_block_id ibi -> Some ibi | _ -> None)
          (fun ibi -> Invalid_block_id ibi) ;
      ]
    )
  
  type t = {
    micro_block_index : micro_block_index ;
    rejection_content : rejection_content ;
  }

  let encoding : t Data_encoding.t = Data_encoding.(
      conv
        (fun { micro_block_index = mbi ; rejection_content = rc } -> ( mbi , rc ))
        (fun ( mbi , rc ) -> { micro_block_index = mbi ; rejection_content = rc })
      @@ tup2 micro_block_index_encoding rejection_content_encoding
    )
end

type deposit = unit
type withdrawal = unit

module Block_rejection = struct
  type double_injection =
    (micro_block_index * transaction_index) * (micro_block_index * transaction_index)
  let double_injection_encoding : double_injection Data_encoding.t = Data_encoding.(
      conv
        (fun ((mb_i_a , tx_i_a) , (mb_i_b , tx_i_b)) -> (mb_i_a , tx_i_a , mb_i_b , tx_i_b))
        (fun (mb_i_a , tx_i_a , mb_i_b , tx_i_b) -> ((mb_i_a , tx_i_a) , (mb_i_b , tx_i_b)))
      @@ Data_encoding.tup4
        transaction_index_encoding transaction_index_encoding
        transaction_index_encoding transaction_index_encoding
    )
      
  type content =
  | Reject_micro_block of Micro_block_rejection.t (* A micro block was invalid *)
  | Double_injection of double_injection (* A tx was included twice in a block *)

  let content_encoding : content Data_encoding.t = Data_encoding.(
      union [
        case
          "reject_micro_block"
          (Tag 0)
          Micro_block_rejection.encoding
          (function Reject_micro_block mbr -> Some mbr | _ -> None)
          (fun mbr -> Reject_micro_block mbr) ;
        case (Tag 1) ~title:"double_injection"
          double_injection_encoding
          (function Double_injection di -> Some di | _ -> None)
          (fun di -> Double_injection di) ;
      ]
    )

  type t = {
    rollup_id : Z.t ;
    level : Rollup_level.t ;
    rejection_content : content ;
  }
  let encoding : t Data_encoding.t = Data_encoding.(
      conv
        (fun { level = l ; rollup_id = ri ; rejection_content = rc } -> ( l , ri , rc ))
        (fun ( l , ri , rc ) -> { level = l ; rollup_id = ri ; rejection_content = rc })
      @@ tup3 z z content_encoding
    )
  
end

type operation_content =
  | Create_rollup of Rollup_creation.t
  | Commit_block of Block_commitment.t
  | Reject_block of Block_rejection.t
  | Deposit of deposit
  | Withdraw of withdrawal

let encoding : operation_content Data_encoding.t =
  Data_encoding.(
    let case_commit_block =
      case
        "commit_block"
        (Tag 0)
        Block_commitment.encoding
        (function Commit_block cb -> Some cb | _ -> None)
        (fun cb -> Commit_block cb)
    in
    let case_reject_block =
      case
        "reject_block"
        (Tag 1)
        Block_rejection.encoding
        (function Reject_block br -> Some br | _ -> None)
        (fun br -> Reject_block br)
    in
    let case_create_rollup =
      case
        "create_rollup"
        (Tag 2)
        Rollup_creation.encoding
        (function Create_rollup cr -> Some cr | _ -> None)
        (fun cr -> Create_rollup cr)
    in
    let case_deposit =
      case
        "deposit"
        (Tag 3)
        unit
        (function Deposit () -> Some () | _ -> None)
        (fun () -> Deposit ())
    in
    let case_withdraw =
      case
        "withdraw"
        (Tag 4)
        unit
        (function Withdraw () -> Some () | _ -> None)
        (fun () -> Withdraw ())
    in
    union [case_commit_block; case_reject_block; case_create_rollup; case_deposit; case_withdraw])

module Chain_state = struct
  type t = unit
  let empty : t = ()
  let encoding = Data_encoding.unit
                   
end

module NS = New_storage
module M = NS.Patricia

module type ROLLUP_STORAGE = sig
  open NS
  type t
  val empty : t
  val get_full : unit -> t
  val set_full : t -> unit
  val get : key -> value
  val set : key -> value -> unit
  val get_hash : unit -> hash
end

module type ROLLUP = sig

  type operation
  val operation_encoding : operation Data_encoding.t
  
  module Make : functor (M : ROLLUP_STORAGE) -> sig

    val init : unit -> unit
    val transition : source:Signature.public_key -> operation:operation -> unit
    (* val raw_transition : source:Signature.public_key -> message:bytes -> unit *)

  end
  
end

