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
    parameter : bytes ;
    events : Event.t list ;
    before_root : root ;
    after_root : root ;
  }
  
  type t = {
    micro_blocks : micro list ;
    tezos_level : Level_repr.t ;
    timestamp : Time_repr.t ;
  }

  let micro_encoding : micro Data_encoding.t = Data_encoding.(
      conv
        ( fun { parameter = p ; events = e ; before_root = br ; after_root = ar } ->
            ( p, e , br , ar) )
        ( fun ( p , e , br , ar) ->
            { parameter = p ; events = e ; before_root = br ; after_root = ar})
      @@ tup4 bytes (list Event.encoding) root_encoding root_encoding
    )

  let encoding : t Data_encoding.t = Data_encoding.(
      conv
        ( fun { micro_blocks = mb ; tezos_level = tl ; timestamp = t } ->
            ( mb , tl , t ) )
        ( fun ( mb , tl , t ) ->
            { micro_blocks = mb ; tezos_level = tl ; timestamp = t })
      @@ tup3 (list micro_encoding) Level_repr.encoding Time_repr.encoding
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
    parameter : bytes ;
    after_root : root ;
  }    
  
  type t = {
    micro_block_commitments : micro list ;
    rollup_id : Z.t ;
  }

  let micro_encoding : micro Data_encoding.t = Data_encoding.(
      conv
        (fun { parameter = p ; after_root = r } -> (p , r))
        (fun (p , r) -> { parameter = p ; after_root = r })
      @@ tup2 bytes root_encoding
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
  
  type t = {
    micro_block_index : micro_block_index ;
    state_trace : state_trace ;
  }

  let encoding : t Data_encoding.t = Data_encoding.(
      conv
        (fun { micro_block_index = mbi ; state_trace = st } -> ( mbi , st ))
        (fun ( mbi , st ) -> { micro_block_index = mbi ; state_trace = st })
      @@ tup2 micro_block_index_encoding state_trace_encoding
    )
end

type deposit = unit
type withdrawal = unit

module Block_rejection = struct
      
  type content =
  | Reject_micro_block of Micro_block_rejection.t (* A micro block was invalid *)

  let content_encoding : content Data_encoding.t = Data_encoding.(
      union [
        case
          "reject_micro_block"
          (Tag 0)
          Micro_block_rejection.encoding
          (function Reject_micro_block mbr -> Some mbr)
          (fun mbr -> Reject_micro_block mbr) ;
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
  val mem : key -> bool
  val get_hash : unit -> hash
end

module type ROLLUP = sig

  type operation
  val operation_encoding : operation Data_encoding.t
  
  module Make : functor (M : ROLLUP_STORAGE) -> sig

    val transition : source:Signature.public_key -> operation:operation -> unit
    (* val raw_transition : source:Signature.public_key -> message:bytes -> unit *)

  end
  
end

