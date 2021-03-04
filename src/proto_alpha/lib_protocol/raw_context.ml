(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(*
  [TODO] temprarily open Lens
  should be removed after adding Lens into protocol environment.
 *)

open Lens

let[@inline] (|>|) l1 l2 = compose l2 l1

let[@inline] lensappRes f lens blob =
  (* f (blob |. lens) >|? (fun x -> (lens ^= x) @@ blob) *)
  f (lens.get blob) >|? (fun x -> lens.set x blob)

let[@inline] lensappLwt f lens blob =
  f (lens.get blob) >|= (fun x -> lens.set x blob)

module Int_set = Set.Make (Compare.Int)

(*

   Gas levels maintainance
   =======================

   The context maintains two levels of gas, one corresponds to the gas
   available for the current operation while the other is the gas
   available for the current block.

   When gas is consumed, we must morally decrement these two levels to
   check if one of them hits zero. However, since these decrements are
   the same on both levels, it is not strictly necessary to update the
   two levels: we can simply maintain the minimum of both levels in a
   [gas_counter]. The meaning of [gas_counter] is denoted by
   [gas_counter_status]: *)

type gas_counter_status =
  (* When the operation gas is unaccounted: *)
  | Unlimited_operation_gas
  (* When the operation gas level is the minimum: *)
  | Count_operation_gas of {block_gas_delta : Gas_limit_repr.Arith.fp}
  (* When the block gas level is the minimum. *)
  | Count_block_gas of {operation_gas_delta : Gas_limit_repr.Arith.fp}

(*
   In each case, we keep enough information in [gas_counter_status] to
   reconstruct the level that is not represented by [gas_counter]. In
   the gas [Unlimited_operation_gas], the block gas level is stored
   in [gas_counter].

   [Raw_context] interface provides two accessors for the operation
   gas level and the block gas level. These accessors compute these values
   on-the-fly based on the current value of [gas_counter] and
   [gas_counter_status].

   A layered context
   =================

   Updating the context [gas_counter] is a critical routine called
   very frequently by the operations performed by the protocol.
   On the contrary, other fields are less frequently updated.

   In a previous version of the context datatype definition, all
   the fields were represented at the toplevel. To update the
   [gas_counter], we had to copy ~25 fields (that is 200 bytes).

   With the following layered representation, we only have to
   copy 2 fields (16 bytes) during [gas_counter] update. This
   has a significant impact on the Michelson runtime efficiency.

   Here are the fields on the [back] of the context:

 *)
type back = {
  context : Context.t;
  constants : Constants_repr.parametric;
  first_level : Raw_level_repr.t;
  level : Level_repr.t;
  predecessor_timestamp : Time.t;
  timestamp : Time.t;
  fitness : Int64.t;
  deposits : Tez_repr.t Signature.Public_key_hash.Map.t;
  included_endorsements : int;
  allowed_endorsements :
    (Signature.Public_key.t * int list * bool) Signature.Public_key_hash.Map.t;
  fees : Tez_repr.t;
  rewards : Tez_repr.t;
  storage_space_to_pay : Z.t option;
  allocated_contracts : int option;
  origination_nonce : Contract_repr.origination_nonce option;
  temporary_lazy_storage_ids : Lazy_storage_kind.Temp_ids.t;
  internal_nonce : int;
  internal_nonces_used : Int_set.t;
  gas_counter_status : gas_counter_status;
}

(* internal lenses for back *)

let _lens_back_context = {
    get = (fun x -> x.context) ;
    set = (fun v x -> {x with context = v})
  }
let _lens_back_constants = {
    get = (fun x -> x.constants) ;
    set = (fun v x -> {x with constants = v})
  }
let _lens_back_first_level = {
    get = (fun x -> x.first_level) ;
    set = (fun v x -> {x with first_level = v})
  }
let _lens_back_level = {
    get = (fun x -> x.level) ;
    set = (fun v x -> {x with level = v})
  }
let _lens_back_predecessor_timestamp = {
    get = (fun x -> x.predecessor_timestamp) ;
    set = (fun v x -> {x with predecessor_timestamp = v})
  }
let _lens_back_timestamp = {
    get = (fun x -> x.timestamp) ;
    set = (fun v x -> {x with timestamp = v})
  }
let _lens_back_fitness = {
    get = (fun x -> x.fitness) ;
    set = (fun v x -> {x with fitness = v})
  }
let _lens_back_deposits = {
    get = (fun x -> x.deposits) ;
    set = (fun v x -> {x with deposits = v})
  }
let _lens_back_included_endorsements = {
    get = (fun x -> x.included_endorsements) ;
    set = (fun v x -> {x with included_endorsements = v})
  }
let _lens_back_allowed_endorsements = {
    get = (fun x -> x.allowed_endorsements) ;
    set = (fun v x -> {x with allowed_endorsements = v})
  }
let _lens_back_fees = {
    get = (fun x -> x.fees) ;
    set = (fun v x -> {x with fees = v})
  }
let _lens_back_rewards = {
    get = (fun x -> x.rewards) ;
    set = (fun v x -> {x with rewards = v})
  }
let _lens_back_storage_space_to_pay = {
    get = (fun x -> x.storage_space_to_pay) ;
    set = (fun v x -> {x with storage_space_to_pay = v})
  }
let _lens_back_allocated_contracts = {
    get = (fun x -> x.allocated_contracts) ;
    set = (fun v x -> {x with allocated_contracts = v})
  }
let _lens_back_origination_nonce = {
    get = (fun x -> x.origination_nonce) ;
    set = (fun v x -> {x with origination_nonce = v})
  }
let _lens_back_temporary_lazy_storage_ids = {
    get = (fun x -> x.temporary_lazy_storage_ids) ;
    set = (fun v x -> {x with temporary_lazy_storage_ids = v})
  }
let _lens_back_internal_nonce = {
    get = (fun x -> x.internal_nonce) ;
    set = (fun v x -> {x with internal_nonce = v})
  }
let _lens_back_internal_nonces_used = {
    get = (fun x -> x.internal_nonces_used) ;
    set = (fun v x -> {x with internal_nonces_used = v})
  }
let _lens_back_gas_counter_status = {
    get = (fun x -> x.gas_counter_status) ;
    set = (fun v x -> {x with gas_counter_status = v})
  }

(*

   The context is simply a record with two fields which
   limits the cost of updating the [gas_counter].

*)
type t = {gas_counter : Gas_limit_repr.Arith.fp; back : back}

type root = t

(* internal lenses for root *)

let _lens_root_gas_counter = {
    get = (fun x -> x.gas_counter) ;
    set = fun v x -> { x with gas_counter = v }
  }

let _lens_root_back = {
    get = (fun x -> x.back) ;
    set = fun v x -> { x with back = v }
  }

(*

   Context fields accessors
   ========================

   To have the context related code more robust to evolutions,
   we introduce accessors to get and to update the context
   components.

*)

(* context accessing lens *)

(* let lens_back = _lens_root_back *)

let lens_gas_counter = _lens_root_gas_counter

let lens_context = _lens_root_back |>| _lens_back_context

let lens_constants = _lens_root_back |>| _lens_back_constants

let lens_first_level = _lens_root_back |>| _lens_back_first_level

let lens_level = _lens_root_back |>| _lens_back_level

let lens_predecessor_timestamp =
  _lens_root_back |>| _lens_back_predecessor_timestamp

let lens_timestamp = _lens_root_back |>| _lens_back_timestamp

let lens_fitness = _lens_root_back |>| _lens_back_fitness

let lens_deposits = _lens_root_back |>| _lens_back_deposits

let lens_included_endorsements =
  _lens_root_back |>| _lens_back_included_endorsements

let lens_allowed_endorsements =
  _lens_root_back |>| _lens_back_allowed_endorsements

let lens_fees = _lens_root_back |>| _lens_back_fees

let lens_rewards = _lens_root_back |>| _lens_back_rewards

let lens_storage_space_to_pay =
  _lens_root_back |>| _lens_back_storage_space_to_pay

let lens_allocated_contracts =
  _lens_root_back |>| _lens_back_allocated_contracts

let lens_origination_nonce = _lens_root_back |>| _lens_back_origination_nonce

let lens_temporary_lazy_storage_ids =
  _lens_root_back |>| _lens_back_temporary_lazy_storage_ids

let lens_internal_nonce = _lens_root_back |>| _lens_back_internal_nonce

let lens_internal_nonces_used =
  _lens_root_back |>| _lens_back_internal_nonces_used

let lens_gas_counter_status = _lens_root_back |>| _lens_back_gas_counter_status

(* explicit accessing functions *)

let included_endorsements = lens_included_endorsements.get
let allowed_endorsements = lens_allowed_endorsements.get
let first_level = lens_first_level.get
let constants = lens_constants.get
let current_fitness = lens_fitness.get
let current_timestamp = lens_timestamp.get
let predecessor_timestamp = lens_predecessor_timestamp.get
let current_level = lens_level.get
let recover = lens_context.get

let record_endorsement ctxt k =
  match
    Signature.Public_key_hash.Map.find_opt k
      (lens_allowed_endorsements.get ctxt)
  with
  | None ->
      assert false
  | Some (_, _, true) ->
      assert false (* right already used *)
  | Some (d, s, false) ->
      let ctxt =
        modify lens_included_endorsements (fun x -> x + List.length s) ctxt
      in
      lens_allowed_endorsements.set
        (Signature.Public_key_hash.Map.add
           k
           (d, s, true)
           (lens_allowed_endorsements.get ctxt))
        ctxt

let init_endorsements ctxt allowed_endorsements' =
  if Signature.Public_key_hash.Map.is_empty allowed_endorsements' then
    assert false (* can't initialize to empty *)
  else if Signature.Public_key_hash.Map.is_empty
            (lens_allowed_endorsements.get ctxt)
  then lens_allowed_endorsements.set allowed_endorsements' ctxt
  else assert false

type error += Too_many_internal_operations (* `Permanent *)

type error += Block_quota_exceeded (* `Temporary *)

type error += Operation_quota_exceeded (* `Temporary *)

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"too_many_internal_operations"
    ~title:"Too many internal operations"
    ~description:
      "A transaction exceeded the hard limit of internal operations it can emit"
    empty
    (function Too_many_internal_operations -> Some () | _ -> None)
    (fun () -> Too_many_internal_operations) ;
  register_error_kind
    `Temporary
    ~id:"gas_exhausted.operation"
    ~title:"Gas quota exceeded for the operation"
    ~description:
      "A script or one of its callee took more time than the operation said \
       it would"
    empty
    (function Operation_quota_exceeded -> Some () | _ -> None)
    (fun () -> Operation_quota_exceeded) ;
  register_error_kind
    `Temporary
    ~id:"gas_exhausted.block"
    ~title:"Gas quota exceeded for the block"
    ~description:
      "The sum of gas consumed by all the operations in the block exceeds the \
       hard gas limit per block"
    empty
    (function Block_quota_exceeded -> Some () | _ -> None)
    (fun () -> Block_quota_exceeded)

let fresh_internal_nonce ctxt =
  let inonce = lens_internal_nonce.get ctxt in
  if Compare.Int.(inonce >= 65_535) then
    error Too_many_internal_operations
  else
    ok (modify lens_internal_nonce (fun x -> x + 1) ctxt, inonce)

let reset_internal_nonce ctxt =
  let ctxt = lens_internal_nonce.set 0 ctxt in
  lens_internal_nonces_used.set Int_set.empty ctxt

let record_internal_nonce ctxt k =
  modify lens_internal_nonces_used (Int_set.add k) ctxt

let internal_nonce_already_recorded ctxt k =
  Int_set.mem k (lens_internal_nonces_used.get ctxt)

let set_current_fitness ctxt fitness = lens_fitness.set fitness ctxt

let add_fees ctxt fees' =
  (* Tez_repr.(fees ctxt +? fees') >|? update_fees ctxt *)
  lensappRes (Tez_repr.(+?) fees') lens_fees ctxt

let add_rewards ctxt rewards' =
  lensappRes (fun x -> Tez_repr.(x +? rewards')) lens_rewards ctxt

let add_deposit ctxt delegate deposit =
  let open Signature.Public_key_hash.Map in
  let previous =
    match find_opt delegate (lens_deposits.get ctxt) with
    | Some tz ->
        tz
    | None ->
        Tez_repr.zero
  in
  Tez_repr.(previous +? deposit)
  >|? fun deposit ->
      modify lens_deposits (add delegate deposit) ctxt

let get_deposits = lens_deposits.get

let get_rewards = lens_rewards.get

(*let get_fees = fees*)
let get_fees ctx = lens_fees.get ctx

type error += Undefined_operation_nonce (* `Permanent *)

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"undefined_operation_nonce"
    ~title:"Ill timed access to the origination nonce"
    ~description:
      "An origination was attempted out of the scope of a manager operation"
    empty
    (function Undefined_operation_nonce -> Some () | _ -> None)
    (fun () -> Undefined_operation_nonce)

let init_origination_nonce ctxt operation_hash =
  let origination_nonce =
    Some (Contract_repr.initial_origination_nonce operation_hash)
  in
  lens_origination_nonce.set origination_nonce ctxt

let increment_origination_nonce ctxt =
  match lens_origination_nonce.get ctxt with
  | None ->
      error Undefined_operation_nonce
  | Some cur_origination_nonce ->
      let origination_nonce =
        Some (Contract_repr.incr_origination_nonce cur_origination_nonce)
      in
      let ctxt = lens_origination_nonce.set origination_nonce ctxt in
      ok (ctxt, cur_origination_nonce)

let origination_nonce ctxt =
  match lens_origination_nonce.get ctxt with
  | None ->
      error Undefined_operation_nonce
  | Some origination_nonce ->
      ok origination_nonce

let unset_origination_nonce ctxt = lens_origination_nonce.set None ctxt

type error += Gas_limit_too_high (* `Permanent *)

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"gas_limit_too_high"
    ~title:"Gas limit out of protocol hard bounds"
    ~description:"A transaction tried to exceed the hard limit on gas"
    empty
    (function Gas_limit_too_high -> Some () | _ -> None)
    (fun () -> Gas_limit_too_high)

let gas_level ctxt =
  let open Gas_limit_repr in
  match lens_gas_counter_status.get ctxt with
  | Unlimited_operation_gas ->
      Unaccounted
  | Count_block_gas {operation_gas_delta} ->
      Limited {remaining =
                 Arith.(add (lens_gas_counter.get ctxt) operation_gas_delta)}
  | Count_operation_gas _ ->
      Limited {remaining = lens_gas_counter.get ctxt}

let block_gas_level ctxt =
  let open Gas_limit_repr in
  match lens_gas_counter_status.get ctxt with
  | Unlimited_operation_gas | Count_block_gas _ ->
      lens_gas_counter.get ctxt
  | Count_operation_gas {block_gas_delta} ->
      Arith.(add (lens_gas_counter.get ctxt) block_gas_delta)

let check_gas_limit ctxt (remaining : 'a Gas_limit_repr.Arith.t) =
  if
    Gas_limit_repr.Arith.(
      remaining > (lens_constants.get ctxt).hard_gas_limit_per_operation
      || remaining < zero)
  then error Gas_limit_too_high
  else ok_unit

let set_gas_limit ctxt (remaining : 'a Gas_limit_repr.Arith.t) =
  let open Gas_limit_repr in
  let remaining = Arith.fp remaining in
  let block_gas = block_gas_level ctxt in
  let (gas_counter_status', gas_counter') =
    if Arith.(remaining < block_gas) then
      let block_gas_delta = Arith.sub block_gas remaining in
      (Count_operation_gas {block_gas_delta}, remaining)
    else
      let operation_gas_delta = Arith.sub remaining block_gas in
      (Count_block_gas {operation_gas_delta}, block_gas)
  in
  ctxt
  |> lens_gas_counter_status.set gas_counter_status'
  |> lens_gas_counter.set gas_counter'

let set_gas_unlimited ctxt =
  let block_gas = block_gas_level ctxt in
  ctxt
  |> lens_gas_counter_status.set Unlimited_operation_gas
  |> lens_gas_counter.set block_gas

let is_gas_unlimited ctxt =
  match lens_gas_counter_status.get ctxt with
  | Unlimited_operation_gas -> true
  | _ -> false

let is_counting_block_gas ctxt =
  match lens_gas_counter_status.get ctxt with
  | Count_block_gas _ -> true
  | _ -> false

let consume_gas ctxt cost =
  if is_gas_unlimited ctxt then ok ctxt
  else
    match Gas_limit_repr.raw_consume (lens_gas_counter.get ctxt) cost with
    | Some gas_counter' ->
        Ok (lens_gas_counter.set gas_counter' ctxt)
    | None ->
        if is_counting_block_gas ctxt then error Block_quota_exceeded
        else error Operation_quota_exceeded

let check_enough_gas ctxt cost = consume_gas ctxt cost >>? fun _ -> ok_unit

let gas_consumed ~since ~until =
  match (gas_level since, gas_level until) with
  | (Limited {remaining = before}, Limited {remaining = after}) ->
      Gas_limit_repr.Arith.sub before after
  | (_, _) ->
      Gas_limit_repr.Arith.zero

let init_storage_space_to_pay ctxt =
  match lens_storage_space_to_pay.get ctxt with
  | Some _ ->
      assert false
  | None ->
      let ctxt = lens_storage_space_to_pay.set (Some Z.zero) ctxt in
      lens_allocated_contracts.set (Some 0) ctxt

let clear_storage_space_to_pay ctxt =
  match ( lens_storage_space_to_pay.get ctxt
        , lens_allocated_contracts.get ctxt) with
  | (None, _) | (_, None) ->
      assert false
  | (Some storage_space_to_pay, Some allocated_contracts) ->
     ctxt
     |> lens_storage_space_to_pay.set None
     |> lens_allocated_contracts.set None
     |> fun ctxt -> (ctxt, storage_space_to_pay, allocated_contracts)

let update_storage_space_to_pay ctxt n =
  match lens_storage_space_to_pay.get ctxt with
  | None ->
      assert false
  | Some storage_space_to_pay ->
      lens_storage_space_to_pay.set (Some (Z.add n storage_space_to_pay)) ctxt

let update_allocated_contracts_count ctxt =
  match lens_allocated_contracts.get ctxt with
  | None ->
      assert false
  | Some allocated_contracts ->
      lens_allocated_contracts.set (Some (succ allocated_contracts)) ctxt

type missing_key_kind = Get | Set | Del | Copy

type storage_error =
  | Incompatible_protocol_version of string
  | Missing_key of string list * missing_key_kind
  | Existing_key of string list
  | Corrupted_data of string list

let storage_error_encoding =
  let open Data_encoding in
  union
    [ case
        (Tag 0)
        ~title:"Incompatible_protocol_version"
        (obj1 (req "incompatible_protocol_version" string))
        (function Incompatible_protocol_version arg -> Some arg | _ -> None)
        (fun arg -> Incompatible_protocol_version arg);
      case
        (Tag 1)
        ~title:"Missing_key"
        (obj2
           (req "missing_key" (list string))
           (req
              "function"
              (string_enum
                 [("get", Get); ("set", Set); ("del", Del); ("copy", Copy)])))
        (function Missing_key (key, f) -> Some (key, f) | _ -> None)
        (fun (key, f) -> Missing_key (key, f));
      case
        (Tag 2)
        ~title:"Existing_key"
        (obj1 (req "existing_key" (list string)))
        (function Existing_key key -> Some key | _ -> None)
        (fun key -> Existing_key key);
      case
        (Tag 3)
        ~title:"Corrupted_data"
        (obj1 (req "corrupted_data" (list string)))
        (function Corrupted_data key -> Some key | _ -> None)
        (fun key -> Corrupted_data key) ]

let pp_storage_error ppf = function
  | Incompatible_protocol_version version ->
      Format.fprintf
        ppf
        "Found a context with an unexpected version '%s'."
        version
  | Missing_key (key, Get) ->
      Format.fprintf ppf "Missing key '%s'." (String.concat "/" key)
  | Missing_key (key, Set) ->
      Format.fprintf
        ppf
        "Cannot set undefined key '%s'."
        (String.concat "/" key)
  | Missing_key (key, Del) ->
      Format.fprintf
        ppf
        "Cannot delete undefined key '%s'."
        (String.concat "/" key)
  | Missing_key (key, Copy) ->
      Format.fprintf
        ppf
        "Cannot copy undefined key '%s'."
        (String.concat "/" key)
  | Existing_key key ->
      Format.fprintf
        ppf
        "Cannot initialize defined key '%s'."
        (String.concat "/" key)
  | Corrupted_data key ->
      Format.fprintf
        ppf
        "Failed to parse the data at '%s'."
        (String.concat "/" key)

type error += Storage_error of storage_error

let () =
  register_error_kind
    `Permanent
    ~id:"context.storage_error"
    ~title:"Storage error (fatal internal error)"
    ~description:
      "An error that should never happen unless something has been deleted or \
       corrupted in the database."
    ~pp:(fun ppf err ->
      Format.fprintf ppf "@[<v 2>Storage error:@ %a@]" pp_storage_error err)
    storage_error_encoding
    (function Storage_error err -> Some err | _ -> None)
    (fun err -> Storage_error err)

let storage_error err = error (Storage_error err)

(* Initialization *********************************************************)

(* This key should always be populated for every version of the
   protocol.  It's absence meaning that the context is empty. *)
let version_key = ["version"]

(* This value is set by the snapshot_alpha.sh script, don't change it. *)
let version_value = "alpha_current"

let version = "v1"

let first_level_key = [version; "first_level"]

let constants_key = [version; "constants"]

let protocol_param_key = ["protocol_parameters"]

let get_first_level ctxt =
  Context.find ctxt first_level_key
  >|= function
  | None ->
      storage_error (Missing_key (first_level_key, Get))
  | Some bytes -> (
    match Data_encoding.Binary.of_bytes Raw_level_repr.encoding bytes with
    | None ->
        storage_error (Corrupted_data first_level_key)
    | Some level ->
        ok level )

let set_first_level ctxt level =
  let bytes =
    Data_encoding.Binary.to_bytes_exn Raw_level_repr.encoding level
  in
  Context.add ctxt first_level_key bytes >|= ok

type error += Failed_to_parse_parameter of bytes

type error += Failed_to_decode_parameter of Data_encoding.json * string

let () =
  register_error_kind
    `Temporary
    ~id:"context.failed_to_parse_parameter"
    ~title:"Failed to parse parameter"
    ~description:"The protocol parameters are not valid JSON."
    ~pp:(fun ppf bytes ->
      Format.fprintf
        ppf
        "@[<v 2>Cannot parse the protocol parameter:@ %s@]"
        (Bytes.to_string bytes))
    Data_encoding.(obj1 (req "contents" bytes))
    (function Failed_to_parse_parameter data -> Some data | _ -> None)
    (fun data -> Failed_to_parse_parameter data) ;
  register_error_kind
    `Temporary
    ~id:"context.failed_to_decode_parameter"
    ~title:"Failed to decode parameter"
    ~description:"Unexpected JSON object."
    ~pp:(fun ppf (json, msg) ->
      Format.fprintf
        ppf
        "@[<v 2>Cannot decode the protocol parameter:@ %s@ %a@]"
        msg
        Data_encoding.Json.pp
        json)
    Data_encoding.(obj2 (req "contents" json) (req "error" string))
    (function
      | Failed_to_decode_parameter (json, msg) -> Some (json, msg) | _ -> None)
    (fun (json, msg) -> Failed_to_decode_parameter (json, msg))

let get_proto_param ctxt =
  Context.find ctxt protocol_param_key
  >>= function
  | None ->
      failwith "Missing protocol parameters."
  | Some bytes -> (
    match Data_encoding.Binary.of_bytes Data_encoding.json bytes with
    | None ->
        fail (Failed_to_parse_parameter bytes)
    | Some json -> (
        Context.remove ctxt protocol_param_key
        >|= fun ctxt ->
        match Data_encoding.Json.destruct Parameters_repr.encoding json with
        | exception (Data_encoding.Json.Cannot_destruct _ as exn) ->
            Format.kasprintf
              failwith
              "Invalid protocol_parameters: %a %a"
              (fun ppf -> Data_encoding.Json.print_error ppf)
              exn
              Data_encoding.Json.pp
              json
        | param ->
            ok (param, ctxt) ) )

let add_constants ctxt constants =
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      Constants_repr.parametric_encoding
      constants
  in
  Context.add ctxt constants_key bytes

let get_constants ctxt =
  Context.find ctxt constants_key
  >|= function
  | None ->
      failwith "Internal error: cannot read constants in context."
  | Some bytes -> (
    match
      Data_encoding.Binary.of_bytes Constants_repr.parametric_encoding bytes
    with
    | None ->
        failwith "Internal error: cannot parse constants in context."
    | Some constants ->
        ok constants )

let patch_constants ctxt f =
  let constants = f (lens_constants.get ctxt) in
  add_constants (lens_context.get ctxt) constants
  >|= fun context ->
  ctxt
  |> lens_context.set context
  |> lens_constants.set constants

let check_inited ctxt =
  Context.find ctxt version_key
  >|= function
  | None ->
      failwith "Internal error: un-initialized context."
  | Some bytes ->
      let s = Bytes.to_string bytes in
      if Compare.String.(s = version_value) then ok_unit
      else storage_error (Incompatible_protocol_version s)

let prepare ~level ~predecessor_timestamp ~timestamp ~fitness ctxt =
  Raw_level_repr.of_int32 level
  >>?= fun level ->
  Fitness_repr.to_int64 fitness
  >>?= fun fitness ->
  check_inited ctxt
  >>=? fun () ->
  get_constants ctxt
  >>=? fun constants ->
  get_first_level ctxt
  >|=? fun first_level ->
  let level =
    Level_repr.level_from_raw
      ~first_level
      ~blocks_per_cycle:constants.Constants_repr.blocks_per_cycle
      ~blocks_per_commitment:constants.Constants_repr.blocks_per_commitment
      level
  in
  {
    gas_counter =
      Gas_limit_repr.Arith.fp constants.Constants_repr.hard_gas_limit_per_block;
    back =
      {
        context = ctxt;
        constants;
        level;
        predecessor_timestamp;
        timestamp;
        fitness;
        first_level;
        allowed_endorsements = Signature.Public_key_hash.Map.empty;
        included_endorsements = 0;
        fees = Tez_repr.zero;
        rewards = Tez_repr.zero;
        deposits = Signature.Public_key_hash.Map.empty;
        storage_space_to_pay = None;
        allocated_contracts = None;
        origination_nonce = None;
        temporary_lazy_storage_ids = Lazy_storage_kind.Temp_ids.init;
        internal_nonce = 0;
        internal_nonces_used = Int_set.empty;
        gas_counter_status = Unlimited_operation_gas;
      };
  }

type previous_protocol = Genesis of Parameters_repr.t | Edo_008

let check_and_update_protocol_version ctxt =
  Context.find ctxt version_key
  >>= (function
        | None ->
            failwith
              "Internal error: un-initialized context in check_first_block."
        | Some bytes ->
            let s = Bytes.to_string bytes in
            if Compare.String.(s = version_value) then
              failwith "Internal error: previously initialized context."
            else if Compare.String.(s = "genesis") then
              get_proto_param ctxt
              >|=? fun (param, ctxt) -> (Genesis param, ctxt)
            else if Compare.String.(s = "edo_008") then return (Edo_008, ctxt)
            else Lwt.return @@ storage_error (Incompatible_protocol_version s))
  >>=? fun (previous_proto, ctxt) ->
  Context.add ctxt version_key (Bytes.of_string version_value)
  >|= fun ctxt -> ok (previous_proto, ctxt)

let prepare_first_block ~level ~timestamp ~fitness ctxt =
  check_and_update_protocol_version ctxt
  >>=? fun (previous_proto, ctxt) ->
  ( match previous_proto with
  | Genesis param ->
      Raw_level_repr.of_int32 level
      >>?= fun first_level ->
      set_first_level ctxt first_level
      >>=? fun ctxt -> add_constants ctxt param.constants >|= ok
  | Edo_008 ->
      return ctxt )
  >>=? fun ctxt ->
  prepare ctxt ~level ~predecessor_timestamp:timestamp ~timestamp ~fitness
  >|=? fun ctxt -> (previous_proto, ctxt)

let activate ctxt h =
  lensappLwt (fun ctx -> Updater.activate ctx h) lens_context ctxt
  (* Updater.activate (context ctxt) h >|= update_context ctxt *)

(* Generic context ********************************************************)

type key = string list

type value = bytes

type tree = Context.tree

module type T =
  Raw_context_intf.T
    with type root := root
     and type key := key
     and type value := value
     and type tree := tree

let mem ctxt k = Context.mem (lens_context.get ctxt) k

let mem_tree ctxt k = Context.mem_tree (lens_context.get ctxt) k

let get ctxt k =
  Context.find (lens_context.get ctxt) k
  >|= function None -> storage_error (Missing_key (k, Get)) | Some v -> ok v

let get_tree ctxt k =
  Context.find_tree (lens_context.get ctxt) k
  >|= function None -> storage_error (Missing_key (k, Get)) | Some v -> ok v

let find ctxt k = Context.find (lens_context.get ctxt) k

let find_tree ctxt k = Context.find_tree (lens_context.get ctxt) k

let add ctxt k v =
  lensappLwt (fun ctx -> Context.add ctx k v) lens_context ctxt

let add_tree ctxt k v =
  lensappLwt (fun ctx -> Context.add_tree ctx k v) lens_context ctxt

let init ctxt k v =
  Context.mem (lens_context.get ctxt) k
  >>= function
  | true ->
      Lwt.return @@ storage_error (Existing_key k)
  | _ ->
     lensappLwt (fun ctx -> Context.add ctx k v) lens_context ctxt >|= ok

let init_tree ctxt k v : _ tzresult Lwt.t =
  Context.mem_tree (lens_context.get ctxt) k
  >>= function
  | true ->
      Lwt.return @@ storage_error (Existing_key k)
  | _ ->
     lensappLwt (fun ctx -> Context.add_tree ctx k v) lens_context ctxt >|= ok

let update ctxt k v =
  Context.mem (lens_context.get ctxt) k
  >>= function
  | false ->
      Lwt.return @@ storage_error (Missing_key (k, Set))
  | _ ->
     lensappLwt (fun ctx -> Context.add ctx k v) lens_context ctxt >|= ok

let update_tree ctxt k v =
  Context.mem_tree (lens_context.get ctxt) k
  >>= function
  | false ->
      Lwt.return @@ storage_error (Missing_key (k, Set))
  | _ ->
     lensappLwt (fun ctx -> Context.add_tree ctx k v) lens_context ctxt >|= ok

(* Verify that the key is present before deleting *)
let remove_existing ctxt k =
  Context.mem (lens_context.get ctxt) k
  >>= function
  | false ->
      Lwt.return @@ storage_error (Missing_key (k, Del))
  | _ ->
     lensappLwt (fun ctx -> Context.remove ctx k) lens_context ctxt >|= ok

(* Verify that the key is present before deleting *)
let remove_existing_tree ctxt k =
  Context.mem_tree (lens_context.get ctxt) k
  >>= function
  | false ->
      Lwt.return @@ storage_error (Missing_key (k, Del))
  | _ ->
     lensappLwt (fun ctx -> Context.remove ctx k) lens_context ctxt >|= ok

(* Do not verify before deleting *)
let remove ctxt k =
  lensappLwt (fun ctx -> Context.remove ctx k) lens_context ctxt

let add_or_remove ctxt k = function
  | None ->
      remove ctxt k
  | Some v ->
      add ctxt k v

let add_or_remove_tree ctxt k = function
  | None ->
      remove ctxt k
  | Some v ->
      add_tree ctxt k v

let list ctxt ?offset ?length k =
  Context.list (lens_context.get ctxt) ?offset ?length k

let fold ?depth ctxt k ~init ~f =
  Context.fold ?depth (lens_context.get ctxt) k ~init ~f

module Tree = struct
  include Context.Tree

  let empty ctxt = Context.Tree.empty (lens_context.get ctxt)

  let get t k =
    find t k
    >|= function
    | None -> storage_error (Missing_key (k, Get)) | Some v -> ok v

  let get_tree t k =
    find_tree t k
    >|= function
    | None -> storage_error (Missing_key (k, Get)) | Some v -> ok v

  let init t k v =
    mem t k
    >>= function
    | true ->
        Lwt.return @@ storage_error (Existing_key k)
    | _ ->
        add t k v >|= ok

  let init_tree t k v =
    mem_tree t k
    >>= function
    | true ->
        Lwt.return @@ storage_error (Existing_key k)
    | _ ->
        add_tree t k v >|= ok

  let update t k v =
    mem t k
    >>= function
    | false ->
        Lwt.return @@ storage_error (Missing_key (k, Set))
    | _ ->
        add t k v >|= ok

  let update_tree t k v =
    mem_tree t k
    >>= function
    | false ->
        Lwt.return @@ storage_error (Missing_key (k, Set))
    | _ ->
        add_tree t k v >|= ok

  (* Verify that the key is present before deleting *)
  let remove_existing t k =
    mem t k
    >>= function
    | false ->
        Lwt.return @@ storage_error (Missing_key (k, Del))
    | _ ->
        remove t k >|= ok

  (* Verify that the key is present before deleting *)
  let remove_existing_tree t k =
    mem_tree t k
    >>= function
    | false ->
        Lwt.return @@ storage_error (Missing_key (k, Del))
    | _ ->
        remove t k >|= ok

  let add_or_remove t k = function None -> remove t k | Some v -> add t k v

  let add_or_remove_tree t k = function
    | None ->
        remove t k
    | Some v ->
        add_tree t k v
end

let project x = x

let absolute_key _ k = k

let description = Storage_description.create ()

let fold_map_temporary_lazy_storage_ids ctxt f =
  let (ids', x) = f (lens_temporary_lazy_storage_ids.get ctxt) in
  (lens_temporary_lazy_storage_ids.set ids' ctxt, x)

let map_temporary_lazy_storage_ids_s ctxt f =
  f (lens_temporary_lazy_storage_ids.get ctxt)
  >|= fun (ctxt, ids') ->
  lens_temporary_lazy_storage_ids.set ids' ctxt
