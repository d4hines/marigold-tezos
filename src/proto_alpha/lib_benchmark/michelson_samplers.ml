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

open Protocol

(* ------------------------------------------------------------------------- *)
(* Helpers. *)

(* This function is in the protocol but is not exported. *)
let rec comparable_downcast :
    type a. a Script_typed_cps_ir.comparable_ty -> a Script_typed_cps_ir.ty = function
  | Unit_key tname ->
      Unit_t tname
  | Never_key tname ->
      Never_t tname
  | Int_key tname ->
      Int_t tname
  | Nat_key tname ->
      Nat_t tname
  | String_key tname ->
      String_t tname
  | Bytes_key tname ->
      Bytes_t tname
  | Mutez_key tname ->
      Mutez_t tname
  | Bool_key tname ->
      Bool_t tname
  | Key_hash_key tname ->
      Key_hash_t tname
  | Timestamp_key tname ->
      Timestamp_t tname
  | Address_key tname ->
      Address_t tname
  | Pair_key ((l, al), (r, ar), tname) ->
      Pair_t
        ( (comparable_downcast l, al, None),
          (comparable_downcast r, ar, None),
          tname )
  | Union_key ((l, al), (r, ar), tname) ->
      Union_t ((comparable_downcast l, al), (comparable_downcast r, ar), tname)
  | Option_key (t, tname) ->
      Option_t (comparable_downcast t, tname)
  | Key_key tname ->
      Key_t tname
  | Chain_id_key tname ->
      Chain_id_t tname
  | Signature_key tname ->
      Signature_t tname

(* ------------------------------------------------------------------------- *)
(* Type names. *)

(* These are not exhaustive (new comparable types in 007)  *)

type type_name =
  [ `TUnit
  | `TInt
  | `TNat
  | `TSignature
  | `TString
  | `TBytes
  | `TMutez
  | `TKey_hash
  | `TKey
  | `Timestamp
  | `TAddress
  | `TBool
  | `TPair
  | `TUnion
  | `TLambda
  | `TOption
  | `TList
  | `TSet
  | `TMap
  | `TBig_map
  | `TContract
  | `TOperation
  | `TTicket ]

let all_type_names : type_name array =
  [| `TUnit;
     `TInt;
     `TNat;
     `TSignature;
     `TString;
     `TBytes;
     `TMutez;
     `TKey_hash;
     `TKey;
     `Timestamp;
     `TAddress;
     `TBool;
     `TPair;
     `TUnion;
     `TLambda;
     `TOption;
     `TList;
     `TSet;
     `TMap;
     `TBig_map;
     `TContract;
     `TOperation;
     `TTicket |]

type atomic_type_name =
  [ `TUnit
  | `TInt
  | `TNat
  | `TSignature
  | `TString
  | `TBytes
  | `TMutez
  | `TKey_hash
  | `TKey
  | `Timestamp
  | `TAddress
  | `TBool ]

let all_atomic_type_names : atomic_type_name array =
  [| `TUnit;
     `TInt;
     `TNat;
     `TSignature;
     `TString;
     `TBytes;
     `TMutez;
     `TKey_hash;
     `TKey;
     `Timestamp;
     `TAddress;
     `TBool |]

type comparable_type_name =
  [ `TInt
  | `TNat
  | `TString
  | `TBytes
  | `TMutez
  | `TKey_hash
  | `Timestamp
  | `TAddress
  | `TBool ]

let all_comparable_type_names : comparable_type_name array =
  [| `TInt;
     `TNat;
     `TString;
     `TBytes;
     `TMutez;
     `TKey_hash;
     `Timestamp;
     `TAddress;
     `TBool |]

let type_names_count = Array.length all_type_names

let atomic_type_names_count = Array.length all_atomic_type_names

let comparable_type_names_count = Array.length all_comparable_type_names

(* ------------------------------------------------------------------------- *)
(* Uniform type name generators *)

let uniform_type_name : unit -> type_name =
 fun () ->
  let i = Random.int type_names_count in
  all_type_names.(i)

let uniform_atomic_type_name : unit -> atomic_type_name =
 fun () ->
  let i = Random.int atomic_type_names_count in
  all_atomic_type_names.(i)

let uniform_comparable_type_name : unit -> comparable_type_name =
 fun () ->
  let i = Random.int comparable_type_names_count in
  all_comparable_type_names.(i)

(* ------------------------------------------------------------------------- *)
(* Existentially packed typed value. *)

type ex_value = Ex_value : {typ : 'a Script_typed_cps_ir.ty; v : 'a} -> ex_value

(* ------------------------------------------------------------------------- *)
(* Random generation functor. *)

module type S = sig
  val state : Random.State.t

  val sampling_parameters : Michelson_samplers_parameters.t

  module Crypto_samplers : Crypto_samplers.Finite_key_pool_S

  module Michelson_base : sig
    val int : unit -> Alpha_context.Script_int.z Alpha_context.Script_int.num

    val nat : unit -> Alpha_context.Script_int.n Alpha_context.Script_int.num

    val signature : unit -> Tezos_crypto.Signature.t

    val string : unit -> string

    val bytes : unit -> bytes

    val tez : unit -> Alpha_context.Tez.tez

    val timestamp : unit -> Alpha_context.Script_timestamp.t

    val bool : unit -> bool
  end

  module Random_type : sig
    val type_of_atomic_type_name :
      atomic_type_name -> Script_ir_translator.ex_ty

    val comparable_type_of_comparable_type_name :
      comparable_type_name -> Script_ir_translator.ex_comparable_ty

    val m_type : max_depth:int -> Script_ir_translator.ex_ty

    val m_comparable_type : unit -> Script_ir_translator.ex_comparable_ty
  end

  module rec Random_value : sig
    val value : 'a Script_typed_cps_ir.ty -> 'a

    val stack : 'a Script_typed_cps_ir.stack_ty -> 'a
  end
end

module Make (P : Michelson_samplers_parameters.S) : S = struct
  include Michelson_samplers_base.Make_full (P)

  (* Random generation of Michelson types. *)
  module Random_type = struct
    let type_of_atomic_type_name (at_tn : atomic_type_name) :
        Script_ir_translator.ex_ty =
      match at_tn with
      | `TString ->
          Ex_ty (String_t None)
      | `TNat ->
          Ex_ty (Nat_t None)
      | `TKey ->
          Ex_ty (Key_t None)
      | `TBytes ->
          Ex_ty (Bytes_t None)
      | `TBool ->
          Ex_ty (Bool_t None)
      | `TAddress ->
          Ex_ty (Address_t None)
      | `Timestamp ->
          Ex_ty (Timestamp_t None)
      | `TKey_hash ->
          Ex_ty (Key_hash_t None)
      | `TMutez ->
          Ex_ty (Mutez_t None)
      | `TSignature ->
          Ex_ty (Signature_t None)
      | `TUnit ->
          Ex_ty (Unit_t None)
      | `TInt ->
          Ex_ty (Int_t None)

    let comparable_type_of_comparable_type_name (cmp_tn : comparable_type_name)
        : Script_ir_translator.ex_comparable_ty =
      match cmp_tn with
      | `TString ->
          Ex_comparable_ty (String_key None)
      | `TNat ->
          Ex_comparable_ty (Nat_key None)
      | `TBytes ->
          Ex_comparable_ty (Bytes_key None)
      | `TBool ->
          Ex_comparable_ty (Bool_key None)
      | `TAddress ->
          Ex_comparable_ty (Address_key None)
      | `Timestamp ->
          Ex_comparable_ty (Timestamp_key None)
      | `TKey_hash ->
          Ex_comparable_ty (Key_hash_key None)
      | `TMutez ->
          Ex_comparable_ty (Mutez_key None)
      | `TInt ->
          Ex_comparable_ty (Int_key None)

    let rec m_type ~max_depth : Script_ir_translator.ex_ty =
      let open Script_ir_translator in
      if max_depth = 0 then
        let at_tn = uniform_atomic_type_name () in
        type_of_atomic_type_name at_tn
      else
        let tn = uniform_type_name () in
        match tn with
        | #atomic_type_name as at_tn ->
            type_of_atomic_type_name at_tn
        | `TPair -> (
            let left = m_type ~max_depth:(max_depth - 1) in
            let right = m_type ~max_depth:(max_depth - 1) in
            match (left, right) with
            | (Ex_ty left_ty, Ex_ty right_ty) ->
                Ex_ty
                  (Pair_t ((left_ty, None, None), (right_ty, None, None), None))
            )
        | `TLambda -> (
            let domain = m_type ~max_depth:(max_depth - 1) in
            let codomain = m_type ~max_depth:(max_depth - 1) in
            match (domain, codomain) with
            | (Ex_ty domain_ty, Ex_ty codomain_ty) ->
                Ex_ty (Lambda_t (domain_ty, codomain_ty, None)) )
        | `TUnion -> (
            let left = m_type ~max_depth:(max_depth - 1) in
            let right = m_type ~max_depth:(max_depth - 1) in
            match (left, right) with
            | (Ex_ty left_ty, Ex_ty right_ty) ->
                Ex_ty (Union_t ((left_ty, None), (right_ty, None), None)) )
        | `TOption -> (
            let opt = m_type ~max_depth:(max_depth - 1) in
            match opt with Ex_ty opt_ty -> Ex_ty (Option_t (opt_ty, None)) )
        | `TMap -> (
            let key = m_comparable_type () in
            let elt = m_type ~max_depth:(max_depth - 1) in
            match (key, elt) with
            | (Ex_comparable_ty key_ty, Ex_ty elt_ty) ->
                Ex_ty (Map_t (key_ty, elt_ty, None)) )
        | `TSet -> (
            let key = m_comparable_type () in
            match key with
            | Ex_comparable_ty key_ty ->
                Ex_ty (Set_t (key_ty, None)) )
        | `TList -> (
            let elt = m_type ~max_depth:(max_depth - 1) in
            match elt with Ex_ty elt_ty -> Ex_ty (List_t (elt_ty, None)) )
        | `TTicket -> (
            let content = m_comparable_type () in
            match content with
            | Ex_comparable_ty content_ty ->
                Ex_ty (Ticket_t (content_ty, None)) )
        | `TContract | `TOperation | `TBig_map ->
            (* Don't know what to do with theses. Redraw. *)
            m_type ~max_depth

    and m_comparable_type () : Script_ir_translator.ex_comparable_ty =
      let cmp_tn = uniform_comparable_type_name () in
      comparable_type_of_comparable_type_name cmp_tn
  end

  (* Type-directed generation of random values. *)
  module rec Random_value : sig
    val value : 'a Script_typed_cps_ir.ty -> 'a

    val stack : 'a Script_typed_cps_ir.stack_ty -> 'a
  end = struct
    let rec value : type a. a Script_typed_cps_ir.ty -> a =
      let open Script_typed_cps_ir in
      fun typ ->
        match typ with
        | Never_t _ ->
            assert false
        | Unit_t _ ->
            (() : a)
        | Int_t _ ->
            Michelson_base.int ()
        | Nat_t _ ->
            Michelson_base.nat ()
        | Signature_t _ ->
            Michelson_base.signature ()
        | String_t _ ->
            Michelson_base.string ()
        | Bytes_t _ ->
            Michelson_base.bytes ()
        | Mutez_t _ ->
            Michelson_base.tez ()
        | Key_hash_t _ ->
            Crypto_samplers.pkh ()
        | Key_t _ ->
            Crypto_samplers.pk ()
        | Timestamp_t _ ->
            Michelson_base.timestamp ()
        | Bool_t _ ->
            Michelson_base.bool ()
        | Address_t _ ->
            ( Alpha_context.Contract.implicit_contract (Crypto_samplers.pkh ()),
              "default" )
        | Pair_t ((left_t, _, _), (right_t, _, _), _) ->
            let left_v = value left_t in
            let right_v = value right_t in
            (left_v, right_v)
        | Union_t ((left_t, _), (right_t, _), _) ->
            if Michelson_base.bool () then L (value left_t)
            else R (value right_t)
        | Lambda_t (arg_ty, ret_ty, _) ->
            generate_lambda arg_ty ret_ty
        | Option_t (ty, _) ->
            if Michelson_base.bool () then None else Some (value ty)
        | List_t (elt_ty, _) ->
            generate_list elt_ty
        | Set_t (elt_ty, _) ->
            generate_set elt_ty
        | Map_t (key_ty, val_ty, _) ->
            generate_map key_ty val_ty
        | Contract_t (arg_ty, _) ->
            generate_contract arg_ty
        | Operation_t _ ->
            generate_operation ()
        | Big_map_t (key_ty, val_ty, _) ->
            generate_big_map key_ty val_ty
        | Chain_id_t _ ->
            Chain_id.zero
        | Bls12_381_g1_t _ ->
            generate_bls12_381_g1 ()
        | Bls12_381_g2_t _ ->
            generate_bls12_381_g2 ()
        | Bls12_381_fr_t _ ->
            generate_bls12_381_fr ()
        | Ticket_t (contents_ty, _) ->
            let ty = comparable_downcast contents_ty in
            generate_ticket ty
        | Sapling_transaction_t _ ->
            Stdlib.failwith
              "Michelson_samplers: sapling transactions not handled yet"
        | Sapling_state_t _ ->
            Stdlib.failwith "Michelson_samplers: sapling state not handled yet"

    and generate_lambda :
        type arg ret.
        arg Script_typed_cps_ir.ty ->
        ret Script_typed_cps_ir.ty ->
        (arg, ret) Script_typed_cps_ir.lambda =
     fun _arg_ty _ret_ty ->
      (* TODO: plug michelson sampler *)
      assert false

    and generate_list :
        type elt. elt Script_typed_cps_ir.ty -> elt Script_typed_cps_ir.boxed_list =
     fun elt_type ->
      let (length, elements) =
        Structure_samplers.list
          P.state
          ~range:P.parameters.list_size
          ~sampler:(fun () -> value elt_type)
      in
      Script_typed_cps_ir.{elements; length}

    and generate_set :
        type elt. elt Script_typed_cps_ir.comparable_ty -> elt Script_typed_cps_ir.set
        =
     fun elt_ty ->
      let ety = comparable_downcast elt_ty in
      let {Script_typed_cps_ir.elements; length = _} = generate_list ety in
      List.fold_left
        (fun set x -> Script_ir_translator.set_update x true set)
        (Script_ir_translator.empty_set elt_ty)
        elements

    and generate_map :
        type key elt.
        key Script_typed_cps_ir.comparable_ty ->
        elt Script_typed_cps_ir.ty ->
        (key, elt) Script_typed_cps_ir.map =
     fun key_ty elt_ty ->
      let size =
        Base_samplers.sample_in_interval P.state ~range:P.parameters.map_size
      in
      let kty = comparable_downcast key_ty in
      let keys = List.init size (fun _ -> value kty) in
      let elts = List.init size (fun _ -> value elt_ty) in
      List.fold_left2
        (fun map key elt -> Script_ir_translator.map_update key (Some elt) map)
        (Script_ir_translator.empty_map key_ty)
        keys
        elts

    and generate_big_map :
        type key elt.
        key Script_typed_cps_ir.comparable_ty ->
        elt Script_typed_cps_ir.ty ->
        (key, elt) Script_typed_cps_ir.big_map =
      let open Script_typed_cps_ir in
      fun key_type elt_ty ->
        (* Cannot have big maps under big maps *)
        let opt_elt_ty = Option_t (elt_ty, None) in
        let diff = generate_map key_type opt_elt_ty in
        {id = None; diff; key_type; value_type = elt_ty}

    and generate_contract :
        type arg. arg Script_typed_cps_ir.ty -> arg Script_typed_cps_ir.typed_contract
        =
     fun _arg_ty -> Stdlib.failwith "generate_contract: unimplemented"

    and generate_operation :
        unit ->
        Alpha_context.packed_internal_operation
        * Alpha_context.Lazy_storage.diffs option =
     fun () -> (generate_transfer_tokens (), None)

    and generate_transfer_tokens :
        unit -> Alpha_context.packed_internal_operation =
     fun () -> Stdlib.failwith "generate_transfer_tokens: unimplemented"

    and generate_bls12_381_g1 : unit -> Environment.Bls12_381.G1.t =
     fun () ->
      let b = Bls12_381.G1.Uncompressed.(to_bytes (random ())) in
      match Environment.Bls12_381.G1.of_bytes_opt b with
      | Some x ->
          x
      | None ->
          (* Must not happen *)
          Environment.Bls12_381.G1.one

    and generate_bls12_381_g2 : unit -> Environment.Bls12_381.G2.t =
     fun () ->
      let b = Bls12_381.G2.Uncompressed.(to_bytes (random ())) in
      match Environment.Bls12_381.G2.of_bytes_opt b with
      | Some x ->
          x
      | None ->
          (* Must not happen *)
          Environment.Bls12_381.G2.one

    and generate_bls12_381_fr : unit -> Environment.Bls12_381.Fr.t =
     fun () ->
      let b = Bls12_381.Fr.(to_bytes (random ())) in
      match Environment.Bls12_381.Fr.of_bytes_opt b with
      | Some x ->
          x
      | None ->
          (* Must not happen *)
          Environment.Bls12_381.Fr.one

    and generate_ticket :
        type a. a Script_typed_cps_ir.ty -> a Script_typed_cps_ir.ticket =
     fun ty ->
      let contents = value ty in
      let ticketer =
        ( Alpha_context.Contract.implicit_contract (Crypto_samplers.pkh ()),
          "default" )
      in
      let amount = Michelson_base.nat () in
      Script_typed_cps_ir.{ticketer; contents; amount}

    (* Random stack generation. *)
    let rec stack : type a. a Script_typed_cps_ir.stack_ty -> a =
      let open Script_typed_cps_ir in
      fun stack_ty ->
        match stack_ty with
        | Item_t (ty, tl, _) ->
            let elt = value ty in
            let rest = stack tl in
            ((elt, rest) : a)
        | Empty_t ->
            ()
  end
end
