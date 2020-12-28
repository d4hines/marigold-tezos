(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Alpha_context
open Script_int

(* Preliminary definitions. *)

type var_annot = Var_annot of string

type type_annot = Type_annot of string

type field_annot = Field_annot of string

type never = |

type address = Contract.t * string

type ('a, 'b) pair = 'a * 'b

type ('a, 'b) union = L of 'a | R of 'b

type operation = packed_internal_operation * Lazy_storage.diffs option

type 'a ticket = {ticketer : address; contents : 'a; amount : n num}

type end_of_stack = unit * unit

type _ comparable_ty =
  | Unit_key : type_annot option -> unit comparable_ty
  | Never_key : type_annot option -> never comparable_ty
  | Int_key : type_annot option -> z num comparable_ty
  | Nat_key : type_annot option -> n num comparable_ty
  | Signature_key : type_annot option -> signature comparable_ty
  | String_key : type_annot option -> string comparable_ty
  | Bytes_key : type_annot option -> Bytes.t comparable_ty
  | Mutez_key : type_annot option -> Tez.t comparable_ty
  | Bool_key : type_annot option -> bool comparable_ty
  | Key_hash_key : type_annot option -> public_key_hash comparable_ty
  | Key_key : type_annot option -> public_key comparable_ty
  | Timestamp_key : type_annot option -> Script_timestamp.t comparable_ty
  | Chain_id_key : type_annot option -> Chain_id.t comparable_ty
  | Address_key : type_annot option -> address comparable_ty
  | Pair_key :
      ('a comparable_ty * field_annot option)
      * ('b comparable_ty * field_annot option)
      * type_annot option
      -> ('a, 'b) pair comparable_ty
  | Union_key :
      ('a comparable_ty * field_annot option)
      * ('b comparable_ty * field_annot option)
      * type_annot option
      -> ('a, 'b) union comparable_ty
  | Option_key :
      'v comparable_ty * type_annot option
      -> 'v option comparable_ty

module type Boxed_set = sig
  type elt

  val elt_ty : elt comparable_ty

  module OPS : S.SET with type elt = elt

  val boxed : OPS.t

  val size : int
end

type 'elt set = (module Boxed_set with type elt = 'elt)

module type Boxed_map = sig
  type key

  type value

  val key_ty : key comparable_ty

  module OPS : S.MAP with type key = key

  val boxed : value OPS.t * int
end

type ('key, 'value) map =
  (module Boxed_map with type key = 'key and type value = 'value)

(* ---- Instructions --------------------------------------------------------*)

(*

   The instructions of Michelson are represented in the following
   Generalized Algebraic Datatypes.

   There are three important aspects in that type declaration.

   First, we follow a tagless approach for values: they are directly
   represented as OCaml values. This reduces the computational cost of
   interpretation because there is no need to check the shape of a
   value before applying an operation to it. To achieve that, the GADT
   encodes the typing rules of the Michelson programming
   language. This static information is sufficient for the typechecker
   to justify the absence of runtime checks.  As a bonus, it also
   ensures that well-typed Michelson programs cannot go wrong: if the
   interpreter typechecks then we have the static guarantee that no
   stack underflow or type error can occur at runtime.

   Second, we maintain the invariant that the stack type always has a
   distinguished topmost element. This invariant is important to
   implement the stack as an accumulator followed by a linked list of
   cells. This representation is considered in the literature as an
   efficient representation of the stack for a stack-based abstract
   machine, mainly because this opens the opportunity for the
   accumulator to be stored in a hardware register. In the GADT, this
   invariant is encoded by representing the stack type using two
   parameters instead of one: the first one is the type of the
   accumulator while the second is the type of the rest of the stack.

   Third, in this representation, each instruction embeds its
   potential successor instructions in the control flow. This design
   choice permits an efficient implementation of the continuation
   stack in the interpreter. Assigning a precise type to this kind of
   instruction which is a cell in a linked list of instructions is
   similar to the typing of delimited continuations: we need to give a
   type [`bef] to the stack before the execution of the instruction, a
   type [`aft] to the stack after the execution of the instruction and
   before the execution of the next, and a type [`res] for the resulting
   stack type after the execution of the whole chain of instructions.

   Combining these three aspects, the type [kinstr] needs four parameters:

   ('bef_top, 'bef, 'res_top, `res) kinstr

   Notice that we could have chosen to only give two parameters to [kinstr]
   by manually enforcing each argument to be a pair but this is
   error-prone: with four parameters, this constraint is enforced by the arity of
   the type constructor itself.

   Hence, an instruction which has a successor instruction enjoys a
   type of the form:

   ('aft_top, 'aft, 'res_top, 'res) kinstr ->
   ('bef_top, 'bef, 'res_top, 'res) kinstr

   where [bef_top] and [bef] are the types of the stack top and rest
   before the instruction chain, and [res_top] and [res] are the types
   of the stack top and rest after the instruction chain

   Notations:
   ----------

   In the following declaration, we use 'a, 'b, 'c, 'd, ...
   to assign types to stack cell contents while we use 's, 't,
   'u, 'v, ... to assign types to stacks.

   The types for the final result and stack rest of a whole
   sequence of instructions are written 'r and 'f
   (standing for "result" and "final stack rest", respectively).

   Instructions for internal execution steps
   =========================================

   Some instructions of the following list are not present in the
   source language. They only appear during evaluation to account
   for intermediate execution steps. Indeed, since the interpreter
   follows a small-step style, it is sometimes necessary to decompose
   a source-level instruction (e.g. List_map) into several instructions
   with smaller steps. This technique seems required to get an efficient
   tail-recursive interpreter.

*)

type ('bef_top, 'bef, 'res_top, 'res) kinstr =
  (*
     Stack
     -----
  *)
  | KDrop : ('a, 'b * 's, 'b, 's) kinstr
  | KDup : ('a, 's, 'a, 'a * 's) kinstr
  | KSwap : ('a, 'b * 's, 'b, 'a * 's) kinstr
  | KConst : 'ty -> ('a, 's, 'ty, 'a * 's) kinstr
  (*
     Pairs
     -----
  *)
  | KCons_pair : ('a, 'b * 's, 'a * 'b, 's) kinstr
  | KCar : ('a * 'b, 's, 'a, 's) kinstr
  | KCdr : ('a * 'b, 's, 'b, 's) kinstr
  | KUnpair : ('a * 'b, 's, 'a, 'b * 's) kinstr
  (*
     Options
     -------
   *)
  | KCons_some : ('v, 's, 'v option, 's) kinstr
  | KCons_none : 'b ty -> ('a, 's, 'b option, 'a * 's) kinstr
  | KIf_none :
      (* Notice that the continuations of the following two
         instructions should have a shared suffix to avoid code
         duplication. *)
      ('b, 's, 'r, 'f) knext
      * ('a, 'b * 's, 'r, 'f) knext
      -> ('a option, 'b * 's, 'r, 'f) kinstr
  (*
     Unions
     ------
   *)
  | KCons_left : ('a, 's, ('a, 'b) union, 's) kinstr
  | KCons_right : ('b, 's, ('a, 'b) union, 's) kinstr
  | KIf_left :
      (* Notice that the continuations of the following two
         instructions should have a shared suffix to avoid code
         duplication. *)
      ('a, 's, 'r, 'f) knext
      * ('b, 's, 'r, 'f) knext
      -> (('a, 'b) union, 's, 'r, 'f) kinstr
  (*
     Lists
     -----
  *)
  | KCons_list : ('a, 'a boxed_list * 's, 'a boxed_list, 's) kinstr
  | KNil : ('a, 's, 'b boxed_list, 'a * 's) kinstr
  | KIf_cons :
      (* Notice that the continuations of the following two
         instructions should have a shared suffix to avoid code
         duplication. *)
      ('a, 'a boxed_list * ('b * 's), 'r, 'f) knext
      * ('b, 's, 'r, 'f) knext
      -> ('a boxed_list, 'b * 's, 'r, 'f) kinstr
  | KList_map :
      ('a, 'c * 's, 'b, 'c * 's) knext
      -> ('a boxed_list, 'c * 's, 'b boxed_list, 'c * 's) kinstr
  | KList_iter :
      ('a, 'b * 's, 'b, 's) knext
      -> ('a boxed_list, 'b * 's, 'b, 's) kinstr
  | KList_size : ('a boxed_list, 's, n num, 's) kinstr
  (*
    Sets
    ----
  *)
  | KEmpty_set : 'b comparable_ty -> ('a, 's, 'b set, 'a * 's) kinstr
  | KSet_iter : ('a, 'b * 's, 'b, 's) knext -> ('a set, 'b * 's, 'b, 's) kinstr
  | KSet_mem : ('a, 'a set * 's, bool, 's) kinstr
  | KSet_update : ('a, bool * ('a set * 's), 'a set, 's) kinstr
  | KSet_size : ('a set, 's, n num, 's) kinstr
  (*
     Maps
     ----
   *)
  | KEmpty_map :
      'b comparable_ty * 'c ty
      -> ('a, 's, ('b, 'c) map, 'a * 's) kinstr
  | KMap_map :
      ('a * 'b, 'd * 's, 'c, 'd * 's) knext
      -> (('a, 'b) map, 'd * 's, ('a, 'c) map, 'd * 's) kinstr
  | KMap_iter :
      ('a * 'b, 'c * 's, 'c, 's) knext
      -> (('a, 'b) map, 'c * 's, 'c, 's) kinstr
  | KMap_mem : ('a, ('a, 'b) map * 's, bool, 's) kinstr
  | KMap_get : ('a, ('a, 'b) map * 's, 'b option, 's) kinstr
  | KMap_update
      : ('a, 'b option * (('a, 'b) map * 's), ('a, 'b) map, 's) kinstr
  | KMap_get_and_update
      : ( 'a,
          'v option * (('a, 'v) map * 'rest),
          'v option,
          ('a, 'v) map * 'rest )
        kinstr
  | KMap_size : (('a, 'b) map, 's, n num, 's) kinstr
  (*
     Big maps
     --------
  *)
  | KEmpty_big_map :
      'b comparable_ty * 'c ty
      -> ('a, 's, ('b, 'c) big_map, 'a * 's) kinstr
  | KBig_map_mem : ('a, ('a, 'b) big_map * 's, bool, 's) kinstr
  | KBig_map_get : ('a, ('a, 'b) big_map * 's, 'b option, 's) kinstr
  | KBig_map_update
      : ('a, 'b option * (('a, 'b) big_map * 's), ('a, 'b) big_map, 's) kinstr
  | KBig_map_get_and_update
      : ( 'a,
          'v option * (('a, 'v) big_map * 'rest),
          'v option,
          ('a, 'v) big_map * 'rest )
        kinstr
  (*
     Strings
     -------
  *)
  | KConcat_string : (string boxed_list, 's, string, 's) kinstr
  | KConcat_string_pair : (string, string * 's, string, 's) kinstr
  | KSlice_string : (n num, n num * (string * 's), string option, 's) kinstr
  | KString_size : (string, 's, n num, 's) kinstr
  (*
     Bytes
     -----
  *)
  | KConcat_bytes : (bytes boxed_list, 's, bytes, 's) kinstr
  | KConcat_bytes_pair : (bytes, bytes * 's, bytes, 's) kinstr
  | KSlice_bytes : (n num, n num * (bytes * 's), bytes option, 's) kinstr
  | KBytes_size : (bytes, 's, n num, 's) kinstr
  (*
     Timestamps
     ----------
   *)
  | KAdd_seconds_to_timestamp
      : (z num, Script_timestamp.t * 's, Script_timestamp.t, 's) kinstr
  | KAdd_timestamp_to_seconds
      : (Script_timestamp.t, z num * 's, Script_timestamp.t, 's) kinstr
  | KSub_timestamp_seconds
      : (Script_timestamp.t, z num * 's, Script_timestamp.t, 's) kinstr
  | KDiff_timestamps
      : (Script_timestamp.t, Script_timestamp.t * 's, z num, 's) kinstr
  (*
     Tez
     ---
    *)
  | KAdd_tez : (Tez.t, Tez.t * 's, Tez.t, 's) kinstr
  | KSub_tez : (Tez.t, Tez.t * 's, Tez.t, 's) kinstr
  | KMul_teznat : (Tez.t, n num * 's, Tez.t, 's) kinstr
  | KMul_nattez : (n num, Tez.t * 's, Tez.t, 's) kinstr
  | KEdiv_teznat : (Tez.t, n num * 's, (Tez.t, Tez.t) pair option, 's) kinstr
  | KEdiv_tez : (Tez.t, Tez.t * 's, (n num, Tez.t) pair option, 's) kinstr
  (*
     Booleans
     --------
   *)
  | KOr : (bool, bool * 's, bool, 's) kinstr
  | KAnd : (bool, bool * 's, bool, 's) kinstr
  | KXor : (bool, bool * 's, bool, 's) kinstr
  | KNot : (bool, 's, bool, 's) kinstr
  (*
     Integers
     --------
  *)
  | KIs_nat : (z num, 's, n num option, 's) kinstr
  | KNeg_nat : (n num, 's, z num, 's) kinstr
  | KNeg_int : (z num, 's, z num, 's) kinstr
  | KAbs_int : (z num, 's, n num, 's) kinstr
  | KInt_nat : (n num, 's, z num, 's) kinstr
  | KAdd_intint : (z num, z num * 's, z num, 's) kinstr
  | KAdd_intnat : (z num, n num * 's, z num, 's) kinstr
  | KAdd_natint : (n num, z num * 's, z num, 's) kinstr
  | KAdd_natnat : (n num, n num * 's, n num, 's) kinstr
  | KSub_int : ('a num, 'b num * 's, z num, 's) kinstr
  | KMul_intint : (z num, z num * 's, z num, 's) kinstr
  | KMul_intnat : (z num, n num * 's, z num, 's) kinstr
  | KMul_natint : (n num, z num * 's, z num, 's) kinstr
  | KMul_natnat : (n num, n num * 's, n num, 's) kinstr
  | KEdiv_intint : (z num, z num * 's, (z num, n num) pair option, 's) kinstr
  | KEdiv_intnat : (z num, n num * 's, (z num, n num) pair option, 's) kinstr
  | KEdiv_natint : (n num, z num * 's, (z num, n num) pair option, 's) kinstr
  | KEdiv_natnat : (n num, n num * 's, (n num, n num) pair option, 's) kinstr
  | KLsl_nat : (n num, n num * 's, n num, 's) kinstr
  | KLsr_nat : (n num, n num * 's, n num, 's) kinstr
  | KOr_nat : (n num, n num * 's, n num, 's) kinstr
  | KAnd_nat : (n num, n num * 's, n num, 's) kinstr
  | KAnd_int_nat : (z num, n num * 's, n num, 's) kinstr
  | KXor_nat : (n num, n num * 's, n num, 's) kinstr
  | KNot_nat : (n num, 's, z num, 's) kinstr
  | KNot_int : (z num, 's, z num, 's) kinstr
  (*
     Control
     -------
  *)
  | KIf :
      (* Notice that the continuations of the following two
         instructions should have a shared suffix to avoid code
         duplication. *)
      ('a, 's, 'r, 'f) knext
      * ('a, 's, 'r, 'f) knext
      -> (bool, 'a * 's, 'r, 'f) kinstr
  | KLoop : ('a, 's, bool, 'a * 's) knext -> (bool, 'a * 's, 'a, 's) kinstr
  | KLoop_left :
      ('a, 's, ('a, 'b) union, 's) knext
      -> (('a, 'b) union, 's, 'b, 's) kinstr
  | KDip : ('b, 's, 'c, 't) knext -> ('a, 'b * 's, 'a, 'c * 't) kinstr
  | KExec : ('a, ('a, 'b) lambda * 's, 'b, 's) kinstr
  | KApply :
      'a ty
      -> ('a, ('a * 't, 'b) lambda * 's, ('t, 'b) lambda, 's) kinstr
  | KLambda : ('b, 'c) lambda -> ('a, 's, ('b, 'c) lambda, 'a * 's) kinstr
  | KFailwith : Script.location * 'a ty -> ('a, 's, 'b, 't) kinstr
  | KNop : ('a, 's, 'a, 's) kinstr
  (*
     Comparison
     ----------
  *)
  | KCompare : 'a comparable_ty -> ('a, 'a * 's, z num, 's) kinstr
  (*
     Comparators
     -----------
  *)
  | KEq : (z num, 's, bool, 's) kinstr
  | KNeq : (z num, 's, bool, 's) kinstr
  | KLt : (z num, 's, bool, 's) kinstr
  | KGt : (z num, 's, bool, 's) kinstr
  | KLe : (z num, 's, bool, 's) kinstr
  | KGe : (z num, 's, bool, 's) kinstr
  (*
     Protocol
     --------
  *)
  | KAddress : ('a typed_contract, 's, address, 's) kinstr
  | KContract :
      'a ty * string
      -> (address, 's, 'a typed_contract option, 's) kinstr
  | KTransfer_tokens
      : ('a, Tez.t * ('a typed_contract * 's), operation, 's) kinstr
  | KImplicit_account : (public_key_hash, 's, unit typed_contract, 's) kinstr
  | KCreate_contract :
      'a ty
      * 'b ty
      * ('b * 'a, operation boxed_list * 'a) lambda
      * field_annot option
      -> ( public_key_hash option,
           Tez.t * ('a * 's),
           operation,
           address * 's )
         kinstr
  | KSet_delegate : (public_key_hash option, 's, operation, 's) kinstr
  | KNow : ('a, 's, Script_timestamp.t, 'a * 's) kinstr
  | KBalance : ('a, 's, Tez.t, 'a * 's) kinstr
  | KLevel : ('a, 's, n num, 'a * 's) kinstr
  | KCheck_signature : (public_key, signature * (bytes * 's), bool, 's) kinstr
  | KHash_key : (public_key, 's, public_key_hash, 's) kinstr
  | KPack : 'a ty -> ('a, 's, bytes, 's) kinstr
  | KUnpack : 'a ty -> (bytes, 's, 'a option, 's) kinstr
  | KBlake2b : (bytes, 's, bytes, 's) kinstr
  | KSha256 : (bytes, 's, bytes, 's) kinstr
  | KSha512 : (bytes, 's, bytes, 's) kinstr
  | KSource : ('a, 's, address, 'a * 's) kinstr
  | KSender : ('a, 's, address, 'a * 's) kinstr
  | KSelf : 'b ty * string -> ('a, 's, 'b typed_contract, 'a * 's) kinstr
  | KSelf_address : ('a, 's, address, 'a * 's) kinstr
  | KAmount : ('a, 's, Tez.t, 'a * 's) kinstr
  | KSapling_empty_state :
      Sapling.Memo_size.t
      -> ('a, 's, Sapling.state, 'a * 's) kinstr
  | KSapling_verify_update
      : ( Sapling.transaction,
          Sapling.state * 's,
          (z num, Sapling.state) pair option,
          's )
        kinstr
  | KDig :
      int
      * ('b, 'c * 't, 'c, 't, 'a, 's, 'd, 'u) stack_prefix_preservation_witness
      -> ('a, 's, 'b, 'd * 'u) kinstr
  | KDug :
      int
      * ('c, 't, 'a, 'c * 't, 'b, 's, 'd, 'u) stack_prefix_preservation_witness
      -> ('a, 'b * 's, 'd, 'u) kinstr
  | KDipn :
      int
      * ('c, 't, 'd, 'v, 'a, 's, 'b, 'u) stack_prefix_preservation_witness
      * ('c, 't, 'd, 'v) knext
      -> ('a, 's, 'b, 'u) kinstr
  | KDropn :
      int * ('b, 'u, 'b, 'u, 'a, 's, 'a, 's) stack_prefix_preservation_witness
      -> ('a, 's, 'b, 'u) kinstr
  | KChainId : ('a, 's, Chain_id.t, 'a * 's) kinstr
  | KNever : (never, 's, 'b, 'u) kinstr
  | KVoting_power : (public_key_hash, 's, n num, 's) kinstr
  | KTotal_voting_power : ('a, 's, n num, 'a * 's) kinstr
  | KKeccak : (bytes, 's, bytes, 's) kinstr
  | KSha3 : (bytes, 's, bytes, 's) kinstr
  | KAdd_bls12_381_g1
      : (Bls12_381.G1.t, Bls12_381.G1.t * 's, Bls12_381.G1.t, 's) kinstr
  | KAdd_bls12_381_g2
      : (Bls12_381.G2.t, Bls12_381.G2.t * 's, Bls12_381.G2.t, 's) kinstr
  | KAdd_bls12_381_fr
      : (Bls12_381.Fr.t, Bls12_381.Fr.t * 's, Bls12_381.Fr.t, 's) kinstr
  | KMul_bls12_381_g1
      : (Bls12_381.G1.t, Bls12_381.Fr.t * 's, Bls12_381.G1.t, 's) kinstr
  | KMul_bls12_381_g2
      : (Bls12_381.G2.t, Bls12_381.Fr.t * 's, Bls12_381.G2.t, 's) kinstr
  | KMul_bls12_381_fr
      : (Bls12_381.Fr.t, Bls12_381.Fr.t * 's, Bls12_381.Fr.t, 's) kinstr
  | KMul_bls12_381_z_fr
      : (Bls12_381.Fr.t, 'a num * 's, Bls12_381.Fr.t, 's) kinstr
  | KMul_bls12_381_fr_z
      : ('a num, Bls12_381.Fr.t * 's, Bls12_381.Fr.t, 's) kinstr
  | KInt_bls12_381_fr : (Bls12_381.Fr.t, 's, z num, 's) kinstr
  | KNeg_bls12_381_g1 : (Bls12_381.G1.t, 's, Bls12_381.G1.t, 's) kinstr
  | KNeg_bls12_381_g2 : (Bls12_381.G2.t, 's, Bls12_381.G2.t, 's) kinstr
  | KNeg_bls12_381_fr : (Bls12_381.Fr.t, 's, Bls12_381.Fr.t, 's) kinstr
  | KPairing_check_bls12_381
      : ((Bls12_381.G1.t, Bls12_381.G2.t) pair boxed_list, 's, bool, 's) kinstr
  | KComb :
      int * ('a * 's, 'b * 'u) comb_gadt_witness
      -> ('a, 's, 'b, 'u) kinstr
  | KUncomb :
      int * ('a * 's, 'b * 'u) uncomb_gadt_witness
      -> ('a, 's, 'b, 'u) kinstr
  | KComb_get : int * ('t, 'v) comb_get_gadt_witness -> ('t, 's, 'v, 's) kinstr
  | KComb_set :
      int * ('a, 'b, 'c) comb_set_gadt_witness
      -> ('a, 'b * 's, 'c, 's) kinstr
  | KDup_n :
      int * ('a * 's, 't) dup_n_gadt_witness
      -> ('a, 's, 't, 'a * 's) kinstr
  | KTicket : ('a, n num * 's, 'a ticket, 's) kinstr
  | KRead_ticket
      : ('a ticket, 's, address * ('a * n num), 'a ticket * 's) kinstr
  | KSplit_ticket
      : ( 'a ticket,
          (n num * n num) * 's,
          ('a ticket * 'a ticket) option,
          's )
        kinstr
  | KJoin_tickets :
      'a comparable_ty
      -> ('a ticket * 'a ticket, 's, 'a ticket option, 's) kinstr

and ('arg, 'ret) lambda =
  | Lam :
      ('arg, end_of_stack, 'ret, end_of_stack) kdescr * Script.node
      -> ('arg, 'ret) lambda

and 'arg typed_contract = 'arg ty * address

(* ---- Auxiliary types -----------------------------------------------------*)
and 'ty ty =
  | Unit_t : type_annot option -> unit ty
  | Int_t : type_annot option -> z num ty
  | Nat_t : type_annot option -> n num ty
  | Signature_t : type_annot option -> signature ty
  | String_t : type_annot option -> string ty
  | Bytes_t : type_annot option -> bytes ty
  | Mutez_t : type_annot option -> Tez.t ty
  | Key_hash_t : type_annot option -> public_key_hash ty
  | Key_t : type_annot option -> public_key ty
  | Timestamp_t : type_annot option -> Script_timestamp.t ty
  | Address_t : type_annot option -> address ty
  | Bool_t : type_annot option -> bool ty
  | Pair_t :
      ('a ty * field_annot option * var_annot option)
      * ('b ty * field_annot option * var_annot option)
      * type_annot option
      -> ('a, 'b) pair ty
  | Union_t :
      ('a ty * field_annot option)
      * ('b ty * field_annot option)
      * type_annot option
      -> ('a, 'b) union ty
  | Lambda_t : 'arg ty * 'ret ty * type_annot option -> ('arg, 'ret) lambda ty
  | Option_t : 'v ty * type_annot option -> 'v option ty
  | List_t : 'v ty * type_annot option -> 'v boxed_list ty
  | Set_t : 'v comparable_ty * type_annot option -> 'v set ty
  | Map_t : 'k comparable_ty * 'v ty * type_annot option -> ('k, 'v) map ty
  | Big_map_t :
      'k comparable_ty * 'v ty * type_annot option
      -> ('k, 'v) big_map ty
  | Contract_t : 'arg ty * type_annot option -> 'arg typed_contract ty
  | Sapling_transaction_t :
      Sapling.Memo_size.t * type_annot option
      -> Sapling.transaction ty
  | Sapling_state_t :
      Sapling.Memo_size.t * type_annot option
      -> Sapling.state ty
  | Operation_t : type_annot option -> operation ty
  | Chain_id_t : type_annot option -> Chain_id.t ty
  | Never_t : type_annot option -> never ty
  | Bls12_381_g1_t : type_annot option -> Bls12_381.G1.t ty
  | Bls12_381_g2_t : type_annot option -> Bls12_381.G2.t ty
  | Bls12_381_fr_t : type_annot option -> Bls12_381.Fr.t ty
  | Ticket_t : 'a comparable_ty * type_annot option -> 'a ticket ty

and 'ty stack_ty =
  | Item_t :
      'ty ty * 'rest stack_ty * var_annot option
      -> ('ty * 'rest) stack_ty
  | Empty_t : unit stack_ty

and ('key, 'value) big_map = {
  id : Big_map.Id.t option;
  diff : ('key, 'value option) map;
  key_type : 'key comparable_ty;
  value_type : 'value ty;
}

and 'elt boxed_list = {elements : 'elt list; length : int}

and ('arg, 'storage) script = {
  code : (('arg, 'storage) pair, (operation boxed_list, 'storage) pair) lambda;
  arg_type : 'arg ty;
  storage : 'storage;
  storage_type : 'storage ty;
  root_name : field_annot option;
}

and ('a, 's, 'r, 'f) kdescr = {
  kloc : Script.location;
  kbef : ('a * 's) stack_ty;
  kaft : ('r * 'f) stack_ty;
  kinstr : ('a, 's, 'r, 'f) knext;
}

and ('a, 's, 'b, 'u) descr = {
  loc : Script.location;
  bef : ('a * 's) stack_ty;
  aft : ('b * 'u) stack_ty;
  instr : ('a, 's, 'b, 'u) cinstr;
}

and ('a, 's, 'b, 'u) cinstr = {
  size : int;
  apply :
    'r 'f. ('a, 's) kinfo -> ('b, 'u, 'r, 'f) knext -> ('a, 's, 'r, 'f) knext;
}

and ('a, 's) kinfo = {iloc : Script.location; kstack_ty : ('a * 's) stack_ty}

and (_, _, _, _, _, _, _, _) stack_prefix_preservation_witness =
  | KPrefix :
      ('y, 'u) kinfo
      * ('c, 'v, 'd, 'w, 'x, 's, 'y, 'u) stack_prefix_preservation_witness
      -> ( 'c,
           'v,
           'd,
           'w,
           'a,
           'x * 's,
           'a,
           'y * 'u )
         stack_prefix_preservation_witness
  | KRest : ('a, 's, 'b, 'u, 'a, 's, 'b, 'u) stack_prefix_preservation_witness

and ('before, 'after) comb_gadt_witness =
  | Comb_one : ('a * ('x * 'before), 'a * ('x * 'before)) comb_gadt_witness
  | Comb_succ :
      ('before, 'b * 'after) comb_gadt_witness
      -> ('a * 'before, ('a * 'b) * 'after) comb_gadt_witness

and ('before, 'after) uncomb_gadt_witness =
  | Uncomb_one : ('rest, 'rest) uncomb_gadt_witness
  | Uncomb_succ :
      ('b * 'before, 'after) uncomb_gadt_witness
      -> (('a * 'b) * 'before, 'a * 'after) uncomb_gadt_witness

and ('before, 'after) comb_get_gadt_witness =
  | Comb_get_zero : ('b, 'b) comb_get_gadt_witness
  | Comb_get_one : ('a * 'b, 'a) comb_get_gadt_witness
  | Comb_get_plus_two :
      ('before, 'after) comb_get_gadt_witness
      -> ('a * 'before, 'after) comb_get_gadt_witness

and ('value, 'before, 'after) comb_set_gadt_witness =
  | Comb_set_zero : ('value, _, 'value) comb_set_gadt_witness
  | Comb_set_one : ('value, 'hd * 'tl, 'value * 'tl) comb_set_gadt_witness
  | Comb_set_plus_two :
      ('value, 'before, 'after) comb_set_gadt_witness
      -> ('value, 'a * 'before, 'a * 'after) comb_set_gadt_witness

(*

   [dup_n_gadt_witness ('s, 't)] ensures that the n-th element of ['s]
   is of type ['t].

   This relational predicate is defined by induction on [n].

*)
and (_, _) dup_n_gadt_witness =
  | Dup_n_zero : ('a * 'rest, 'a) dup_n_gadt_witness
  | Dup_n_succ :
      ('stack, 'b) dup_n_gadt_witness
      -> ('a * 'stack, 'b) dup_n_gadt_witness

and (_, _, _) exkinstr =
  | ExKInstr : ('x, 'z, 'b, 'u) kinstr -> ('x * 'z, 'b, 'u) exkinstr
[@@unboxed]

and ('bef_top, 'bef, 'res_top, 'res) knext =
  | KNext :
      ('bef_top, 'bef) kinfo
      * ('bef_top, 'bef, 'res_top_t, 'res_t) kinstr
      * ('res_top_t, 'res_t, 'res_top, 'res) knext
      -> ('bef_top, 'bef, 'res_top, 'res) knext
  | KHalt : ('bef_top, 'bef) kinfo -> ('bef_top, 'bef, 'bef_top, 'bef) knext

let kinfo_of_knext : type a s b f. (a, s, b, f) knext -> (a, s) kinfo =
  function
  | KNext (kinfo, _, _) ->
      kinfo
  | KHalt kinfo ->
      kinfo

(* TODO: This is dumb and shouldn't be needed *)
let rec kinfo_of_khalt : type a s b f. (a, s, b, f) knext -> (b, f) kinfo =
  function
  | KNext (_, _, next) ->
      kinfo_of_khalt next
  | KHalt kinfo ->
      kinfo

let rec ty_of_comparable_ty : type a. a comparable_ty -> a ty =
 fun s ->
  match s with
  | Unit_key _ ->
      Unit_t None
  | Never_key _ ->
      Never_t None
  | Int_key _ ->
      Int_t None
  | Nat_key _ ->
      Nat_t None
  | Signature_key _ ->
      Signature_t None
  | String_key _ ->
      String_t None
  | Bytes_key _ ->
      Bytes_t None
  | Mutez_key _ ->
      Mutez_t None
  | Bool_key _ ->
      Bool_t None
  | Key_hash_key _ ->
      Key_hash_t None
  | Key_key _ ->
      Key_t None
  | Timestamp_key _ ->
      Timestamp_t None
  | Chain_id_key _ ->
      Chain_id_t None
  | Address_key _ ->
      Address_t None
  | Pair_key ((a, _), (b, _), _) ->
      Pair_t
        ( (ty_of_comparable_ty a, None, None),
          (ty_of_comparable_ty b, None, None),
          None )
  | Union_key ((a, _), (b, _), _) ->
      Union_t
        ((ty_of_comparable_ty a, None), (ty_of_comparable_ty b, None), None)
  | Option_key (t, _) ->
      Option_t (ty_of_comparable_ty t, None)

let unlist_ty : type a. a boxed_list ty -> a ty = function
  | List_t (a, _) ->
      a
  | _ ->
      (* FIXME: This is not robust to evolutions. *)
      (* because of the concrete implementations of the type
        constructors occurring in the definition of [ty]: *)
      assert false

let unset_ty : type a. a set ty -> a ty = function
  | Set_t (a, _) ->
      ty_of_comparable_ty a
  | _ ->
      (* FIXME: This is not robust to evolutions. *)
      (* because of the concrete implementations of the type
        constructors occurring in the definition of [ty]: *)
      assert false

let unmap_ty : type k v. (k, v) map ty -> k ty * v ty = function
  | Map_t (k, v, _) ->
      (ty_of_comparable_ty k, v)
  | _ ->
      (* FIXME: This is not robust to evolutions. *)
      (* because of the concrete implementations of the type
        constructors occurring in the definition of [ty]: *)
      assert false

let close_descr {loc; bef; aft; instr} =
  let kinfo = {iloc = loc; kstack_ty = aft} in
  let kinfo' = {iloc = loc; kstack_ty = bef} in
  let kinstr = instr.apply kinfo' (KHalt kinfo) in
  {kloc = loc; kbef = bef; kaft = aft; kinstr}

let kinfo_of_descr {loc; bef; _} = {iloc = loc; kstack_ty = bef}

let compose_descr :
    type a s b u c v.
    Script.location ->
    (a, s, b, u) descr ->
    (b, u, c, v) descr ->
    (a, s, c, v) descr =
 fun loc d1 d2 ->
  {
    loc;
    bef = d1.bef;
    aft = d2.aft;
    instr =
      {
        size = d1.instr.size + d2.instr.size;
        apply =
          (fun _ k ->
            d1.instr.apply
              (kinfo_of_descr d1)
              (d2.instr.apply (kinfo_of_descr d2) k));
      };
  }
