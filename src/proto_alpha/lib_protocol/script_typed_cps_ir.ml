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
  | KDrop :
      ('a, 'b * 's) kinfo * ('b, 's, 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | KDup :
      ('a, 's) kinfo * ('a, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KSwap :
      ('a, 'b * 's) kinfo * ('b, 'a * 's, 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | KConst :
      ('a, 's) kinfo * 'ty * ('ty, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  (*
     Pairs
     -----
  *)
  | KCons_pair :
      ('a, 'b * 's) kinfo * ('a * 'b, 's, 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | KCar :
      ('a * 'b, 's) kinfo * ('a, 's, 'r, 'f) kinstr
      -> ('a * 'b, 's, 'r, 'f) kinstr
  | KCdr :
      ('a * 'b, 's) kinfo * ('b, 's, 'r, 'f) kinstr
      -> ('a * 'b, 's, 'r, 'f) kinstr
  | KUnpair :
      ('a * 'b, 's) kinfo * ('a, 'b * 's, 'r, 'f) kinstr
      -> ('a * 'b, 's, 'r, 'f) kinstr
  (*
     Options
     -------
   *)
  | KCons_some :
      ('v, 's) kinfo * ('v option, 's, 'r, 'f) kinstr
      -> ('v, 's, 'r, 'f) kinstr
  | KCons_none :
      ('a, 's) kinfo * 'b ty * ('b option, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KIf_none :
      ('a option, 'b * 's) kinfo
      (* Notice that the continuations of the following two
         instructions should have a shared suffix to avoid code
         duplication. *)
      * ('b, 's, 'r, 'f) kinstr
      * ('a, 'b * 's, 'r, 'f) kinstr
      -> ('a option, 'b * 's, 'r, 'f) kinstr
  (*
     Unions
     ------
   *)
  | KCons_left :
      ('a, 's) kinfo * (('a, 'b) union, 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KCons_right :
      ('b, 's) kinfo * (('a, 'b) union, 's, 'r, 'f) kinstr
      -> ('b, 's, 'r, 'f) kinstr
  | KIf_left :
      (('a, 'b) union, 's) kinfo
      (* Notice that the continuations of the following two
         instructions should have a shared suffix to avoid code
         duplication. *)
      * ('a, 's, 'r, 'f) kinstr
      * ('b, 's, 'r, 'f) kinstr
      -> (('a, 'b) union, 's, 'r, 'f) kinstr
  (*
     Lists
     -----
  *)
  | KCons_list :
      ('a, 'a boxed_list * 's) kinfo * ('a boxed_list, 's, 'r, 'f) kinstr
      -> ('a, 'a boxed_list * 's, 'r, 'f) kinstr
  | KNil :
      ('a, 's) kinfo * ('b boxed_list, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KIf_cons :
      ('a boxed_list, 'b * 's) kinfo
      (* Notice that the continuations of the following two
         instructions should have a shared suffix to avoid code
         duplication. *)
      * ('a, 'a boxed_list * ('b * 's), 'r, 'f) kinstr
      * ('b, 's, 'r, 'f) kinstr
      -> ('a boxed_list, 'b * 's, 'r, 'f) kinstr
  | KList_map :
      ('a boxed_list, 'c * 's) kinfo
      * ('a, 'c * 's, 'b, 'c * 's) kinstr
      * ('b boxed_list, 'c * 's, 'r, 'f) kinstr
      -> ('a boxed_list, 'c * 's, 'r, 'f) kinstr
  | KList_iter :
      ('a boxed_list, 'b * 's) kinfo
      * ('a, 'b * 's, 'b, 's) kinstr
      * ('b, 's, 'r, 'f) kinstr
      -> ('a boxed_list, 'b * 's, 'r, 'f) kinstr
  | KList_size :
      ('a boxed_list, 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> ('a boxed_list, 's, 'r, 'f) kinstr
  (*
    Sets
    ----
  *)
  | KEmpty_set :
      ('a, 's) kinfo * 'b comparable_ty * ('b set, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KSet_iter :
      ('a set, 'b * 's) kinfo
      * ('a, 'b * 's, 'b, 's) kinstr
      * ('b, 's, 'r, 'f) kinstr
      -> ('a set, 'b * 's, 'r, 'f) kinstr
  | KSet_mem :
      ('a, 'a set * 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> ('a, 'a set * 's, 'r, 'f) kinstr
  | KSet_update :
      ('a, bool * ('a set * 's)) kinfo * ('a set, 's, 'r, 'f) kinstr
      -> ('a, bool * ('a set * 's), 'r, 'f) kinstr
  | KSet_size :
      ('a set, 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> ('a set, 's, 'r, 'f) kinstr
  (*
     Maps
     ----
   *)
  | KEmpty_map :
      ('a, 's) kinfo
      * 'b comparable_ty
      * 'c ty
      * (('b, 'c) map, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KMap_map :
      (('a, 'b) map, 'd * 's) kinfo
      * ('a * 'b, 'd * 's, 'c, 'd * 's) kinstr
      * (('a, 'c) map, 'd * 's, 'r, 'f) kinstr
      -> (('a, 'b) map, 'd * 's, 'r, 'f) kinstr
  | KMap_iter :
      (('a, 'b) map, 'c * 's) kinfo
      * ('a * 'b, 'c * 's, 'c, 's) kinstr
      * ('c, 's, 'r, 'f) kinstr
      -> (('a, 'b) map, 'c * 's, 'r, 'f) kinstr
  | KMap_mem :
      ('a, ('a, 'b) map * 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> ('a, ('a, 'b) map * 's, 'r, 'f) kinstr
  | KMap_get :
      ('a, ('a, 'b) map * 's) kinfo * ('b option, 's, 'r, 'f) kinstr
      -> ('a, ('a, 'b) map * 's, 'r, 'f) kinstr
  | KMap_update :
      ('a, 'b option * (('a, 'b) map * 's)) kinfo
      * (('a, 'b) map, 's, 'r, 'f) kinstr
      -> ('a, 'b option * (('a, 'b) map * 's), 'r, 'f) kinstr
  | KMap_get_and_update :
      ('a, 'v option * (('a, 'v) map * 'rest)) kinfo
      * ('v option, ('a, 'v) map * 'rest, 'r, 'f) kinstr
      -> ('a, 'v option * (('a, 'v) map * 'rest), 'r, 'f) kinstr
  | KMap_size :
      (('a, 'b) map, 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (('a, 'b) map, 's, 'r, 'f) kinstr
  (*
     Big maps
     --------
  *)
  | KEmpty_big_map :
      ('a, 's) kinfo
      * 'b comparable_ty
      * 'c ty
      * (('b, 'c) big_map, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KBig_map_mem :
      ('a, ('a, 'b) big_map * 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> ('a, ('a, 'b) big_map * 's, 'r, 'f) kinstr
  | KBig_map_get :
      ('a, ('a, 'b) big_map * 's) kinfo * ('b option, 's, 'r, 'f) kinstr
      -> ('a, ('a, 'b) big_map * 's, 'r, 'f) kinstr
  | KBig_map_update :
      ('a, 'b option * (('a, 'b) big_map * 's)) kinfo
      * (('a, 'b) big_map, 's, 'r, 'f) kinstr
      -> ('a, 'b option * (('a, 'b) big_map * 's), 'r, 'f) kinstr
  | KBig_map_get_and_update :
      ('a, 'v option * (('a, 'v) big_map * 'rest)) kinfo
      * ('v option, ('a, 'v) big_map * 'rest, 'r, 'f) kinstr
      -> ('a, 'v option * (('a, 'v) big_map * 'rest), 'r, 'f) kinstr
  (*
     Strings
     -------
  *)
  | KConcat_string :
      (string boxed_list, 's) kinfo * (string, 's, 'r, 'f) kinstr
      -> (string boxed_list, 's, 'r, 'f) kinstr
  | KConcat_string_pair :
      (string, string * 's) kinfo * (string, 's, 'r, 'f) kinstr
      -> (string, string * 's, 'r, 'f) kinstr
  | KSlice_string :
      (n num, n num * (string * 's)) kinfo * (string option, 's, 'r, 'f) kinstr
      -> (n num, n num * (string * 's), 'r, 'f) kinstr
  | KString_size :
      (string, 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (string, 's, 'r, 'f) kinstr
  (*
     Bytes
     -----
  *)
  | KConcat_bytes :
      (bytes boxed_list, 's) kinfo * (bytes, 's, 'r, 'f) kinstr
      -> (bytes boxed_list, 's, 'r, 'f) kinstr
  | KConcat_bytes_pair :
      (bytes, bytes * 's) kinfo * (bytes, 's, 'r, 'f) kinstr
      -> (bytes, bytes * 's, 'r, 'f) kinstr
  | KSlice_bytes :
      (n num, n num * (bytes * 's)) kinfo * (bytes option, 's, 'r, 'f) kinstr
      -> (n num, n num * (bytes * 's), 'r, 'f) kinstr
  | KBytes_size :
      (bytes, 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (bytes, 's, 'r, 'f) kinstr
  (*
     Timestamps
     ----------
   *)
  | KAdd_seconds_to_timestamp :
      (z num, Script_timestamp.t * 's) kinfo
      * (Script_timestamp.t, 's, 'r, 'f) kinstr
      -> (z num, Script_timestamp.t * 's, 'r, 'f) kinstr
  | KAdd_timestamp_to_seconds :
      (Script_timestamp.t, z num * 's) kinfo
      * (Script_timestamp.t, 's, 'r, 'f) kinstr
      -> (Script_timestamp.t, z num * 's, 'r, 'f) kinstr
  | KSub_timestamp_seconds :
      (Script_timestamp.t, z num * 's) kinfo
      * (Script_timestamp.t, 's, 'r, 'f) kinstr
      -> (Script_timestamp.t, z num * 's, 'r, 'f) kinstr
  | KDiff_timestamps :
      (Script_timestamp.t, Script_timestamp.t * 's) kinfo
      * (z num, 's, 'r, 'f) kinstr
      -> (Script_timestamp.t, Script_timestamp.t * 's, 'r, 'f) kinstr
  (*
     Tez
     ---
    *)
  | KAdd_tez :
      (Tez.t, Tez.t * 's) kinfo * (Tez.t, 's, 'r, 'f) kinstr
      -> (Tez.t, Tez.t * 's, 'r, 'f) kinstr
  | KSub_tez :
      (Tez.t, Tez.t * 's) kinfo * (Tez.t, 's, 'r, 'f) kinstr
      -> (Tez.t, Tez.t * 's, 'r, 'f) kinstr
  | KMul_teznat :
      (Tez.t, n num * 's) kinfo * (Tez.t, 's, 'r, 'f) kinstr
      -> (Tez.t, n num * 's, 'r, 'f) kinstr
  | KMul_nattez :
      (n num, Tez.t * 's) kinfo * (Tez.t, 's, 'r, 'f) kinstr
      -> (n num, Tez.t * 's, 'r, 'f) kinstr
  | KEdiv_teznat :
      (Tez.t, n num * 's) kinfo
      * ((Tez.t, Tez.t) pair option, 's, 'r, 'f) kinstr
      -> (Tez.t, n num * 's, 'r, 'f) kinstr
  | KEdiv_tez :
      (Tez.t, Tez.t * 's) kinfo
      * ((n num, Tez.t) pair option, 's, 'r, 'f) kinstr
      -> (Tez.t, Tez.t * 's, 'r, 'f) kinstr
  (*
     Booleans
     --------
   *)
  | KOr :
      (bool, bool * 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (bool, bool * 's, 'r, 'f) kinstr
  | KAnd :
      (bool, bool * 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (bool, bool * 's, 'r, 'f) kinstr
  | KXor :
      (bool, bool * 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (bool, bool * 's, 'r, 'f) kinstr
  | KNot :
      (bool, 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (bool, 's, 'r, 'f) kinstr
  (*
     Integers
     --------
  *)
  | KIs_nat :
      (z num, 's) kinfo * (n num option, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  | KNeg_nat :
      (n num, 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> (n num, 's, 'r, 'f) kinstr
  | KNeg_int :
      (z num, 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  | KAbs_int :
      (z num, 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  | KInt_nat :
      (n num, 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> (n num, 's, 'r, 'f) kinstr
  | KAdd_intint :
      (z num, z num * 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> (z num, z num * 's, 'r, 'f) kinstr
  | KAdd_intnat :
      (z num, n num * 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> (z num, n num * 's, 'r, 'f) kinstr
  | KAdd_natint :
      (n num, z num * 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> (n num, z num * 's, 'r, 'f) kinstr
  | KAdd_natnat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (n num, n num * 's, 'r, 'f) kinstr
  | KSub_int :
      ('a num, 'b num * 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> ('a num, 'b num * 's, 'r, 'f) kinstr
  | KMul_intint :
      (z num, z num * 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> (z num, z num * 's, 'r, 'f) kinstr
  | KMul_intnat :
      (z num, n num * 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> (z num, n num * 's, 'r, 'f) kinstr
  | KMul_natint :
      (n num, z num * 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> (n num, z num * 's, 'r, 'f) kinstr
  | KMul_natnat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (n num, n num * 's, 'r, 'f) kinstr
  | KEdiv_intint :
      (z num, z num * 's) kinfo
      * ((z num, n num) pair option, 's, 'r, 'f) kinstr
      -> (z num, z num * 's, 'r, 'f) kinstr
  | KEdiv_intnat :
      (z num, n num * 's) kinfo
      * ((z num, n num) pair option, 's, 'r, 'f) kinstr
      -> (z num, n num * 's, 'r, 'f) kinstr
  | KEdiv_natint :
      (n num, z num * 's) kinfo
      * ((z num, n num) pair option, 's, 'r, 'f) kinstr
      -> (n num, z num * 's, 'r, 'f) kinstr
  | KEdiv_natnat :
      (n num, n num * 's) kinfo
      * ((n num, n num) pair option, 's, 'r, 'f) kinstr
      -> (n num, n num * 's, 'r, 'f) kinstr
  | KLsl_nat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (n num, n num * 's, 'r, 'f) kinstr
  | KLsr_nat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (n num, n num * 's, 'r, 'f) kinstr
  | KOr_nat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (n num, n num * 's, 'r, 'f) kinstr
  | KAnd_nat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (n num, n num * 's, 'r, 'f) kinstr
  | KAnd_int_nat :
      (z num, n num * 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (z num, n num * 's, 'r, 'f) kinstr
  | KXor_nat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (n num, n num * 's, 'r, 'f) kinstr
  | KNot_nat :
      (n num, 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> (n num, 's, 'r, 'f) kinstr
  | KNot_int :
      (z num, 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  (*
     Control
     -------
  *)
  | KIf :
      (bool, 'a * 's) kinfo
      (* Notice that the continuations of the following two
         instructions should have a shared suffix to avoid code
         duplication. *)
      * ('a, 's, 'r, 'f) kinstr
      * ('a, 's, 'r, 'f) kinstr
      -> (bool, 'a * 's, 'r, 'f) kinstr
  | KLoop :
      (bool, 'a * 's) kinfo
      * ('a, 's, bool, 'a * 's) kinstr
      * ('a, 's, 'r, 'f) kinstr
      -> (bool, 'a * 's, 'r, 'f) kinstr
  | KLoop_left :
      (('a, 'b) union, 's) kinfo
      * ('a, 's, ('a, 'b) union, 's) kinstr
      * ('b, 's, 'r, 'f) kinstr
      -> (('a, 'b) union, 's, 'r, 'f) kinstr
  | KDip :
      ('a, 'b * 's) kinfo
      * ('c, 't) kinfo
      * ('b, 's, 'c, 't) kinstr
      * ('a, 'c * 't, 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | KExec :
      ('a, ('a, 'b) lambda * 's) kinfo * ('b, 's, 'r, 'f) kinstr
      -> ('a, ('a, 'b) lambda * 's, 'r, 'f) kinstr
  | KApply :
      ('a, ('a * 't, 'b) lambda * 's) kinfo
      * 'a ty
      * (('t, 'b) lambda, 's, 'r, 'f) kinstr
      -> ('a, ('a * 't, 'b) lambda * 's, 'r, 'f) kinstr
  | KLambda :
      ('a, 's) kinfo
      * ('b, 'c) lambda
      * (('b, 'c) lambda, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KFailwith :
      ('a, 's) kinfo * Script.location * 'a ty * ('b, 't, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KNop : ('a, 's) kinfo * ('a, 's, 'r, 'f) kinstr -> ('a, 's, 'r, 'f) kinstr
  (*
     Comparison
     ----------
  *)
  | KCompare :
      ('a, 'a * 's) kinfo * 'a comparable_ty * (z num, 's, 'r, 'f) kinstr
      -> ('a, 'a * 's, 'r, 'f) kinstr
  (*
     Comparators
     -----------
  *)
  | KEq :
      (z num, 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  | KNeq :
      (z num, 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  | KLt :
      (z num, 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  | KGt :
      (z num, 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  | KLe :
      (z num, 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  | KGe :
      (z num, 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  (*
     Protocol
     --------
  *)
  | KAddress :
      ('a typed_contract, 's) kinfo * (address, 's, 'r, 'f) kinstr
      -> ('a typed_contract, 's, 'r, 'f) kinstr
  | KContract :
      (address, 's) kinfo
      * 'a ty
      * string
      * ('a typed_contract option, 's, 'r, 'f) kinstr
      -> (address, 's, 'r, 'f) kinstr
  | KTransfer_tokens :
      ('a, Tez.t * ('a typed_contract * 's)) kinfo
      * (operation, 's, 'r, 'f) kinstr
      -> ('a, Tez.t * ('a typed_contract * 's), 'r, 'f) kinstr
  | KImplicit_account :
      (public_key_hash, 's) kinfo * (unit typed_contract, 's, 'r, 'f) kinstr
      -> (public_key_hash, 's, 'r, 'f) kinstr
  | KCreate_contract :
      (public_key_hash option, Tez.t * ('a * 's)) kinfo
      * 'a ty
      * 'b ty
      * ('b * 'a, operation boxed_list * 'a) lambda
      * field_annot option
      * (operation, address * 's, 'r, 'f) kinstr
      -> (public_key_hash option, Tez.t * ('a * 's), 'r, 'f) kinstr
  | KSet_delegate :
      (public_key_hash option, 's) kinfo * (operation, 's, 'r, 'f) kinstr
      -> (public_key_hash option, 's, 'r, 'f) kinstr
  | KNow :
      ('a, 's) kinfo * (Script_timestamp.t, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KBalance :
      ('a, 's) kinfo * (Tez.t, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KLevel :
      ('a, 's) kinfo * (n num, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KCheck_signature :
      (public_key, signature * (bytes * 's)) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (public_key, signature * (bytes * 's), 'r, 'f) kinstr
  | KHash_key :
      (public_key, 's) kinfo * (public_key_hash, 's, 'r, 'f) kinstr
      -> (public_key, 's, 'r, 'f) kinstr
  | KPack :
      ('a, 's) kinfo * 'a ty * (bytes, 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KUnpack :
      (bytes, 's) kinfo * 'a ty * ('a option, 's, 'r, 'f) kinstr
      -> (bytes, 's, 'r, 'f) kinstr
  | KBlake2b :
      (bytes, 's) kinfo * (bytes, 's, 'r, 'f) kinstr
      -> (bytes, 's, 'r, 'f) kinstr
  | KSha256 :
      (bytes, 's) kinfo * (bytes, 's, 'r, 'f) kinstr
      -> (bytes, 's, 'r, 'f) kinstr
  | KSha512 :
      (bytes, 's) kinfo * (bytes, 's, 'r, 'f) kinstr
      -> (bytes, 's, 'r, 'f) kinstr
  | KSource :
      ('a, 's) kinfo * (address, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KSender :
      ('a, 's) kinfo * (address, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KSelf :
      ('a, 's) kinfo
      * 'b ty
      * string
      * ('b typed_contract, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KSelf_address :
      ('a, 's) kinfo * (address, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KAmount :
      ('a, 's) kinfo * (Tez.t, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KSapling_empty_state :
      ('a, 's) kinfo
      * Sapling.Memo_size.t
      * (Sapling.state, 'a * 's, 'b, 'f) kinstr
      -> ('a, 's, 'b, 'f) kinstr
  | KSapling_verify_update :
      (Sapling.transaction, Sapling.state * 's) kinfo
      * ((z num, Sapling.state) pair option, 's, 'r, 'f) kinstr
      -> (Sapling.transaction, Sapling.state * 's, 'r, 'f) kinstr
  | KDig :
      ('a, 's) kinfo
      * int
      * ('b, 'c * 't, 'c, 't, 'a, 's, 'd, 'u) stack_prefix_preservation_witness
      * ('b, 'd * 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KDug :
      ('a, 'b * 's) kinfo
      * int
      * ('c, 't, 'a, 'c * 't, 'b, 's, 'd, 'u) stack_prefix_preservation_witness
      * ('d, 'u, 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | KDipn :
      ('a, 's) kinfo
      * int
      * ('c, 't, 'd, 'v, 'a, 's, 'b, 'u) stack_prefix_preservation_witness
      * ('c, 't, 'd, 'v) kinstr
      * ('b, 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KDropn :
      ('a, 's) kinfo
      * int
      * ('b, 'u, 'b, 'u, 'a, 's, 'a, 's) stack_prefix_preservation_witness
      * ('b, 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KChainId :
      ('a, 's) kinfo * (Chain_id.t, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KNever :
      (never, 's) kinfo * ('b, 'u, 'r, 'f) kinstr
      -> (never, 's, 'r, 'f) kinstr
  | KVoting_power :
      (public_key_hash, 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (public_key_hash, 's, 'r, 'f) kinstr
  | KTotal_voting_power :
      ('a, 's) kinfo * (n num, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KKeccak :
      (bytes, 's) kinfo * (bytes, 's, 'r, 'f) kinstr
      -> (bytes, 's, 'r, 'f) kinstr
  | KSha3 :
      (bytes, 's) kinfo * (bytes, 's, 'r, 'f) kinstr
      -> (bytes, 's, 'r, 'f) kinstr
  | KAdd_bls12_381_g1 :
      (Bls12_381.G1.t, Bls12_381.G1.t * 's) kinfo
      * (Bls12_381.G1.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.G1.t, Bls12_381.G1.t * 's, 'r, 'f) kinstr
  | KAdd_bls12_381_g2 :
      (Bls12_381.G2.t, Bls12_381.G2.t * 's) kinfo
      * (Bls12_381.G2.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.G2.t, Bls12_381.G2.t * 's, 'r, 'f) kinstr
  | KAdd_bls12_381_fr :
      (Bls12_381.Fr.t, Bls12_381.Fr.t * 's) kinfo
      * (Bls12_381.Fr.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.Fr.t, Bls12_381.Fr.t * 's, 'r, 'f) kinstr
  | KMul_bls12_381_g1 :
      (Bls12_381.G1.t, Bls12_381.Fr.t * 's) kinfo
      * (Bls12_381.G1.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.G1.t, Bls12_381.Fr.t * 's, 'r, 'f) kinstr
  | KMul_bls12_381_g2 :
      (Bls12_381.G2.t, Bls12_381.Fr.t * 's) kinfo
      * (Bls12_381.G2.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.G2.t, Bls12_381.Fr.t * 's, 'r, 'f) kinstr
  | KMul_bls12_381_fr :
      (Bls12_381.Fr.t, Bls12_381.Fr.t * 's) kinfo
      * (Bls12_381.Fr.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.Fr.t, Bls12_381.Fr.t * 's, 'r, 'f) kinstr
  | KMul_bls12_381_z_fr :
      (Bls12_381.Fr.t, 'a num * 's) kinfo * (Bls12_381.Fr.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.Fr.t, 'a num * 's, 'r, 'f) kinstr
  | KMul_bls12_381_fr_z :
      ('a num, Bls12_381.Fr.t * 's) kinfo * (Bls12_381.Fr.t, 's, 'r, 'f) kinstr
      -> ('a num, Bls12_381.Fr.t * 's, 'r, 'f) kinstr
  | KInt_bls12_381_fr :
      (Bls12_381.Fr.t, 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> (Bls12_381.Fr.t, 's, 'r, 'f) kinstr
  | KNeg_bls12_381_g1 :
      (Bls12_381.G1.t, 's) kinfo * (Bls12_381.G1.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.G1.t, 's, 'r, 'f) kinstr
  | KNeg_bls12_381_g2 :
      (Bls12_381.G2.t, 's) kinfo * (Bls12_381.G2.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.G2.t, 's, 'r, 'f) kinstr
  | KNeg_bls12_381_fr :
      (Bls12_381.Fr.t, 's) kinfo * (Bls12_381.Fr.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.Fr.t, 's, 'r, 'f) kinstr
  | KPairing_check_bls12_381 :
      ((Bls12_381.G1.t, Bls12_381.G2.t) pair boxed_list, 's) kinfo
      * (bool, 's, 'r, 'f) kinstr
      -> ((Bls12_381.G1.t, Bls12_381.G2.t) pair boxed_list, 's, 'r, 'f) kinstr
  | KComb :
      ('a, 's) kinfo
      * int
      * ('a * 's, 'b * 'u) comb_gadt_witness
      * ('b, 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KUncomb :
      ('a, 's) kinfo
      * int
      * ('a * 's, 'b * 'u) uncomb_gadt_witness
      * ('b, 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KComb_get :
      ('t, 's) kinfo
      * int
      * ('t, 'v) comb_get_gadt_witness
      * ('v, 's, 'r, 'f) kinstr
      -> ('t, 's, 'r, 'f) kinstr
  | KComb_set :
      ('a, 'b * 's) kinfo
      * int
      * ('a, 'b, 'c) comb_set_gadt_witness
      * ('c, 's, 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | KDup_n :
      ('a, 's) kinfo
      * int
      * ('a * 's, 't) dup_n_gadt_witness
      * ('t, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KTicket :
      ('a, n num * 's) kinfo * ('a ticket, 's, 'r, 'f) kinstr
      -> ('a, n num * 's, 'r, 'f) kinstr
  | KRead_ticket :
      ('a ticket, 's) kinfo
      * (address * ('a * n num), 'a ticket * 's, 'r, 'f) kinstr
      -> ('a ticket, 's, 'r, 'f) kinstr
  | KSplit_ticket :
      ('a ticket, (n num * n num) * 's) kinfo
      * (('a ticket * 'a ticket) option, 's, 'r, 'f) kinstr
      -> ('a ticket, (n num * n num) * 's, 'r, 'f) kinstr
  | KJoin_tickets :
      ('a ticket * 'a ticket, 's) kinfo
      * 'a comparable_ty
      * ('a ticket option, 's, 'r, 'f) kinstr
      -> ('a ticket * 'a ticket, 's, 'r, 'f) kinstr
  | KHalt : ('a, 's) kinfo -> ('a, 's, 'a, 's) kinstr

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
  kinstr : ('a, 's, 'r, 'f) kinstr;
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
    'r 'f. ('a, 's) kinfo -> ('b, 'u, 'r, 'f) kinstr -> ('a, 's, 'r, 'f) kinstr;
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

let kinfo_of_kinstr : type a s b f. (a, s, b, f) kinstr -> (a, s) kinfo =
 fun i ->
  match i with
  | KDrop (kinfo, _) ->
      kinfo
  | KDup (kinfo, _) ->
      kinfo
  | KSwap (kinfo, _) ->
      kinfo
  | KConst (kinfo, _, _) ->
      kinfo
  | KCons_pair (kinfo, _) ->
      kinfo
  | KCar (kinfo, _) ->
      kinfo
  | KCdr (kinfo, _) ->
      kinfo
  | KUnpair (kinfo, _) ->
      kinfo
  | KCons_some (kinfo, _) ->
      kinfo
  | KCons_none (kinfo, _, _) ->
      kinfo
  | KIf_none (kinfo, _, _) ->
      kinfo
  | KCons_left (kinfo, _) ->
      kinfo
  | KCons_right (kinfo, _) ->
      kinfo
  | KIf_left (kinfo, _, _) ->
      kinfo
  | KCons_list (kinfo, _) ->
      kinfo
  | KNil (kinfo, _) ->
      kinfo
  | KIf_cons (kinfo, _, _) ->
      kinfo
  | KList_map (kinfo, _, _) ->
      kinfo
  | KList_iter (kinfo, _, _) ->
      kinfo
  | KList_size (kinfo, _) ->
      kinfo
  | KEmpty_set (kinfo, _, _) ->
      kinfo
  | KSet_iter (kinfo, _, _) ->
      kinfo
  | KSet_mem (kinfo, _) ->
      kinfo
  | KSet_update (kinfo, _) ->
      kinfo
  | KSet_size (kinfo, _) ->
      kinfo
  | KEmpty_map (kinfo, _, _, _) ->
      kinfo
  | KMap_map (kinfo, _, _) ->
      kinfo
  | KMap_iter (kinfo, _, _) ->
      kinfo
  | KMap_mem (kinfo, _) ->
      kinfo
  | KMap_get (kinfo, _) ->
      kinfo
  | KMap_update (kinfo, _) ->
      kinfo
  | KMap_get_and_update (kinfo, _) ->
      kinfo
  | KMap_size (kinfo, _) ->
      kinfo
  | KEmpty_big_map (kinfo, _, _, _) ->
      kinfo
  | KBig_map_mem (kinfo, _) ->
      kinfo
  | KBig_map_get (kinfo, _) ->
      kinfo
  | KBig_map_update (kinfo, _) ->
      kinfo
  | KBig_map_get_and_update (kinfo, _) ->
      kinfo
  | KConcat_string (kinfo, _) ->
      kinfo
  | KConcat_string_pair (kinfo, _) ->
      kinfo
  | KSlice_string (kinfo, _) ->
      kinfo
  | KString_size (kinfo, _) ->
      kinfo
  | KConcat_bytes (kinfo, _) ->
      kinfo
  | KConcat_bytes_pair (kinfo, _) ->
      kinfo
  | KSlice_bytes (kinfo, _) ->
      kinfo
  | KBytes_size (kinfo, _) ->
      kinfo
  | KAdd_seconds_to_timestamp (kinfo, _) ->
      kinfo
  | KAdd_timestamp_to_seconds (kinfo, _) ->
      kinfo
  | KSub_timestamp_seconds (kinfo, _) ->
      kinfo
  | KDiff_timestamps (kinfo, _) ->
      kinfo
  | KAdd_tez (kinfo, _) ->
      kinfo
  | KSub_tez (kinfo, _) ->
      kinfo
  | KMul_teznat (kinfo, _) ->
      kinfo
  | KMul_nattez (kinfo, _) ->
      kinfo
  | KEdiv_teznat (kinfo, _) ->
      kinfo
  | KEdiv_tez (kinfo, _) ->
      kinfo
  | KOr (kinfo, _) ->
      kinfo
  | KAnd (kinfo, _) ->
      kinfo
  | KXor (kinfo, _) ->
      kinfo
  | KNot (kinfo, _) ->
      kinfo
  | KIs_nat (kinfo, _) ->
      kinfo
  | KNeg_nat (kinfo, _) ->
      kinfo
  | KNeg_int (kinfo, _) ->
      kinfo
  | KAbs_int (kinfo, _) ->
      kinfo
  | KInt_nat (kinfo, _) ->
      kinfo
  | KAdd_intint (kinfo, _) ->
      kinfo
  | KAdd_intnat (kinfo, _) ->
      kinfo
  | KAdd_natint (kinfo, _) ->
      kinfo
  | KAdd_natnat (kinfo, _) ->
      kinfo
  | KSub_int (kinfo, _) ->
      kinfo
  | KMul_intint (kinfo, _) ->
      kinfo
  | KMul_intnat (kinfo, _) ->
      kinfo
  | KMul_natint (kinfo, _) ->
      kinfo
  | KMul_natnat (kinfo, _) ->
      kinfo
  | KEdiv_intint (kinfo, _) ->
      kinfo
  | KEdiv_intnat (kinfo, _) ->
      kinfo
  | KEdiv_natint (kinfo, _) ->
      kinfo
  | KEdiv_natnat (kinfo, _) ->
      kinfo
  | KLsl_nat (kinfo, _) ->
      kinfo
  | KLsr_nat (kinfo, _) ->
      kinfo
  | KOr_nat (kinfo, _) ->
      kinfo
  | KAnd_nat (kinfo, _) ->
      kinfo
  | KAnd_int_nat (kinfo, _) ->
      kinfo
  | KXor_nat (kinfo, _) ->
      kinfo
  | KNot_nat (kinfo, _) ->
      kinfo
  | KNot_int (kinfo, _) ->
      kinfo
  | KIf (kinfo, _, _) ->
      kinfo
  | KLoop (kinfo, _, _) ->
      kinfo
  | KLoop_left (kinfo, _, _) ->
      kinfo
  | KDip (kinfo, _, _, _) ->
      kinfo
  | KExec (kinfo, _) ->
      kinfo
  | KApply (kinfo, _, _) ->
      kinfo
  | KLambda (kinfo, _, _) ->
      kinfo
  | KFailwith (kinfo, _, _, _) ->
      kinfo
  | KNop (kinfo, _) ->
      kinfo
  | KCompare (kinfo, _, _) ->
      kinfo
  | KEq (kinfo, _) ->
      kinfo
  | KNeq (kinfo, _) ->
      kinfo
  | KLt (kinfo, _) ->
      kinfo
  | KGt (kinfo, _) ->
      kinfo
  | KLe (kinfo, _) ->
      kinfo
  | KGe (kinfo, _) ->
      kinfo
  | KAddress (kinfo, _) ->
      kinfo
  | KContract (kinfo, _, _, _) ->
      kinfo
  | KTransfer_tokens (kinfo, _) ->
      kinfo
  | KImplicit_account (kinfo, _) ->
      kinfo
  | KCreate_contract (kinfo, _, _, _, _, _) ->
      kinfo
  | KSet_delegate (kinfo, _) ->
      kinfo
  | KNow (kinfo, _) ->
      kinfo
  | KBalance (kinfo, _) ->
      kinfo
  | KLevel (kinfo, _) ->
      kinfo
  | KCheck_signature (kinfo, _) ->
      kinfo
  | KHash_key (kinfo, _) ->
      kinfo
  | KPack (kinfo, _, _) ->
      kinfo
  | KUnpack (kinfo, _, _) ->
      kinfo
  | KBlake2b (kinfo, _) ->
      kinfo
  | KSha256 (kinfo, _) ->
      kinfo
  | KSha512 (kinfo, _) ->
      kinfo
  | KSource (kinfo, _) ->
      kinfo
  | KSender (kinfo, _) ->
      kinfo
  | KSelf (kinfo, _, _, _) ->
      kinfo
  | KSelf_address (kinfo, _) ->
      kinfo
  | KAmount (kinfo, _) ->
      kinfo
  | KSapling_empty_state (kinfo, _, _) ->
      kinfo
  | KSapling_verify_update (kinfo, _) ->
      kinfo
  | KDig (kinfo, _, _, _) ->
      kinfo
  | KDug (kinfo, _, _, _) ->
      kinfo
  | KDipn (kinfo, _, _, _, _) ->
      kinfo
  | KDropn (kinfo, _, _, _) ->
      kinfo
  | KChainId (kinfo, _) ->
      kinfo
  | KNever (kinfo, _) ->
      kinfo
  | KVoting_power (kinfo, _) ->
      kinfo
  | KTotal_voting_power (kinfo, _) ->
      kinfo
  | KKeccak (kinfo, _) ->
      kinfo
  | KSha3 (kinfo, _) ->
      kinfo
  | KAdd_bls12_381_g1 (kinfo, _) ->
      kinfo
  | KAdd_bls12_381_g2 (kinfo, _) ->
      kinfo
  | KAdd_bls12_381_fr (kinfo, _) ->
      kinfo
  | KMul_bls12_381_g1 (kinfo, _) ->
      kinfo
  | KMul_bls12_381_g2 (kinfo, _) ->
      kinfo
  | KMul_bls12_381_fr (kinfo, _) ->
      kinfo
  | KMul_bls12_381_z_fr (kinfo, _) ->
      kinfo
  | KMul_bls12_381_fr_z (kinfo, _) ->
      kinfo
  | KInt_bls12_381_fr (kinfo, _) ->
      kinfo
  | KNeg_bls12_381_g1 (kinfo, _) ->
      kinfo
  | KNeg_bls12_381_g2 (kinfo, _) ->
      kinfo
  | KNeg_bls12_381_fr (kinfo, _) ->
      kinfo
  | KPairing_check_bls12_381 (kinfo, _) ->
      kinfo
  | KComb (kinfo, _, _, _) ->
      kinfo
  | KUncomb (kinfo, _, _, _) ->
      kinfo
  | KComb_get (kinfo, _, _, _) ->
      kinfo
  | KComb_set (kinfo, _, _, _) ->
      kinfo
  | KDup_n (kinfo, _, _, _) ->
      kinfo
  | KTicket (kinfo, _) ->
      kinfo
  | KRead_ticket (kinfo, _) ->
      kinfo
  | KSplit_ticket (kinfo, _) ->
      kinfo
  | KJoin_tickets (kinfo, _, _) ->
      kinfo
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
