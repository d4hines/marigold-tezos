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
open Script_typed_ir

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
   invariant is encoding by representing the stack type using two
   parameters instead of one: the first one is the type of the
   accumulator while the second is the type of the rest of the stack.

   Third, in this representation, each instruction embeds its
   potential successor instructions in the control flow. This design
   choice permits an efficient implementation of the continuation
   stack in the interpreter. Assigning a precise type to this kind of
   instruction which is a cell in linked list of instructions is
   similar to the typing of delimited continuations: we need to give a
   type [`bef] to the stack before the execution of the instruction, a
   type [`aft] to the stack after the execution of the instruction and
   before the execution of the next, and a type [`res] for the resulting
   stack type after the execution of the whole chain of instructions.

   Combining these three aspects, the type [kinstr] needs four parameters:

   ('bef_top, 'bef, 'res_top, `res) kinstr

   Notice that we could have chosen two only give two parameters to [kinstr]
   by manually enforcing each argument to be a pair but this is
   error-prone: with four parameters, this is enforced by the arity of
   the type constructor itself.

   Hence, an instruction which has a successor instruction enjoys a
   type of the form:

   ('aft_top, 'aft, 'res_top, 'res) kinstr ->
   ('bef_top, 'bef, 'res_top, 'res) kinstr

   Notations:
   ----------

   In the following declaration, we use 'a, 'b, 'c, 'd, ...
   to assign types to stack cell contents while we use 's, 't,
   'u, 'v, ... to assign types to stacks.

   The final types for the whole sequence of instructions
   are written 'r and 'f.

*)
type ('bef_top, 'bef, 'res_top, 'res) kinstr =
  (*
     Stack
     -----
  *)
  | KDrop : ('b, 's, 'r, 'f) kinstr -> ('a, 'b * 's, 'r, 'f) kinstr
  | KDup : ('a, 'a * 's, 'r, 'f) kinstr -> ('a, 's, 'r, 'f) kinstr
  | KSwap : ('b, 'a * 's, 'r, 'f) kinstr -> ('a, 'b * 's, 'r, 'f) kinstr
  | KConst : 'ty * ('ty, 'a * 's, 'r, 'f) kinstr -> ('a, 's, 'r, 'f) kinstr
  (*
     Pairs
     -----
  *)
  | KCons_pair : ('a * 'b, 's, 'r, 'f) kinstr -> ('a, 'b * 's, 'r, 'f) kinstr
  | KCar : ('a, 's, 'r, 'f) kinstr -> ('a * 'b, 's, 'r, 'f) kinstr
  | KCdr : ('b, 's, 'r, 'f) kinstr -> ('a * 'b, 's, 'r, 'f) kinstr
  | KUnpair : ('a, 'b * 's, 'r, 'f) kinstr -> ('a * 'b, 's, 'r, 'f) kinstr
  (*
     Options
     -------
   *)
  | KCons_some : ('v option, 's, 'r, 'f) kinstr -> ('v, 's, 'r, 'f) kinstr
  | KCons_none :
      'b ty * ('b option, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KIf_none :
      ('b, 's, 'r, 'f) kinstr * ('a, 'b * 's, 'r, 'f) kinstr
      -> ('a option, 'b * 's, 'r, 'f) kinstr
  (*
     Unions
     ------
   *)
  | KCons_left : (('a, 'b) union, 's, 'r, 'f) kinstr -> ('a, 's, 'r, 'f) kinstr
  | KCons_right :
      (('a, 'b) union, 's, 'r, 'f) kinstr
      -> ('b, 's, 'r, 'f) kinstr
  | KIf_left :
      ('a, 's, 'r, 'f) kinstr * ('b, 's, 'r, 'f) kinstr
      -> (('a, 'b) union, 's, 'r, 'f) kinstr
  (*
     Lists
     -----
  *)
  | KCons_list :
      ('a boxed_list, 's, 'r, 'f) kinstr
      -> ('a, 'a boxed_list * 's, 'r, 'f) kinstr
  | KNil : ('b boxed_list, 'a * 's, 'r, 'f) kinstr -> ('a, 's, 'r, 'f) kinstr
  | KIf_cons :
      ('a, 'a boxed_list * ('b * 's), 'r, 'f) kinstr * ('b, 's, 'r, 'f) kinstr
      -> ('a boxed_list, 'b * 's, 'r, 'f) kinstr
  | KList_map :
      ('a, 's, 'b, 's) kinstr * ('b boxed_list, 's, 'r, 'f) kinstr
      -> ('a boxed_list, 's, 'r, 'f) kinstr
  | KList_iter :
      ('a, 'b * 's, 'b, 's) kinstr * ('b, 's, 'r, 'f) kinstr
      -> ('a boxed_list, 'b * 's, 'r, 'f) kinstr
  | KList_size :
      (n num, 's, 'r, 'f) kinstr
      -> ('a boxed_list, 's, 'r, 'f) kinstr
  (*
    Sets
    ----
  *)
  | KEmpty_set :
      'b comparable_ty * ('b set, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KSet_iter :
      ('a, 'b * 's, 'b, 's) kinstr * ('b, 's, 'r, 'f) kinstr
      -> ('a set, 'b * 's, 'r, 'f) kinstr
  | KSet_mem : (bool, 's, 'r, 'f) kinstr -> ('a, 'a set * 's, 'r, 'f) kinstr
  | KSet_update :
      ('a set, 's, 'r, 'f) kinstr
      -> ('a, bool * ('a set * 's), 'r, 'f) kinstr
  | KSet_size : (n num, 's, 'r, 'f) kinstr -> ('a set, 's, 'r, 'f) kinstr
  (*
     Maps
     ----
   *)
  | KEmpty_map :
      'b comparable_ty * 'c ty * (('b, 'c) map, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KMap_map :
      ('a * 'b, 's, 'c, 's) kinstr * (('a, 'c) map, 's, 'r, 'f) kinstr
      -> (('a, 'b) map, 's, 'r, 'f) kinstr
  | KMap_iter :
      ('a * 'b, 'c * 's, 'c, 's) kinstr * ('c, 's, 'r, 'f) kinstr
      -> (('a, 'b) map, 'c * 's, 'r, 'f) kinstr
  | KMap_mem :
      (bool, 's, 'r, 'f) kinstr
      -> ('a, ('a, 'b) map * 's, 'r, 'f) kinstr
  | KMap_get :
      ('b option, 's, 'r, 'f) kinstr
      -> ('a, ('a, 'b) map * 's, 'r, 'f) kinstr
  | KMap_update :
      (('a, 'b) map, 's, 'r, 'f) kinstr
      -> ('a, 'b option * (('a, 'b) map * 's), 'r, 'f) kinstr
  | KMap_size : (n num, 's, 'r, 'f) kinstr -> (('a, 'b) map, 's, 'r, 'f) kinstr
  (*
     Big maps
     --------
  *)
  | KEmpty_big_map :
      'b comparable_ty * 'c ty * (('b, 'c) big_map, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KBig_map_mem :
      (bool, 's, 'r, 'f) kinstr
      -> ('a, ('a, 'b) big_map * 's, 'r, 'f) kinstr
  | KBig_map_get :
      ('b option, 's, 'r, 'f) kinstr
      -> ('a, ('a, 'b) big_map * 's, 'r, 'f) kinstr
  | KBig_map_update :
      (('a, 'b) big_map, 's, 'r, 'f) kinstr
      -> ('a, 'b option * (('a, 'b) big_map * 's), 'r, 'f) kinstr
  (*
     Strings
     -------
  *)
  | KConcat_string :
      (string, 's, 'r, 'f) kinstr
      -> (string boxed_list, 's, 'r, 'f) kinstr
  | KConcat_string_pair :
      (string, 's, 'r, 'f) kinstr
      -> (string, string * 's, 'r, 'f) kinstr
  | KSlice_string :
      (string option, 's, 'r, 'f) kinstr
      -> (n num, n num * (string * 's), 'r, 'f) kinstr
  | KString_size : (n num, 's, 'r, 'f) kinstr -> (string, 's, 'r, 'f) kinstr
  (*
     Bytes
     -----
  *)
  | KConcat_bytes :
      (bytes, 's, 'r, 'f) kinstr
      -> (bytes boxed_list, 's, 'r, 'f) kinstr
  | KConcat_bytes_pair :
      (bytes, 's, 'r, 'f) kinstr
      -> (bytes, bytes * 's, 'r, 'f) kinstr
  | KSlice_bytes :
      (bytes option, 's, 'r, 'f) kinstr
      -> (n num, n num * (bytes * 's), 'r, 'f) kinstr
  | KBytes_size : (n num, 's, 'r, 'f) kinstr -> (bytes, 's, 'r, 'f) kinstr
  (*
     Timestamps
     ----------
   *)
  | KAdd_seconds_to_timestamp :
      (Script_timestamp.t, 's, 'r, 'f) kinstr
      -> (z num, Script_timestamp.t * 's, 'r, 'f) kinstr
  | KAdd_timestamp_to_seconds :
      (Script_timestamp.t, 's, 'r, 'f) kinstr
      -> (Script_timestamp.t, z num * 's, 'r, 'f) kinstr
  | KSub_timestamp_seconds :
      (Script_timestamp.t, 's, 'r, 'f) kinstr
      -> (Script_timestamp.t, z num * 's, 'r, 'f) kinstr
  | KDiff_timestamps :
      (z num, 's, 'r, 'f) kinstr
      -> (Script_timestamp.t, Script_timestamp.t * 's, 'r, 'f) kinstr
  (*
     Tez
     ---
    *)
  | KAdd_tez : (Tez.t, 's, 'r, 'f) kinstr -> (Tez.t, Tez.t * 's, 'r, 'f) kinstr
  | KSub_tez : (Tez.t, 's, 'r, 'f) kinstr -> (Tez.t, Tez.t * 's, 'r, 'f) kinstr
  | KMul_teznat :
      (Tez.t, 's, 'r, 'f) kinstr
      -> (Tez.t, n num * 's, 'r, 'f) kinstr
  | KMul_nattez :
      (Tez.t, 's, 'r, 'f) kinstr
      -> (n num, Tez.t * 's, 'r, 'f) kinstr
  | KEdiv_teznat :
      ((Tez.t, Tez.t) pair option, 's, 'r, 'f) kinstr
      -> (Tez.t, n num * 's, 'r, 'f) kinstr
  | KEdiv_tez :
      ((n num, Tez.t) pair option, 's, 'r, 'f) kinstr
      -> (Tez.t, Tez.t * 's, 'r, 'f) kinstr
  (*
     Booleans
     --------
   *)
  | KOr : (bool, 's, 'r, 'f) kinstr -> (bool, bool * 's, 'r, 'f) kinstr
  | KAnd : (bool, 's, 'r, 'f) kinstr -> (bool, bool * 's, 'r, 'f) kinstr
  | KXor : (bool, 's, 'r, 'f) kinstr -> (bool, bool * 's, 'r, 'f) kinstr
  | KNot : (bool, 's, 'r, 'f) kinstr -> (bool, 's, 'r, 'f) kinstr
  (*
     Integers
     --------
  *)
  | KIs_nat : (n num option, 's, 'r, 'f) kinstr -> (z num, 's, 'r, 'f) kinstr
  | KNeg_nat : (z num, 's, 'r, 'f) kinstr -> (n num, 's, 'r, 'f) kinstr
  | KNeg_int : (z num, 's, 'r, 'f) kinstr -> (z num, 's, 'r, 'f) kinstr
  | KAbs_int : (n num, 's, 'r, 'f) kinstr -> (z num, 's, 'r, 'f) kinstr
  | KInt_nat : (z num, 's, 'r, 'f) kinstr -> (n num, 's, 'r, 'f) kinstr
  | KAdd_intint :
      (z num, 's, 'r, 'f) kinstr
      -> (z num, z num * 's, 'r, 'f) kinstr
  | KAdd_intnat :
      (z num, 's, 'r, 'f) kinstr
      -> (z num, n num * 's, 'r, 'f) kinstr
  | KAdd_natint :
      (z num, 's, 'r, 'f) kinstr
      -> (n num, z num * 's, 'r, 'f) kinstr
  | KAdd_natnat :
      (n num, 's, 'r, 'f) kinstr
      -> (n num, n num * 's, 'r, 'f) kinstr
  | KSub_int :
      (z num, 's, 'r, 'f) kinstr
      -> ('a num, 'b num * 's, 'r, 'f) kinstr
  | KMul_intint :
      (z num, 's, 'r, 'f) kinstr
      -> (z num, z num * 's, 'r, 'f) kinstr
  | KMul_intnat :
      (z num, 's, 'r, 'f) kinstr
      -> (z num, n num * 's, 'r, 'f) kinstr
  | KMul_natint :
      (z num, 's, 'r, 'f) kinstr
      -> (n num, z num * 's, 'r, 'f) kinstr
  | KMul_natnat :
      (n num, 's, 'r, 'f) kinstr
      -> (n num, n num * 's, 'r, 'f) kinstr
  | KEdiv_intint :
      ((z num, n num) pair option, 's, 'r, 'f) kinstr
      -> (z num, z num * 's, 'r, 'f) kinstr
  | KEdiv_intnat :
      ((z num, n num) pair option, 's, 'r, 'f) kinstr
      -> (z num, n num * 's, 'r, 'f) kinstr
  | KEdiv_natint :
      ((z num, n num) pair option, 's, 'r, 'f) kinstr
      -> (n num, z num * 's, 'r, 'f) kinstr
  | KEdiv_natnat :
      ((n num, n num) pair option, 's, 'r, 'f) kinstr
      -> (n num, n num * 's, 'r, 'f) kinstr
  | KLsl_nat : (n num, 's, 'r, 'f) kinstr -> (n num, n num * 's, 'r, 'f) kinstr
  | KLsr_nat : (n num, 's, 'r, 'f) kinstr -> (n num, n num * 's, 'r, 'f) kinstr
  | KOr_nat : (n num, 's, 'r, 'f) kinstr -> (n num, n num * 's, 'r, 'f) kinstr
  | KAnd_nat : (n num, 's, 'r, 'f) kinstr -> (n num, n num * 's, 'r, 'f) kinstr
  | KAnd_int_nat :
      (n num, 's, 'r, 'f) kinstr
      -> (z num, n num * 's, 'r, 'f) kinstr
  | KXor_nat : (n num, 's, 'r, 'f) kinstr -> (n num, n num * 's, 'r, 'f) kinstr
  | KNot_nat : (z num, 's, 'r, 'f) kinstr -> (n num, 's, 'r, 'f) kinstr
  | KNot_int : (z num, 's, 'r, 'f) kinstr -> (z num, 's, 'r, 'f) kinstr
  (*
     Control
     -------
  *)
  | KIf :
      ('a, 's, 'r, 'f) kinstr * ('a, 's, 'r, 'f) kinstr
      -> (bool, 'a * 's, 'r, 'f) kinstr
  | KLoop :
      ('a, 's, bool, 'a * 's) kinstr * ('a, 's, 'r, 'f) kinstr
      -> (bool, 'a * 's, 'r, 'f) kinstr
  | KLoop_left :
      ('a, 's, ('a, 'b) union, 's) kinstr * ('b, 's, 'r, 'f) kinstr
      -> (('a, 'b) union, 's, 'r, 'f) kinstr
  | KDip :
      ('b, 's, 'c, 't) kinstr * ('a, 'c * 't, 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | KExec :
      ('b, 's, 'r, 'f) kinstr
      -> ('a, ('a, 'b) lambda * 's, 'r, 'f) kinstr
  | KApply :
      'a ty * (('t, 'b) lambda, 's, 'r, 'f) kinstr
      -> ('a, ('a * 't, 'b) lambda * 's, 'r, 'f) kinstr
  | KLambda :
      ('b, 'c) lambda * (('b, 'c) lambda, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KFailwith : 'a ty * ('b, 't, 'r, 'f) kinstr -> ('a, 's, 'r, 'f) kinstr
  | KNop : ('a, 's, 'r, 'f) kinstr -> ('a, 's, 'r, 'f) kinstr
  (*
     Comparison
     ----------
  *)
  | KCompare :
      'a comparable_ty * (z num, 's, 'r, 'f) kinstr
      -> ('a, 'a * 's, 'r, 'f) kinstr
  (*
     Comparators
     -----------
  *)
  | KEq : (bool, 's, 'r, 'f) kinstr -> (z num, 's, 'r, 'f) kinstr
  | KNeq : (bool, 's, 'r, 'f) kinstr -> (z num, 's, 'r, 'f) kinstr
  | KLt : (bool, 's, 'r, 'f) kinstr -> (z num, 's, 'r, 'f) kinstr
  | KGt : (bool, 's, 'r, 'f) kinstr -> (z num, 's, 'r, 'f) kinstr
  | KLe : (bool, 's, 'r, 'f) kinstr -> (z num, 's, 'r, 'f) kinstr
  | KGe : (bool, 's, 'r, 'f) kinstr -> (z num, 's, 'r, 'f) kinstr
  (*
     Protocol
     --------
  *)
  | KAddress :
      (address, 's, 'r, 'f) kinstr
      -> (_ typed_contract, 's, 'r, 'f) kinstr
  | KContract :
      'a ty * string * ('a typed_contract option, 's, 'r, 'f) kinstr
      -> (address, 's, 'r, 'f) kinstr
  | KTransfer_tokens :
      (operation, 's, 'r, 'f) kinstr
      -> ('a, Tez.t * ('a typed_contract * 's), 'r, 'f) kinstr
  | KImplicit_account :
      (unit typed_contract, 's, 'r, 'f) kinstr
      -> (public_key_hash, 's, 'r, 'f) kinstr
  | KCreate_contract :
      'a ty
      * 'b ty
      * ('b * 'a, operation boxed_list * 'a) lambda
      * field_annot option
      * (operation, address * 's, 'r, 'f) kinstr
      -> (public_key_hash option, Tez.t * ('a * 's), 'r, 'f) kinstr
  | KSet_delegate :
      (operation, 's, 'r, 'f) kinstr
      -> (public_key_hash option, 's, 'r, 'f) kinstr
  | KNow :
      (Script_timestamp.t, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KBalance : (Tez.t, 'a * 's, 'r, 'f) kinstr -> ('a, 's, 'r, 'f) kinstr
  | KLevel : (n num, 'a * 's, 'r, 'f) kinstr -> ('a, 's, 'r, 'f) kinstr
  | KCheck_signature :
      (bool, 's, 'r, 'f) kinstr
      -> (public_key, signature * (bytes * 's), 'r, 'f) kinstr
  | KHash_key :
      (public_key_hash, 's, 'r, 'f) kinstr
      -> (public_key, 's, 'r, 'f) kinstr
  | KPack : 'a ty * (bytes, 's, 'r, 'f) kinstr -> ('a, 's, 'r, 'f) kinstr
  | KUnpack :
      'a ty * ('a option, 's, 'r, 'f) kinstr
      -> (bytes, 's, 'r, 'f) kinstr
  | KBlake2b : (bytes, 's, 'r, 'f) kinstr -> (bytes, 's, 'r, 'f) kinstr
  | KSha256 : (bytes, 's, 'r, 'f) kinstr -> (bytes, 's, 'r, 'f) kinstr
  | KSha512 : (bytes, 's, 'r, 'f) kinstr -> (bytes, 's, 'r, 'f) kinstr
  | KSource : (address, 'a * 's, 'r, 'f) kinstr -> ('a, 's, 'r, 'f) kinstr
  | KSender : (address, 'a * 's, 'r, 'f) kinstr -> ('a, 's, 'r, 'f) kinstr
  | KSelf :
      'b ty * string * ('b typed_contract, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KSelf_address :
      (address, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KAmount : (Tez.t, 'a * 's, 'r, 'f) kinstr -> ('a, 's, 'r, 'f) kinstr
  | KDig :
      int
      * ('b * 't, 't, 'a * 's, 'u) stack_prefix_preservation_witness
      * ('b, 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KDug :
      int
      * ('t, 'a * 't, 's, 'b * 'u) stack_prefix_preservation_witness
      * ('b, 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KDipn :
      int
      * ('c * 't, 'd * 'v, 'a * 's, 'b * 'u) stack_prefix_preservation_witness
      * ('c, 't, 'd, 'v) kinstr
      * ('b, 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KDropn :
      int
      * ('b * 'u, 'b * 'u, 'a * 's, 'a * 's) stack_prefix_preservation_witness
      * ('b, 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KChainId : (Chain_id.t, 'a * 's, 'r, 'f) kinstr -> ('a, 's, 'r, 'f) kinstr
  | KNever : ('b, 'u, 'r, 'f) kinstr -> (never, 's, 'r, 'f) kinstr
  | KVoting_power :
      (n num, 's, 'r, 'f) kinstr
      -> (public_key_hash, 's, 'r, 'f) kinstr
  | KTotal_voting_power :
      (n num, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KKeccak : (bytes, 's, 'r, 'f) kinstr -> (bytes, 's, 'r, 'f) kinstr
  | KSha3 : (bytes, 's, 'r, 'f) kinstr -> (bytes, 's, 'r, 'f) kinstr
  | KAdd_bls12_381_g1 :
      (Bls12_381.G1.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.G1.t, Bls12_381.G1.t * 's, 'r, 'f) kinstr
  | KAdd_bls12_381_g2 :
      (Bls12_381.G2.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.G2.t, Bls12_381.G2.t * 's, 'r, 'f) kinstr
  | KAdd_bls12_381_fr :
      (Bls12_381.Fr.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.Fr.t, Bls12_381.Fr.t * 's, 'r, 'f) kinstr
  | KMul_bls12_381_g1 :
      (Bls12_381.G1.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.G1.t, Bls12_381.Fr.t * 's, 'r, 'f) kinstr
  | KMul_bls12_381_g2 :
      (Bls12_381.G2.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.G2.t, Bls12_381.Fr.t * 's, 'r, 'f) kinstr
  | KMul_bls12_381_fr :
      (Bls12_381.Fr.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.Fr.t, Bls12_381.Fr.t * 's, 'r, 'f) kinstr
  | KNeg_bls12_381_g1 :
      (Bls12_381.G1.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.G1.t, 's, 'r, 'f) kinstr
  | KNeg_bls12_381_g2 :
      (Bls12_381.G2.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.G2.t, 's, 'r, 'f) kinstr
  | KNeg_bls12_381_fr :
      (Bls12_381.Fr.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.Fr.t, 's, 'r, 'f) kinstr
  | KPairing_check_bls12_381 :
      (bool, 's, 'r, 'f) kinstr
      -> ((Bls12_381.G1.t, Bls12_381.G2.t) pair boxed_list, 's, 'r, 'f) kinstr
  | KDup_n :
      int * ('a * 's, 't) dup_n_gadt_witness * ('t, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | KHalt : ('a, 's, 'a, 's) kinstr

and ('bef, 'aft) kdescr =
  | KDescr : {
      kloc : Script.location;
      kbef : 'bef stack_ty;
      kaft : 'aft stack_ty;
      kli : ('bef, 'a * 's) lift;
      klo : ('aft, 'r * 'f) lift;
      kinstr : ('a, 's, 'r, 'f) kinstr;
    }
      -> ('bef, 'aft) kdescr

and (_, _) lift =
  | BaseLift : (unit, unit * unit) lift
  | IndLift : ('a, 'y * 'b) lift -> ('x * 'a, 'x * ('y * 'b)) lift

let succ_lift : ('a, 'y * 'b) lift -> ('x * 'a, 'x * ('y * 'b)) lift =
 fun l -> IndLift l

let coerce_lift : type x y a b. (x * a, x * b) lift -> (y * a, y * b) lift =
  function
  | IndLift l ->
      IndLift l

type (_, _) eq = Refl : ('a, 'a) eq

type (_, _) exlift_inverse =
  | ExLiftInverse : ('a * 'w, 'v) eq -> ('u, 'v) exlift_inverse

let inverse_lift : type u v. (u, v) lift -> (u, v) exlift_inverse = function
  | BaseLift ->
      ExLiftInverse Refl
  | IndLift _ ->
      ExLiftInverse Refl

type _ exlift = ExLift : ('v, 'a * 'w) lift -> 'v exlift

let rec lift_type :
    type v. v stack_ty -> (* ∃ a w. (v, a * w) lift *) v exlift = function
  | Empty_t ->
      ExLift BaseLift
  | Item_t (_, s, _) -> (
    match lift_type s with ExLift l -> ExLift (IndLift l) )

let rec fun_lift : type a b c. (a, b) lift -> (a, c) lift -> (b, c) eq =
 fun l1 l2 ->
  match (l1, l2) with
  | (BaseLift, BaseLift) ->
      Refl
  | (IndLift l1, IndLift l2) -> (
    match fun_lift l1 l2 with Refl -> Refl )

type (_, _, _, _) exlift_stack_prefix_preservation_witness =
  | ExLiftStackPrefixPreservationWitness :
      ('ds, 'lds) lift
      * ('du, 'ldu) lift
      * ('lds, 'ldu, 's, 'u) stack_prefix_preservation_witness
      -> ('ds, 'du, 's, 'u) exlift_stack_prefix_preservation_witness

let rec lift_stack_prefix_preservation_witness :
    type s u s' u' ds du.
    (ds, du, s, u) stack_prefix_preservation_witness ->
    (s, s') lift ->
    (u, u') lift ->
    (ds, du, s', u') exlift_stack_prefix_preservation_witness =
 fun w ls lu ->
  match w with
  | Rest ->
      (* ds = s, du = u *)
      ExLiftStackPrefixPreservationWitness (ls, lu, Rest)
  | Prefix w -> (
    (*
       s = x * s0
       u = x * u0
    *)
    match (ls, lu) with
    | (IndLift ls, IndLift lu) -> (
      (*
         s' = x * s'0
         u' = x * u'0
         ls : (s0, s'0)
         lu : (u0, u'0)
       *)
      match lift_stack_prefix_preservation_witness w ls lu with
      | ExLiftStackPrefixPreservationWitness (lds, ldu, w) ->
          (*
              lds : lift (ds, 'lds)
              ldu : lift (du, 'ldu)
              w   : ('lds, 'ldu, s'0, u'0)
          *)
          ExLiftStackPrefixPreservationWitness (lds, ldu, Prefix w) ) )

let rec lift_dup_n_gadt_witness :
    type s s' a.
    (s, a) dup_n_gadt_witness -> (s, s') lift -> (s', a) dup_n_gadt_witness =
 fun w l ->
  match w with
  | Dup_n_zero -> (
    (* s = a * s0 *)
    match l with IndLift _ -> Dup_n_zero )
  | Dup_n_succ w -> (
    (* s = a * s0 *)
    match l with
    | IndLift l ->
        let w = lift_dup_n_gadt_witness w l in
        Dup_n_succ w )

let rec translate_instr :
    type a b s t v u r f.
    (t, v) instr ->
    (t, a * s) lift ->
    (v, b * u) lift ->
    (b, u, r, f) kinstr ->
    (a, s, r, f) kinstr =
 fun i li lo k ->
  match i with
  | Seq (i1, i2) -> (
    match lift_type i1.aft with
    | ExLift lii ->
        let ki2 = translate_instr i2.instr lii lo k in
        translate_instr i1.instr li lii ki2 )
  | Drop -> (
    match li with IndLift l -> ( match fun_lift l lo with Refl -> KDrop k ) )
  | Dup -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match lo with
        | IndLift lo -> (
          match fun_lift lo li with Refl -> KDup k ) ) ) )
  | Swap -> (
    match lo with
    | IndLift lo -> (
      match lo with
      | IndLift lo -> (
        match li with
        | IndLift li -> (
          match li with
          | IndLift li -> (
            match fun_lift lo li with Refl -> KSwap k ) ) ) ) )
  | Const ty -> (
    match lo with
    | IndLift lo -> (
      match fun_lift lo li with Refl -> KConst (ty, k) ) )
  | Cons_pair -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift lo li with Refl -> KCons_pair k ) ) ) )
  | Car -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift lo li with Refl -> KCar k ) ) )
  | Cdr -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift lo li with Refl -> KCdr k ) ) )
  | Unpair -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match lo with
        | IndLift lo -> (
          match fun_lift lo li with Refl -> KUnpair k ) ) ) )
  | Cons_some -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift lo li with Refl -> KCons_some k ) ) )
  | Cons_none ty -> (
    match lo with
    | IndLift lo -> (
      match fun_lift lo li with Refl -> KCons_none (ty, k) ) )
  | If_none (i1, i2) -> (
    match li with
    | IndLift li' ->
        let ki1 = translate_instr i1.instr li' lo k
        and ki2 = translate_instr i2.instr (coerce_lift li) lo k in
        KIf_none (ki1, ki2) )
  | Cons_left -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift lo li with Refl -> KCons_left k ) ) )
  | Cons_right -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift lo li with Refl -> KCons_right k ) ) )
  | If_left (i1, i2) -> (
    match li with
    | IndLift _ ->
        let ki1 = translate_instr i1.instr (coerce_lift li) lo k
        and ki2 = translate_instr i2.instr (coerce_lift li) lo k in
        KIf_left (ki1, ki2) )
  | Cons_list -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift lo li with Refl -> KCons_list k ) ) ) )
  | Nil -> (
    match lo with
    | IndLift lo -> (
      match fun_lift lo li with Refl -> KNil k ) )
  | If_cons (i1, i2) -> (
    match li with
    | IndLift li' ->
        let ki1 = translate_instr i1.instr (succ_lift li) lo k
        and ki2 = translate_instr i2.instr li' lo k in
        KIf_cons (ki1, ki2) )
  | List_map i -> (
    match li with
    | IndLift li' -> (
      match lo with
      | IndLift lo' -> (
        match fun_lift li' lo' with
        | Refl ->
            let ki =
              translate_instr i.instr (coerce_lift li) (coerce_lift lo) KHalt
            in
            KList_map (ki, k) ) ) )
  | List_iter i -> (
    match li with
    | IndLift li' -> (
      match fun_lift li' lo with
      | Refl ->
          let ki = translate_instr i.instr (coerce_lift li) lo KHalt in
          KList_iter (ki, k) ) )
  | List_size -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KList_size k ) ) )
  | Empty_set ty -> (
    match lo with
    | IndLift lo -> (
      match fun_lift li lo with Refl -> KEmpty_set (ty, k) ) )
  | Set_iter i -> (
    match li with
    | IndLift li' -> (
      match fun_lift li' lo with
      | Refl ->
          let ki = translate_instr i.instr (coerce_lift li) lo KHalt in
          KSet_iter (ki, k) ) )
  | Set_mem -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KSet_mem k ) ) ) )
  | Set_update -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> KSet_update k ) ) ) ) )
  | Set_size -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KSet_size k ) ) )
  | Empty_map (cty, ty) -> (
    match lo with
    | IndLift lo -> (
      match fun_lift li lo with Refl -> KEmpty_map (cty, ty, k) ) )
  | Map_map i -> (
    match li with
    | IndLift li' -> (
      match lo with
      | IndLift lo' -> (
        match fun_lift li' lo' with
        | Refl ->
            let ki =
              translate_instr i.instr (coerce_lift li) (coerce_lift lo) KHalt
            in
            KMap_map (ki, k) ) ) )
  | Map_iter i -> (
    match li with
    | IndLift li' -> (
      match fun_lift li' lo with
      | Refl ->
          let ki = translate_instr i.instr (coerce_lift li) lo KHalt in
          KMap_iter (ki, k) ) )
  | Map_mem -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KMap_mem k ) ) ) )
  | Map_get -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KMap_get k ) ) ) )
  | Map_update -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> KMap_update k ) ) ) ) )
  | Map_size -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KMap_size k ) ) )
  | Empty_big_map (cty, ty) -> (
    match lo with
    | IndLift lo -> (
      match fun_lift li lo with Refl -> KEmpty_big_map (cty, ty, k) ) )
  | Big_map_mem -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KBig_map_mem k ) ) ) )
  | Big_map_get -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KBig_map_get k ) ) ) )
  | Big_map_update -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> KBig_map_update k ) ) ) ) )
  | Concat_string -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KConcat_string k ) ) )
  | Concat_string_pair -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KConcat_string_pair k ) ) ) )
  | Slice_string -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> KSlice_string k ) ) ) ) )
  | String_size -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KString_size k ) ) )
  | Concat_bytes -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KConcat_bytes k ) ) )
  | Concat_bytes_pair -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KConcat_bytes_pair k ) ) ) )
  | Slice_bytes -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> KSlice_bytes k ) ) ) ) )
  | Bytes_size -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KBytes_size k ) ) )
  | Add_seconds_to_timestamp -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KAdd_seconds_to_timestamp k ) ) ) )
  | Add_timestamp_to_seconds -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KAdd_timestamp_to_seconds k ) ) ) )
  | Sub_timestamp_seconds -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KSub_timestamp_seconds k ) ) ) )
  | Diff_timestamps -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KDiff_timestamps k ) ) ) )
  | Add_tez -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KAdd_tez k ) ) ) )
  | Sub_tez -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KSub_tez k ) ) ) )
  | Mul_teznat -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KMul_teznat k ) ) ) )
  | Mul_nattez -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KMul_nattez k ) ) ) )
  | Ediv_teznat -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KEdiv_teznat k ) ) ) )
  | Ediv_tez -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KEdiv_tez k ) ) ) )
  | Or -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KOr k ) ) ) )
  | And -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KAnd k ) ) ) )
  | Xor -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KXor k ) ) ) )
  | Not -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KNot k ) ) )
  | Is_nat -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KIs_nat k ) ) )
  | Neg_nat -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KNeg_nat k ) ) )
  | Neg_int -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KNeg_int k ) ) )
  | Abs_int -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KAbs_int k ) ) )
  | Int_nat -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KInt_nat k ) ) )
  | Add_intint -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KAdd_intint k ) ) ) )
  | Add_intnat -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KAdd_intnat k ) ) ) )
  | Add_natint -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KAdd_natint k ) ) ) )
  | Add_natnat -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KAdd_natnat k ) ) ) )
  | Sub_int -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KSub_int k ) ) ) )
  | Mul_intint -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KMul_intint k ) ) ) )
  | Mul_intnat -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KMul_intnat k ) ) ) )
  | Mul_natint -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KMul_natint k ) ) ) )
  | Mul_natnat -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KMul_natnat k ) ) ) )
  | Ediv_intint -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KEdiv_intint k ) ) ) )
  | Ediv_intnat -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KEdiv_intnat k ) ) ) )
  | Ediv_natint -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KEdiv_natint k ) ) ) )
  | Ediv_natnat -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KEdiv_natnat k ) ) ) )
  | Lsl_nat -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KLsl_nat k ) ) ) )
  | Lsr_nat -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KLsr_nat k ) ) ) )
  | Or_nat -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KOr_nat k ) ) ) )
  | And_nat -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KAnd_nat k ) ) ) )
  | And_int_nat -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KAnd_int_nat k ) ) ) )
  | Xor_nat -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KXor_nat k ) ) ) )
  | Not_nat -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KNot_nat k ) ) )
  | Not_int -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KNot_int k ) ) )
  | If (i1, i2) -> (
    match li with
    | IndLift li ->
        let ki1 = translate_instr i1.instr li lo k
        and ki2 = translate_instr i2.instr li lo k in
        KIf (ki1, ki2) )
  | Loop i -> (
    match li with
    | IndLift li' -> (
      match fun_lift li' lo with
      | Refl ->
          let ki = translate_instr i.instr li' li KHalt in
          KLoop (ki, k) ) )
  | Loop_left i -> (
    match li with
    | IndLift li' -> (
      match lo with
      | IndLift lo' -> (
        match fun_lift li' lo' with
        | Refl ->
            let ki = translate_instr i.instr (coerce_lift li) li KHalt in
            KLoop_left (ki, k) ) ) )
  | Dip i -> (
    match li with
    | IndLift li' -> (
      match lo with
      | IndLift lo ->
          let ki = translate_instr i.instr li' lo KHalt in
          KDip (ki, k) ) )
  | Exec -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KExec k ) ) ) )
  | Apply f -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KApply (f, k) ) ) ) )
  | Lambda b -> (
    match lo with
    | IndLift lo -> (
      match fun_lift li lo with Refl -> KLambda (b, k) ) )
  | Failwith e -> (
    match li with IndLift _ -> KFailwith (e, k) )
  | Nop -> (
    match fun_lift li lo with Refl -> KNop k )
  | Compare c -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KCompare (c, k) ) ) ) )
  | Eq -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KEq k ) ) )
  | Neq -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KNeq k ) ) )
  | Lt -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KLt k ) ) )
  | Gt -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KGt k ) ) )
  | Le -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KLe k ) ) )
  | Ge -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KGe k ) ) )
  | Address -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KAddress k ) ) )
  | Contract (a, b) -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KContract (a, b, k) ) ) )
  | Transfer_tokens -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> KTransfer_tokens k ) ) ) ) )
  | Implicit_account -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KImplicit_account k ) ) )
  | Create_contract (a, b, c, d) -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match lo with
            | IndLift lo -> (
              match fun_lift li lo with
              | Refl ->
                  KCreate_contract (a, b, c, d, k) ) ) ) ) ) )
  | Set_delegate -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KSet_delegate k ) ) )
  | Now -> (
    match lo with
    | IndLift lo -> (
      match fun_lift li lo with Refl -> KNow k ) )
  | Balance -> (
    match lo with
    | IndLift lo -> (
      match fun_lift li lo with Refl -> KBalance k ) )
  | Level -> (
    match lo with
    | IndLift lo -> (
      match fun_lift li lo with Refl -> KLevel k ) )
  | Check_signature -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> KCheck_signature k ) ) ) ) )
  | Hash_key -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KHash_key k ) ) )
  | Pack ty -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KPack (ty, k) ) ) )
  | Unpack ty -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KUnpack (ty, k) ) ) )
  | Blake2b -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KBlake2b k ) ) )
  | Sha256 -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KSha256 k ) ) )
  | Sha512 -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KSha512 k ) ) )
  | Source -> (
    match lo with
    | IndLift lo -> (
      match fun_lift li lo with Refl -> KSource k ) )
  | Sender -> (
    match lo with
    | IndLift lo -> (
      match fun_lift li lo with Refl -> KSender k ) )
  | Self (a, b) -> (
    match lo with
    | IndLift lo -> (
      match fun_lift li lo with Refl -> KSelf (a, b, k) ) )
  | Self_address -> (
    match lo with
    | IndLift lo -> (
      match fun_lift li lo with Refl -> KSelf_address k ) )
  | Amount -> (
    match lo with
    | IndLift lo -> (
      match fun_lift li lo with Refl -> KAmount k ) )
  | Dig (n, w) -> (
    match lo with
    | IndLift lo' -> (
      match lift_stack_prefix_preservation_witness w li lo' with
      | ExLiftStackPrefixPreservationWitness (lds, ldu, w') -> (
        match lds with
        | IndLift lds -> (
          match fun_lift lds ldu with Refl -> KDig (n, w', k) ) ) ) )
  | Dug (n, w) -> (
    match li with
    | IndLift li' -> (
      match lift_stack_prefix_preservation_witness w li' lo with
      | ExLiftStackPrefixPreservationWitness (lds, ldu, w') -> (
        match ldu with
        | IndLift ldu -> (
          match fun_lift lds ldu with Refl -> KDug (n, w', k) ) ) ) )
  | Dipn (n, w, i) -> (
    match lift_stack_prefix_preservation_witness w li lo with
    | ExLiftStackPrefixPreservationWitness (lds, ldu, w') -> (
      match inverse_lift lds with
      | ExLiftInverse Refl -> (
        match inverse_lift ldu with
        | ExLiftInverse Refl ->
            let ki = translate_instr i.instr lds ldu KHalt in
            KDipn (n, w', ki, k) ) ) )
  | Dropn (n, w) -> (
    match lift_stack_prefix_preservation_witness w li li with
    | ExLiftStackPrefixPreservationWitness (lds, ldu, w') -> (
      match inverse_lift lds with
      | ExLiftInverse Refl -> (
        match inverse_lift ldu with
        | ExLiftInverse Refl -> (
          match fun_lift lds ldu with
          | Refl -> (
            match fun_lift lo lds with Refl -> KDropn (n, w', k) ) ) ) ) )
  | ChainId -> (
    match lo with
    | IndLift lo -> (
      match fun_lift li lo with Refl -> KChainId k ) )
  | Never -> (
    match li with IndLift _ -> KNever k )
  | Voting_power -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KVoting_power k ) ) )
  | Total_voting_power -> (
    match lo with
    | IndLift lo -> (
      match fun_lift li lo with Refl -> KTotal_voting_power k ) )
  | Keccak -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KKeccak k ) ) )
  | Sha3 -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KSha3 k ) ) )
  | Add_bls12_381_g1 -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KAdd_bls12_381_g1 k ) ) ) )
  | Add_bls12_381_g2 -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KAdd_bls12_381_g2 k ) ) ) )
  | Add_bls12_381_fr -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KAdd_bls12_381_fr k ) ) ) )
  | Mul_bls12_381_g1 -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KMul_bls12_381_g1 k ) ) ) )
  | Mul_bls12_381_g2 -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KMul_bls12_381_g2 k ) ) ) )
  | Mul_bls12_381_fr -> (
    match li with
    | IndLift li -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> KMul_bls12_381_fr k ) ) ) )
  | Neg_bls12_381_g1 -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KNeg_bls12_381_g1 k ) ) )
  | Neg_bls12_381_g2 -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KNeg_bls12_381_g2 k ) ) )
  | Neg_bls12_381_fr -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KNeg_bls12_381_fr k ) ) )
  | Pairing_check_bls12_381 -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KPairing_check_bls12_381 k ) ) )
  | Dup_n (n, i) -> (
      let i = lift_dup_n_gadt_witness i li in
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> KDup_n (n, i, k) ) )

let translate : type bef aft. (bef, aft) descr -> (bef, aft) kdescr =
 fun d ->
  match (lift_type d.bef, lift_type d.aft) with
  | (ExLift kli, ExLift klo) ->
      KDescr
        {
          kloc = d.loc;
          kbef = d.bef;
          kaft = d.aft;
          kli;
          klo;
          kinstr = translate_instr d.instr kli klo KHalt;
        }
