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

   Instructions with a static cost
   ===============================

   Instructions are classified into two categories: instructions whose
   cost can be determined statically and the other instructions whose
   cost depends on the stack. The third parameter encodes this
   classification.

*)
type static_cost = StaticCost

type dynamic_cost = DynamicCost

type ('bef_top, 'bef, 'res_top, 'res, 'cost) kinstr =
  (*
     Stack
     -----
  *)
  | KDrop :
      ('a, 'b * 's) kinfo * ('b, 's, 'r, 'f, _) kinstr
      -> ('a, 'b * 's, 'r, 'f, static_cost) kinstr
  | KDup :
      ('a, 's) kinfo * ('a, 'a * 's, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KSwap :
      ('a, 'b * 's) kinfo * ('b, 'a * 's, 'r, 'f, _) kinstr
      -> ('a, 'b * 's, 'r, 'f, static_cost) kinstr
  | KConst :
      ('a, 's) kinfo * 'ty * ('ty, 'a * 's, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  (*
     Pairs
     -----
  *)
  | KCons_pair :
      ('a, 'b * 's) kinfo * ('a * 'b, 's, 'r, 'f, _) kinstr
      -> ('a, 'b * 's, 'r, 'f, static_cost) kinstr
  | KCar :
      ('a * 'b, 's) kinfo * ('a, 's, 'r, 'f, _) kinstr
      -> ('a * 'b, 's, 'r, 'f, static_cost) kinstr
  | KCdr :
      ('a * 'b, 's) kinfo * ('b, 's, 'r, 'f, _) kinstr
      -> ('a * 'b, 's, 'r, 'f, static_cost) kinstr
  | KUnpair :
      ('a * 'b, 's) kinfo * ('a, 'b * 's, 'r, 'f, _) kinstr
      -> ('a * 'b, 's, 'r, 'f, static_cost) kinstr
  (*
     Options
     -------
   *)
  | KCons_some :
      ('v, 's) kinfo * ('v option, 's, 'r, 'f, _) kinstr
      -> ('v, 's, 'r, 'f, static_cost) kinstr
  | KCons_none :
      ('a, 's) kinfo * 'b ty * ('b option, 'a * 's, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KIf_none :
      ('a option, 'b * 's) kinfo
      * ('b, 's, 'r, 'f, _) kinstr
      * ('a, 'b * 's, 'r, 'f, _) kinstr
      -> ('a option, 'b * 's, 'r, 'f, static_cost) kinstr
  (*
     Unions
     ------
   *)
  | KCons_left :
      ('a, 's) kinfo * (('a, 'b) union, 's, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KCons_right :
      ('b, 's) kinfo * (('a, 'b) union, 's, 'r, 'f, _) kinstr
      -> ('b, 's, 'r, 'f, static_cost) kinstr
  | KIf_left :
      (('a, 'b) union, 's) kinfo
      * ('a, 's, 'r, 'f, _) kinstr
      * ('b, 's, 'r, 'f, _) kinstr
      -> (('a, 'b) union, 's, 'r, 'f, static_cost) kinstr
  (*
     Lists
     -----
  *)
  | KCons_list :
      ('a, 'a boxed_list * 's) kinfo * ('a boxed_list, 's, 'r, 'f, _) kinstr
      -> ('a, 'a boxed_list * 's, 'r, 'f, static_cost) kinstr
  | KNil :
      ('a, 's) kinfo * ('b boxed_list, 'a * 's, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KIf_cons :
      ('a boxed_list, 'b * 's) kinfo
      * ('a, 'a boxed_list * ('b * 's), 'r, 'f, _) kinstr
      * ('b, 's, 'r, 'f, _) kinstr
      -> ('a boxed_list, 'b * 's, 'r, 'f, static_cost) kinstr
  | KList_map :
      ('a boxed_list, 'c * 's) kinfo
      * ('a, 'c * 's, 'b, 'c * 's, _) kinstr
      * ('b boxed_list, 'c * 's, 'r, 'f, _) kinstr
      -> ('a boxed_list, 'c * 's, 'r, 'f, dynamic_cost) kinstr
  | KList_mapping :
      ('c, 's) kinfo
      * ('b, 'c * 's) kinfo
      * ('a, 'c * 's, 'b, 'c * 's, _) kinstr
      * 'a list
      * 'b list
      * int
      * ('b boxed_list, 'c * 's, 'r, 'f, _) kinstr
      -> ('c, 's, 'r, 'f, dynamic_cost) kinstr
  | KList_mapped :
      ('b, 'c * 's) kinfo
      * ('c, 's) kinfo
      * ('a, 'c * 's, 'b, 'c * 's, _) kinstr
      * 'a list
      * 'b list
      * int
      * ('b boxed_list, 'c * 's, 'r, 'f, _) kinstr
      -> ('b, 'c * 's, 'r, 'f, dynamic_cost) kinstr
  | KList_iter :
      ('a boxed_list, 'b * 's) kinfo
      * ('b, 's) kinfo
      * ('a, 'b * 's, 'b, 's, _) kinstr
      * ('b, 's, 'r, 'f, _) kinstr
      -> ('a boxed_list, 'b * 's, 'r, 'f, dynamic_cost) kinstr
  | KIter :
      ('b, 's) kinfo
      * ('a, 'b * 's, 'b, 's, _) kinstr
      * 'a list
      * ('b, 's, 'r, 'f, _) kinstr
      -> ('b, 's, 'r, 'f, dynamic_cost) kinstr
  | KList_size :
      ('a boxed_list, 's) kinfo * (n num, 's, 'r, 'f, _) kinstr
      -> ('a boxed_list, 's, 'r, 'f, static_cost) kinstr
  (*
    Sets
    ----
  *)
  | KEmpty_set :
      ('a, 's) kinfo * 'b comparable_ty * ('b set, 'a * 's, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KSet_iter :
      ('a set, 'b * 's) kinfo
      * ('b, 's) kinfo
      * ('a, 'b * 's, 'b, 's, _) kinstr
      * ('b, 's, 'r, 'f, _) kinstr
      -> ('a set, 'b * 's, 'r, 'f, dynamic_cost) kinstr
  | KSet_mem :
      ('a, 'a set * 's) kinfo * (bool, 's, 'r, 'f, _) kinstr
      -> ('a, 'a set * 's, 'r, 'f, dynamic_cost) kinstr
  | KSet_update :
      ('a, bool * ('a set * 's)) kinfo * ('a set, 's, 'r, 'f, _) kinstr
      -> ('a, bool * ('a set * 's), 'r, 'f, dynamic_cost) kinstr
  | KSet_size :
      ('a set, 's) kinfo * (n num, 's, 'r, 'f, _) kinstr
      -> ('a set, 's, 'r, 'f, static_cost) kinstr
  (*
     Maps
     ----
   *)
  | KEmpty_map :
      ('a, 's) kinfo
      * 'b comparable_ty
      * 'c ty
      * (('b, 'c) map, 'a * 's, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KMap_map :
      (('a, 'b) map, 'd * 's) kinfo
      * ('d, 's) kinfo
      * ('c, 'd * 's) kinfo
      * ('a * 'b, 'd * 's, 'c, 'd * 's, _) kinstr
      * (('a, 'c) map, 'd * 's, 'r, 'f, _) kinstr
      -> (('a, 'b) map, 'd * 's, 'r, 'f, dynamic_cost) kinstr
  | KMap_mapping :
      ('d, 's) kinfo
      * ('c, 'd * 's) kinfo
      * ('a * 'b, 'd * 's, 'c, 'd * 's, _) kinstr
      * ('a * 'b) list
      * ('a, 'c) map
      * (('a, 'c) map, 'd * 's, 'r, 'f, _) kinstr
      -> ('d, 's, 'r, 'f, dynamic_cost) kinstr
  | KMap_mapped :
      ('c, 'd * 's) kinfo
      * ('d, 's) kinfo
      * ('a * 'b, 'd * 's, 'c, 'd * 's, _) kinstr
      * ('a * 'b) list
      * ('a, 'c) map
      * 'a
      * (('a, 'c) map, 'd * 's, 'r, 'f, _) kinstr
      -> ('c, 'd * 's, 'r, 'f, dynamic_cost) kinstr
  | KMap_iter :
      (('a, 'b) map, 'c * 's) kinfo
      * ('c, 's) kinfo
      * ('a * 'b, 'c * 's, 'c, 's, _) kinstr
      * ('c, 's, 'r, 'f, _) kinstr
      -> (('a, 'b) map, 'c * 's, 'r, 'f, dynamic_cost) kinstr
  | KMap_mem :
      ('a, ('a, 'b) map * 's) kinfo * (bool, 's, 'r, 'f, _) kinstr
      -> ('a, ('a, 'b) map * 's, 'r, 'f, dynamic_cost) kinstr
  | KMap_get :
      ('a, ('a, 'b) map * 's) kinfo * ('b option, 's, 'r, 'f, _) kinstr
      -> ('a, ('a, 'b) map * 's, 'r, 'f, dynamic_cost) kinstr
  | KMap_update :
      ('a, 'b option * (('a, 'b) map * 's)) kinfo
      * (('a, 'b) map, 's, 'r, 'f, _) kinstr
      -> ('a, 'b option * (('a, 'b) map * 's), 'r, 'f, dynamic_cost) kinstr
  | KMap_get_and_update :
      ('a, 'v option * (('a, 'v) map * 'rest)) kinfo
      * ('v option, ('a, 'v) map * 'rest, 'r, 'f, _) kinstr
      -> ('a, 'v option * (('a, 'v) map * 'rest), 'r, 'f, dynamic_cost) kinstr
  | KMap_size :
      (('a, 'b) map, 's) kinfo * (n num, 's, 'r, 'f, _) kinstr
      -> (('a, 'b) map, 's, 'r, 'f, static_cost) kinstr
  (*
     Big maps
     --------
  *)
  | KEmpty_big_map :
      ('a, 's) kinfo
      * 'b comparable_ty
      * 'c ty
      * (('b, 'c) big_map, 'a * 's, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KBig_map_mem :
      ('a, ('a, 'b) big_map * 's) kinfo * (bool, 's, 'r, 'f, _) kinstr
      -> ('a, ('a, 'b) big_map * 's, 'r, 'f, dynamic_cost) kinstr
  | KBig_map_get :
      ('a, ('a, 'b) big_map * 's) kinfo * ('b option, 's, 'r, 'f, _) kinstr
      -> ('a, ('a, 'b) big_map * 's, 'r, 'f, dynamic_cost) kinstr
  | KBig_map_update :
      ('a, 'b option * (('a, 'b) big_map * 's)) kinfo
      * (('a, 'b) big_map, 's, 'r, 'f, _) kinstr
      -> ('a, 'b option * (('a, 'b) big_map * 's), 'r, 'f, dynamic_cost) kinstr
  | KBig_map_get_and_update :
      ('a, 'v option * (('a, 'v) big_map * 'rest)) kinfo
      * ('v option, ('a, 'v) big_map * 'rest, 'r, 'f, _) kinstr
      -> ( 'a,
           'v option * (('a, 'v) big_map * 'rest),
           'r,
           'f,
           dynamic_cost )
         kinstr
  (*
     Strings
     -------
  *)
  | KConcat_string :
      (string boxed_list, 's) kinfo * (string, 's, 'r, 'f, _) kinstr
      -> (string boxed_list, 's, 'r, 'f, dynamic_cost) kinstr
  | KConcat_string_pair :
      (string, string * 's) kinfo * (string, 's, 'r, 'f, _) kinstr
      -> (string, string * 's, 'r, 'f, dynamic_cost) kinstr
  | KSlice_string :
      (n num, n num * (string * 's)) kinfo
      * (string option, 's, 'r, 'f, _) kinstr
      -> (n num, n num * (string * 's), 'r, 'f, dynamic_cost) kinstr
  | KString_size :
      (string, 's) kinfo * (n num, 's, 'r, 'f, _) kinstr
      -> (string, 's, 'r, 'f, static_cost) kinstr
  (*
     Bytes
     -----
  *)
  | KConcat_bytes :
      (bytes boxed_list, 's) kinfo * (bytes, 's, 'r, 'f, _) kinstr
      -> (bytes boxed_list, 's, 'r, 'f, dynamic_cost) kinstr
  | KConcat_bytes_pair :
      (bytes, bytes * 's) kinfo * (bytes, 's, 'r, 'f, _) kinstr
      -> (bytes, bytes * 's, 'r, 'f, dynamic_cost) kinstr
  | KSlice_bytes :
      (n num, n num * (bytes * 's)) kinfo
      * (bytes option, 's, 'r, 'f, _) kinstr
      -> (n num, n num * (bytes * 's), 'r, 'f, dynamic_cost) kinstr
  | KBytes_size :
      (bytes, 's) kinfo * (n num, 's, 'r, 'f, _) kinstr
      -> (bytes, 's, 'r, 'f, static_cost) kinstr
  (*
     Timestamps
     ----------
   *)
  | KAdd_seconds_to_timestamp :
      (z num, Script_timestamp.t * 's) kinfo
      * (Script_timestamp.t, 's, 'r, 'f, _) kinstr
      -> (z num, Script_timestamp.t * 's, 'r, 'f, dynamic_cost) kinstr
  | KAdd_timestamp_to_seconds :
      (Script_timestamp.t, z num * 's) kinfo
      * (Script_timestamp.t, 's, 'r, 'f, _) kinstr
      -> (Script_timestamp.t, z num * 's, 'r, 'f, dynamic_cost) kinstr
  | KSub_timestamp_seconds :
      (Script_timestamp.t, z num * 's) kinfo
      * (Script_timestamp.t, 's, 'r, 'f, _) kinstr
      -> (Script_timestamp.t, z num * 's, 'r, 'f, dynamic_cost) kinstr
  | KDiff_timestamps :
      (Script_timestamp.t, Script_timestamp.t * 's) kinfo
      * (z num, 's, 'r, 'f, _) kinstr
      -> ( Script_timestamp.t,
           Script_timestamp.t * 's,
           'r,
           'f,
           dynamic_cost )
         kinstr
  (*
     Tez
     ---
    *)
  | KAdd_tez :
      (Tez.t, Tez.t * 's) kinfo * (Tez.t, 's, 'r, 'f, _) kinstr
      -> (Tez.t, Tez.t * 's, 'r, 'f, static_cost) kinstr
  | KSub_tez :
      (Tez.t, Tez.t * 's) kinfo * (Tez.t, 's, 'r, 'f, _) kinstr
      -> (Tez.t, Tez.t * 's, 'r, 'f, static_cost) kinstr
  | KMul_teznat :
      (Tez.t, n num * 's) kinfo * (Tez.t, 's, 'r, 'f, _) kinstr
      -> (Tez.t, n num * 's, 'r, 'f, dynamic_cost) kinstr
  | KMul_nattez :
      (n num, Tez.t * 's) kinfo * (Tez.t, 's, 'r, 'f, _) kinstr
      -> (n num, Tez.t * 's, 'r, 'f, dynamic_cost) kinstr
  | KEdiv_teznat :
      (Tez.t, n num * 's) kinfo
      * ((Tez.t, Tez.t) pair option, 's, 'r, 'f, _) kinstr
      -> (Tez.t, n num * 's, 'r, 'f, dynamic_cost) kinstr
  | KEdiv_tez :
      (Tez.t, Tez.t * 's) kinfo
      * ((n num, Tez.t) pair option, 's, 'r, 'f, _) kinstr
      -> (Tez.t, Tez.t * 's, 'r, 'f, static_cost) kinstr
  (*
     Booleans
     --------
   *)
  | KOr :
      (bool, bool * 's) kinfo * (bool, 's, 'r, 'f, _) kinstr
      -> (bool, bool * 's, 'r, 'f, static_cost) kinstr
  | KAnd :
      (bool, bool * 's) kinfo * (bool, 's, 'r, 'f, _) kinstr
      -> (bool, bool * 's, 'r, 'f, static_cost) kinstr
  | KXor :
      (bool, bool * 's) kinfo * (bool, 's, 'r, 'f, _) kinstr
      -> (bool, bool * 's, 'r, 'f, static_cost) kinstr
  | KNot :
      (bool, 's) kinfo * (bool, 's, 'r, 'f, _) kinstr
      -> (bool, 's, 'r, 'f, static_cost) kinstr
  (*
     Integers
     --------
  *)
  | KIs_nat :
      (z num, 's) kinfo * (n num option, 's, 'r, 'f, _) kinstr
      -> (z num, 's, 'r, 'f, static_cost) kinstr
  | KNeg_nat :
      (n num, 's) kinfo * (z num, 's, 'r, 'f, _) kinstr
      -> (n num, 's, 'r, 'f, dynamic_cost) kinstr
  | KNeg_int :
      (z num, 's) kinfo * (z num, 's, 'r, 'f, _) kinstr
      -> (z num, 's, 'r, 'f, dynamic_cost) kinstr
  | KAbs_int :
      (z num, 's) kinfo * (n num, 's, 'r, 'f, _) kinstr
      -> (z num, 's, 'r, 'f, dynamic_cost) kinstr
  | KInt_nat :
      (n num, 's) kinfo * (z num, 's, 'r, 'f, _) kinstr
      -> (n num, 's, 'r, 'f, static_cost) kinstr
  | KAdd_intint :
      (z num, z num * 's) kinfo * (z num, 's, 'r, 'f, _) kinstr
      -> (z num, z num * 's, 'r, 'f, dynamic_cost) kinstr
  | KAdd_intnat :
      (z num, n num * 's) kinfo * (z num, 's, 'r, 'f, _) kinstr
      -> (z num, n num * 's, 'r, 'f, dynamic_cost) kinstr
  | KAdd_natint :
      (n num, z num * 's) kinfo * (z num, 's, 'r, 'f, _) kinstr
      -> (n num, z num * 's, 'r, 'f, dynamic_cost) kinstr
  | KAdd_natnat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f, _) kinstr
      -> (n num, n num * 's, 'r, 'f, dynamic_cost) kinstr
  | KSub_int :
      ('a num, 'b num * 's) kinfo * (z num, 's, 'r, 'f, _) kinstr
      -> ('a num, 'b num * 's, 'r, 'f, dynamic_cost) kinstr
  | KMul_intint :
      (z num, z num * 's) kinfo * (z num, 's, 'r, 'f, _) kinstr
      -> (z num, z num * 's, 'r, 'f, dynamic_cost) kinstr
  | KMul_intnat :
      (z num, n num * 's) kinfo * (z num, 's, 'r, 'f, _) kinstr
      -> (z num, n num * 's, 'r, 'f, dynamic_cost) kinstr
  | KMul_natint :
      (n num, z num * 's) kinfo * (z num, 's, 'r, 'f, _) kinstr
      -> (n num, z num * 's, 'r, 'f, dynamic_cost) kinstr
  | KMul_natnat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f, _) kinstr
      -> (n num, n num * 's, 'r, 'f, dynamic_cost) kinstr
  | KEdiv_intint :
      (z num, z num * 's) kinfo
      * ((z num, n num) pair option, 's, 'r, 'f, _) kinstr
      -> (z num, z num * 's, 'r, 'f, dynamic_cost) kinstr
  | KEdiv_intnat :
      (z num, n num * 's) kinfo
      * ((z num, n num) pair option, 's, 'r, 'f, _) kinstr
      -> (z num, n num * 's, 'r, 'f, dynamic_cost) kinstr
  | KEdiv_natint :
      (n num, z num * 's) kinfo
      * ((z num, n num) pair option, 's, 'r, 'f, _) kinstr
      -> (n num, z num * 's, 'r, 'f, dynamic_cost) kinstr
  | KEdiv_natnat :
      (n num, n num * 's) kinfo
      * ((n num, n num) pair option, 's, 'r, 'f, _) kinstr
      -> (n num, n num * 's, 'r, 'f, dynamic_cost) kinstr
  | KLsl_nat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f, _) kinstr
      -> (n num, n num * 's, 'r, 'f, dynamic_cost) kinstr
  | KLsr_nat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f, _) kinstr
      -> (n num, n num * 's, 'r, 'f, dynamic_cost) kinstr
  | KOr_nat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f, _) kinstr
      -> (n num, n num * 's, 'r, 'f, dynamic_cost) kinstr
  | KAnd_nat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f, _) kinstr
      -> (n num, n num * 's, 'r, 'f, dynamic_cost) kinstr
  | KAnd_int_nat :
      (z num, n num * 's) kinfo * (n num, 's, 'r, 'f, _) kinstr
      -> (z num, n num * 's, 'r, 'f, dynamic_cost) kinstr
  | KXor_nat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f, _) kinstr
      -> (n num, n num * 's, 'r, 'f, dynamic_cost) kinstr
  | KNot_nat :
      (n num, 's) kinfo * (z num, 's, 'r, 'f, _) kinstr
      -> (n num, 's, 'r, 'f, dynamic_cost) kinstr
  | KNot_int :
      (z num, 's) kinfo * (z num, 's, 'r, 'f, _) kinstr
      -> (z num, 's, 'r, 'f, dynamic_cost) kinstr
  (*
     Control
     -------
  *)
  | KIf :
      (bool, 'a * 's) kinfo
      * ('a, 's, 'r, 'f, _) kinstr
      * ('a, 's, 'r, 'f, _) kinstr
      -> (bool, 'a * 's, 'r, 'f, static_cost) kinstr
  | KLoop :
      (bool, 'a * 's) kinfo
      * ('a, 's, bool, 'a * 's, _) kinstr
      * ('a, 's, 'r, 'f, _) kinstr
      -> (bool, 'a * 's, 'r, 'f, static_cost) kinstr
  | KLoop_left :
      (('a, 'b) union, 's) kinfo
      * ('a, 's, ('a, 'b) union, 's, _) kinstr
      * ('b, 's, 'r, 'f, _) kinstr
      -> (('a, 'b) union, 's, 'r, 'f, static_cost) kinstr
  | KDip :
      ('a, 'b * 's) kinfo
      * ('c, 't) kinfo
      * ('b, 's, 'c, 't, _) kinstr
      * ('a, 'c * 't, 'r, 'f, _) kinstr
      -> ('a, 'b * 's, 'r, 'f, static_cost) kinstr
  | KExec :
      ('a, ('a, 'b) lambda * 's) kinfo * ('b, 's, 'r, 'f, _) kinstr
      -> ('a, ('a, 'b) lambda * 's, 'r, 'f, static_cost) kinstr
  | KApply :
      ('a, ('a * 't, 'b) lambda * 's) kinfo
      * 'a ty
      * (('t, 'b) lambda, 's, 'r, 'f, _) kinstr
      -> ('a, ('a * 't, 'b) lambda * 's, 'r, 'f, static_cost) kinstr
  | KLambda :
      ('a, 's) kinfo
      * ('b, 'c) lambda
      * (('b, 'c) lambda, 'a * 's, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KFailwith :
      ('a, 's) kinfo * Script.location * 'a ty * ('b, 't, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KNop :
      ('a, 's) kinfo * ('a, 's, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  (*
     Comparison
     ----------
  *)
  | KCompare :
      ('a, 'a * 's) kinfo * 'a comparable_ty * (z num, 's, 'r, 'f, _) kinstr
      -> ('a, 'a * 's, 'r, 'f, dynamic_cost) kinstr
  (*
     Comparators
     -----------
  *)
  | KEq :
      (z num, 's) kinfo * (bool, 's, 'r, 'f, _) kinstr
      -> (z num, 's, 'r, 'f, static_cost) kinstr
  | KNeq :
      (z num, 's) kinfo * (bool, 's, 'r, 'f, _) kinstr
      -> (z num, 's, 'r, 'f, static_cost) kinstr
  | KLt :
      (z num, 's) kinfo * (bool, 's, 'r, 'f, _) kinstr
      -> (z num, 's, 'r, 'f, static_cost) kinstr
  | KGt :
      (z num, 's) kinfo * (bool, 's, 'r, 'f, _) kinstr
      -> (z num, 's, 'r, 'f, static_cost) kinstr
  | KLe :
      (z num, 's) kinfo * (bool, 's, 'r, 'f, _) kinstr
      -> (z num, 's, 'r, 'f, static_cost) kinstr
  | KGe :
      (z num, 's) kinfo * (bool, 's, 'r, 'f, _) kinstr
      -> (z num, 's, 'r, 'f, static_cost) kinstr
  (*
     Protocol
     --------
  *)
  | KAddress :
      ('a typed_contract, 's) kinfo * (address, 's, 'r, 'f, _) kinstr
      -> ('a typed_contract, 's, 'r, 'f, static_cost) kinstr
  | KContract :
      (address, 's) kinfo
      * 'a ty
      * string
      * ('a typed_contract option, 's, 'r, 'f, _) kinstr
      -> (address, 's, 'r, 'f, static_cost) kinstr
  | KTransfer_tokens :
      ('a, Tez.t * ('a typed_contract * 's)) kinfo
      * (operation, 's, 'r, 'f, _) kinstr
      -> ('a, Tez.t * ('a typed_contract * 's), 'r, 'f, static_cost) kinstr
  | KImplicit_account :
      (public_key_hash, 's) kinfo * (unit typed_contract, 's, 'r, 'f, _) kinstr
      -> (public_key_hash, 's, 'r, 'f, static_cost) kinstr
  | KCreate_contract :
      (public_key_hash option, Tez.t * ('a * 's)) kinfo
      * 'a ty
      * 'b ty
      * ('b * 'a, operation boxed_list * 'a) lambda
      * field_annot option
      * (operation, address * 's, 'r, 'f, _) kinstr
      -> ( public_key_hash option,
           Tez.t * ('a * 's),
           'r,
           'f,
           static_cost )
         kinstr
  | KSet_delegate :
      (public_key_hash option, 's) kinfo * (operation, 's, 'r, 'f, _) kinstr
      -> (public_key_hash option, 's, 'r, 'f, static_cost) kinstr
  | KNow :
      ('a, 's) kinfo * (Script_timestamp.t, 'a * 's, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KBalance :
      ('a, 's) kinfo * (Tez.t, 'a * 's, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KLevel :
      ('a, 's) kinfo * (n num, 'a * 's, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KCheck_signature :
      (public_key, signature * (bytes * 's)) kinfo
      * (bool, 's, 'r, 'f, _) kinstr
      -> (public_key, signature * (bytes * 's), 'r, 'f, dynamic_cost) kinstr
  | KHash_key :
      (public_key, 's) kinfo * (public_key_hash, 's, 'r, 'f, _) kinstr
      -> (public_key, 's, 'r, 'f, dynamic_cost) kinstr
  | KPack :
      ('a, 's) kinfo * 'a ty * (bytes, 's, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KUnpack :
      (bytes, 's) kinfo * 'a ty * ('a option, 's, 'r, 'f, _) kinstr
      -> (bytes, 's, 'r, 'f, static_cost) kinstr
  | KBlake2b :
      (bytes, 's) kinfo * (bytes, 's, 'r, 'f, _) kinstr
      -> (bytes, 's, 'r, 'f, dynamic_cost) kinstr
  | KSha256 :
      (bytes, 's) kinfo * (bytes, 's, 'r, 'f, _) kinstr
      -> (bytes, 's, 'r, 'f, dynamic_cost) kinstr
  | KSha512 :
      (bytes, 's) kinfo * (bytes, 's, 'r, 'f, _) kinstr
      -> (bytes, 's, 'r, 'f, dynamic_cost) kinstr
  | KSource :
      ('a, 's) kinfo * (address, 'a * 's, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KSender :
      ('a, 's) kinfo * (address, 'a * 's, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KSelf :
      ('a, 's) kinfo
      * 'b ty
      * string
      * ('b typed_contract, 'a * 's, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KSelf_address :
      ('a, 's) kinfo * (address, 'a * 's, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KAmount :
      ('a, 's) kinfo * (Tez.t, 'a * 's, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KSapling_empty_state :
      ('a, 's) kinfo
      * Sapling.Memo_size.t
      * (Sapling.state, 'a * 's, 'b, 'f, _) kinstr
      -> ('a, 's, 'b, 'f, static_cost) kinstr
  | KSapling_verify_update :
      (Sapling.transaction, Sapling.state * 's) kinfo
      * ((z num, Sapling.state) pair option, 's, 'r, 'f, _) kinstr
      -> (Sapling.transaction, Sapling.state * 's, 'r, 'f, dynamic_cost) kinstr
  | KDig :
      ('a, 's) kinfo
      * int
      * ('b * 't, 't, 'a * 's, 'u) stack_prefix_preservation_witness
      * ('b, 'u, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KDug :
      ('a, 's) kinfo
      * int
      * ('t, 'a * 't, 's, 'b * 'u) stack_prefix_preservation_witness
      * ('b, 'u, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KDipn :
      ('a, 's) kinfo
      * int
      * ('c * 't, 'd * 'v, 'a * 's, 'b * 'u) kstack_prefix_preservation_witness
      * ('c, 't, 'd, 'v, _) kinstr
      * ('b, 'u, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KDropn :
      ('a, 's) kinfo
      * int
      * ('b * 'u, 'b * 'u, 'a * 's, 'a * 's) stack_prefix_preservation_witness
      * ('b, 'u, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KChainId :
      ('a, 's) kinfo * (Chain_id.t, 'a * 's, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KNever :
      (never, 's) kinfo * ('b, 'u, 'r, 'f, _) kinstr
      -> (never, 's, 'r, 'f, static_cost) kinstr
  | KVoting_power :
      (public_key_hash, 's) kinfo * (n num, 's, 'r, 'f, _) kinstr
      -> (public_key_hash, 's, 'r, 'f, static_cost) kinstr
  | KTotal_voting_power :
      ('a, 's) kinfo * (n num, 'a * 's, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KKeccak :
      (bytes, 's) kinfo * (bytes, 's, 'r, 'f, _) kinstr
      -> (bytes, 's, 'r, 'f, dynamic_cost) kinstr
  | KSha3 :
      (bytes, 's) kinfo * (bytes, 's, 'r, 'f, _) kinstr
      -> (bytes, 's, 'r, 'f, dynamic_cost) kinstr
  | KAdd_bls12_381_g1 :
      (Bls12_381.G1.t, Bls12_381.G1.t * 's) kinfo
      * (Bls12_381.G1.t, 's, 'r, 'f, _) kinstr
      -> (Bls12_381.G1.t, Bls12_381.G1.t * 's, 'r, 'f, static_cost) kinstr
  | KAdd_bls12_381_g2 :
      (Bls12_381.G2.t, Bls12_381.G2.t * 's) kinfo
      * (Bls12_381.G2.t, 's, 'r, 'f, _) kinstr
      -> (Bls12_381.G2.t, Bls12_381.G2.t * 's, 'r, 'f, static_cost) kinstr
  | KAdd_bls12_381_fr :
      (Bls12_381.Fr.t, Bls12_381.Fr.t * 's) kinfo
      * (Bls12_381.Fr.t, 's, 'r, 'f, _) kinstr
      -> (Bls12_381.Fr.t, Bls12_381.Fr.t * 's, 'r, 'f, static_cost) kinstr
  | KMul_bls12_381_g1 :
      (Bls12_381.G1.t, Bls12_381.Fr.t * 's) kinfo
      * (Bls12_381.G1.t, 's, 'r, 'f, _) kinstr
      -> (Bls12_381.G1.t, Bls12_381.Fr.t * 's, 'r, 'f, static_cost) kinstr
  | KMul_bls12_381_g2 :
      (Bls12_381.G2.t, Bls12_381.Fr.t * 's) kinfo
      * (Bls12_381.G2.t, 's, 'r, 'f, _) kinstr
      -> (Bls12_381.G2.t, Bls12_381.Fr.t * 's, 'r, 'f, static_cost) kinstr
  | KMul_bls12_381_fr :
      (Bls12_381.Fr.t, Bls12_381.Fr.t * 's) kinfo
      * (Bls12_381.Fr.t, 's, 'r, 'f, _) kinstr
      -> (Bls12_381.Fr.t, Bls12_381.Fr.t * 's, 'r, 'f, static_cost) kinstr
  | KMul_bls12_381_z_fr :
      (Bls12_381.Fr.t, 'a num * 's) kinfo
      * (Bls12_381.Fr.t, 's, 'r, 'f, _) kinstr
      -> (Bls12_381.Fr.t, 'a num * 's, 'r, 'f, static_cost) kinstr
  | KMul_bls12_381_fr_z :
      ('a num, Bls12_381.Fr.t * 's) kinfo
      * (Bls12_381.Fr.t, 's, 'r, 'f, _) kinstr
      -> ('a num, Bls12_381.Fr.t * 's, 'r, 'f, static_cost) kinstr
  | KInt_bls12_381_fr :
      (Bls12_381.Fr.t, 's) kinfo * (z num, 's, 'r, 'f, _) kinstr
      -> (Bls12_381.Fr.t, 's, 'r, 'f, static_cost) kinstr
  | KNeg_bls12_381_g1 :
      (Bls12_381.G1.t, 's) kinfo * (Bls12_381.G1.t, 's, 'r, 'f, _) kinstr
      -> (Bls12_381.G1.t, 's, 'r, 'f, static_cost) kinstr
  | KNeg_bls12_381_g2 :
      (Bls12_381.G2.t, 's) kinfo * (Bls12_381.G2.t, 's, 'r, 'f, _) kinstr
      -> (Bls12_381.G2.t, 's, 'r, 'f, static_cost) kinstr
  | KNeg_bls12_381_fr :
      (Bls12_381.Fr.t, 's) kinfo * (Bls12_381.Fr.t, 's, 'r, 'f, _) kinstr
      -> (Bls12_381.Fr.t, 's, 'r, 'f, static_cost) kinstr
  | KPairing_check_bls12_381 :
      ((Bls12_381.G1.t, Bls12_381.G2.t) pair boxed_list, 's) kinfo
      * (bool, 's, 'r, 'f, _) kinstr
      -> ( (Bls12_381.G1.t, Bls12_381.G2.t) pair boxed_list,
           's,
           'r,
           'f,
           dynamic_cost )
         kinstr
  | KComb :
      ('a, 's) kinfo
      * int
      * ('a * 's, 'b * 'u) comb_gadt_witness
      * ('b, 'u, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KUncomb :
      ('a, 's) kinfo
      * int
      * ('a * 's, 'b * 'u) uncomb_gadt_witness
      * ('b, 'u, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KComb_get :
      ('t, 's) kinfo
      * int
      * ('t, 'v) comb_get_gadt_witness
      * ('v, 's, 'r, 'f, _) kinstr
      -> ('t, 's, 'r, 'f, static_cost) kinstr
  | KComb_set :
      ('a, 'b * 's) kinfo
      * int
      * ('a, 'b, 'c) comb_set_gadt_witness
      * ('c, 's, 'r, 'f, _) kinstr
      -> ('a, 'b * 's, 'r, 'f, static_cost) kinstr
  | KDup_n :
      ('a, 's) kinfo
      * int
      * ('a * 's, 't) dup_n_gadt_witness
      * ('t, 'a * 's, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KTicket :
      ('a, n num * 's) kinfo * ('a ticket, 's, 'r, 'f, _) kinstr
      -> ('a, n num * 's, 'r, 'f, static_cost) kinstr
  | KRead_ticket :
      ('a ticket, 's) kinfo
      * (address * ('a * n num), 'a ticket * 's, 'r, 'f, _) kinstr
      -> ('a ticket, 's, 'r, 'f, static_cost) kinstr
  | KSplit_ticket :
      ('a ticket, (n num * n num) * 's) kinfo
      * (('a ticket * 'a ticket) option, 's, 'r, 'f, _) kinstr
      -> ('a ticket, (n num * n num) * 's, 'r, 'f, dynamic_cost) kinstr
  | KJoin_tickets :
      ('a ticket * 'a ticket, 's) kinfo
      * 'a comparable_ty
      * ('a ticket option, 's, 'r, 'f, _) kinstr
      -> ('a ticket * 'a ticket, 's, 'r, 'f, dynamic_cost) kinstr
  | KHalt : ('a, 's) kinfo -> ('a, 's, 'a, 's, static_cost) kinstr
  | KPayGas :
      ('a, 's) kinfo
      * ('b, 't) kinfo
      * Gas.cost
      * ('a, 's, 'b, 't, _) kinstr
      * ('b, 't, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr
  | KCountGas :
      ('a, 's) kinfo * ('a, 's, 'r, 'f, _) kinstr
      -> ('a, 's, 'r, 'f, static_cost) kinstr

and ('bef, 'aft) kdescr =
  | KDescr : {
      kloc : Script.location;
      kbef : 'bef stack_ty;
      kaft : 'aft stack_ty;
      kli : ('bef, 'a * 's) lift;
      klo : ('aft, 'r * 'f) lift;
      kinstr : ('a, 's, 'r, 'f, 'cost) kinstr;
    }
      -> ('bef, 'aft) kdescr

and ('a, 's) kinfo = {kloc : Script.location; kstack_ty : ('a * 's) stack_ty}

and (_, _) lift =
  | BaseLift : (unit, unit * unit) lift
  | IndLift : ('a, 'y * 'b) lift -> ('x * 'a, 'x * ('y * 'b)) lift

and _ is_lifted = IsLifted : ('a, 'b) lift -> 'b is_lifted

and ('bef, 'aft, 'bef_suffix, 'aft_suffix) kstack_prefix_preservation_witness =
  | KPrefix :
      ('y, 'aft) kinfo
      * 'bef is_lifted
      * ('y * 'aft) is_lifted
      * ('fbef, 'faft, 'bef, 'y * 'aft) kstack_prefix_preservation_witness
      -> ( 'fbef,
           'faft,
           'x * 'bef,
           'x * ('y * 'aft) )
         kstack_prefix_preservation_witness
  | KRest :
      'bef is_lifted * 'aft is_lifted
      -> ('bef, 'aft, 'bef, 'aft) kstack_prefix_preservation_witness

(*

   A context is an instruction with a hole where a continuation can
   be grafted. Contexts are useful to implement program rewritings.

*)
type ('a, 's, 'b, 't, 'c) kinstr_context = {
  graft :
    'r 'f 'c2. ('b, 't, 'r, 'f, 'c2) kinstr -> ('a, 's, 'r, 'f, 'c) kinstr;
}
[@@unboxed]

let kinfo_of_kinstr :
    type a s b f cost. (a, s, b, f, cost) kinstr -> (a, s) kinfo =
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
  | KList_mapping (kinfo, _, _, _, _, _, _) ->
      kinfo
  | KList_mapped (kinfo, _, _, _, _, _, _) ->
      kinfo
  | KList_iter (kinfo, _, _, _) ->
      kinfo
  | KIter (kinfo, _, _, _) ->
      kinfo
  | KList_size (kinfo, _) ->
      kinfo
  | KEmpty_set (kinfo, _, _) ->
      kinfo
  | KSet_iter (kinfo, _, _, _) ->
      kinfo
  | KSet_mem (kinfo, _) ->
      kinfo
  | KSet_update (kinfo, _) ->
      kinfo
  | KSet_size (kinfo, _) ->
      kinfo
  | KEmpty_map (kinfo, _, _, _) ->
      kinfo
  | KMap_map (kinfo, _, _, _, _) ->
      kinfo
  | KMap_mapping (kinfo, _, _, _, _, _) ->
      kinfo
  | KMap_mapped (kinfo, _, _, _, _, _, _) ->
      kinfo
  | KMap_iter (kinfo, _, _, _) ->
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
  | KPayGas (kinfo, _, _, _, _) ->
      kinfo
  | KCountGas (kinfo, _) ->
      kinfo

let rec lift : type s t. (s, t) lift -> s -> t =
 fun l s ->
  match l with BaseLift -> ((), ()) | IndLift l -> (fst s, lift l (snd s))

let rec unlift : type s t. (s, t) lift -> t -> s =
 fun l t ->
  match l with BaseLift -> () | IndLift l -> (fst t, unlift l (snd t))

let succ_lift : ('a, 'y * 'b) lift -> ('x * 'a, 'x * ('y * 'b)) lift =
 fun l -> IndLift l

let coerce_lift : type x y a b. (x * a, x * b) lift -> (y * a, y * b) lift =
  function
  | IndLift l ->
      IndLift l

let succ_is_lifted : type x a. a is_lifted -> (x * a) is_lifted = function
  | IsLifted BaseLift ->
      IsLifted (IndLift BaseLift)
  | IsLifted (IndLift l) ->
      IsLifted (IndLift (IndLift l))

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

let rec kstack_prefix_preservation_witness :
    type s u s' u' ds du ds' du'.
    Script.location ->
    u' stack_ty ->
    (ds, du, s, u) stack_prefix_preservation_witness ->
    (s, s') lift ->
    (u, u') lift ->
    (ds, ds') lift ->
    (du, du') lift ->
    (ds', du', s', u') kstack_prefix_preservation_witness =
 fun kloc kstack_ty w ls lu lds ldu ->
  match w with
  | Rest -> (
    match fun_lift ls lds with
    | Refl -> (
      match fun_lift lu ldu with
      | Refl ->
          (* ds  = s, du = u *)
          KRest (IsLifted lds, IsLifted ldu) ) )
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
      match kstack_ty with
      | Item_t (_, kstack_ty, _) ->
          let kw =
            kstack_prefix_preservation_witness kloc kstack_ty w ls lu lds ldu
          in
          let kinfo = {kloc; kstack_ty} in
          KPrefix (kinfo, IsLifted ls, IsLifted lu, kw) ) )

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

let rec lift_comb_gadt_witness :
    type s s' u u'.
    (s, u) comb_gadt_witness ->
    (s, s') lift ->
    (u, u') lift ->
    (s', u') comb_gadt_witness =
 fun w li lo ->
  match w with
  | Comb_one -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> Comb_one ) ) )
  | Comb_succ w -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo ->
          let w = lift_comb_gadt_witness w li (IndLift lo) in
          Comb_succ w ) )

let rec lift_stack_ty :
    type t a s. (t, a * s) lift -> t stack_ty -> (a * s) stack_ty =
 fun li stack ->
  match li with
  | BaseLift ->
      Item_t (Unit_t None, stack, None)
  | IndLift li -> (
    match stack with
    | Item_t (ty, stack, a) ->
        let kstack = lift_stack_ty li stack in
        Item_t (ty, kstack, a) )

let rec lift_uncomb_gadt_witness :
    type s s' u u'.
    (s, u) uncomb_gadt_witness ->
    (s, s') lift ->
    (u, u') lift ->
    (s', u') uncomb_gadt_witness =
 fun w li lo ->
  match w with
  | Uncomb_one -> (
    match fun_lift li lo with Refl -> Uncomb_one )
  | Uncomb_succ w -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo ->
          let w = lift_uncomb_gadt_witness w (IndLift li) lo in
          Uncomb_succ w ) )

type (_, _, _, _) exkinstr =
  | ExKInstr : ('a, 's, 'r, 'f, 'c) kinstr -> ('a, 's, 'r, 'f) exkinstr

let rec translate_instr :
    type a b s t v u r f c.
    (t, v) descr ->
    (t, a * s) lift ->
    (v, b * u) lift ->
    (b, u, r, f, c) kinstr ->
    (a, s, r, f) exkinstr =
  let return k = ExKInstr k in
  fun i li lo k ->
    let kstack_ty = lift_stack_ty li i.bef in
    let kinfo = {kloc = i.loc; kstack_ty} in
    match i.instr with
    | Seq (i1, i2) -> (
      match lift_type i1.aft with
      | ExLift lii ->
          let (ExKInstr ki2) = translate_instr i2 lii lo k in
          translate_instr i1 li lii ki2 )
    | Drop -> (
      match li with
      | IndLift l -> (
        match fun_lift l lo with Refl -> return (KDrop (kinfo, k)) ) )
    | Dup -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match lo with
          | IndLift lo -> (
            match fun_lift lo li with Refl -> return @@ KDup (kinfo, k) ) ) ) )
    | Swap -> (
      match lo with
      | IndLift lo -> (
        match lo with
        | IndLift lo -> (
          match li with
          | IndLift li -> (
            match li with
            | IndLift li -> (
              match fun_lift lo li with Refl -> return @@ KSwap (kinfo, k) ) )
          ) ) )
    | Const ty -> (
      match lo with
      | IndLift lo -> (
        match fun_lift lo li with Refl -> return @@ KConst (kinfo, ty, k) ) )
    | Cons_pair -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift lo li with Refl -> return @@ KCons_pair (kinfo, k) )
          ) ) )
    | Car -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift lo li with Refl -> return @@ KCar (kinfo, k) ) ) )
    | Cdr -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift lo li with Refl -> return @@ KCdr (kinfo, k) ) ) )
    | Unpair -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match lo with
          | IndLift lo -> (
            match fun_lift lo li with Refl -> return @@ KUnpair (kinfo, k) ) )
        ) )
    | Cons_some -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift lo li with Refl -> return @@ KCons_some (kinfo, k) ) )
      )
    | Cons_none ty -> (
      match lo with
      | IndLift lo -> (
        match fun_lift lo li with Refl -> return @@ KCons_none (kinfo, ty, k) )
      )
    | If_none (i1, i2) -> (
      match li with
      | IndLift li' ->
          let (ExKInstr ki1) = translate_instr i1 li' lo k in
          let (ExKInstr ki2) = translate_instr i2 (coerce_lift li) lo k in
          return @@ KIf_none (kinfo, ki1, ki2) )
    | Cons_left -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift lo li with Refl -> return @@ KCons_left (kinfo, k) ) )
      )
    | Cons_right -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift lo li with Refl -> return @@ KCons_right (kinfo, k) )
        ) )
    | If_left (i1, i2) -> (
      match li with
      | IndLift _ ->
          let (ExKInstr ki1) = translate_instr i1 (coerce_lift li) lo k in
          let (ExKInstr ki2) = translate_instr i2 (coerce_lift li) lo k in
          return @@ KIf_left (kinfo, ki1, ki2) )
    | Cons_list -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift lo li with Refl -> return @@ KCons_list (kinfo, k) )
          ) ) )
    | Nil -> (
      match lo with
      | IndLift lo -> (
        match fun_lift lo li with Refl -> return @@ KNil (kinfo, k) ) )
    | If_cons (i1, i2) -> (
      match li with
      | IndLift li' ->
          let (ExKInstr ki1) = translate_instr i1 (succ_lift li) lo k in
          let (ExKInstr ki2) = translate_instr i2 li' lo k in
          return @@ KIf_cons (kinfo, ki1, ki2) )
    | List_map i -> (
      match li with
      | IndLift li' -> (
        match lo with
        | IndLift lo' -> (
          match fun_lift li' lo' with
          | Refl ->
              let khalt =
                KHalt
                  {
                    kloc = i.loc;
                    kstack_ty = lift_stack_ty (coerce_lift lo) i.aft;
                  }
              in
              let (ExKInstr ki) =
                translate_instr i (coerce_lift li) (coerce_lift lo) khalt
              in
              return @@ KList_map (kinfo, ki, k) ) ) )
    | List_iter i -> (
      match li with
      | IndLift li' -> (
        match fun_lift li' lo with
        | Refl ->
            let kinfo' = {kloc = i.loc; kstack_ty = lift_stack_ty lo i.aft} in
            let (ExKInstr ki) =
              translate_instr i (coerce_lift li) lo (KHalt kinfo')
            in
            return @@ KList_iter (kinfo, kinfo', ki, k) ) )
    | List_size -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KList_size (kinfo, k) ) )
      )
    | Empty_set ty -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> return @@ KEmpty_set (kinfo, ty, k) )
      )
    | Set_iter i -> (
      match li with
      | IndLift li' -> (
        match fun_lift li' lo with
        | Refl ->
            let kinfo' = {kloc = i.loc; kstack_ty = lift_stack_ty lo i.aft} in
            let (ExKInstr ki) =
              translate_instr i (coerce_lift li) lo (KHalt kinfo')
            in
            return @@ KSet_iter (kinfo, kinfo', ki, k) ) )
    | Set_mem -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KSet_mem (kinfo, k) ) )
        ) )
    | Set_update -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match li with
          | IndLift li -> (
            match lo with
            | IndLift lo -> (
              match fun_lift li lo with
              | Refl ->
                  return @@ KSet_update (kinfo, k) ) ) ) ) )
    | Set_size -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KSet_size (kinfo, k) ) )
      )
    | Empty_map (cty, ty) -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with
        | Refl ->
            return @@ KEmpty_map (kinfo, cty, ty, k) ) )
    | Map_map i -> (
      match li with
      | IndLift li' -> (
        match lo with
        | IndLift lo' -> (
          match fun_lift li' lo' with
          | Refl ->
              let khalt =
                KHalt
                  {
                    kloc = i.loc;
                    kstack_ty = lift_stack_ty (coerce_lift lo) i.aft;
                  }
              in
              let (ExKInstr ki) =
                translate_instr i (coerce_lift li) (coerce_lift lo) khalt
              in
              let kinfo_mapped =
                match (kinfo_of_kinstr k).kstack_ty with
                | Item_t (ty, s, a) ->
                    {kinfo with kstack_ty = Item_t (snd (unmap_ty ty), s, a)}
              in
              let kinfo_mapping =
                match kinfo.kstack_ty with
                | Item_t (_, kstack_ty, _) ->
                    {kloc = kinfo.kloc; kstack_ty}
              in
              return @@ KMap_map (kinfo, kinfo_mapping, kinfo_mapped, ki, k) )
        ) )
    | Map_iter i -> (
      match li with
      | IndLift li' -> (
        match fun_lift li' lo with
        | Refl ->
            let khalt =
              KHalt {kloc = i.loc; kstack_ty = lift_stack_ty lo i.aft}
            in
            let (ExKInstr ki) = translate_instr i (coerce_lift li) lo khalt in
            let kinfo_iter =
              match kinfo.kstack_ty with
              | Item_t (_, kstack_ty, _) ->
                  {kinfo with kstack_ty}
            in
            return @@ KMap_iter (kinfo, kinfo_iter, ki, k) ) )
    | Map_mem -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KMap_mem (kinfo, k) ) )
        ) )
    | Map_get -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KMap_get (kinfo, k) ) )
        ) )
    | Map_update -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match li with
          | IndLift li -> (
            match lo with
            | IndLift lo -> (
              match fun_lift li lo with
              | Refl ->
                  return @@ KMap_update (kinfo, k) ) ) ) ) )
    | Map_get_and_update -> (
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
                    return @@ KMap_get_and_update (kinfo, k) ) ) ) ) ) )
    | Map_size -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KMap_size (kinfo, k) ) )
      )
    | Empty_big_map (cty, ty) -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with
        | Refl ->
            return @@ KEmpty_big_map (kinfo, cty, ty, k) ) )
    | Big_map_mem -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ KBig_map_mem (kinfo, k) ) ) ) )
    | Big_map_get -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ KBig_map_get (kinfo, k) ) ) ) )
    | Big_map_get_and_update -> (
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
                    return @@ KBig_map_get_and_update (kinfo, k) ) ) ) ) ) )
    | Big_map_update -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match li with
          | IndLift li -> (
            match lo with
            | IndLift lo -> (
              match fun_lift li lo with
              | Refl ->
                  return @@ KBig_map_update (kinfo, k) ) ) ) ) )
    | Concat_string -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with
          | Refl ->
              return @@ KConcat_string (kinfo, k) ) ) )
    | Concat_string_pair -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ KConcat_string_pair (kinfo, k) ) ) ) )
    | Slice_string -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match li with
          | IndLift li -> (
            match lo with
            | IndLift lo -> (
              match fun_lift li lo with
              | Refl ->
                  return @@ KSlice_string (kinfo, k) ) ) ) ) )
    | String_size -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KString_size (kinfo, k) )
        ) )
    | Concat_bytes -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KConcat_bytes (kinfo, k)
          ) ) )
    | Concat_bytes_pair -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ KConcat_bytes_pair (kinfo, k) ) ) ) )
    | Slice_bytes -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match li with
          | IndLift li -> (
            match lo with
            | IndLift lo -> (
              match fun_lift li lo with
              | Refl ->
                  return @@ KSlice_bytes (kinfo, k) ) ) ) ) )
    | Bytes_size -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KBytes_size (kinfo, k) )
        ) )
    | Add_seconds_to_timestamp -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ KAdd_seconds_to_timestamp (kinfo, k) ) ) ) )
    | Add_timestamp_to_seconds -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ KAdd_timestamp_to_seconds (kinfo, k) ) ) ) )
    | Sub_timestamp_seconds -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ KSub_timestamp_seconds (kinfo, k) ) ) ) )
    | Diff_timestamps -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ KDiff_timestamps (kinfo, k) ) ) ) )
    | Add_tez -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KAdd_tez (kinfo, k) ) )
        ) )
    | Sub_tez -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KSub_tez (kinfo, k) ) )
        ) )
    | Mul_teznat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KMul_teznat (kinfo, k)
            ) ) ) )
    | Mul_nattez -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KMul_nattez (kinfo, k)
            ) ) ) )
    | Ediv_teznat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ KEdiv_teznat (kinfo, k) ) ) ) )
    | Ediv_tez -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KEdiv_tez (kinfo, k) )
          ) ) )
    | Or -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KOr (kinfo, k) ) ) ) )
    | And -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KAnd (kinfo, k) ) ) ) )
    | Xor -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KXor (kinfo, k) ) ) ) )
    | Not -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KNot (kinfo, k) ) ) )
    | Is_nat -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KIs_nat (kinfo, k) ) ) )
    | Neg_nat -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KNeg_nat (kinfo, k) ) ) )
    | Neg_int -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KNeg_int (kinfo, k) ) ) )
    | Abs_int -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KAbs_int (kinfo, k) ) ) )
    | Int_bls12_381_fr -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with
          | Refl ->
              return @@ KInt_bls12_381_fr (kinfo, k) ) ) )
    | Int_nat -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KInt_nat (kinfo, k) ) ) )
    | Add_intint -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KAdd_intint (kinfo, k)
            ) ) ) )
    | Add_intnat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KAdd_intnat (kinfo, k)
            ) ) ) )
    | Add_natint -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KAdd_natint (kinfo, k)
            ) ) ) )
    | Add_natnat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KAdd_natnat (kinfo, k)
            ) ) ) )
    | Sub_int -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KSub_int (kinfo, k) ) )
        ) )
    | Mul_intint -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KMul_intint (kinfo, k)
            ) ) ) )
    | Mul_intnat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KMul_intnat (kinfo, k)
            ) ) ) )
    | Mul_natint -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KMul_natint (kinfo, k)
            ) ) ) )
    | Mul_natnat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KMul_natnat (kinfo, k)
            ) ) ) )
    | Ediv_intint -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ KEdiv_intint (kinfo, k) ) ) ) )
    | Ediv_intnat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ KEdiv_intnat (kinfo, k) ) ) ) )
    | Ediv_natint -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ KEdiv_natint (kinfo, k) ) ) ) )
    | Ediv_natnat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ KEdiv_natnat (kinfo, k) ) ) ) )
    | Lsl_nat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KLsl_nat (kinfo, k) ) )
        ) )
    | Lsr_nat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KLsr_nat (kinfo, k) ) )
        ) )
    | Or_nat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KOr_nat (kinfo, k) ) )
        ) )
    | And_nat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KAnd_nat (kinfo, k) ) )
        ) )
    | And_int_nat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ KAnd_int_nat (kinfo, k) ) ) ) )
    | Xor_nat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KXor_nat (kinfo, k) ) )
        ) )
    | Not_nat -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KNot_nat (kinfo, k) ) ) )
    | Not_int -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KNot_int (kinfo, k) ) ) )
    | If (i1, i2) -> (
      match li with
      | IndLift li ->
          let (ExKInstr ki1) = translate_instr i1 li lo k in
          let (ExKInstr ki2) = translate_instr i2 li lo k in
          return @@ KIf (kinfo, ki1, ki2) )
    | Loop i -> (
      match li with
      | IndLift li' -> (
        match fun_lift li' lo with
        | Refl ->
            let khalt =
              KHalt {kloc = i.loc; kstack_ty = lift_stack_ty li i.aft}
            in
            let (ExKInstr ki) = translate_instr i li' li khalt in
            return @@ KLoop (kinfo, ki, k) ) )
    | Loop_left i -> (
      match li with
      | IndLift li' -> (
        match lo with
        | IndLift lo' -> (
          match fun_lift li' lo' with
          | Refl ->
              let khalt =
                KHalt {kloc = i.loc; kstack_ty = lift_stack_ty li i.aft}
              in
              let (ExKInstr ki) =
                translate_instr i (coerce_lift li) li khalt
              in
              return @@ KLoop_left (kinfo, ki, k) ) ) )
    | Dip i -> (
      match li with
      | IndLift li' -> (
        match lo with
        | IndLift lo ->
            let kinfo_const =
              {kloc = i.loc; kstack_ty = lift_stack_ty lo i.aft}
            in
            let (ExKInstr ki) = translate_instr i li' lo (KHalt kinfo_const) in
            return @@ KDip (kinfo, kinfo_const, ki, k) ) )
    | Exec -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KExec (kinfo, k) ) ) )
      )
    | Apply f -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KApply (kinfo, f, k) )
          ) ) )
    | Lambda b -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> return @@ KLambda (kinfo, b, k) ) )
    | Failwith e -> (
      match li with IndLift _ -> return @@ KFailwith (kinfo, i.loc, e, k) )
    | Nop -> (
      match fun_lift li lo with Refl -> return @@ KNop (kinfo, k) )
    | Compare c -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KCompare (kinfo, c, k)
            ) ) ) )
    | Eq -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KEq (kinfo, k) ) ) )
    | Neq -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KNeq (kinfo, k) ) ) )
    | Lt -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KLt (kinfo, k) ) ) )
    | Gt -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KGt (kinfo, k) ) ) )
    | Le -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KLe (kinfo, k) ) ) )
    | Ge -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KGe (kinfo, k) ) ) )
    | Address -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KAddress (kinfo, k) ) ) )
    | Contract (a, b) -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with
          | Refl ->
              return @@ KContract (kinfo, a, b, k) ) ) )
    | Transfer_tokens -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match li with
          | IndLift li -> (
            match lo with
            | IndLift lo -> (
              match fun_lift li lo with
              | Refl ->
                  return @@ KTransfer_tokens (kinfo, k) ) ) ) ) )
    | Implicit_account -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with
          | Refl ->
              return @@ KImplicit_account (kinfo, k) ) ) )
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
                    return @@ KCreate_contract (kinfo, a, b, c, d, k) ) ) ) ) )
      )
    | Set_delegate -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KSet_delegate (kinfo, k)
          ) ) )
    | Now -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> return @@ KNow (kinfo, k) ) )
    | Balance -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> return @@ KBalance (kinfo, k) ) )
    | Level -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> return @@ KLevel (kinfo, k) ) )
    | Check_signature -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match li with
          | IndLift li -> (
            match lo with
            | IndLift lo -> (
              match fun_lift li lo with
              | Refl ->
                  return @@ KCheck_signature (kinfo, k) ) ) ) ) )
    | Hash_key -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KHash_key (kinfo, k) ) )
      )
    | Pack ty -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KPack (kinfo, ty, k) ) )
      )
    | Unpack ty -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KUnpack (kinfo, ty, k) )
        ) )
    | Blake2b -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KBlake2b (kinfo, k) ) ) )
    | Sha256 -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KSha256 (kinfo, k) ) ) )
    | Sha512 -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KSha512 (kinfo, k) ) ) )
    | Source -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> return @@ KSource (kinfo, k) ) )
    | Sender -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> return @@ KSender (kinfo, k) ) )
    | Self (a, b) -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> return @@ KSelf (kinfo, a, b, k) ) )
    | Self_address -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> return @@ KSelf_address (kinfo, k) )
      )
    | Amount -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> return @@ KAmount (kinfo, k) ) )
    | Sapling_empty_state m -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with
        | Refl ->
            return @@ KSapling_empty_state (kinfo, m.memo_size, k) ) )
    | Sapling_verify_update -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ KSapling_verify_update (kinfo, k) ) ) ) )
    | Dig (n, w) -> (
      match lo with
      | IndLift lo' -> (
        match lift_stack_prefix_preservation_witness w li lo' with
        | ExLiftStackPrefixPreservationWitness (lds, ldu, w') -> (
          match lds with
          | IndLift lds -> (
            match fun_lift lds ldu with
            | Refl ->
                return @@ KDig (kinfo, n, w', k) ) ) ) )
    | Dug (n, w) -> (
      match li with
      | IndLift li' -> (
        match lift_stack_prefix_preservation_witness w li' lo with
        | ExLiftStackPrefixPreservationWitness (lds, ldu, w') -> (
          match ldu with
          | IndLift ldu -> (
            match fun_lift lds ldu with
            | Refl ->
                return @@ KDug (kinfo, n, w', k) ) ) ) )
    | Dipn (n, w, i') -> (
      match lift_stack_prefix_preservation_witness w li lo with
      | ExLiftStackPrefixPreservationWitness (lds, ldu, _) -> (
        match inverse_lift lds with
        | ExLiftInverse Refl -> (
          match inverse_lift ldu with
          | ExLiftInverse Refl ->
              let hinfo =
                {kloc = i.loc; kstack_ty = lift_stack_ty ldu i'.aft}
              in
              let (ExKInstr ki') = translate_instr i' lds ldu (KHalt hinfo) in
              let sty = lift_stack_ty lo i.aft in
              let l = i.loc in
              let w =
                kstack_prefix_preservation_witness l sty w li lo lds ldu
              in
              return @@ KDipn (kinfo, n, w, ki', k) ) ) )
    | Dropn (n, w) -> (
      match lift_stack_prefix_preservation_witness w li li with
      | ExLiftStackPrefixPreservationWitness (lds, ldu, w') -> (
        match inverse_lift lds with
        | ExLiftInverse Refl -> (
          match inverse_lift ldu with
          | ExLiftInverse Refl -> (
            match fun_lift lds ldu with
            | Refl -> (
              match fun_lift lo lds with
              | Refl ->
                  return @@ KDropn (kinfo, n, w', k) ) ) ) ) )
    | ChainId -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> return @@ KChainId (kinfo, k) ) )
    | Never -> (
      match li with IndLift _ -> return @@ KNever (kinfo, k) )
    | Voting_power -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KVoting_power (kinfo, k)
          ) ) )
    | Total_voting_power -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with
        | Refl ->
            return @@ KTotal_voting_power (kinfo, k) ) )
    | Keccak -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KKeccak (kinfo, k) ) ) )
    | Sha3 -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KSha3 (kinfo, k) ) ) )
    | Add_bls12_381_g1 -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ KAdd_bls12_381_g1 (kinfo, k) ) ) ) )
    | Add_bls12_381_g2 -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ KAdd_bls12_381_g2 (kinfo, k) ) ) ) )
    | Add_bls12_381_fr -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ KAdd_bls12_381_fr (kinfo, k) ) ) ) )
    | Mul_bls12_381_g1 -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ KMul_bls12_381_g1 (kinfo, k) ) ) ) )
    | Mul_bls12_381_g2 -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ KMul_bls12_381_g2 (kinfo, k) ) ) ) )
    | Mul_bls12_381_z_fr -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ KMul_bls12_381_z_fr (kinfo, k) ) ) ) )
    | Mul_bls12_381_fr_z -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ KMul_bls12_381_fr_z (kinfo, k) ) ) ) )
    | Mul_bls12_381_fr -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ KMul_bls12_381_fr (kinfo, k) ) ) ) )
    | Neg_bls12_381_g1 -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with
          | Refl ->
              return @@ KNeg_bls12_381_g1 (kinfo, k) ) ) )
    | Neg_bls12_381_g2 -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with
          | Refl ->
              return @@ KNeg_bls12_381_g2 (kinfo, k) ) ) )
    | Neg_bls12_381_fr -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with
          | Refl ->
              return @@ KNeg_bls12_381_fr (kinfo, k) ) ) )
    | Pairing_check_bls12_381 -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with
          | Refl ->
              return @@ KPairing_check_bls12_381 (kinfo, k) ) ) )
    | Dup_n (n, i) -> (
        let i = lift_dup_n_gadt_witness i li in
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ KDup_n (kinfo, n, i, k) )
        )
    | Comb (n, w) ->
        let w = lift_comb_gadt_witness w li lo in
        return @@ KComb (kinfo, n, w, k)
    | Uncomb (n, w) ->
        let w = lift_uncomb_gadt_witness w li lo in
        return @@ KUncomb (kinfo, n, w, k)
    | Comb_get (n, w) -> (
      match li with
      | IndLift li' -> (
        match lo with
        | IndLift lo' -> (
          match fun_lift li' lo' with
          | Refl ->
              return @@ KComb_get (kinfo, n, w, k) ) ) )
    | Comb_set (n, w) -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ KComb_set (kinfo, n, w, k) ) ) ) )
    | Ticket -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ KTicket (kinfo, k) ) )
        ) )
    | Read_ticket -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ KRead_ticket (kinfo, k) ) ) ) )
    | Split_ticket -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ KSplit_ticket (kinfo, k) ) ) ) )
    | Join_tickets cty -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with
          | Refl ->
              return @@ KJoin_tickets (kinfo, cty, k) ) ) )

let translate : type bef aft. (bef, aft) descr -> (bef, aft) kdescr =
 fun d ->
  match (lift_type d.bef, lift_type d.aft) with
  | (ExLift kli, ExLift klo) ->
      let khalt = KHalt {kloc = d.loc; kstack_ty = lift_stack_ty klo d.aft} in
      let (ExKInstr kinstr) = translate_instr d kli klo khalt in
      KDescr {kloc = d.loc; kbef = d.bef; kaft = d.aft; kli; klo; kinstr}
