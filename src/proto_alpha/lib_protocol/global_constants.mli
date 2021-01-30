(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <team@marigold.dev>                     *)
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

(* Represents access to a global table of constant Micheline values.
Users may asociate a name as a string with a paritcular value,
storing the value on the chain (and paying the price of storage).

TODO: come up with a real naming scheme rather than allowing arbitrary
strings.
*)

type error += Set_on_existing_global_constant

(** [get_opt context key] Gets the value at a given address;
  returns returns [None] if the value is not set ;
  returns {!Storage_error Corrupted_data} if the deserialisation fails.
  Consumes [Gas_repr.read_bytes_cost <size of the value>] if present
  or [Gas_repr.read_bytes_cost Z.zero].*)
val get_opt :
  Raw_context.t ->
  string ->
  (Raw_context.t * (Script_repr.expr * Script_repr.expr) option) tzresult Lwt.t

(** [set context key expr] Allocates storages and sets the value at
  the given address. Returns result of a pair of the new context
  and the number of bytes stored as an integer [Z.t]. Returns error
  [Set_on_existing_global_constant] if a value already exists at
  that key. Consumes
  [Gas_repr.write_bytes_cost <size of the new value>].
  
  DH 1/12/2021
  Note, this function makes no guarantee that the type represented
  by [ty] actually matches the value represented by [value]. This
  is not good, as we currently rely on Apply.ml to maintain this
  invariant for us. However, moving the invariant here will involve
  breaking several circular dependencies.
  DH 1/30/2021
  This isn't isolated to this function - see this comment:
  https://gitlab.com/tezos/tezos/-/merge_requests/2474#note_484119636

  TODO: refactor Alpha_context, Script_typed_ir, and Global_constants
  so this can happen.
  *)
val set :
  Raw_context.t ->
  string ->
  Script_repr.expr ->
  Script_repr.expr ->
  (Raw_context.t * Z.t, error trace) result Lwt.t
