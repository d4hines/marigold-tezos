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

let (let*) = (>>=?)


type t = Raw_context.t

let init c =
  Storage.Rollups.Global_counter.init c Z.zero

let increment_counter ctxt =
  let* counter = Storage.Rollups.Global_counter.get ctxt in
  let counter = Z.succ counter in
  let* ctxt = Storage.Rollups.Global_counter.update ctxt counter in
  return (counter , ctxt)

(* Used for tests, debugging, etc. *)
module Dev = struct

  let get_counter ctxt =
    Storage.Rollups.Global_counter.get ctxt

end

module type TYPE = sig

  type t

  val init : t -> t tzresult Lwt.t
  val increment_counter : t -> (Z.t * t) tzresult Lwt.t

  module Dev : sig
    val get_counter : t -> Z.t tzresult Lwt.t
  end

end
