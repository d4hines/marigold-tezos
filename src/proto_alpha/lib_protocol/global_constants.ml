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

type error += Set_on_existing_global_constant

let () =
  register_error_kind
    `Branch
    ~id:"Set_on_existing_global_constant"
    ~title:"Set called on existing global constant"
    ~description:
      "Set was called on an existing global constant. You cannot overwrite an \
       existing constant."
    ~pp:(fun ppf () -> Format.fprintf ppf "Set on existing global constant")
    Data_encoding.empty
    (function Set_on_existing_global_constant -> Some () | _ -> None)
    (fun () -> Set_on_existing_global_constant)

let get_opt context address =
  let key = Storage.Global_constants.Index.of_string address in
  Storage.Global_constants.get_option context key

let set context address ty value =
  let key = Storage.Global_constants.Index.of_string address in
  (* It is forbidden to update an existing global constant,
    so we first check to see if one exists at the given address
    and fail if this is the case. *)
  Storage.Global_constants.mem context key
  >>=? fun (context, exists) ->
  if exists then fail Set_on_existing_global_constant
  else
    Storage.Global_constants.init context key (ty, value)
    >|=? fun (context, size) ->
    let fee = Z.of_int size in
    (Raw_context.update_storage_space_to_pay context fee, fee)
