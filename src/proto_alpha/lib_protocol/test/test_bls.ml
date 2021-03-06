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

open Protocol.Bls_scheme

let test msg f = Test_services.tztest msg `Quick f

let iter (type a) (lst : a list) (f : a -> unit tzresult Lwt.t) : unit tzresult Lwt.t =
  let* _lst = all_ep @@ List.map f lst in
  return ()

let (let*) x f = x >>=? f
let (let*+) x f = Error_monad.all_ep (f x)
let (!*) i = Incremental.alpha_ctxt i
let (let**) x f = x >>= fun x -> Lwt.return @@ Environment.wrap_tzresult x >>=? f

let check condition msg =
  if condition then return () else failwith msg

let simple_signature_check = test "simple signature check" @@ fun () ->
  let account = Dev.create_account () in
  let message_content = Bytes.of_string "This is a message!" in
  let message = Message message_content in
  let hash = do_hash message in
  let signed_hash = account_sign_hash account hash in
  let* () = check (check_signed_hash signed_hash) "Bad signature" in
  return ()

let simple_signature_fail = test "simple signature fail" @@ fun () ->
  let account = Dev.create_account () in
  let message_content = Bytes.of_string "This is a message!" in
  let message = Message message_content in
  let hash = do_hash message in
  let signed_hash = account_sign_hash account hash in

  let* () =
    let (Hash h) = signed_hash.hash in
    let modified_hash = Hash (Dev.G2.(add one h)) in
    let modified_signed_hash = { signed_hash with hash = modified_hash } in
    check (not @@ check_signed_hash modified_signed_hash) "Unexpectedly good signature"
  in
  let* () =
    let (Public_key pk) = signed_hash.signer in
    let modified_signer = Public_key (Dev.G1.(add one pk)) in
    let modified_signed_hash = { signed_hash with signer = modified_signer } in
    check (not @@ check_signed_hash modified_signed_hash) "Unexpectedly good signature"
  in
  let* () =
    let (Signature s) = signed_hash.signature in
    let modified_signature = Signature (Dev.G2.(add one s)) in
    let modified_signed_hash = { signed_hash with signature = modified_signature } in
    check (not @@ check_signed_hash modified_signed_hash) "Unexpectedly good signature"
  in
  
  return ()

let aggregated_signature_check = test "aggregated signature check" @@ fun () ->
  let n = 10 in
  let ns = 1 -- n in
  let accounts = List.map (fun _ -> Dev.create_account ()) ns in
  let messages = List.map (fun k -> Message (Bytes.of_string ("toto" ^ (string_of_int k)))) ns in
  let hashes = List.map do_hash messages in
  let signed_hashes =
    match List.map2 ~when_different_lengths:() account_sign_hash accounts hashes with
    | Ok x -> x
    | _ -> assert false
  in
  let aggregated_signed_hashes = Dev.list_signed_hashes signed_hashes in
  let* () = check (check_signed_hashes aggregated_signed_hashes) "Bad aggregated signatures" in
  
  return ()

let aggregated_signature_fail = test "aggregated signature fail" @@ fun () ->
  let n = 10 in
  let accounts = Array.init n (fun _ -> Dev.create_account ()) in
  let messages = Array.init n (fun k -> Message (Bytes.of_string ("toto" ^ (string_of_int k)))) in
  let hashes = Array.map do_hash messages in
  let signed_hashes = Array.map2 account_sign_hash accounts hashes in

  let* () =
    iter (0 -- (n - 1)) @@ fun k ->
    let modified_signed_hashes = Array.copy signed_hashes in
    let modified_sign_hash =
      let current_sign_hash = signed_hashes.(k) in
      let modified_left_pair =
        let (Left_pair lp) = current_sign_hash.left_pair in
        Left_pair (Dev.Gt.(add one lp))
      in
      { current_sign_hash with left_pair = modified_left_pair }
    in
    modified_signed_hashes.(k) <- modified_sign_hash ;
    let modified_aggregated_signed_hashes = Dev.list_signed_hashes (Array.to_list modified_signed_hashes) in
    check (not @@ check_signed_hashes modified_aggregated_signed_hashes) "Unexpected good signature"
  in

  let* () =
    iter (0 -- (n - 1)) @@ fun k ->
    let modified_signed_hashes = Array.copy signed_hashes in
    let modified_sign_hash =
      let current_sign_hash = signed_hashes.(k) in
      let modified_right_pair =
        let (Right_pair rp) = current_sign_hash.right_pair in
        Right_pair (Dev.Gt.(add one rp))
      in
      { current_sign_hash with right_pair = modified_right_pair }
    in
    modified_signed_hashes.(k) <- modified_sign_hash ;
    let modified_aggregated_signed_hashes = Dev.list_signed_hashes (Array.to_list modified_signed_hashes) in
    check (not @@ check_signed_hashes modified_aggregated_signed_hashes) "Unexpected good signature"
  in

  return ()
  
let tests = [
  simple_signature_check ;
  simple_signature_fail ;
  aggregated_signature_check ;
  aggregated_signature_fail ;
]
