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

module BLS_SHA = Bls_signature.Make(struct
    let hash_function bytes = Blake2B.(to_bytes @@ hash_bytes ?key:None [ bytes ])
  end)

open BLS_SHA

let test msg f = Alcotest.test_case msg `Quick f

let check condition msg =
  if condition then () else failwith msg

let iter x f = List.iter f x
let rec (--) a b = if a < b then a :: ((a + 1) -- b) else [ a ]

let simple_signature_check = test "simple signature check" @@ fun () ->
  let account = Dev.create_account () in
  let message_content = Bytes.of_string "This is a message!" in
  let message = Message message_content in
  let hash = do_hash message in
  let signed_hash = account_sign_hash account hash in
  let () = check (check_signed_hash signed_hash) "Bad signature" in
  ()

let simple_signature_fail = test "simple signature fail" @@ fun () ->
  let account = Dev.create_account () in
  let message_content = Bytes.of_string "This is a message!" in
  let message = Message message_content in
  let hash = do_hash message in
  let signed_hash = account_sign_hash account hash in

  let () =
    let (Hash h) = signed_hash.hash in
    let modified_hash = Hash (Dev.G2.(add one h)) in
    let modified_signed_hash = { signed_hash with hash = modified_hash } in
    check (not @@ check_signed_hash modified_signed_hash) "Unexpectedly good signature"
  in
  let () =
    let (Public_key pk) = signed_hash.signer in
    let modified_signer = Public_key (Dev.G1.(add one pk)) in
    let modified_signed_hash = { signed_hash with signer = modified_signer } in
    check (not @@ check_signed_hash modified_signed_hash) "Unexpectedly good signature"
  in
  let () =
    let (Signature s) = signed_hash.signature in
    let modified_signature = Signature (Dev.G2.(add one s)) in
    let modified_signed_hash = { signed_hash with signature = modified_signature } in
    check (not @@ check_signed_hash modified_signed_hash) "Unexpectedly good signature"
  in
  
  ()

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
  let () = check (check_signed_hashes aggregated_signed_hashes) "Bad aggregated signatures" in
  
  ()

let linear_pairing_ouch = test "linear pairing OUCH" @@ fun () ->
  let a = Dev.create_account () in
  let b = Dev.create_account () in
  let msg_a = Message (Bytes.of_string "foo") in
  let msg_b = Message (Bytes.of_string "bar") in
  let hash_a = do_hash msg_a in
  let hash_b = do_hash msg_b in
  let sg_a =
    let (Secret_key sk_a) = a.secret_key in
    let (Hash h_a) = hash_a in
    Dev.G2.mul h_a sk_a
  in
  let sg_b =
    let (Secret_key sk_b) = b.secret_key in
    let (Hash h_b) = hash_b in
    Dev.G2.mul h_b sk_b
  in
  let sgs = Dev.G2.add sg_a sg_b in
  let p_a = Dev.pairing Dev.g sg_a in
  let p_b = Dev.pairing Dev.g sg_b in
  let p = Dev.pairing Dev.g sgs in
  let p' = Dev.Gt.mul p_a p_b in
  let sh_a =
    let (Public_key pk_a) = a.public_key in
    let (Hash h_a) = hash_a in
    Dev.pairing pk_a h_a
  in
  let sh_b =
    let (Public_key pk_b) = b.public_key in
    let (Hash h_b) = hash_b in
    Dev.pairing pk_b h_b
  in
  check (Dev.Gt.eq p p') "Pairings are not equal" ;
  check (Dev.Gt.eq p (Dev.Gt.mul sh_a sh_b)) "Pairings are not equal" ;
  ()
  
let aggregated_signature_fail = test "aggregated signature fail" @@ fun () ->
  let n = 10 in
  let accounts = Array.init n (fun _ -> Dev.create_account ()) in
  let messages = Array.init n (fun k -> Message (Bytes.of_string ("toto" ^ (string_of_int k)))) in
  let hashes = Array.map do_hash messages in
  let signed_hashes = Array.map2 account_sign_hash accounts hashes in

  let () =
    let aggregated_signed_hashes = Dev.list_signed_hashes (Array.to_list signed_hashes) in
    let modified_signature =
      let (Signature s) = aggregated_signed_hashes.aggregated_signature in
      Signature (Dev.G2.(add one s))
    in
    let modified_aggregated_signed_hashes =
    { aggregated_signed_hashes with aggregated_signature = modified_signature } in
    check (not @@ check_signed_hashes modified_aggregated_signed_hashes) "Unexpected good signature"
  in

  let () =
    iter (0 -- (n - 1)) @@ fun k ->
    (
      let modified_signed_hashes = Array.copy signed_hashes in
      let modified_sign_hash =
        let current_sign_hash = signed_hashes.(k) in
        let modified_signature =
          let (Signature s) = current_sign_hash.signature in
          Signature (Dev.G2.(add one s))
        in
        { current_sign_hash with signature = modified_signature }
      in
      modified_signed_hashes.(k) <- modified_sign_hash ;
      let modified_aggregated_signed_hashes = Dev.list_signed_hashes (Array.to_list modified_signed_hashes) in
      check (not @@ check_signed_hashes modified_aggregated_signed_hashes) "Unexpected good signature"
    ) ;
    (
      let modified_signed_hashes = Array.copy signed_hashes in
      let modified_sign_hash =
        let current_sign_hash = signed_hashes.(k) in
        let modified_hash =
          let (Hash s) = current_sign_hash.hash in
          Hash (Dev.G2.(add one s))
        in
        { current_sign_hash with hash = modified_hash }
      in
      modified_signed_hashes.(k) <- modified_sign_hash ;
      let modified_aggregated_signed_hashes = Dev.list_signed_hashes (Array.to_list modified_signed_hashes) in
      check (not @@ check_signed_hashes modified_aggregated_signed_hashes) "Unexpected good signature"
    ) ;
    (
      let modified_signed_hashes = Array.copy signed_hashes in
      let modified_sign_hash =
        let current_sign_hash = signed_hashes.(k) in
        let modified_signer =
          let (Public_key s) = current_sign_hash.signer in
          Public_key (Dev.G1.(add one s))
        in
        { current_sign_hash with signer = modified_signer }
      in
      modified_signed_hashes.(k) <- modified_sign_hash ;
      let modified_aggregated_signed_hashes = Dev.list_signed_hashes (Array.to_list modified_signed_hashes) in
      check (not @@ check_signed_hashes modified_aggregated_signed_hashes) "Unexpected good signature"
    ) ;
    ()
  in

  ()
  
let tests = [
  simple_signature_check ;
  simple_signature_fail ;
  linear_pairing_ouch ;
  aggregated_signature_check ;
  aggregated_signature_fail ;
]

let () = Alcotest.run "tezos-crypto" [("bls-signature", tests)]
