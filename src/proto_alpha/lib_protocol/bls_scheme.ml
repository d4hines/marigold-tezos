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

let (let*) x f = x >>=? f
let (let=) x f = x >>= f

open Bls12_381

let generator = G1.one
let g = generator

type secret_key = Secret_key of Fr.t
type public_key = Public_key of G1.t

type account = {
  secret_key : secret_key ;
  public_key : public_key ;
}

type message = Message of bytes
type hash = Hash of G2.t
type signature = Signature of G2.t

(* E(G , S) = E(P , H) *)
type left_pair = Left_pair of Gt.t
type right_pair = Right_pair of Gt.t

type signed_hash = {
  signer : public_key ;
  hash : hash ;
  signature : signature ;
  right_pair : right_pair ;
}

type signed_hashes = {
  signature : signature ;
  identified_hashes : (public_key * hash) list ;
  right_pair : right_pair ;
}

let secret_to_public (Secret_key sk) = (Public_key (G1.mul g sk))

let hash_function = Raw_hashes.sha256

let hash_aux ~(current : bytes) =
  let z = Z.of_bits (Bytes.to_string current) in
  let r = Z.rem z Fr.order in
  G2.mul G2.one (Fr.of_z r)

let do_hash (Message msg) =
  let content = hash_aux (hash_function msg) in
  Hash content

let check_signature (Public_key pk) (Hash h) (Signature s) =
  let left = pairing pk h in
  let right = pairing generator s in
  Fq12.eq left right

let sign_hash (Secret_key sk) (Hash h) = Signature (G2.mul h sk)

let right_pairing (Public_key pk) (Hash h) = Right_pair (pairing pk h)
let left_pairing (Signature s) = Left_pair (pairing generator s)

let compare_pairing (Left_pair l) (Right_pair r) = Gt.eq l r

let account_sign_hash { secret_key ; public_key = signer } hash =
  { signer ; hash ; signature = sign_hash secret_key hash ; right_pair = right_pairing signer hash }

let check_signed_hash { signer ; hash ; signature ; right_pair = _ } = check_signature signer hash signature

let check_signed_hashes { signature ; identified_hashes = _ ; right_pair } =
  let left = left_pairing signature in
  compare_pairing left right_pair

let single_signed_hashes { signer ; hash ; signature ; right_pair } = { signature ; right_pair ; identified_hashes = [ (signer , hash) ] }

let cons_signed_hashes (hd : signed_hash) (tl : signed_hashes) =
  let { signer ; hash ; signature = Signature s_hd ; right_pair = Right_pair p_hd } = hd in
  let { identified_hashes ; signature = Signature s_tl ; right_pair = Right_pair p_tl } = tl in
  { signature = Signature (G2.add s_hd s_tl) ; right_pair = Right_pair (Gt.add p_hd p_tl) ; identified_hashes = (signer , hash) :: identified_hashes }


(* Only used outside of the protocol. For tests, debugging, etc. *)
module Dev = struct

  (* Not random, at all *)
  let create_account =
    let seed = ref Fr.one in
    let next () = seed := Fr.(add one !seed) ; ! seed in
    fun ?(seed = next()) () ->
      let secret_key = Secret_key seed in
      let public_key = secret_to_public secret_key in
      { secret_key ; public_key }

  module G1 = G1
  module G2 = G2
  module Fr = Fr
  
end
