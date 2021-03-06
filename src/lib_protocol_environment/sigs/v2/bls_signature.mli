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

module type PARAM = sig
  val hash_function : bytes -> bytes
end

open Bls12_381

module Make : functor (P : PARAM) -> sig

  val generator : G1.t
  val g : G1.t
  type secret_key = Secret_key of Fr.t
  type public_key = Public_key of G1.t
  type account = {
    secret_key : secret_key;
    public_key : public_key;
  }
  type message = Message of bytes
  type hash = Hash of G2.t
  type signature = Signature of G2.t
  type left_pair = Left_pair of Fq12.t
  type right_pair = Right_pair of Fq12.t
  type signed_hash = {
    signer : public_key;
    hash : hash;
    signature : signature;
  }
  type aggregated_signed_hashes = {
    aggregated_signature : signature;
    identified_hashes : (public_key * hash) list;
  }
  val secret_to_public : secret_key -> public_key
  val hash_aux : current:bytes -> G2.t
  val do_hash : message -> hash
  val right_pairing : public_key -> hash -> right_pair
  val left_pairing : signature -> left_pair
  val compare_r_pairing : right_pair -> right_pair -> bool
  val compare_lr_pairing : left_pair -> right_pair -> bool
  val add_signature : signature -> signature -> signature
  val compare_signature : signature -> signature -> bool
  val add_left_pairing : left_pair -> left_pair -> left_pair
  val compare_left_pairing : left_pair -> left_pair -> bool
  val check_signature : public_key -> hash -> signature -> bool
  val sign_hash : secret_key -> hash -> signature
  val account_sign_hash : account -> hash -> signed_hash
  val check_signed_hash : signed_hash -> bool
  val check_signed_hashes : aggregated_signed_hashes -> bool
  val single_signed_hashes : signed_hash -> aggregated_signed_hashes
  val cons_signed_hashes :
    signed_hash -> aggregated_signed_hashes -> aggregated_signed_hashes
  module Dev : sig
    val create_account : ?seed:Fr.t -> unit -> account
    module Fr = Fr
    module Fq12 = Fq12
    module G1 = G1
    module G2 = G2
    module Gt = Gt
    val miller_loop : (G1.t * G2.t) list -> Fq12.t
    val final_exponentiation_opt : Fq12.t -> Fq12.t option
    val pairing : G1.t -> G2.t -> Fq12.t
    val g : G1.t
    val list_signed_hashes : signed_hash list -> aggregated_signed_hashes
  end

end

