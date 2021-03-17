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

open BLS12_381

module type PARAM = sig
  val hash_function : bytes -> bytes
end

module type SUITE = sig
  val m : int
  val l : int
  val r_in_bytes : int
end

module Suite : SUITE = struct
  let m = 2
  let l = 64
  let r_in_bytes = 64
end

module Make (P : PARAM) (S : SUITE) = struct
  open P
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

(*
  E(g , signature) = E(signer , hash)
  - Elements of Gt (signatures) are in `Left_pair`
  - Elements of Gt (identified hashes) are in `Rigth_pair`
*)
  type left_pair = Left_pair of Gt.t
  type right_pair = Right_pair of Gt.t

  type signed_hash = {
    signer : public_key ;
    hash : hash ;
    signature : signature ;
  }

  type aggregated_signed_hashes = {
    aggregated_signature : signature ;
    identified_hashes : (public_key * hash) list ;
  }

  let secret_to_public (Secret_key sk) = (Public_key (G1.mul g sk))

  let hash_aux ~(current : bytes) =
    let z = Z.of_bits (Bytes.to_string current) in
    let r = Z.rem z Fr.order in
    G2.mul G2.one (Fr.of_z r)

  let do_hash (Message msg) =
    let content = hash_aux ~current:(hash_function msg) in
    Hash content

  (*hash_to_curve(msg)*)

  (*Input: msg, an arbitrary-length byte string.*)
  (*Output: P, a point in G.*)

  (*Steps:*)
  (*1. u = hash_to_field(msg, 2)*)
  (*2. Q0 = map_to_curve(u[0])*)
  (*3. Q1 = map_to_curve(u[1])*)
  (*4. R = Q0 + Q1              # Point addition*)
  (*5. P = clear_cofactor(R)*)
  (*6. return P

  let hash_to_curve (Message _msg) = G2.one
*)
  (*
  hash_to_field(msg, count)
  Parameters:
  - DST, a domain separation tag (see discussion above).
  - F, a finite field of characteristic p and order q = p^m.
  - p, the characteristic of F (see immediately above).
  - m, the extension degree of F, m >= 1 (see immediately above).
  - L = ceil((ceil(log2(p)) + k) / 8), where k is the security
    parameter of the suite (e.g., k = 128).
  - expand_message, a function that expands a byte string and
    domain separation tag into a uniformly random byte string
    (see discussion above).

  Inputs:
  - msg, a byte string containing the message to hash.
  - count, the number of elements of F to output.

  Outputs:
  - (u_0, ..., u_(count - 1)), a list of field elements.

  Steps:
  1. len_in_bytes = count * m * L
  2. uniform_bytes = expand_message(msg, DST, len_in_bytes)
  3. for i in (0, ..., count - 1):
  4.   for j in (0, ..., m - 1):
  5.     elm_offset = L * (j + i * m)
  6.     tv = substr(uniform_bytes, elm_offset, L)
  7.     e_j = OS2IP(tv) mod p
  8.   u_i = (e_0, ..., e_(m - 1))
  9. return (u_0, ..., u_(count - 1))
  let hash_to_field (Message _msg) _count =
    let _len_in_bytes = _count * S.m * S.l in
    G1.one
  *)

  (*
   * I2OSP converts a nonnegative integer to an octet string of a
   specified length
   * https://tools.ietf.org/html/rfc8017#section-4.1
   * *)
  let i2osp x xlen =
    if Z.(of_int x >= pow (of_int 256) xlen) then
      failwith "integer too large"
    else
      let rec conv t b =
        let d = Int.div t 256 in
        let r = Int.rem t 256 in
        let new_b = Bytes.create 1 in
        if d <> 0 then
          let () = Bytes.set_uint8 new_b 0 d in
          conv r @@ Bytes.cat b new_b
        else
          let () = Bytes.set_uint8 new_b 0 r in
          Bytes.cat b new_b
      in
      let t = conv x Bytes.empty in
      let l = xlen - 1 in
      if Z.(of_int x < pow (of_int 256) l) then
        let c = xlen - Bytes.length t in
        Bytes.cat (Bytes.make c '\x00') t
      else
        t

  let strxor xs ys =
    let lx = Bytes.length xs in
    let ly = Bytes.length ys in
    let (major, other) =
      if lx >= ly then (xs, Bytes.extend ys (lx - ly) 0)
      else (ys, Bytes.extend xs (ly - lx) 0)
    in
    let op_idxed i =
      let mc = Char.code (Bytes.get major i) in
      let oc = Char.code (Bytes.get other i) in
      Bytes.make 1 (Char.chr (Int.logxor mc oc))
    in
    let step x hs = Bytes.cat (op_idxed x) hs in
    List.init ~when_negative_length:() (Bytes.length major) (fun i -> i)
    |> function
    | Error () -> failwith "length can never be less zero"
    | Ok ids -> List.fold_right step ids Bytes.empty

  (*
   * https://www.ietf.org/archive/id/draft-irtf-cfrg-hash-to-curve-10.html#name-expand_message_xmd-2
   * *)
  let expand_message_xmd (Message msg) dst len_in_bytes =
    let b_in_bytes = 256 / 8 in
    let ell = Float.to_int @@ ceil (Float.of_int len_in_bytes /. Float.of_int b_in_bytes) in
    if ell > 255 then
      failwith "invalid len in bytes for expand_message_xmd"
    else
      let (>@<) x y = Bytes.cat x y in
      let dst_prim = dst >@< i2osp (Bytes.length dst) 1 in
      let z_pad = Bytes.make S.r_in_bytes '\x00' in
      let l_i_b = i2osp len_in_bytes 2 in
      let msg_prime = z_pad >@< msg >@< l_i_b >@< i2osp 0 1 >@< dst_prim in
      let b_0 = Hacl.Hash.SHA256.digest msg_prime in
      let b_1 = Hacl.Hash.SHA256.digest (b_0  >@< i2osp 1 1 >@< dst_prim) in
      let step (y, b) x =
        match x with
        | 1 ->
          (b, b)
        | i ->
            let b_i = Hacl.Hash.SHA256.digest (strxor b_0 b >@< i2osp i 1 >@< dst_prim) in
              (y >@< b_i, b_i)
      in
      let uniform_bytes =
      List.init ~when_negative_length:() ell (fun i -> i + 1)
      |> function
      | Error () -> failwith "length can never less zero"
      | Ok ids ->
        fst @@ List.fold_left step (Bytes.empty, b_1) ids in
      Bytes.sub uniform_bytes 0 len_in_bytes

  let right_pairing (Public_key pk) (Hash h) = Right_pair (pairing pk h)
  let left_pairing (Signature s) = Left_pair (pairing generator s)

  let compare_r_pairing (Right_pair l) (Right_pair r) = Fq12.eq l r
  let compare_lr_pairing (Left_pair l) (Right_pair r) = Fq12.eq l r

  let add_signature (Signature a) (Signature b) = Signature (G2.add a b)
  let compare_signature (Signature a) (Signature b) = G2.eq a b

  let add_left_pairing (Left_pair a) (Left_pair b) = Left_pair (Gt.add a b)
  let compare_left_pairing (Left_pair a) (Left_pair b) = Gt.eq a b

  let check_signature pk hash signature =
    let left = left_pairing signature in
    let right = right_pairing pk hash in
    compare_lr_pairing left right

  let sign_hash (Secret_key sk) (Hash h) = Signature (G2.mul h sk)

  let account_sign_hash { secret_key ; public_key = signer } hash =
    let signature = sign_hash secret_key hash in
    { signer ; hash ; signature }

  let check_signed_hash { signer ; hash ; signature } = check_signature signer hash signature

  let check_signed_hashes { identified_hashes ; aggregated_signature } =
    let left_pair = left_pairing aggregated_signature in
    (* let right_pair = miller_loop (List.map (fun (Public_key pk , Hash h) -> pk , h) identified_hashes) in *)
    let right_pair =
      let right_pairs =
        List.map (fun (Right_pair rp) -> rp) @@
        List.map (fun (pk , h) -> right_pairing pk h) @@
        identified_hashes in
      List.fold_left Gt.mul Gt.one right_pairs
    in
    compare_lr_pairing left_pair (Right_pair right_pair)

  let single_signed_hashes { signer ; hash ; signature } =
    { aggregated_signature = signature ; identified_hashes = [ (signer , hash) ] }

(*
  The right pair of the whole list is computed incrementally.
*)
  let cons_signed_hashes (hd : signed_hash) (tl : aggregated_signed_hashes) =
    let { signer ; hash ; signature } = hd in
    let { identified_hashes ; aggregated_signature } = tl in
    {
      aggregated_signature = add_signature signature aggregated_signature ;
      identified_hashes = (signer , hash) :: identified_hashes ;
    }

  (* Only used outside of the protocol. For tests, debugging, etc. *)
  module Dev = struct
    (* Not random, at all *)
    let create_account =
      let seed = ref @@ Fr.of_z @@ Z.of_int 42 in
      let next () = seed := Fr.(add one !seed) ; ! seed in
      fun ?(seed = next()) () ->
        let secret_key = Secret_key seed in
        let public_key = secret_to_public secret_key in
        { secret_key ; public_key }

    include BLS12_381

    let g = g
    
    let rec list_signed_hashes (lst : signed_hash list) =
      match lst with
      | [] -> raise (Failure "empty list of signed hashes")
      | [ single ] -> single_signed_hashes single
      | hd :: tl -> cons_signed_hashes hd (list_signed_hashes tl)

  end
end

