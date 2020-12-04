(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
open Script_typed_ir
module Interp_costs = Michelson_v1_gas.Cost_of.Interpreter

let cost_of_instr : type b a. (b, a) descr -> b -> Gas.cost =
 fun descr stack ->
  match (descr.instr, stack) with
  | (Drop, _) ->
      Interp_costs.drop
  | (Dup, _) ->
      Interp_costs.dup
  | (Swap, _) ->
      Interp_costs.swap
  | (Const _, _) ->
      Interp_costs.push
  | (Cons_some, _) ->
      Interp_costs.cons_some
  | (Cons_none _, _) ->
      Interp_costs.cons_none
  | (If_none _, _) ->
      Interp_costs.if_none
  | (Cons_pair, _) ->
      Interp_costs.cons_pair
  | (Unpair, _) ->
      Interp_costs.unpair
  | (Car, _) ->
      Interp_costs.car
  | (Cdr, _) ->
      Interp_costs.cdr
  | (Cons_left, _) ->
      Interp_costs.cons_left
  | (Cons_right, _) ->
      Interp_costs.cons_right
  | (If_left _, _) ->
      Interp_costs.if_left
  | (Cons_list, _) ->
      Interp_costs.cons_list
  | (Nil, _) ->
      Interp_costs.nil
  | (If_cons _, _) ->
      Interp_costs.if_cons
  | (List_map _, (list, _)) ->
      Interp_costs.list_map list
  | (List_size, _) ->
      Interp_costs.list_size
  | (List_iter _, (l, _)) ->
      Interp_costs.list_iter l
  | (Empty_set _, _) ->
      Interp_costs.empty_set
  | (Set_iter _, (set, _)) ->
      Interp_costs.set_iter set
  | (Set_mem, (v, (set, _))) ->
      Interp_costs.set_mem v set
  | (Set_update, (v, (_, (set, _)))) ->
      Interp_costs.set_update v set
  | (Set_size, _) ->
      Interp_costs.set_size
  | (Empty_map _, _) ->
      Interp_costs.empty_map
  | (Map_map _, (map, _)) ->
      Interp_costs.map_map map
  | (Map_iter _, (map, _)) ->
      Interp_costs.map_iter map
  | (Map_mem, (v, (map, _rest))) ->
      Interp_costs.map_mem v map
  | (Map_get, (v, (map, _rest))) ->
      Interp_costs.map_get v map
  | (Map_update, (k, (_, (map, _)))) ->
      Interp_costs.map_update k map
  | (Map_get_and_update, (k, (_, (map, _)))) ->
      Interp_costs.map_get_and_update k map
  | (Map_size, _) ->
      Interp_costs.map_size
  | (Empty_big_map _, _) ->
      Interp_costs.empty_map
  | (Big_map_mem, (key, (map, _))) ->
      Interp_costs.map_mem key map.diff
  | (Big_map_get, (key, (map, _))) ->
      Interp_costs.map_get key map.diff
  | (Big_map_update, (key, (_, (map, _)))) ->
      Interp_costs.map_update key map.diff
  | (Big_map_get_and_update, (key, (_, (map, _)))) ->
      Interp_costs.map_get_and_update key map.diff
  | (Add_seconds_to_timestamp, (n, (t, _))) ->
      Interp_costs.add_seconds_timestamp n t
  | (Add_timestamp_to_seconds, (t, (n, _))) ->
      Interp_costs.add_seconds_timestamp n t
  | (Sub_timestamp_seconds, (t, (n, _))) ->
      Interp_costs.sub_seconds_timestamp n t
  | (Diff_timestamps, (t1, (t2, _))) ->
      Interp_costs.diff_timestamps t1 t2
  | (Concat_string_pair, (x, (y, _))) ->
      Interp_costs.concat_string_pair x y
  | (Concat_string, (ss, _)) ->
      Interp_costs.concat_string_precheck ss
  | (Slice_string, (_offset, (_length, (s, _)))) ->
      Interp_costs.slice_string s
  | (String_size, _) ->
      Interp_costs.string_size
  | (Concat_bytes_pair, (x, (y, _))) ->
      Interp_costs.concat_bytes_pair x y
  | (Concat_bytes, (ss, _)) ->
      Interp_costs.concat_string_precheck ss
  | (Slice_bytes, (_offset, (_length, (s, _)))) ->
      Interp_costs.slice_bytes s
  | (Bytes_size, _) ->
      Interp_costs.bytes_size
  | (Add_tez, _) ->
      Interp_costs.add_tez
  | (Sub_tez, _) ->
      Interp_costs.sub_tez
  | (Mul_teznat, (_, (n, _))) ->
      Interp_costs.mul_teznat n
  | (Mul_nattez, (n, (_, _))) ->
      Interp_costs.mul_teznat n
  | (Or, _) ->
      Interp_costs.bool_or
  | (And, _) ->
      Interp_costs.bool_and
  | (Xor, _) ->
      Interp_costs.bool_xor
  | (Not, _) ->
      Interp_costs.bool_not
  | (Is_nat, _) ->
      Interp_costs.is_nat
  | (Abs_int, (x, _)) ->
      Interp_costs.abs_int x
  | (Int_nat, _) ->
      Interp_costs.int_nat
  | (Neg_int, (x, _)) ->
      Interp_costs.neg_int x
  | (Neg_nat, (x, _)) ->
      Interp_costs.neg_nat x
  | (Add_intint, (x, (y, _))) ->
      Interp_costs.add_bigint x y
  | (Add_intnat, (x, (y, _))) ->
      Interp_costs.add_bigint x y
  | (Add_natint, (x, (y, _))) ->
      Interp_costs.add_bigint x y
  | (Add_natnat, (x, (y, _))) ->
      Interp_costs.add_bigint x y
  | (Sub_int, (x, (y, _))) ->
      Interp_costs.sub_bigint x y
  | (Mul_intint, (x, (y, _))) ->
      Interp_costs.mul_bigint x y
  | (Mul_intnat, (x, (y, _))) ->
      Interp_costs.mul_bigint x y
  | (Mul_natint, (x, (y, _))) ->
      Interp_costs.mul_bigint x y
  | (Mul_natnat, (x, (y, _))) ->
      Interp_costs.mul_bigint x y
  | (Ediv_teznat, (x, (y, _))) ->
      Interp_costs.ediv_teznat x y
  | (Ediv_tez, _) ->
      Interp_costs.ediv_tez
  | (Ediv_intint, (x, (y, _))) ->
      Interp_costs.ediv_bigint x y
  | (Ediv_intnat, (x, (y, _))) ->
      Interp_costs.ediv_bigint x y
  | (Ediv_natint, (x, (y, _))) ->
      Interp_costs.ediv_bigint x y
  | (Ediv_natnat, (x, (y, _))) ->
      Interp_costs.ediv_bigint x y
  | (Lsl_nat, (x, _)) ->
      Interp_costs.lsl_nat x
  | (Lsr_nat, (x, _)) ->
      Interp_costs.lsr_nat x
  | (Or_nat, (x, (y, _))) ->
      Interp_costs.or_nat x y
  | (And_nat, (x, (y, _))) ->
      Interp_costs.and_nat x y
  | (And_int_nat, (x, (y, _))) ->
      Interp_costs.and_nat x y
  | (Xor_nat, (x, (y, _))) ->
      Interp_costs.xor_nat x y
  | (Not_int, (x, _)) ->
      Interp_costs.not_nat x
  | (Not_nat, (x, _)) ->
      Interp_costs.not_nat x
  | (Seq _, _) ->
      Interp_costs.seq
  | (If _, _) ->
      Interp_costs.if_
  | (Loop _, _) ->
      Interp_costs.loop
  | (Loop_left _, _) ->
      Interp_costs.loop_left
  | (Dip _, _) ->
      Interp_costs.dip
  | (Exec, _) ->
      Interp_costs.exec
  | (Apply _, _) ->
      Interp_costs.apply
  | (Lambda _, _) ->
      Interp_costs.push
  | (Failwith _, _) ->
      Gas.free
  | (Nop, _) ->
      Interp_costs.nop
  | (Compare ty, (a, (b, _))) ->
      Interp_costs.compare ty a b
  | (Eq, _) ->
      Interp_costs.neq
  | (Neq, _) ->
      Interp_costs.neq
  | (Lt, _) ->
      Interp_costs.neq
  | (Le, _) ->
      Interp_costs.neq
  | (Gt, _) ->
      Interp_costs.neq
  | (Ge, _) ->
      Interp_costs.neq
  | (Pack _, _) ->
      Gas.free
  | (Unpack _, _) ->
      Gas.free
  | (Address, _) ->
      Interp_costs.address
  | (Contract _, _) ->
      Interp_costs.contract
  | (Transfer_tokens, _) ->
      Interp_costs.transfer_tokens
  | (Implicit_account, _) ->
      Interp_costs.implicit_account
  | (Set_delegate, _) ->
      Interp_costs.set_delegate
  | (Balance, _) ->
      Interp_costs.balance
  | (Level, _) ->
      Interp_costs.level
  | (Now, _) ->
      Interp_costs.now
  | (Check_signature, (key, (_, (message, _)))) ->
      Interp_costs.check_signature key message
  | (Hash_key, (pk, _)) ->
      Interp_costs.hash_key pk
  | (Blake2b, (bytes, _)) ->
      Interp_costs.blake2b bytes
  | (Sha256, (bytes, _)) ->
      Interp_costs.sha256 bytes
  | (Sha512, (bytes, _)) ->
      Interp_costs.sha512 bytes
  | (Source, _) ->
      Interp_costs.source
  | (Sender, _) ->
      Interp_costs.source
  | (Self _, _) ->
      Interp_costs.self
  | (Self_address, _) ->
      Interp_costs.self
  | (Amount, _) ->
      Interp_costs.amount
  | (Dig (n, _), _) ->
      Interp_costs.dign n
  | (Dug (n, _), _) ->
      Interp_costs.dugn n
  | (Dipn (n, _, _), _) ->
      Interp_costs.dipn n
  | (Dropn (n, _), _) ->
      Interp_costs.dropn n
  | (ChainId, _) ->
      Interp_costs.chain_id
  | (Create_contract _, _) ->
      Interp_costs.create_contract
  | (Never, (_, _)) ->
      .
  | (Voting_power, _) ->
      Interp_costs.voting_power
  | (Total_voting_power, _) ->
      Interp_costs.total_voting_power
  | (Keccak, (bytes, _)) ->
      Interp_costs.keccak bytes
  | (Sha3, (bytes, _)) ->
      Interp_costs.sha3 bytes
  | (Add_bls12_381_g1, _) ->
      Interp_costs.add_bls12_381_g1
  | (Add_bls12_381_g2, _) ->
      Interp_costs.add_bls12_381_g2
  | (Add_bls12_381_fr, _) ->
      Interp_costs.add_bls12_381_fr
  | (Mul_bls12_381_g1, _) ->
      Interp_costs.mul_bls12_381_g1
  | (Mul_bls12_381_g2, _) ->
      Interp_costs.mul_bls12_381_g2
  | (Mul_bls12_381_fr, _) ->
      Interp_costs.mul_bls12_381_fr
  | (Mul_bls12_381_fr_z, _) ->
      Interp_costs.mul_bls12_381_fr_z
  | (Mul_bls12_381_z_fr, _) ->
      Interp_costs.mul_bls12_381_fr_z
  | (Int_bls12_381_fr, _) ->
      Interp_costs.int_bls12_381_fr
  | (Neg_bls12_381_g1, _) ->
      Interp_costs.neg_bls12_381_g1
  | (Neg_bls12_381_g2, _) ->
      Interp_costs.neg_bls12_381_g2
  | (Neg_bls12_381_fr, _) ->
      Interp_costs.neg_bls12_381_fr
  | (Pairing_check_bls12_381, (pairs, _)) ->
      Interp_costs.pairing_check_bls12_381 pairs
  | (Comb (n, _), _) ->
      Interp_costs.comb n
  | (Uncomb (n, _), _) ->
      Interp_costs.uncomb n
  | (Comb_get (n, _), _) ->
      Interp_costs.comb_get n
  | (Comb_set (n, _), _) ->
      Interp_costs.comb_set n
  | (Dup_n (n, _), _) ->
      Interp_costs.dupn n
  | (Sapling_empty_state _, _) ->
      Interp_costs.sapling_empty_state
  | (Sapling_verify_update, (tx, _)) ->
      let inputs = List.length tx.inputs in
      let outputs = List.length tx.outputs in
      Interp_costs.sapling_verify_update ~inputs ~outputs
  | (Ticket, _) ->
      Interp_costs.ticket
  | (Read_ticket, _) ->
      Interp_costs.read_ticket
  | (Split_ticket, (ticket, ((amount_a, amount_b), _))) ->
      Interp_costs.split_ticket ticket.amount amount_a amount_b
  | (Join_tickets ty, ((ticket_a, ticket_b), _)) ->
      Interp_costs.join_tickets ty ticket_a ticket_b

let unpack_failed = Michelson_v1_gas.Cost_of.Interpreter.unpack_failed

let concat_string = Michelson_v1_gas.Cost_of.Interpreter.concat_string

include Script_interpreter_functor.Make (struct
  include Alpha_context
  module Pervasives = Pervasives
  module Z = Z
  module Signature = Signature
  module Bls12_381 = Bls12_381
  module Chain_id = Chain_id
  module Compare = Compare
  module Data_encoding = Data_encoding
  module Bytes = Bytes
  module TzEndian = TzEndian
  module String = String
  module Raw_hashes = Raw_hashes
  module Option = Option
  module Micheline = Micheline
  module Lwt = Lwt
  module List = List
  module Format = Format
  module Raw_context = Alpha_context
  module Error_monad = Error_monad

  module Michelson_v1_primitives = struct
    include Michelson_v1_primitives

    let i_push = I_PUSH

    let i_pair = I_PAIR

    let k_parameter = K_parameter

    let k_storage = K_storage

    let k_code = K_code
  end

  module Alpha_context = Alpha_context
  module Operation = Alpha_context
  module Script_ir_translator = Script_ir_translator
  module Script_typed_ir = Script_typed_ir

  module Script_interpreter_cost = struct
    let cost_of_instr = cost_of_instr

    let unpack_failed = unpack_failed

    let concat_string = concat_string
  end
end)
