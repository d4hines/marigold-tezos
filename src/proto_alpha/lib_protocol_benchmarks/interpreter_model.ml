(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <nomadic@tezcore.com>                    *)
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

(* ------------------------------------------------------------------------- *)

let trace_error expected given =
  let open Interpreter_workload in
  let exp = string_of_instruction_name expected in
  let given = string_of_instruction_name given in
  let msg =
    Format.asprintf
      "Interpreter_model: trace error, expected %s, given %s"
      exp
      given
  in
  Stdlib.failwith msg

let arity_error instr expected given =
  let open Interpreter_workload in
  let s = string_of_instruction_name instr in
  let msg =
    Format.asprintf
      "Interpreter_model: arity error (%s), expected %d, given %d"
      s
      expected
      given
  in
  Stdlib.failwith msg

type model_error =
  | Bad_arity of Interpreter_workload.instruction_name * int * int
  | Unhandled_instruction of string

exception Model_error of model_error

(* ------------------------------------------------------------------------- *)

let model_0 instr model =
  let open Interpreter_workload in
  Model.make
    ~conv:(function
      | {instr_name; args = []} ->
          if instr_name = instr then () else trace_error instr instr_name
      | {args; _} ->
          arity_error instr 0 (List.length args))
    ~model

let model_1 instr model =
  let open Interpreter_workload in
  Model.make
    ~conv:(function
      | {instr_name; args = [{name = _; arg}]} ->
          if instr_name = instr then (arg, ())
          else trace_error instr instr_name
      | {args; _} ->
          arity_error instr 1 (List.length args))
    ~model

let model_2 instr model =
  let open Interpreter_workload in
  Model.make
    ~conv:(function
      | {instr_name; args = [{name = _; arg = x}; {name = _; arg = y}]} ->
          if instr_name = instr then (x, (y, ()))
          else trace_error instr instr_name
      | {args; _} ->
          arity_error instr 2 (List.length args))
    ~model

let model_3 instr model =
  let open Interpreter_workload in
  Model.make
    ~conv:(function
      | { instr_name;
          args = [{name = _; arg = x}; {name = _; arg = y}; {name = _; arg = z}]
        } ->
          if instr_name = instr then (x, (y, (z, ())))
          else trace_error instr instr_name
      | {args; _} ->
          arity_error instr 3 (List.length args))
    ~model

let division_cost name =
  let const = Free_variable.of_string (Format.asprintf "%s_const" name) in
  let coeff = Free_variable.of_string (Format.asprintf "%s_coeff" name) in
  let module M = struct
    type arg_type = int * (int * unit)

    module Def (X : Costlang.S) = struct
      open X

      type model_type = size -> size -> size

      let arity = Model.arity_2

      let model =
        lam ~name:"size1"
        @@ fun size1 ->
        lam ~name:"size2"
        @@ fun size2 ->
        let_ ~name:"q" (size1 - size2)
        @@ fun q ->
        (free ~name:coeff * if_ (lt size2 size1) (q * size2) (int 0))
        + free ~name:const
    end
  end in
  (module M : Model.Model_impl with type arg_type = int * (int * unit))

let addlogadd name =
  let const = Free_variable.of_string (Format.asprintf "%s_const" name) in
  let coeff = Free_variable.of_string (Format.asprintf "%s_coeff" name) in
  let module M = struct
    type arg_type = int * (int * unit)

    module Def (X : Costlang.S) = struct
      open X

      type model_type = size -> size -> size

      let arity = Model.arity_2

      let model =
        lam ~name:"size1"
        @@ fun size1 ->
        lam ~name:"size2"
        @@ fun size2 ->
        let_ ~name:"a" (size1 + size2)
        @@ fun a ->
        (free ~name:coeff * (a * log2 (int 1 + a))) + free ~name:const
    end
  end in
  (module M : Model.Model_impl with type arg_type = int * (int * unit))

(* Some instructions are oveloaded (eg COMPARE). In order to generate distinct
   models at different types, we must specialize these models. The [specialization]
   parameter acts as a mangling scheme to produce distinct models. *)
let name_of_instr ?specialization instr =
  let spec = Option.fold ~none:"" ~some:(fun s -> "_" ^ s) specialization in
  Interpreter_workload.string_of_instruction_name instr ^ spec

let ir_model ?specialization instr =
  let open Interpreter_workload in
  let name = name_of_instr ?specialization instr in
  let const1_model =
    (* For constant-time instructions *)
    Model.unknown_const1
      ~const:(Free_variable.of_string (Format.asprintf "%s_const" name))
  in
  let affine_model =
    (* For instructions with cost function
       [\lambda size. const + coeff * size] *)
    Model.affine
      ~intercept:(Free_variable.of_string (Format.asprintf "%s_const" name))
      ~coeff:(Free_variable.of_string (Format.asprintf "%s_coeff" name))
  in
  let nlogm_model =
    (* For instructions with cost function
       [\lambda size1. \lambda size2. const + coeff * size1 log2(size2)] *)
    Model.nlogm
      ~intercept:(Free_variable.of_string (Format.asprintf "%s_const" name))
      ~coeff:(Free_variable.of_string (Format.asprintf "%s_coeff" name))
  in
  let concat_model =
    Model.bilinear_affine
      ~intercept:(Free_variable.of_string (Format.asprintf "%s_const" name))
      ~coeff1:(Free_variable.of_string (Format.asprintf "%s_total_bytes" name))
      ~coeff2:(Free_variable.of_string (Format.asprintf "%s_list_length" name))
  in
  let concat_pair_model =
    Model.linear_sum
      ~intercept:(Free_variable.of_string (Format.asprintf "%s_const" name))
      ~coeff:(Free_variable.of_string (Format.asprintf "%s_coeff" name))
  in
  let linear_max_model =
    (* For instructions with cost function
       [\lambda size1. \lambda size2. const + coeff * max(size1,size2)] *)
    Model.linear_max
      ~intercept:(Free_variable.of_string (Format.asprintf "%s_const" name))
      ~coeff:(Free_variable.of_string (Format.asprintf "%s_coeff" name))
  in
  let linear_min_model =
    (* For instructions with cost function
       [\lambda size1. \lambda size2. const + coeff * min(size1,size2)] *)
    Model.linear_min
      ~intercept:(Free_variable.of_string (Format.asprintf "%s_const" name))
      ~coeff:(Free_variable.of_string (Format.asprintf "%s_coeff" name))
  in
  let pack_model =
    Model.trilinear
      ~coeff1:
        (Free_variable.of_string (Format.asprintf "%s_micheline_nodes" name))
      ~coeff2:
        (Free_variable.of_string
           (Format.asprintf "%s_micheline_int_bytes" name))
      ~coeff3:
        (Free_variable.of_string
           (Format.asprintf "%s_micheline_string_bytes" name))
  in
  let split_ticket_model =
    let module M = struct
      type arg_type = int * (int * unit)

      module Def (X : Costlang.S) = struct
        open X

        type model_type = size -> size -> size

        let arity = Model.arity_2

        let model =
          lam ~name:"size1"
          @@ fun size1 ->
          lam ~name:"size2"
          @@ fun size2 ->
          free
            ~name:(Free_variable.of_string (Format.asprintf "%s_const" name))
          + free
              ~name:
                (Free_variable.of_string (Format.asprintf "%s_add_coeff" name))
            * max size1 size2
          + free
              ~name:
                (Free_variable.of_string (Format.asprintf "%s_cmp_coeff" name))
            * (size1 + size2)
      end
    end in
    (module M : Model.Model_impl with type arg_type = int * (int * unit))
  in
  let verify_update_model =
    Model.bilinear_affine
      ~intercept:(Free_variable.of_string (Format.asprintf "%s_const" name))
      ~coeff1:(Free_variable.of_string (Format.asprintf "%s_inputs" name))
      ~coeff2:(Free_variable.of_string (Format.asprintf "%s_ouputs" name))
  in
  match instr with
  | N_Drop
  | N_Dup
  | N_Swap
  | N_Const
  | N_Cons_pair
  | N_Car
  | N_Cdr
  | N_Cons_some
  | N_Cons_none
  | N_If_none
  | N_Left
  | N_Right
  | N_If_left
  | N_Cons_list
  | N_Nil
  | N_If_cons
  | N_Empty_set
  | N_Empty_map
  | N_Empty_big_map
  | N_Or
  | N_And
  | N_Xor
  | N_Not
  | N_Seq
  | N_If
  | N_Loop
  | N_Loop_left
  | N_Dip
  | N_Exec
  | N_Lambda
  | N_Failwith
  | N_Nop
  | N_Address
  | N_Create_contract
  | N_Set_delegate
  | N_Now
  | N_Balance
  | N_Hash_key
  | N_Unpack
  | N_Source
  | N_Sender
  | N_Self
  | N_Amount
  | N_ChainId
  | N_Level
  | N_Self_address
  | N_Never
  | N_Unpair
  | N_Voting_power
  | N_Total_voting_power
  | N_List_size
  | N_Set_size
  | N_Map_size
  | N_Sapling_empty_state ->
      model_0 instr const1_model
  | N_List_map | N_List_iter | N_Set_iter | N_Map_map | N_Map_iter ->
      model_1 instr affine_model
  | N_Set_mem
  | N_Set_update
  | N_Map_mem
  | N_Map_get
  | N_Map_update
  | N_Big_map_mem
  | N_Big_map_get
  | N_Big_map_update
  | N_Map_get_and_update
  | N_Big_map_get_and_update ->
      model_2 instr nlogm_model
  | N_Concat_string ->
      model_2 instr concat_model
  | N_Concat_string_pair ->
      model_2 instr concat_pair_model
  | N_Slice_string ->
      model_1 instr affine_model
  | N_String_size ->
      model_0 instr const1_model
  | N_Concat_bytes ->
      model_2 instr concat_model
  | N_Concat_bytes_pair ->
      model_2 instr concat_pair_model
  | N_Slice_bytes ->
      model_1 instr affine_model
  | N_Bytes_size ->
      model_1 instr affine_model
  | N_Add_seconds_to_timestamp
  | N_Add_timestamp_to_seconds
  | N_Sub_timestamp_seconds
  | N_Diff_timestamps ->
      model_2 instr linear_max_model
  | N_Add_tez | N_Sub_tez | N_Ediv_tez ->
      model_0 instr const1_model
  | N_Mul_teznat | N_Mul_nattez ->
      model_1 instr affine_model
  | N_Ediv_teznat ->
      model_2 instr (division_cost name)
  | N_Is_nat ->
      model_0 instr const1_model
  | N_Neg_nat ->
      model_1 instr affine_model
  | N_Neg_int ->
      model_1 instr affine_model
  | N_Abs_int ->
      model_1 instr affine_model
  | N_Int_nat ->
      model_0 instr const1_model
  | N_Add_intint ->
      model_2 instr linear_max_model
  | N_Add_intnat ->
      model_2 instr linear_max_model
  | N_Add_natint ->
      model_2 instr linear_max_model
  | N_Add_natnat ->
      model_2 instr linear_max_model
  | N_Sub_int ->
      model_2 instr linear_max_model
  | N_Mul_intint ->
      model_2 instr (addlogadd name)
  | N_Mul_intnat ->
      model_2 instr (addlogadd name)
  | N_Mul_natint ->
      model_2 instr (addlogadd name)
  | N_Mul_natnat ->
      model_2 instr (addlogadd name)
  | N_Ediv_intint ->
      model_2 instr (division_cost name)
  | N_Ediv_intnat ->
      model_2 instr (division_cost name)
  | N_Ediv_natint ->
      model_2 instr (division_cost name)
  | N_Ediv_natnat ->
      model_2 instr (division_cost name)
  | N_Lsl_nat ->
      model_1 instr affine_model
  | N_Lsr_nat ->
      model_1 instr affine_model
  | N_Or_nat ->
      model_2 instr linear_max_model
  | N_And_nat ->
      model_2 instr linear_min_model
  | N_And_int_nat ->
      model_2 instr linear_min_model
  | N_Xor_nat ->
      model_2 instr linear_max_model
  | N_Not_nat ->
      model_1 instr affine_model
  | N_Not_int ->
      model_1 instr affine_model
  | N_Compare ->
      model_2 instr linear_min_model
  | N_Eq | N_Neq | N_Lt | N_Gt | N_Le | N_Ge ->
      model_0 instr const1_model
  | N_Pack ->
      model_3 instr pack_model
  | N_Blake2b | N_Sha256 | N_Sha512 | N_Keccak | N_Sha3 ->
      model_1 instr affine_model
  | N_Check_signature_ed25519
  | N_Check_signature_secp256k1
  | N_Check_signature_p256 ->
      model_1 instr affine_model
  | N_Contract | N_Transfer_tokens | N_Implicit_account ->
      raise
        (Model_error
           (Unhandled_instruction
              "Interpreter_model.ir_model: operations not handled yet"))
  | N_Dig | N_Dug | N_DipN | N_DropN | N_DupN ->
      model_1 instr affine_model
  | N_Add_bls12_381_g1
  | N_Add_bls12_381_g2
  | N_Add_bls12_381_fr
  | N_Mul_bls12_381_g1
  | N_Mul_bls12_381_g2
  | N_Mul_bls12_381_fr
  | N_Neg_bls12_381_g1
  | N_Neg_bls12_381_g2
  | N_Neg_bls12_381_fr
  | N_Mul_bls12_381_fr_z
  | N_Mul_bls12_381_z_fr
  | N_Int_bls12_381_z_fr ->
      model_0 instr const1_model
  | N_Pairing_check_bls12_381 ->
      model_1 instr affine_model
  | N_Comb | N_Comb_get | N_Comb_set | N_Uncomb ->
      model_1 instr affine_model
  | N_Ticket | N_Read_ticket ->
      model_0 instr const1_model
  | N_Split_ticket ->
      model_2 instr split_ticket_model
  | N_Join_tickets ->
      model_2 instr linear_min_model
  | N_Sapling_verify_update ->
      model_2 instr verify_update_model

(* The following model stitches together the per-instruction models and
   adds a term corresponding to the latency induced by Lwt and by the timer itself. *)
let interpreter_model ?specialization () =
  Model.make_preapplied ~model:(fun trace ->
      let module Def (X : Costlang.S) = struct
        let applied =
          let (module Lwt_applied) =
            Model.apply Tezos_shell_benchmarks.Misc_benchmarks.lwt_model ()
          in
          let module Lwt_result = Lwt_applied (X) in
          List.fold_left
            (fun (acc : X.size X.repr) instr_trace ->
              let (module Applied_instr) =
                Model.apply
                  (ir_model
                     ?specialization
                     instr_trace.Interpreter_workload.instr_name)
                  instr_trace
              in
              let module R = Applied_instr (X) in
              X.(acc + R.applied))
            Lwt_result.applied
            trace
      end in
      ((module Def) : Model.applied))

let make_model ?specialization instr_name_opt =
  match instr_name_opt with
  | None ->
      [("interpreter", interpreter_model ?specialization ())]
  | Some name ->
      (* When generating code, we don't want to consider the terms specific to
         Lwt and to the timer latency. Also, we restrict to single instructions. *)
      let ir_model = ir_model ?specialization name in
      let name = name_of_instr ?specialization name in
      Registration_helpers.register_for_codegen
        name
        (Model.For_codegen ir_model) ;
      let ir_model =
        Model.precompose
          (function [sized_step] -> sized_step | _ -> assert false)
          ir_model
      in
      [ ("interpreter", interpreter_model ?specialization ());
        ("codegen", ir_model) ]
