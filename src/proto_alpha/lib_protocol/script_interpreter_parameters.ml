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

(* ---- Expected parameters to instantiate the interpreter --------------------*)
module type Type = sig
  module Pervasives : sig
    external ( || ) : bool -> bool -> bool = "%sequor"

    external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"

    external not : bool -> bool = "%boolnot"

    val ( ^ ) : string -> string -> string

    external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"

    val ( @ ) : 'a list -> 'a list -> 'a list

    external ( / ) : int -> int -> int = "%divint"

    external ( - ) : int -> int -> int = "%subint"

    external ( + ) : int -> int -> int = "%addint"

    external ( && ) : bool -> bool -> bool = "%sequand"

    type ('ok, 'error) result = Ok of 'ok | Error of 'error
  end

  open Pervasives

  module Signature : sig
    type t

    type watermark

    module Public_key_hash : sig
      type t
    end

    module Public_key : sig
      type t

      val hash : t -> Public_key_hash.t
    end

    val check : ?watermark:watermark -> Public_key.t -> t -> bytes -> bool
  end

  module Z : sig
    type t

    val zero : t

    val to_int : t -> int

    val of_int : int -> t

    val add : t -> t -> t
  end

  module Bytes : sig
    val sub : bytes -> int -> int -> bytes

    external length : bytes -> int = "%bytes_length"

    val empty : bytes

    val concat : bytes -> bytes list -> bytes

    val cat : bytes -> bytes -> bytes
  end

  module Script_int : sig
    type 'a num

    type n = Natural_tag

    type z = Integer_tag

    val zero : z num

    val of_zint : Z.t -> z num

    val to_zint : 'a num -> Z.t

    val to_int64 : 'a num -> int64 option

    val sub : 'a num -> 'b num -> z num

    val shift_right_n : n num -> n num -> n num option

    val shift_left_n : n num -> n num -> n num option

    val of_int64 : int64 -> z num

    val of_int32 : int32 -> z num

    val of_int : int -> z num

    val neg : 'a num -> z num

    val mul_n : n num -> n num -> n num

    val mul : 'a num -> 'b num -> z num

    val logxor : n num -> n num -> n num

    val logor : 'a num -> 'a num -> 'a num

    val lognot : 'a num -> z num

    val logand : 'a num -> n num -> n num

    val is_nat : z num -> n num option

    val int : n num -> z num

    val ediv_n : n num -> n num -> (n num * n num) option

    val ediv : 'a num -> 'b num -> (z num * n num) option

    val compare : 'a num -> 'a num -> int

    val add_n : n num -> n num -> n num

    val add : 'a num -> 'b num -> z num

    val abs : z num -> n num
  end

  module TzEndian : sig
    val get_uint8 : bytes -> int -> int
  end

  module String : sig
    val sub : string -> int -> int -> string

    external length : string -> int = "%string_length"

    val concat : string -> string list -> string
  end

  module Raw_hashes : sig
    val sha512 : bytes -> bytes

    val sha3_256 : bytes -> bytes

    val sha256 : bytes -> bytes

    val keccak256 : bytes -> bytes

    val blake2b : bytes -> bytes
  end

  module Option : sig
    val value : 'a option -> default:'a -> 'a

    val map : ('a -> 'b) -> 'a option -> 'b option
  end

  module Micheline : sig
    type annot = string list

    type ('a, 'b) node =
      | Int of 'a * Z.t
      | String of 'a * string
      | Bytes of 'a * bytes
      | Prim of 'a * 'b * ('a, 'b) node list * annot
      | Seq of 'a * ('a, 'b) node list

    type 'a canonical

    type canonical_location = int

    val strip_locations : ('a, 'p) node -> 'p canonical

    val root : 'p canonical -> (canonical_location, 'p) node
  end

  module Lwt : sig
    type 'a t

    val return : 'a -> 'a t
  end

  module List : sig
    val split : ('a * 'b) list -> 'a list * 'b list

    val rev : 'a list -> 'a list

    val map : ('a -> 'b) -> 'a list -> 'b list

    val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a

    val flatten : 'a list list -> 'a list
  end

  module Format : sig
    type formatter
  end

  module Data_encoding : sig
    type 'a t

    type 'a lazy_t

    type 'a field

    module Binary : sig
      val of_bytes : 'a t -> bytes -> 'a option
    end

    val empty : unit t

    val list : ?max_length:int -> 'a t -> 'a list t

    val opt :
      ?title:string -> ?description:string -> string -> 't t -> 't option field

    val string : string t

    val obj1 : 'f1 field -> 'f1 t

    val obj2 : 'f1 field -> 'f2 field -> ('f1 * 'f2) t

    val obj3 : 'f1 field -> 'f2 field -> 'f3 field -> ('f1 * 'f2 * 'f3) t

    val req :
      ?title:string -> ?description:string -> string -> 't t -> 't field
  end

  module Error_monad : sig
    type 'a trace

    type error_category = [`Branch | `Temporary | `Permanent]

    type error = ..

    type 'a tzresult = ('a, error trace) result

    val trace :
      'err -> ('b, 'err trace) result Lwt.t -> ('b, 'err trace) result Lwt.t

    val return_none : ('a option, 'trace) result Lwt.t

    val return : 'a -> ('a, 'trace) result Lwt.t

    val register_error_kind :
      error_category ->
      id:string ->
      title:string ->
      description:string ->
      ?pp:(Format.formatter -> 'err -> unit) ->
      'err Data_encoding.t ->
      (error -> 'err option) ->
      ('err -> error) ->
      unit

    val record_trace :
      'err -> ('a, 'err trace) result -> ('a, 'err trace) result

    val ok : 'a -> ('a, 'trace) result

    val fail : 'err -> ('a, 'err trace) result Lwt.t

    val ( >|? ) : ('a, 'trace) result -> ('a -> 'b) -> ('b, 'trace) result

    val ( >|=? ) :
      ('a, 'trace) result Lwt.t -> ('a -> 'b) -> ('b, 'trace) result Lwt.t

    val ( >>?= ) :
      ('a, 'trace) result ->
      ('a -> ('b, 'trace) result Lwt.t) ->
      ('b, 'trace) result Lwt.t

    val ( >>? ) :
      ('a, 'trace) result -> ('a -> ('b, 'trace) result) -> ('b, 'trace) result

    val ( >>=? ) :
      ('a, 'trace) result Lwt.t ->
      ('a -> ('b, 'trace) result Lwt.t) ->
      ('b, 'trace) result Lwt.t

    val ( >|= ) : 'a Lwt.t -> ('a -> 'b) -> 'b Lwt.t
  end

  open Error_monad

  module Compare : sig
    module Z : sig
      val ( <= ) : Z.t -> Z.t -> bool

      val ( < ) : Z.t -> Z.t -> bool
    end

    module Int : sig
      val ( >= ) : int -> int -> bool

      val ( > ) : int -> int -> bool

      val ( = ) : int -> int -> bool

      val ( <> ) : int -> int -> bool

      val ( <= ) : int -> int -> bool

      val ( < ) : int -> int -> bool
    end

    module Bool : sig
      val ( <> ) : bool -> bool -> bool
    end
  end

  module Chain_id : sig
    type t

    val to_b58check : t -> string
  end

  module Bls12_381 : sig
    module Gt : sig
      type t

      val one : t

      val eq : t -> t -> bool
    end

    module Fr : sig
      type t

      val negate : t -> t

      val mul : t -> t -> t

      val add : t -> t -> t

      val of_z : Z.t -> t

      val to_z : t -> Z.t
    end

    module G2 : sig
      module Scalar : sig
        type t = Fr.t
      end

      type t

      val negate : t -> t

      val mul : t -> Scalar.t -> t

      val add : t -> t -> t
    end

    module G1 : sig
      module Scalar : sig
        type t = Fr.t
      end

      type t

      val negate : t -> t

      val mul : t -> Scalar.t -> t

      val add : t -> t -> t
    end

    val miller_loop : (G1.t * G2.t) list -> Gt.t

    val final_exponentiation_opt : Gt.t -> Gt.t option
  end

  module Big_map : sig
    module Id : sig
      type t
    end
  end

  module Tez : sig
    type t

    val to_mutez : t -> int64

    val of_mutez : int64 -> t option

    val ( -? ) : t -> t -> t tzresult

    val ( +? ) : t -> t -> t tzresult

    val ( *? ) : t -> int64 -> t tzresult
  end

  module Raw_context : sig
    type t

    val fresh_internal_nonce : t -> (t * int) tzresult
  end

  module Sapling : sig
    module Id : sig
      type t
    end

    module Memo_size : sig
      type t
    end

    type transaction

    type state

    val empty_state : ?id:Id.t -> memo_size:Memo_size.t -> unit -> state

    val verify_update :
      Raw_context.t ->
      state ->
      transaction ->
      string ->
      (Raw_context.t * (int64 * state) option) tzresult Lwt.t
  end

  module Contract : sig
    type t

    val implicit_contract : Signature.Public_key_hash.t -> t

    val get_balance_carbonated :
      Raw_context.t -> t -> (Raw_context.t * Tez.t) tzresult Lwt.t

    val fresh_contract_from_current_nonce :
      Raw_context.t -> (Raw_context.t * t) tzresult

    val encoding : t Data_encoding.t

    val to_b58check : t -> string
  end

  module Raw_level : sig
    type t

    val to_int32 : t -> int32
  end

  module Gas : sig
    type t

    type cost

    val encoding : t Data_encoding.t

    val consume : Raw_context.t -> cost -> Raw_context.t tzresult

    val check_enough : Raw_context.t -> cost -> unit tzresult
  end

  module Script_timestamp : sig
    type t

    val sub_delta : t -> Script_int.z Script_int.num -> t

    val now : Raw_context.t -> t

    val diff : t -> t -> Script_int.z Script_int.num

    val add_delta : t -> Script_int.z Script_int.num -> t
  end

  module Michelson_v1_primitives : sig
    type prim

    val i_push : prim

    val i_pair : prim

    val k_parameter : prim

    val k_storage : prim

    val k_code : prim
  end

  module Script : sig
    type expr = Michelson_v1_primitives.prim Micheline.canonical

    type lazy_expr = expr Data_encoding.lazy_t

    type location = Micheline.canonical_location

    type node = (location, Michelson_v1_primitives.prim) Micheline.node

    type t = {code : lazy_expr; storage : lazy_expr}

    type annot

    val strip_locations_cost : node -> Gas.cost

    val serialized_cost : bytes -> Gas.cost

    val location_encoding : location Data_encoding.t

    val lazy_expr : expr -> lazy_expr

    val force_decode_in_context :
      Raw_context.t -> lazy_expr -> (expr * Raw_context.t, error trace) result

    val expr_encoding : expr Data_encoding.t

    val deserialized_cost : expr -> Gas.cost
  end

  module Lazy_storage : sig
    type diffs_item

    type diffs = diffs_item list
  end

  module Operation : sig
    module Kind : sig
      type reveal = Reveal_kind

      type transaction = Transaction_kind

      type origination = Origination_kind

      type delegation = Delegation_kind
    end

    type 'kind internal_operation = {
      source : Contract.t;
      operation : 'kind manager_operation;
      nonce : int;
    }

    and _ manager_operation =
      | Reveal : Signature.Public_key.t -> Kind.reveal manager_operation
      | Transaction : {
          amount : Tez.t;
          parameters : Script.lazy_expr;
          entrypoint : string;
          destination : Contract.t;
        }
          -> Kind.transaction manager_operation
      | Origination : {
          delegate : Signature.Public_key_hash.t option;
          script : Script.t;
          credit : Tez.t;
          preorigination : Contract.t option;
        }
          -> Kind.origination manager_operation
      | Delegation :
          Signature.Public_key_hash.t option
          -> Kind.delegation manager_operation

    and packed_internal_operation =
      | Internal_operation :
          'kind internal_operation
          -> packed_internal_operation
  end

  module Vote : sig
    val get_voting_power :
      Raw_context.t ->
      Signature.Public_key_hash.t ->
      (Raw_context.t * int32) tzresult Lwt.t

    val get_total_voting_power :
      Raw_context.t -> (Raw_context.t * int32) tzresult Lwt.t
  end

  module Cycle : sig
    type t
  end

  module Level : sig
    type t = private {
      level : Raw_level.t;
      level_position : int32;
      cycle : Cycle.t;
      cycle_position : int32;
      expected_commitment : bool;
    }

    val current : Raw_context.t -> t
  end

  module Script_typed_ir :
    Script_typed_ir_functor.S
      with type tez = Tez.t
       and type 't num = 't Script_int.num
       and type z = Script_int.z
       and type n = Script_int.n
       and type transaction = Sapling.transaction
       and type state = Sapling.state
       and type memo_size = Sapling.Memo_size.t
       and type big_map_id = Big_map.Id.t
       and type signature = Signature.t
       and type public_key = Signature.Public_key.t
       and type public_key_hash = Signature.Public_key_hash.t
       and type packed_internal_operation = Operation.packed_internal_operation
       and type timestamp = Script_timestamp.t
       and type node = Script.node
       and type location = Script.location
       and type diffs = Lazy_storage.diffs
       and type contract = Contract.t
       and type chain_id = Chain_id.t
       and type bls12_381_g1 = Bls12_381.G1.t
       and type bls12_381_g2 = Bls12_381.G2.t
       and type bls12_381_fr = Bls12_381.Fr.t

  module Script_ir_translator : sig
    type unparsing_mode = Optimized | Readable | Optimized_legacy

    type type_logger =
      int ->
      (Script.expr * Script.annot) list ->
      (Script.expr * Script.annot) list ->
      unit

    type ex_script = Ex_script : ('a, 'b) Script_typed_ir.script -> ex_script

    type lazy_storage_ids

    type ex_ty = Ex_ty : 'a Script_typed_ir.ty -> ex_ty

    val unparse_ty :
      Raw_context.t ->
      'a Script_typed_ir.ty ->
      (Script.node * Raw_context.t) tzresult

    val unparse_data :
      Raw_context.t ->
      unparsing_mode ->
      'a Script_typed_ir.ty ->
      'a ->
      (Script.node * Raw_context.t) tzresult Lwt.t

    val set_update :
      'a -> bool -> 'a Script_typed_ir.set -> 'a Script_typed_ir.set

    val set_size : 'elt Script_typed_ir.set -> Script_int.n Script_int.num

    val set_mem : 'elt -> 'elt Script_typed_ir.set -> bool

    val set_fold :
      ('elt -> 'acc -> 'acc) -> 'elt Script_typed_ir.set -> 'acc -> 'acc

    val parse_script :
      ?type_logger:type_logger ->
      Raw_context.t ->
      legacy:bool ->
      allow_forged_in_storage:bool ->
      Script.t ->
      (ex_script * Raw_context.t) tzresult Lwt.t

    val parse_data :
      ?type_logger:type_logger ->
      Raw_context.t ->
      legacy:bool ->
      allow_forged:bool ->
      'a Script_typed_ir.ty ->
      Script.node ->
      ('a * Raw_context.t) tzresult Lwt.t

    val parse_contract_for_script :
      legacy:bool ->
      Raw_context.t ->
      Script.location ->
      'a Script_typed_ir.ty ->
      Contract.t ->
      entrypoint:string ->
      (Raw_context.t * 'a Script_typed_ir.typed_contract option) tzresult Lwt.t

    val pack_data :
      Raw_context.t ->
      'a Script_typed_ir.ty ->
      'a ->
      (bytes * Raw_context.t) tzresult Lwt.t

    val no_lazy_storage_id : lazy_storage_ids

    val map_update :
      'a ->
      'b option ->
      ('a, 'b) Script_typed_ir.map ->
      ('a, 'b) Script_typed_ir.map

    val map_size : ('a, 'b) Script_typed_ir.map -> Script_int.n Script_int.num

    val map_mem : 'key -> ('key, 'value) Script_typed_ir.map -> bool

    val map_key_ty :
      ('a, 'b) Script_typed_ir.map -> 'a Script_typed_ir.comparable_ty

    val map_get : 'key -> ('key, 'value) Script_typed_ir.map -> 'value option

    val map_fold :
      ('key -> 'value -> 'acc -> 'acc) ->
      ('key, 'value) Script_typed_ir.map ->
      'acc ->
      'acc

    val list_empty : 'a Script_typed_ir.boxed_list

    val list_cons :
      'a -> 'a Script_typed_ir.boxed_list -> 'a Script_typed_ir.boxed_list

    val find_entrypoint :
      't Script_typed_ir.ty ->
      root_name:Script_typed_ir.field_annot option ->
      string ->
      ((Script.node -> Script.node) * ex_ty) tzresult

    val extract_lazy_storage_diff :
      Raw_context.t ->
      unparsing_mode ->
      temporary:bool ->
      to_duplicate:lazy_storage_ids ->
      to_update:lazy_storage_ids ->
      'a Script_typed_ir.ty ->
      'a ->
      ('a * Lazy_storage.diffs option * Raw_context.t) tzresult Lwt.t

    val empty_set : 'a Script_typed_ir.comparable_ty -> 'a Script_typed_ir.set

    val empty_map :
      'a Script_typed_ir.comparable_ty -> ('a, 'b) Script_typed_ir.map

    val empty_big_map :
      'a Script_typed_ir.comparable_ty ->
      'b Script_typed_ir.ty ->
      ('a, 'b) Script_typed_ir.big_map

    val compare_comparable :
      'a Script_typed_ir.comparable_ty -> 'a -> 'a -> int

    val collect_lazy_storage :
      Raw_context.t ->
      'a Script_typed_ir.ty ->
      'a ->
      (lazy_storage_ids * Raw_context.t) tzresult

    val big_map_update :
      'key ->
      'value option ->
      ('key, 'value) Script_typed_ir.big_map ->
      ('key, 'value) Script_typed_ir.big_map

    val big_map_mem :
      Raw_context.t ->
      'key ->
      ('key, 'value) Script_typed_ir.big_map ->
      (bool * Raw_context.t) tzresult Lwt.t

    val big_map_get :
      Raw_context.t ->
      'key ->
      ('key, 'value) Script_typed_ir.big_map ->
      ('value option * Raw_context.t) tzresult Lwt.t

    val add_field_annot :
      Script_typed_ir.field_annot option ->
      Script_typed_ir.var_annot option ->
      Script.node ->
      Script.node

    val compare_address :
      Script_typed_ir.address -> Script_typed_ir.address -> int
  end

  module Script_interpreter_cost : sig
    val cost_of_instr : ('b, 'a) Script_typed_ir.descr -> 'b -> Gas.cost

    val unpack_failed : bytes -> Gas.cost

    val concat_string : Z.t -> Gas.cost
  end
end
