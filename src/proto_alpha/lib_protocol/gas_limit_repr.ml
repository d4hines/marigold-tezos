(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

let decimals = 3

type fp_tag

type integral_tag

let scaling_factor = 1000

module Arith = struct
  type 'a t = Saturation_repr.t

  type fp = fp_tag t

  type integral = integral_tag t

  let scaling_factor = Saturation_repr.of_int scaling_factor

  let sub = Saturation_repr.sub

  let add = Saturation_repr.add

  let zero = Saturation_repr.zero

  let min = Saturation_repr.min

  let max = Saturation_repr.max

  let compare = Saturation_repr.compare

  let ( < ) = Saturation_repr.( < )

  let ( <> ) = Saturation_repr.( <> )

  let ( > ) = Saturation_repr.( > )

  let ( <= ) = Saturation_repr.( <= )

  let ( >= ) = Saturation_repr.( >= )

  let ( = ) = Saturation_repr.( = )

  let equal = Saturation_repr.equal

  let of_int i = Saturation_repr.(of_int i)

  let integral_of_int i = Saturation_repr.(mul scaling_factor (of_int i))

  let integral z =
    try integral_of_int (Z.to_int z) with _ -> Saturation_repr.saturated

  let integral_to_z : integral -> Z.t =
   fun i -> Z.of_int (fst (Saturation_repr.ediv i scaling_factor) :> int)

  let ceil x =
    let r = Saturation_repr.erem x scaling_factor in
    if r = zero then x else add x (sub scaling_factor r)

  let floor x =
    let r = Saturation_repr.erem x scaling_factor in
    if r = zero then x else sub x r

  let fp x = x

  let pp fmtr fp =
    let (q, r) = Saturation_repr.ediv fp scaling_factor in
    let q = (q :> int) in
    let r = (r :> int) in
    if Compare.Int.(r = 0) then Format.fprintf fmtr "%d" q
    else Format.fprintf fmtr "%d.%0*d" q decimals r

  let pp_integral = pp

  let n_fp_encoding : fp Data_encoding.t = Saturation_repr.encoding

  let z_fp_encoding : fp Data_encoding.t = Saturation_repr.encoding

  let n_integral_encoding : integral Data_encoding.t =
    Data_encoding.conv integral_to_z integral Data_encoding.n

  let z_integral_encoding : integral Data_encoding.t =
    Data_encoding.conv integral_to_z integral Data_encoding.z

  let unsafe_fp x = of_int (Z.to_int x)

  let sub_opt = Saturation_repr.sub_opt
end

type t = Unaccounted | Limited of {remaining : Arith.fp}

type cost = Z.t

let encoding =
  let open Data_encoding in
  union
    [ case
        (Tag 0)
        ~title:"Limited"
        Arith.z_fp_encoding
        (function Limited {remaining} -> Some remaining | _ -> None)
        (fun remaining -> Limited {remaining});
      case
        (Tag 1)
        ~title:"Unaccounted"
        (constant "unaccounted")
        (function Unaccounted -> Some () | _ -> None)
        (fun () -> Unaccounted) ]

let pp ppf = function
  | Unaccounted ->
      Format.fprintf ppf "unaccounted"
  | Limited {remaining} ->
      Format.fprintf ppf "%a units remaining" Arith.pp remaining

let cost_encoding = Data_encoding.z

let pp_cost fmt z = Z.pp_print fmt z

let allocation_weight = Z.of_int (scaling_factor * 2)

let step_weight = Z.of_int scaling_factor

let read_base_weight = Z.of_int (scaling_factor * 100)

let write_base_weight = Z.of_int (scaling_factor * 160)

let byte_read_weight = Z.of_int (scaling_factor * 10)

let byte_written_weight = Z.of_int (scaling_factor * 15)

let cost_to_milligas (cost : cost) : Arith.fp = Arith.unsafe_fp cost

let raw_consume gas_counter cost =
  let gas = cost_to_milligas cost in
  Arith.sub_opt gas_counter gas

let alloc_cost n = Z.mul allocation_weight (Z.succ n)

let alloc_bytes_cost n = alloc_cost (Z.of_int ((n + 7) / 8))

let atomic_step_cost n = n

let step_cost n = Z.mul step_weight n

let free = Z.zero

let read_bytes_cost n = Z.add read_base_weight (Z.mul byte_read_weight n)

let write_bytes_cost n = Z.add write_base_weight (Z.mul byte_written_weight n)

let ( +@ ) x y = Z.add x y

let ( *@ ) x y = Z.mul x y

let alloc_mbytes_cost n = alloc_cost (Z.of_int 12) +@ alloc_bytes_cost n
