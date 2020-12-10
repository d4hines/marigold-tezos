type 'a input =
  | Input of 'a 
open Tezos_protocol_environment_alpha.Environment
open Pervasives
open Tezos_raw_protocol_alpha.Script_typed_ir
open Tezos_raw_protocol_alpha.Alpha_context
module Script_ir_translator = Tezos_raw_protocol_alpha.Script_ir_translator
module Interp_costs =
  Tezos_raw_protocol_alpha.Michelson_v1_gas.Cost_of.Interpreter
module Script_interpreter = Tezos_raw_protocol_alpha.Script_interpreter
let tz_compiled_0 (Input ((let>>?=), (let>>=?), return, ctx, stack, v_0)) =
  let cost = Interp_costs.push in
  let>>?= ctx = Gas.consume ctx cost
   in
  let stack = (v_0, stack) in
  let cost = Interp_costs.swap in
  let>>?= ctx = Gas.consume ctx cost
   in
  let (vi, (vo, rest)) = stack in
  let stack = (vo, (vi, rest)) in
  let cost = Interp_costs.car in
  let>>?= ctx = Gas.consume ctx cost
   in
  let ((a, _), rest) = stack in
  let stack = (a, rest) in
  let cost = Interp_costs.cons_pair in
  let>>?= ctx = Gas.consume ctx cost
   in
  let (a, (b, stack)) = stack in
  let stack = ((a, b), stack) in
  let cost = Interp_costs.cons_left in
  let>>?= ctx = Gas.consume ctx cost
   in
  let (v, rest) = stack in let stack = ((L v), rest) in return (stack, ctx)
let tz_compiled_1 (Input
  ((let>>?=), (let>>=?), return, ctx, stack, (v_1, v_2, v_3))) =
  let cost = Interp_costs.seq in
  let>>?= ctx = Gas.consume ctx cost
   in
  let cost = Interp_costs.dup in
  let>>?= ctx = Gas.consume ctx cost
   in
  let (v, rest) = stack in
  let stack = (v, (v, rest)) in
  let cost = Interp_costs.seq in
  let>>?= ctx = Gas.consume ctx cost
   in
  let cost = Interp_costs.car in
  let>>?= ctx = Gas.consume ctx cost
   in
  let ((a, _), rest) = stack in
  let stack = (a, rest) in
  let cost = Interp_costs.seq in
  let>>?= ctx = Gas.consume ctx cost
   in
  let cost = Interp_costs.swap in
  let>>?= ctx = Gas.consume ctx cost
   in
  let (vi, (vo, rest)) = stack in
  let stack = (vo, (vi, rest)) in
  let cost = Interp_costs.seq in
  let>>?= ctx = Gas.consume ctx cost
   in
  let cost = Interp_costs.cdr in
  let>>?= ctx = Gas.consume ctx cost
   in
  let ((_, b), rest) = stack in
  let stack = (b, rest) in
  let cost = Interp_costs.seq in
  let>>?= ctx = Gas.consume ctx cost
   in
  let cost = Interp_costs.push in
  let>>?= ctx = Gas.consume ctx cost
   in
  let stack = (v_1, stack) in
  let cost = Interp_costs.seq in
  let>>?= ctx = Gas.consume ctx cost
   in
  let cost = Interp_costs.dign 2 in
  let>>?= ctx = Gas.consume ctx cost
   in
  let (a0, (a1, (a2, rest))) = stack in
  let stack = (a2, (a0, (a1, rest))) in
  let cost = Interp_costs.seq in
  let>>?= ctx = Gas.consume ctx cost
   in
  let cost = Interp_costs.dup in
  let>>?= ctx = Gas.consume ctx cost
   in
  let (v, rest) = stack in
  let stack = (v, (v, rest)) in
  let cost = Interp_costs.seq in
  let>>?= ctx = Gas.consume ctx cost
   in
  let cost = Interp_costs.dugn 3 in
  let>>?= ctx = Gas.consume ctx cost
   in
  let (i0, (i1, (i2, (i3, rest)))) = stack in
  let stack = (i1, (i2, (i3, (i0, rest)))) in
  let cost = Interp_costs.seq in
  let>>?= ctx = Gas.consume ctx cost
   in
  let cost = let (a, (b, _)) = stack in Interp_costs.compare v_2 a b in
  let>>?= ctx = Gas.consume ctx cost
   in
  let (a, (b, stack)) = stack in
  let stack =
    ((Script_int.of_int @@ (Script_ir_translator.compare_comparable v_2 a b)),
      stack) in
  let cost = Interp_costs.seq in
  let>>?= ctx = Gas.consume ctx cost
   in
  let cost = Interp_costs.neq in
  let>>?= ctx = Gas.consume ctx cost
   in
  let (cmpres, rest) = stack in
  let cmpres = Script_int.compare cmpres Script_int.zero in
  let cmpres = let open Compare.Int in cmpres > 0 in
  let stack = (cmpres, rest) in
  let cost = Interp_costs.seq in
  let>>?= ctx = Gas.consume ctx cost
   in
  let cost = Interp_costs.if_ in
  let>>?= ctx = Gas.consume ctx cost
   in
  let (value, stack) = stack in
  let>>=? (stack, ctx) =
    if value
    then
      let cost = Interp_costs.seq in
      let>>?= ctx = Gas.consume ctx cost
       in
      let cost = Interp_costs.swap in
      let>>?= ctx = Gas.consume ctx cost
       in
      let (vi, (vo, rest)) = stack in
      let stack = (vo, (vi, rest)) in
      let cost = Interp_costs.seq in
      let>>?= ctx = Gas.consume ctx cost
       in
      let cost = Interp_costs.dup in
      let>>?= ctx = Gas.consume ctx cost
       in
      let (v, rest) = stack in
      let stack = (v, (v, rest)) in
      let cost = Interp_costs.seq in
      let>>?= ctx = Gas.consume ctx cost
       in
      let cost = Interp_costs.dugn 2 in
      let>>?= ctx = Gas.consume ctx cost
       in
      let (i0, (i1, (i2, rest))) = stack in
      let stack = (i1, (i2, (i0, rest))) in
      let cost = Interp_costs.seq in
      let>>?= ctx = Gas.consume ctx cost
       in
      let cost = Interp_costs.swap in
      let>>?= ctx = Gas.consume ctx cost
       in
      let (vi, (vo, rest)) = stack in
      let stack = (vo, (vi, rest)) in
      let cost = Interp_costs.seq in
      let>>?= ctx = Gas.consume ctx cost
       in
      let cost = let (x, (y, _)) = stack in Interp_costs.mul_bigint x y in
      let>>?= ctx = Gas.consume ctx cost
       in
      let (x, (y, rest)) = stack in
      let stack = ((Script_int.mul x y), rest) in
      let cost = Interp_costs.seq in
      let>>?= ctx = Gas.consume ctx cost
       in
      let cost = Interp_costs.push in
      let>>?= ctx = Gas.consume ctx cost
       in
      let stack = (v_3, stack) in
      let cost = Interp_costs.seq in
      let>>?= ctx = Gas.consume ctx cost
       in
      let cost = Interp_costs.dign 2 in
      let>>?= ctx = Gas.consume ctx cost
       in
      let (a0, (a1, (a2, rest))) = stack in
      let stack = (a2, (a0, (a1, rest))) in
      let cost = Interp_costs.seq in
      let>>?= ctx = Gas.consume ctx cost
       in
      let cost = let (x, (y, _)) = stack in Interp_costs.sub_bigint x y in
      let>>?= ctx = Gas.consume ctx cost
       in
      let (x, (y, rest)) = stack in
      let stack = ((Script_int.sub x y), rest) in
      let cost = Interp_costs.seq in
      let>>?= ctx = Gas.consume ctx cost
       in
      let cost = Interp_costs.cons_pair in
      let>>?= ctx = Gas.consume ctx cost
       in
      let (a, (b, stack)) = stack in
      let stack = ((a, b), stack) in
      let cost = Interp_costs.seq in
      let>>?= ctx = Gas.consume ctx cost
       in
      let cost = Interp_costs.cons_left in
      let>>?= ctx = Gas.consume ctx cost
       in
      let (v, rest) = stack in
      let stack = ((L v), rest) in
      let cost = Interp_costs.nop in
      let>>?= ctx = Gas.consume ctx cost
       in let stack = stack in return (stack, ctx)
    else
      (let cost = Interp_costs.seq in
       let>>?= ctx = Gas.consume ctx cost
        in
       let cost = Interp_costs.swap in
       let>>?= ctx = Gas.consume ctx cost
        in
       let (vi, (vo, rest)) = stack in
       let stack = (vo, (vi, rest)) in
       let cost = Interp_costs.seq in
       let>>?= ctx = Gas.consume ctx cost
        in
       let cost = Interp_costs.drop in
       let>>?= ctx = Gas.consume ctx cost
        in
       let (_, stack) = stack in
       let cost = Interp_costs.seq in
       let>>?= ctx = Gas.consume ctx cost
        in
       let cost = Interp_costs.cons_right in
       let>>?= ctx = Gas.consume ctx cost
        in
       let (v, rest) = stack in
       let stack = ((R v), rest) in
       let cost = Interp_costs.nop in
       let>>?= ctx = Gas.consume ctx cost
        in let stack = stack in return (stack, ctx))
   in
  let cost = Interp_costs.nop in
  let>>?= ctx = Gas.consume ctx cost
   in let stack = stack in return (stack, ctx)
let tz_compiled_2 (Input ((let>>?=), (let>>=?), return, ctx, stack, ())) =
  let cost = Interp_costs.seq in
  let>>?= ctx = Gas.consume ctx cost
   in
  let cost = Interp_costs.nil in
  let>>?= ctx = Gas.consume ctx cost
   in
  let stack = ({ elements = []; length = 0 }, stack) in
  let cost = Interp_costs.seq in
  let>>?= ctx = Gas.consume ctx cost
   in
  let cost = Interp_costs.cons_pair in
  let>>?= ctx = Gas.consume ctx cost
   in
  let (a, (b, stack)) = stack in
  let stack = ((a, b), stack) in
  let cost = Interp_costs.nop in
  let>>?= ctx = Gas.consume ctx cost
   in let stack = stack in return (stack, ctx)
