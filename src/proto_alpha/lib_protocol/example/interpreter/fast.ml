type 'a input = Input of 'a

open Tezos_protocol_environment_alpha.Environment
open Pervasives
open Tezos_raw_protocol_alpha.Script_typed_ir
open Tezos_raw_protocol_alpha.Alpha_context
module Script_ir_translator = Tezos_raw_protocol_alpha.Script_ir_translator

let tz_compiled_0 (Input (stack, v_0)) =
  let stack = (v_0, stack) in
  let stack =
    let (vi, (vo, rest)) = stack in
    (vo, (vi, rest))
  in
  let stack =
    let ((a, _), rest) = stack in
    (a, rest)
  in
  let stack =
    let (a, (b, rest)) = stack in
    ((a, b), rest)
  in
  let (v, rest) = stack in
  (L v, rest)

let tz_compiled_1 (Input (stack, (v_1, v_2, v_3))) =
  let stack =
    let (v, rest) = stack in
    (v, (v, rest))
  in
  let stack =
    let ((a, _), rest) = stack in
    (a, rest)
  in
  let stack =
    let (vi, (vo, rest)) = stack in
    (vo, (vi, rest))
  in
  let stack =
    let ((_, b), rest) = stack in
    (b, rest)
  in
  let stack = (v_1, stack) in
  let stack =
    let (a0, (a1, (a2, rest))) = stack in
    (a2, (a0, (a1, rest)))
  in
  let stack =
    let (v, rest) = stack in
    (v, (v, rest))
  in
  let stack =
    let (i0, (i1, (i2, (i3, rest)))) = stack in
    (i1, (i2, (i3, (i0, rest))))
  in
  let stack =
    let (a, (b, stack)) = stack in
    ( Script_int.of_int @@ Script_ir_translator.compare_comparable v_2 a b,
      stack )
  in
  let stack =
    let (cmpres, rest) = stack in
    let cmpres = Script_int.compare cmpres Script_int.zero in
    let cmpres =
      let open Compare.Int in
      cmpres > 0
    in
    (cmpres, rest)
  in
  let stack =
    let (value, stack) = stack in
    if value then
      let stack =
        let (vi, (vo, rest)) = stack in
        (vo, (vi, rest))
      in
      let stack =
        let (v, rest) = stack in
        (v, (v, rest))
      in
      let stack =
        let (i0, (i1, (i2, rest))) = stack in
        (i1, (i2, (i0, rest)))
      in
      let stack =
        let (vi, (vo, rest)) = stack in
        (vo, (vi, rest))
      in
      let stack =
        let (x, (y, rest)) = stack in
        (Script_int.mul x y, rest)
      in
      let stack = (v_3, stack) in
      let stack =
        let (a0, (a1, (a2, rest))) = stack in
        (a2, (a0, (a1, rest)))
      in
      let stack =
        let (x, (y, rest)) = stack in
        (Script_int.sub x y, rest)
      in
      let stack =
        let (a, (b, rest)) = stack in
        ((a, b), rest)
      in
      let stack =
        let (v, rest) = stack in
        (L v, rest)
      in
      let stack = stack in
      stack
    else
      let stack =
        let (vi, (vo, rest)) = stack in
        (vo, (vi, rest))
      in
      let stack =
        let (_, rest) = stack in
        rest
      in
      let stack =
        let (v, rest) = stack in
        (R v, rest)
      in
      let stack = stack in
      stack
  in
  let stack = stack in
  stack

let tz_compiled_2 (Input (stack, ())) =
  let stack = ({elements = []; length = 0}, stack) in
  let stack =
    let (a, (b, rest)) = stack in
    ((a, b), rest)
  in
  let stack = stack in
  stack
