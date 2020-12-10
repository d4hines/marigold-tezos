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
  log_entry ctx stack;
  (let cost = Interp_costs.swap in
   let>>?= ctx = Gas.consume ctx cost
    in
   let (vi, (vo, rest)) = stack in
   let stack = (vo, (vi, rest)) in
   log_entry ctx stack;
   (let cost = Interp_costs.car in
    let>>?= ctx = Gas.consume ctx cost
     in
    let ((a, _), rest) = stack in
    let stack = (a, rest) in
    log_entry ctx stack;
    (let cost = Interp_costs.cons_pair in
     let>>?= ctx = Gas.consume ctx cost
      in
     let (a, (b, stack)) = stack in
     let stack = ((a, b), stack) in
     log_entry ctx stack;
     (let cost = Interp_costs.cons_left in
      let>>?= ctx = Gas.consume ctx cost
       in
      let (v, rest) = stack in
      let stack = ((L v), rest) in log_entry ctx stack; return (stack, ctx)))))
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
  log_entry ctx stack;
  (let cost = Interp_costs.seq in
   let>>?= ctx = Gas.consume ctx cost
    in
   let cost = Interp_costs.car in
   let>>?= ctx = Gas.consume ctx cost
    in
   let ((a, _), rest) = stack in
   let stack = (a, rest) in
   log_entry ctx stack;
   (let cost = Interp_costs.seq in
    let>>?= ctx = Gas.consume ctx cost
     in
    let cost = Interp_costs.swap in
    let>>?= ctx = Gas.consume ctx cost
     in
    let (vi, (vo, rest)) = stack in
    let stack = (vo, (vi, rest)) in
    log_entry ctx stack;
    (let cost = Interp_costs.seq in
     let>>?= ctx = Gas.consume ctx cost
      in
     let cost = Interp_costs.cdr in
     let>>?= ctx = Gas.consume ctx cost
      in
     let ((_, b), rest) = stack in
     let stack = (b, rest) in
     log_entry ctx stack;
     (let cost = Interp_costs.seq in
      let>>?= ctx = Gas.consume ctx cost
       in
      let cost = Interp_costs.push in
      let>>?= ctx = Gas.consume ctx cost
       in
      let stack = (v_1, stack) in
      log_entry ctx stack;
      (let cost = Interp_costs.seq in
       let>>?= ctx = Gas.consume ctx cost
        in
       let cost = Interp_costs.dign 2 in
       let>>?= ctx = Gas.consume ctx cost
        in
       let (a0, (a1, (a2, rest))) = stack in
       let stack = (a2, (a0, (a1, rest))) in
       log_entry ctx stack;
       (let cost = Interp_costs.seq in
        let>>?= ctx = Gas.consume ctx cost
         in
        let cost = Interp_costs.dup in
        let>>?= ctx = Gas.consume ctx cost
         in
        let (v, rest) = stack in
        let stack = (v, (v, rest)) in
        log_entry ctx stack;
        (let cost = Interp_costs.seq in
         let>>?= ctx = Gas.consume ctx cost
          in
         let cost = Interp_costs.dugn 3 in
         let>>?= ctx = Gas.consume ctx cost
          in
         let (i0, (i1, (i2, (i3, rest)))) = stack in
         let stack = (i1, (i2, (i3, (i0, rest)))) in
         log_entry ctx stack;
         (let cost = Interp_costs.seq in
          let>>?= ctx = Gas.consume ctx cost
           in
          let cost = let (a, (b, _)) = stack in Interp_costs.compare v_2 a b in
          let>>?= ctx = Gas.consume ctx cost
           in
          let (a, (b, stack)) = stack in
          let stack =
            ((Script_int.of_int @@
                (Script_ir_translator.compare_comparable v_2 a b)), stack) in
          log_entry ctx stack;
          (let cost = Interp_costs.seq in
           let>>?= ctx = Gas.consume ctx cost
            in
           let cost = Interp_costs.neq in
           let>>?= ctx = Gas.consume ctx cost
            in
           let (cmpres, rest) = stack in
           let cmpres = Script_int.compare cmpres Script_int.zero in
           let cmpres = let open Compare.Int in cmpres > 0 in
           let stack = (cmpres, rest) in
           log_entry ctx stack;
           (let cost = Interp_costs.seq in
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
                (log_entry ctx stack;
                 (let cost = Interp_costs.seq in
                  let>>?= ctx = Gas.consume ctx cost
                   in
                  let cost = Interp_costs.dup in
                  let>>?= ctx = Gas.consume ctx cost
                   in
                  let (v, rest) = stack in
                  let stack = (v, (v, rest)) in
                  log_entry ctx stack;
                  (let cost = Interp_costs.seq in
                   let>>?= ctx = Gas.consume ctx cost
                    in
                   let cost = Interp_costs.dugn 2 in
                   let>>?= ctx = Gas.consume ctx cost
                    in
                   let (i0, (i1, (i2, rest))) = stack in
                   let stack = (i1, (i2, (i0, rest))) in
                   log_entry ctx stack;
                   (let cost = Interp_costs.seq in
                    let>>?= ctx = Gas.consume ctx cost
                     in
                    let cost = Interp_costs.swap in
                    let>>?= ctx = Gas.consume ctx cost
                     in
                    let (vi, (vo, rest)) = stack in
                    let stack = (vo, (vi, rest)) in
                    log_entry ctx stack;
                    (let cost = Interp_costs.seq in
                     let>>?= ctx = Gas.consume ctx cost
                      in
                     let cost =
                       let (x, (y, _)) = stack in Interp_costs.mul_bigint x y in
                     let>>?= ctx = Gas.consume ctx cost
                      in
                     let (x, (y, rest)) = stack in
                     let stack = ((Script_int.mul x y), rest) in
                     log_entry ctx stack;
                     (let cost = Interp_costs.seq in
                      let>>?= ctx = Gas.consume ctx cost
                       in
                      let cost = Interp_costs.push in
                      let>>?= ctx = Gas.consume ctx cost
                       in
                      let stack = (v_3, stack) in
                      log_entry ctx stack;
                      (let cost = Interp_costs.seq in
                       let>>?= ctx = Gas.consume ctx cost
                        in
                       let cost = Interp_costs.dign 2 in
                       let>>?= ctx = Gas.consume ctx cost
                        in
                       let (a0, (a1, (a2, rest))) = stack in
                       let stack = (a2, (a0, (a1, rest))) in
                       log_entry ctx stack;
                       (let cost = Interp_costs.seq in
                        let>>?= ctx = Gas.consume ctx cost
                         in
                        let cost =
                          let (x, (y, _)) = stack in
                          Interp_costs.sub_bigint x y in
                        let>>?= ctx = Gas.consume ctx cost
                         in
                        let (x, (y, rest)) = stack in
                        let stack = ((Script_int.sub x y), rest) in
                        log_entry ctx stack;
                        (let cost = Interp_costs.seq in
                         let>>?= ctx = Gas.consume ctx cost
                          in
                         let cost = Interp_costs.cons_pair in
                         let>>?= ctx = Gas.consume ctx cost
                          in
                         let (a, (b, stack)) = stack in
                         let stack = ((a, b), stack) in
                         log_entry ctx stack;
                         (let cost = Interp_costs.seq in
                          let>>?= ctx = Gas.consume ctx cost
                           in
                          let cost = Interp_costs.cons_left in
                          let>>?= ctx = Gas.consume ctx cost
                           in
                          let (v, rest) = stack in
                          let stack = ((L v), rest) in
                          log_entry ctx stack;
                          (let cost = Interp_costs.nop in
                           let>>?= ctx = Gas.consume ctx cost
                            in
                           let stack = stack in
                           log_entry ctx stack;
                           log_entry ctx stack;
                           log_entry ctx stack;
                           log_entry ctx stack;
                           log_entry ctx stack;
                           log_entry ctx stack;
                           log_entry ctx stack;
                           log_entry ctx stack;
                           log_entry ctx stack;
                           log_entry ctx stack;
                           log_entry ctx stack;
                           return (stack, ctx))))))))))))
              else
                (let cost = Interp_costs.seq in
                 let>>?= ctx = Gas.consume ctx cost
                  in
                 let cost = Interp_costs.swap in
                 let>>?= ctx = Gas.consume ctx cost
                  in
                 let (vi, (vo, rest)) = stack in
                 let stack = (vo, (vi, rest)) in
                 log_entry ctx stack;
                 (let cost = Interp_costs.seq in
                  let>>?= ctx = Gas.consume ctx cost
                   in
                  let cost = Interp_costs.drop in
                  let>>?= ctx = Gas.consume ctx cost
                   in
                  let (_, stack) = stack in
                  log_entry ctx stack;
                  (let cost = Interp_costs.seq in
                   let>>?= ctx = Gas.consume ctx cost
                    in
                   let cost = Interp_costs.cons_right in
                   let>>?= ctx = Gas.consume ctx cost
                    in
                   let (v, rest) = stack in
                   let stack = ((R v), rest) in
                   log_entry ctx stack;
                   (let cost = Interp_costs.nop in
                    let>>?= ctx = Gas.consume ctx cost
                     in
                    let stack = stack in
                    log_entry ctx stack;
                    log_entry ctx stack;
                    log_entry ctx stack;
                    log_entry ctx stack;
                    return (stack, ctx)))))
             in
            log_entry ctx stack;
            (let cost = Interp_costs.nop in
             let>>?= ctx = Gas.consume ctx cost
              in
             let stack = stack in
             log_entry ctx stack;
             log_entry ctx stack;
             log_entry ctx stack;
             log_entry ctx stack;
             log_entry ctx stack;
             log_entry ctx stack;
             log_entry ctx stack;
             log_entry ctx stack;
             log_entry ctx stack;
             log_entry ctx stack;
             log_entry ctx stack;
             log_entry ctx stack;
             return (stack, ctx))))))))))))
let tz_compiled_2 (Input ((let>>?=), (let>>=?), return, ctx, stack, ())) =
  let cost = Interp_costs.seq in
  let>>?= ctx = Gas.consume ctx cost
   in
  let cost = Interp_costs.nil in
  let>>?= ctx = Gas.consume ctx cost
   in
  let stack = ({ elements = []; length = 0 }, stack) in
  log_entry ctx stack;
  (let cost = Interp_costs.seq in
   let>>?= ctx = Gas.consume ctx cost
    in
   let cost = Interp_costs.cons_pair in
   let>>?= ctx = Gas.consume ctx cost
    in
   let (a, (b, stack)) = stack in
   let stack = ((a, b), stack) in
   log_entry ctx stack;
   (let cost = Interp_costs.nop in
    let>>?= ctx = Gas.consume ctx cost
     in
    let stack = stack in
    log_entry ctx stack;
    log_entry ctx stack;
    log_entry ctx stack;
    return (stack, ctx)))

Unbound value log_entry

Uncaught exception:
  
  Typetexp.Error(_, _, _)

Raised at file "typing/typetexp.ml" (inlined), line 92, characters 16-43
Called from file "typing/typetexp.ml", line 135, characters 2-24
Called from file "typing/typetexp.ml", line 180, characters 4-78
Called from file "typing/typecore.ml", line 3438, characters 21-60
Called from file "typing/typecore.ml", line 2323, characters 23-49
Called from file "parsing/builtin_attributes.ml", line 236, characters 14-18
Re-raised at file "parsing/builtin_attributes.ml", line 241, characters 4-13
Called from file "typing/typecore.ml", line 2290, characters 4-156
Called from file "typing/typecore.ml", line 2476, characters 18-37
Called from file "parsing/builtin_attributes.ml", line 236, characters 14-18
Re-raised at file "parsing/builtin_attributes.ml", line 241, characters 4-13
Called from file "typing/typecore.ml", line 2290, characters 4-156
Called from file "typing/typecore.ml", line 4244, characters 12-29
Called from file "typing/typecore.ml", line 2788, characters 17-88
Called from file "parsing/builtin_attributes.ml", line 236, characters 14-18
Re-raised at file "parsing/builtin_attributes.ml", line 241, characters 4-13
Called from file "typing/typecore.ml", line 2290, characters 4-156
Called from file "typing/typecore.ml", line 2418, characters 8-88
Called from file "parsing/builtin_attributes.ml", line 236, characters 14-18
Re-raised at file "parsing/builtin_attributes.ml", line 241, characters 4-13
Called from file "typing/typecore.ml", line 2290, characters 4-156
Called from file "typing/typecore.ml", line 4411, characters 10-69
Called from file "list.ml", line 92, characters 20-23
Called from file "typing/typecore.ml", line 4370, characters 4-1023
Called from file "typing/typecore.ml", line 3380, characters 8-64
Called from file "parsing/builtin_attributes.ml", line 236, characters 14-18
Re-raised at file "parsing/builtin_attributes.ml", line 241, characters 4-13
Called from file "typing/typecore.ml", line 2290, characters 4-156
Called from file "typing/typecore.ml", line 2418, characters 8-88
Called from file "parsing/builtin_attributes.ml", line 236, characters 14-18
Re-raised at file "parsing/builtin_attributes.ml", line 241, characters 4-13
Called from file "typing/typecore.ml", line 2290, characters 4-156
Called from file "typing/typecore.ml", line 4411, characters 10-69
Called from file "list.ml", line 92, characters 20-23
Called from file "typing/typecore.ml", line 4370, characters 4-1023
Called from file "typing/typecore.ml", line 3528, characters 4-86
Called from file "parsing/builtin_attributes.ml", line 236, characters 14-18
Re-raised at file "parsing/builtin_attributes.ml", line 241, characters 4-13
Called from file "typing/typecore.ml", line 2290, characters 4-156
Called from file "parsing/builtin_attributes.ml", line 236, characters 14-18
Re-raised at file "parsing/builtin_attributes.ml", line 241, characters 4-13
Called from file "list.ml", line 131, characters 32-39
Called from file "typing/typecore.ml", line 4634, characters 4-1023
Called from file "typing/typecore.ml", line 4766, characters 4-203
Called from file "typing/typemod.ml", line 2028, characters 10-56
Called from file "typing/typemod.ml", line 2289, characters 44-68
Called from file "typing/typemod.ml", line 2289, characters 44-68
Called from file "typing/typemod.ml", line 2289, characters 44-68
Called from file "typing/typemod.ml", line 2289, characters 44-68
Called from file "typing/typemod.ml", line 2289, characters 44-68
Called from file "typing/typemod.ml", line 2289, characters 44-68
Called from file "typing/typemod.ml", line 2289, characters 44-68
Called from file "typing/typemod.ml", line 2289, characters 44-68
Called from file "typing/typemod.ml", line 2297, characters 33-53
Called from file "parsing/builtin_attributes.ml", line 236, characters 14-18
Re-raised at file "parsing/builtin_attributes.ml", line 241, characters 4-13
Called from file "typing/typemod.ml" (inlined), line 2314, characters 21-46
Called from file "typing/typemod.ml", line 2427, characters 8-68
Called from file "utils/misc.ml", line 31, characters 8-15
Re-raised at file "utils/misc.ml", line 45, characters 10-56
Called from file "src/proto_alpha/lib_protocol/example/interpreter/compile.ml", line 88, characters 2-67
Re-raised at file "src/proto_alpha/lib_protocol/example/interpreter/compile.ml", line 99, characters 4-13
Called from file "src/proto_alpha/lib_protocol/example/interpreter/interp.ml" (inlined), line 498, characters 7-60
Called from file "src/proto_alpha/lib_protocol/example/interpreter/interp.ml", line 497, characters 4-92
Called from file "src/proto_alpha/lib_protocol/example/interpreter/interp.ml", line 680, characters 13-34
Called from file "src/proto_alpha/lib_protocol/example/interpreter/main.ml", line 77, characters 2-110
Called from file "src/proto_alpha/lib_protocol/example/interpreter/main.ml", line 137, characters 4-72
Called from file "src/proto_alpha/lib_protocol/example/interpreter/main.ml", line 132, characters 4-450
