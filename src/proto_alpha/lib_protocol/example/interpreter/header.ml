open Migrate_parsetree

let header =
  [%str
    type 'a input = Input of 'a

    open Tezos_protocol_environment_alpha.Environment
    open Pervasives
    open Tezos_raw_protocol_alpha.Script_typed_ir
    open Tezos_raw_protocol_alpha.Alpha_context
    module Script_ir_translator = Tezos_raw_protocol_alpha.Script_ir_translator

    module Interp_costs =
      Tezos_raw_protocol_alpha.Michelson_v1_gas.Cost_of.Interpreter

    module Script_interpreter = Tezos_raw_protocol_alpha.Script_interpreter]
