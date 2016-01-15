open HardCamlFramework.Framework

module Make(D : Design) = struct

  type main_message = 
    | MVerilog of Param.p D.Hw_config.t
    | MVhdl of Param.p D.Hw_config.t
    | MSimulate of (Param.p D.Hw_config.t * Param.p D.Tb_config.t)

  type ww_message = 
    | WVerilog of string
    | WVhdl of string
    | WSimulate of HardCamlJS.Wave.wave array
    | WError of string list

  let str_of_main (m : main_message) = Json.output m
  let main_of_str s : main_message = Json.unsafe_input s

  let str_of_ww (m : ww_message) = Json.output m
  let ww_of_str s : ww_message = Json.unsafe_input s

end

