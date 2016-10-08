open HardCamlFramework.Framework

module Make(D : Design) : sig

  type main_message = 
    | MVerilog of Param.p D.Hw_config.t
    | MVhdl of Param.p D.Hw_config.t
    | MSimulate of (Param.p D.Hw_config.t * Param.p D.Tb_config.t)

  type ww_message = 
    | WVerilog of string
    | WVhdl of string
    | WSimulate of HardCamlJS.Wave.wave array
    | WError of string list

  val str_of_main : main_message -> Js.js_string Js.t
  val main_of_str : Js.js_string Js.t -> main_message 

  val str_of_ww : ww_message -> Js.js_string Js.t
  val ww_of_str : Js.js_string Js.t -> ww_message 


end
