module Param = struct
  type t =
    | Flag of bool 
    | Int of int 
    | String of string 
  let get_flag = function Flag i -> i | _ -> failwith "Expecting Flag"
  let get_int = function Int i -> i | _ -> failwith "Expecting Int"
  let get_string = function String i -> i | _ -> failwith "Expecting String"
end

module type Params = sig
  module C : HardCaml.Interface.S
  val params : Param.t C.t
  val desc : string C.t
  val validate : Param.t C.t -> string option
end

module type Design = sig
  val name : string
  val desc : string
  module Hw_config : Params
  module Tb_config : Params
  module Make
    (B : HardCaml.Comb.S)
    (H : Params with type 'a C.t = 'a Hw_config.C.t)
    (T : Params with type 'a C.t = 'a Tb_config.C.t) : sig
    module I : HardCaml.Interface.S
    module O : HardCaml.Interface.S
    val hw : HardCaml.Signal.Comb.t I.t -> HardCaml.Signal.Comb.t O.t
    val tb : B.t HardCaml.Cyclesim.Api.cyclesim -> B.t ref I.t -> B.t ref O.t -> unit
  end
end


