module Param = struct
  type t =
    | Flag of bool 
    | Int of int 
    | Float of float
    | String of string 
    | File of string 
    | Symbol of string list * string
    | Int_list of int list
    | Float_list of float list
  type p = t * string

  let get_bool = function Flag i, _ -> i | _ -> failwith "Expecting Flag"
  let get_int = function Int i, _ -> i | _ -> failwith "Expecting Int"
  let get_float = function Float i, _ -> i | _ -> failwith "Expecting Float"
  let get_string = 
    function String i, _ | File i, _ -> i | Symbol(_, i), _ -> i | _ -> failwith "Expecting String"
  let get_int_list = function Int_list i, _ -> i | _ -> failwith "Expecting Int_list"
  let get_float_list = function Float_list i, _ -> i | _ -> failwith "Expecting Float_list"

  type status = Ok | Error of string list
 
  let (>>) a b = 
    match a, b with
    | Ok,Ok -> Ok
    | Error _, Ok -> a
    | Ok, Error _ -> b
    | Error e0, Error e1 -> Error(e0 @ e1)

  let gt ((_,n) as v) x = if get_int v > x then Ok else Error([n^" must be > " ^ string_of_int x])
  let lt ((_,n) as v) x = if get_int v < x then Ok else Error([n^" must be < " ^ string_of_int x])
  let pow2 ((_,n) as v) = 
    let x = get_int v in 
    if ((x-1) land x) = 0 then Ok
    else Error([n^" must be a power of 2"])

end

module type Params = sig
  include HardCaml.Interface.S
  val params : Param.p t
end

module Params_none = struct
  include HardCaml.Interface.Empty
  let params = None
end

module Display = struct
  type t = B | U | S | H | F of int
  let default (n,_) = n,B
  let bin (n,_) = n,B
  let uint (n,_) = n,U
  let sint (n,_) = n,S
  let hex (n,_) = n,H
  let fixed prec (n,_) = n,F prec
end

module type Design = sig
  val name : string
  val desc : string
  module Hw_config : Params
  module Tb_config : Params
  val validate : Param.p Hw_config.t -> Param.p Tb_config.t -> Param.status
  module Make
    (B : HardCaml.Comb.S)
    (H : Params with type 'a t = 'a Hw_config.t)
    (T : Params with type 'a t = 'a Tb_config.t) : sig
    module I : HardCaml.Interface.S
    module O : HardCaml.Interface.S
    val wave_cfg : (string * Display.t) list option
    val hw : HardCaml.Signal.Comb.t I.t -> HardCaml.Signal.Comb.t O.t
    val tb : B.t HardCaml.Cyclesim.Api.cyclesim -> 
      B.t ref I.t -> B.t ref O.t -> B.t ref O.t -> unit
  end
end

module Design_none = struct
  let name = ""
  let desc = ""
  module Hw_config = Params_none
  module Tb_config = Params_none
  let validate _ _ = Param.Ok
  module Make
    (B : HardCaml.Comb.S)
    (H : Params with type 'a t = 'a Hw_config.t)
    (T : Params with type 'a t = 'a Tb_config.t) = struct
    module I = HardCaml.Interface.Empty
    module O = HardCaml.Interface.Empty
    let wave_cfg = None
    let hw _ = O.None
    let tb _ _ _ _ = ()
  end
end
