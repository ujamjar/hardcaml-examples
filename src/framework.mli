(** HardCaml examples framework *)

(** {2 Overview} *)

(**
  The hardware design examples all provide an interface using the [Design]
  module type provided by this module.

  Each hardware module will provide a description of itself, 
  the parameters required to configure it and functions to instantiate the
  hardware and run a testbench.

  The framework will collect configuration parameters from the user,
  create the hardware design and run some user defined tasks such as RTL 
  generation, simulation etc.

  Implementations of the framework suitable for a command line application
  or javascript based webpage will be provided.

*)

(** {2 Implementation} *)

(** Basic configuration parameters *)
module Param : sig
  type t =
    | Flag of bool 
    | Int of int 
    | String of string 
  val get_flag : t -> bool
  val get_int : t -> int
  val get_string : t -> string
end

(** A set of configuration parameters with default values and textual descriptions *)
module type Params = sig
  (** Configuration parameters *)
  module C : HardCaml.Interface.S
  
  (** Parameter values *)
  val params : Param.t C.t
  
  (** Parameter descriptions *)
  val desc : string C.t
  
  (** Validate parameters - return None if OK or Some(error_string) otherwise *)
  val validate : Param.t C.t -> string option
end

(** A hardware design to be plugged into the framework *)
module type Design = sig
  (** name of the design *)
  val name : string

  (** description of the design *)
  val desc : string

  (** hardware core configuration parameters *)
  module Hw_config : Params
  
  (** testbench configuration parameters *)
  module Tb_config : Params

  (** Functor called with the users configuration parameters *)
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

