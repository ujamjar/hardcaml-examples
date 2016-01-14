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
    | Float of float
    | String of string 
    | File of string 
    | Symbol of string list * string
    | Int_list of int list
    | Float_list of float list
  
  type p = t * string

  (** get congfiguration values *)
  val get_bool : p -> bool
  val get_int : p -> int
  val get_float : p -> float
  val get_string : p -> string
  val get_int_list : p -> int list
  val get_float_list : p -> float list

  (** validation status *)
  type status = Ok | Error of string list
  val (>>) : status -> status -> status

  val gt : p -> int -> status
  val lt : p -> int -> status
  val pow2 : p -> status
end

(** A set of configuration parameters with default values and textual descriptions *)
module type Params = sig
  (** Configuration parameters *)
  include HardCaml.Interface.S
  
  (** Parameter values *)
  val params : Param.p t
end

(** no parameters *)
module Params_none : Params

(** control of waveterm display *)
module Display : sig
  type t = B | U | S | H | F of int
  val default : (string * 'a) -> (string * t)
  val bin : (string * 'a) -> (string * t)
  val uint : (string * 'a) -> (string * t)
  val sint : (string * 'a) -> (string * t)
  val hex : (string * 'a) -> (string * t)
  val fixed : int -> (string * 'a) -> (string * t)
end

(** A hardware design to be plugged into the framework.
 
    Initially we provide a name, description and set of configuration
    parameters.  The framework will then construct the Make functor 
    passing it the user selected configuration values.  This allows
    the modules I and O interfaces to be consturcted based on user values.
 
    The framework then uses the [hw] and [tb] functions to generate netlists
    and execute the testbench.

 *)
module type Design = sig
  (** name of the design *)
  val name : string

  (** description of the design *)
  val desc : string

  (** hardware core configuration parameters *)
  module Hw_config : Params
  
  (** testbench configuration parameters *)
  module Tb_config : Params

  val validate : Param.p Hw_config.t -> Param.p Tb_config.t -> Param.status

  (** Functor called with the users configuration parameters *)
  module Make
    (B : HardCaml.Comb.S)
    (H : Params with type 'a t = 'a Hw_config.t)
    (T : Params with type 'a t = 'a Tb_config.t) : sig

    (** Input interface *)
    module I : HardCaml.Interface.S

    (** Output interface *)
    module O : HardCaml.Interface.S

    (** [waveterm] display configuration.  Sets both display order and
        how values are rendered.  For example; [Some(I.(map hex t) @ O.(map bin t))] 
        display inputs as hex followed by outputs in binary.  The order given within 
        the [I] and [O] interfaces is followed.  *)
    val wave_cfg : (string * Display.t) list option

    (** Construct the hardware design *)
    val hw : HardCaml.Signal.Comb.t I.t -> HardCaml.Signal.Comb.t O.t

    (** Run a testbench *)
    val tb : B.t HardCaml.Cyclesim.Api.cyclesim -> 
      B.t ref I.t -> B.t ref O.t -> B.t ref O.t -> unit
  end
end

(** An emtpy design (does nothing, so completely useless!) *)
module Design_none : Design

