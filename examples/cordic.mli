(** Coordinate rotation digital computer.

  CORDIC is an iterative shift-add algorithm for computing trig and 
  hyperbolic functions like sin, cosh, atan etc.
 
  Generally it requires 3 adders, 2 barrel shifters and a fairly small
  lookup table.
  
  {b Gain}

  The [x] and [y] outputs from the CORDIC are (usually) scaled by the 
  CORDIC gain.  The exact value of the gain depends on the number of 
  iterations performed, but tends towards about 1.647.

  For some functions the gain can be adjusted for by altering input
  constants.  Where it cannot, and it's a problem, it should be removed
  with a constant multiplier.
  
  {b Hyperbolic mode}

  In hyperbolic mode the standard iteration scheme needs to be adjusted.
  Generally the iterations run from [0,1,...,(iters-1)].  In hyperbolic 
  mode the iterations are [1,2,..,4,4,...13,13,...].
  That is they start at 1 and iterations [3k+1] (starting with k=4, then 13,
  40, 121 etc) are repeated.

  The hardware designs require a fixed number of iterations regardless of
  the mode.  Therefore the number of iterations specified is exactly the 
  number run regardless of mode (indices are modified internally in 
  hyperbolic mode).  Some care might need to be taken to not stop
  processing in hyperbolic mode on one of the double iterations to ensure
  convergence.

*)

(** Specifcies the cordic update equations *)
type system = 
  | Circular 
  | Linear 
  | Hyperbolic 

(** iteration mode *)
type mode = 
  | Rotation 
  | Vectoring 
  | Inverse of float

val atanh : float -> float

val cordic_iter : iters:int -> f:(int -> int -> 'a -> 'a) -> 'a -> 'a

(** Cordic gain over given number of iterations *)
val gain : iters:int -> float

(** Cordic hyperbolic gain over given number of iterations *)
val gainh : iters:int -> float

(** {2 Reference implementation} *)
module Double : sig

  (** Main cordic calculation function *)
  val cordic : 
    system:system -> mode:mode -> iters:int -> 
    x:float -> y:float -> z:float -> 
    float * float * float  

  (** Specific functions implementated with cordic.
      The gain is removed from the result *) 
  module Fns(I : sig val iters : int end) : sig
    val gain : float
    val gainh : float
    val cos_sin : float -> float * float
    val polar_to_rect : float -> float -> float * float
    val rotate_vector : float -> float -> float -> float * float
    val atan : float -> float
    val atan2 : float -> float -> float
    val rect_to_polar : float -> float -> float * float
    val asin : float -> float
    val mul : float -> float -> float
    val div : float -> float -> float
    val cosh_sinh : float -> float * float
    val atanh : float -> float
  end

end

open HardCaml

(** specifiy fixed point precision *)
module type Fixpt = sig
  (** overall width *)
  val w : int
  
  (** bits after fixed point *)
  val fp : int
end

(** Fully unrolled combinatorial or pipelined CORDIC implementation.
    Suitable for implementation with [bits] (combinatorial only) 
    or [signals]. *)
module Unrolled(B : Comb.S)(P : Fixpt) : sig
  open B

  val step : 
    x:t -> xsft:t -> y:t -> ysft:t -> z:t -> 
    d:t -> m:t -> e:t -> 
    t * t * t

  (** system *)

  val circular : t
  val hyperbolic : t
  val linear : t

  (** modes *)
  
  val rotation : t
  val vectoring : t
  val inverse : t

  (** CORDIC implementation. [pipe] should be set to [reg r_spec enable] to enable
      piplineing. Otherwise the circuit is combinatorial. *)
  val cordic : 
    ?pipe:(t -> t) ->
    system:t -> mode:t -> iters:int -> c:t -> 
    x:t -> y:t -> z:t -> 
    t * t * t

end

(** Iterative CORDIC *)
module Iterative(P : Fixpt) : sig
  open Signal.Comb

  val cordic : 
    ld:t -> system:t -> mode:t -> iters:int -> c:t -> 
    x:t -> y:t -> z:t -> 
    t * t * t

end

module Design : HardCamlFramework.Framework.Design

