(** Coordinate rotation dIgitial computer *)

type system = 
  | Circular 
  | Linear 
  | Hyperbolic 

type mode = 
  | Rotation 
  | Vectoring 
  | Inverse of float

module Double : sig

  val atanh : float -> float
  val gain : iters:int -> float
  val gainh : iters:int -> float

  val cordic : 
    system:system -> mode:mode -> iters:int -> 
    x:float -> y:float -> z:float -> 
    float * float * float  

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

module type Fixpt = sig
  val w : int
  val fp : int
end

module Unrolled(B : Comb.S)(P : Fixpt) : sig
  open B
  val step : 
    x:t -> y:t -> z:t -> 
    d:t -> m:t -> i:int -> e:t -> 
    t * t * t

  (* system *)
  val circular : t
  val hyperbolic : t
  val linear : t

  (* modes *)
  val rotation : t
  val vectoring : t
  val inverse : t

  val cordic : 
    ?pipe:(t -> t) ->
    system:t -> mode:t -> iters:int -> c:t -> 
    x:t -> y:t -> z:t -> 
    t * t * t
end

module Iterative(P : Fixpt) : sig
  open Signal.Comb
  val cordic : 
    ld:t -> system:t -> mode:t -> iter:t -> c:t -> 
    x:t -> y:t -> z:t -> 
    t * t * t
end

module Design : Framework.Design

