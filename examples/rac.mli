(** {1 ROM-accumulator} *)

(** Evaluate [ a0.x0 + a1+x1 + ... + an.xn ] where [a] is constant
    using distributed arithmetic.

    The architecture uses a rom and add/shift circuit and requires
    no multipliers. *)

(** {2 Architecture} 
 
    The ROM-accumulator extends the idea of multplication by adding and
    shifting.  [a0.x0] can be the calculated by testing each bit of x and
    adding a to a (shifting) accumulator.  Similarly [a0.x0 + a1.x1] can
    be calculated by forming an address vector from each successive
    bit, [b],  of x0 and x1 ie

    [[x1.[b]; x0.[b]]]

    A pre-calculated rom stores all possibile additions of a0 and a1
 
    [[ 0; a0; a1; a0+a1 ]]

    Given n coefficients the required rom will be of size 2^n.

    The address is looked up in the rom and added to (or subtracted from)
    a shifted accumulator (subtraction happens when processing the msb of
    the input values, otherwise addition).

    The RAC can be operated in two modes which we called [integer] and [fixed].
    
    In [integer] mode the coefficients and accumulator are treated as 
    integers, the internal shift registers shift out the msb down to the 
    lsb, and the accumulator shifts to the left.  This in turn specifies 
    an exact result, so long as the accumulator is large enough to hold it.

    In fixed mode the coefficients and accumulator are treated as fixed point
    values, the shift regiters shift out the lsb up to the msb and the 
    accumulator shifts to the right.

 *)

open HardCaml

(** Constructs the rom from the coefficients *)
module Rom(B : Comb.S) : sig
  (** reduce a signed value to the smallest possible precision *)
  val norm : B.t -> B.t

  (** construct the rom *)
  val make : B.t list -> B.t list
end

open Signal.Comb

(** Build the rom accumulator *)
val rac : 
  fixed:bool ->
  accbits:int -> romshift:int -> 
  en:t -> ld:t -> addsub:t -> 
  romcoefs:t list -> x:t list -> t

(** {2 RAC design } *)

module Design : HardCamlFramework.Framework.Design


