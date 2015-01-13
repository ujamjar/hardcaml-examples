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

    The address is looked up in the rom and added to a shifted accumulator

    [ acc = (acc >> 1) + rom.[addr] ]
    
    Given n coefficients the rom will be of size 2^n.

 *)

open HardCaml

(** The coefficients are represented using the shallow embedding to allow
    arbitrary precision *)
type coef = Bits.Comb.IntbitsList.t

(** Constructs the rom from the coefficients *)
module Rom(B : Comb.S) : sig
  (** reduce a signed value to the smallest possible precision *)
  val norm : coef -> coef

  (** construct the rom *)
  val make : coef list -> B.t list
end

open Signal.Comb

(** Build the rom accumulator *)
val rac : 
  accbits:int -> romshift:int -> 
  en:t -> ld:t -> last:t -> 
  coefs:coef list -> x:t list -> t

(** {2 RAC design } *)

module Design : Framework.Design


