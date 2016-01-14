(** Linear feedback shift registers *)

(** maximal length LFSRs with 2 .. 168 taps *)
val taps : int list array

(** create counterpart taps *)
val counterpart : int list -> int list

module type S = sig
  include HardCaml.Comb.S
  val lfsr : (t -> t -> t) -> int list -> t -> t
end

(** Galois lfsr implementation *)
module Galois(B : HardCaml.Comb.S) : (S with type t = B.t)

(** Fibonacci lfsr implementation *)
module Fibonacci(B : HardCaml.Comb.S) : (S with type t = B.t)

module Make(B : S) : sig

  (** lfsr generator *)
  val lfsr : (B.t -> B.t -> B.t) -> int list -> B.t -> B.t

  (** xor based lfsr - all zeros state is illegal *)
  val lfsr_xor : int list -> B.t -> B.t

  (** xnor based lfsr - all ones state is illegal *)
  val lfsr_xnor : int list -> B.t -> B.t

  (** test functions for generating the complete lfsr sequences *)
  
  val seq : (B.t -> B.t) -> B.t -> B.t list
  val seq_xor : int -> B.t list
  val seq_xnor : int -> B.t list

end

module Design : HardCamlFramework.Framework.Design
