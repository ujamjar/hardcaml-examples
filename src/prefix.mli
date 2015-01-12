
type 'a f = ('a -> 'a -> 'a) -> 'a list -> 'a list

(** simple serial prefix structure.  used for testing *)
val serial : 'a f

(** Sklansky's parallel prefix structure.  high fanout *)
val sklansky : 'a f

(** Brent-Kung parallel prefix structure.  lower fanout, more hardware *)
val brent_kung : 'a f

(** Kooge-Stone parallell prefix structure.  Large but fast *)
val kogge_stone : 'a f

(** Parallel prefix adder *)
module Adder(B : HardCaml.Comb.S) : sig

  (** performs addition given one of the prefix structures.  Width
      of arguments must be the same.  For the kooge_stone structure
      the width should also be a power of 2. *)
  val add : (B.t*B.t) f -> B.t -> B.t -> B.t -> B.t

end

