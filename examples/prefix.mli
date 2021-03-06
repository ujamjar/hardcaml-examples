(** Parallel prefix networks *)

type 'a f = ('a -> 'a -> 'a) -> 'a list -> 'a list

(** Simple serial prefix structure.  Used for testing *)
val serial : 'a f

(** Sklansky's parallel prefix structure.  High fanout *)
val sklansky : 'a f

(** Brent-Kung parallel prefix structure.  Lower fanout, more hardware *)
val brent_kung : 'a f

(** Kooge-Stone parallel prefix structure.  Large but fast *)
val kogge_stone : 'a f

(** [to_dot print_string kogge_stone 8] generates a dot file for graphviz 
    showing the networks structure for a particular number of inputs.  
    View with [dot file.dot -Tx11]. *)
val to_dot : (string -> unit) -> 
  ([ `input of int | `node of 'a * 'a * int | `output of 'a * int ] as 'a) f -> 
  int -> unit

(** Parallel prefix adder *)
module Adder(B : HardCaml.Comb.S) : sig

  (** performs addition given one of the prefix structures.  Width
      of arguments must be the same.  For the kooge_stone structure
      the width should also be a power of 2. *)
  val add : (B.t*B.t) f -> B.t -> B.t -> B.t -> B.t

end

module Design : HardCamlFramework.Framework.Design

