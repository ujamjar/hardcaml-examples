(** Single bit adder/subtractor components *)
module Make(B : HardCaml.Comb.S) : sig

  (** full adder (3 to 2 compressor) *)
  val fa : B.t -> B.t -> B.t -> B.t * B.t

  (** half adder *)
  val ha : B.t -> B.t -> B.t * B.t

  (** subtractor *)
  val fs : B.t -> B.t -> B.t -> B.t * B.t

end

