module Make(B : HardCaml.Comb.S) : sig

    (** full adder (3 to 2 compressor) *)
    val fa : B.t -> B.t -> B.t -> B.t * B.t
    (** half adder *)
    val ha : B.t -> B.t -> B.t * B.t
    (** 1 bit subtractor *)
    val fs : B.t -> B.t -> B.t -> B.t * B.t

    (** various prefix addition algorithms *)
    module Prefix : sig

        type 'a f = ('a -> 'a -> 'a) -> 'a list -> 'a list
        
        (* simple serial prefix structure.  used for testing *)
        val serial : 'a f

        (* Sklansky's parallel prefix structure.  high fanout *)
        val sklansky : 'a f

        (* Brent-Kung parallel prefix structure.  lower fanout, more hardware *)
        val brent_kung : 'a f

        (* Kooge-Stone parallell prefix structure.  Large but fast *)
        val kogge_stone : 'a f

        (* performs addition given one of the prefix structures.  width
         * of arguments must be the same and also a power of 2 (though that
         * strictly depends on which prefix structure is used - it
         * could easily be fixed). *)
        val add : (B.t*B.t) f -> B.t -> B.t -> B.t -> B.t

    end

end

