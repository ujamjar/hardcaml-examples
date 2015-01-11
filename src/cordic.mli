val pi : float
module Hardware :
  functor (B : HardCaml.Comb.S) ->
    sig
      type cordic_type_t = Cordic0 | Cordic1 | Cordic2
      type cordic_gen_t =
          CordicComb of cordic_type_t
        | CordicSeq of cordic_type_t
        | CordicIter of cordic_type_t
      val cordic :
        int ->
        cordic_gen_t ->
        int ->
        int ->
        (B.t -> B.t) ->
        B.t -> B.t -> B.t -> B.t -> B.t -> B.t * B.t * B.t * B.t
    end
module Software :
  sig
    val icordic0 :
      int -> int -> float -> float -> float -> float -> float * float * float
    val icordic1 :
      int -> int -> float -> float -> float -> float -> float * float * float
    val icordic2 :
      int -> int -> float -> float -> float -> float -> float * float * float
    val cordic0 :
      int -> float -> float -> float -> float -> float * float * float
    val cordic1 :
      int -> float -> float -> float -> float -> float * float * float
    val cordic2 :
      int -> float -> float -> float -> float -> float * float * float
    val x : 'a * 'b * 'c -> 'a
    val y : 'a * 'b * 'c -> 'b
    val z : 'a * 'b * 'c -> 'c
    val invGain1 : int -> float
    val invGain2 : int -> float
    val prerotate0 : float -> float -> float -> float * float * float
    val prerotate1 : float -> float -> float -> float * float * float
    val prerotate : 'a -> 'b -> 'c -> 'a * 'b * 'c
    val mul : int -> float -> float -> float
    val div : int -> float -> float -> float
    val atan : int -> float -> float
    val sincos : int -> float -> float * float
    val tan : int -> float -> float
    val asin : int -> float -> float
    val magphase : int -> float -> float -> float * float
    val polar_to_rect : int -> float -> float -> float * float
    val sinhcosh : int -> float -> float * float
    val tanh : int -> float -> float
    val atanh : int -> float -> float
    val log : int -> float -> float
    val sqrt : int -> float -> float
    val exp : int -> float -> float
  end
