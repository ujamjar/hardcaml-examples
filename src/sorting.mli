module type Swap =
sig
    type t
    val up : t
    val down : t
    val delay : t -> t
    val compare_swap : t -> t -> t -> t * t
end

module type Sort = 
sig
    type t
    val sort : t list -> t list
end 

module SwapInt : (Swap with type t = int)
module SwapHw(B : HardCaml.Comb.S) : (Swap with type t = B.t)

module Bitonic(S : Swap) : (Sort with type t = S.t)
module BitonicInt : (Sort with type t = int)
module BitonicHw : (Sort with type t = HardCaml.Signal.Comb.t)

module OddEvenMerge(S : Swap) : (Sort with type t = S.t)
module OddEvenMergeInt : (Sort with type t = int)
module OddEvenMergeHw : (Sort with type t = HardCaml.Signal.Comb.t)

