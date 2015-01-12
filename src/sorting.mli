(** {2 Sorting networks} 
 
  A sorting network arranges a fixed configuration of compare/swap
  operations to sort data.  The static nature of the network means
  they can be easily implemented in hardware.

  This module implements [Bitonic] and [Odd-Even merge] sort networks.

  The actual data to be sorted, and low level compare swap operations
  are seperated from the network itself allowing various configurations

  - sort lists of integers
  - [Bits] or [Signals] combinatorial networks
  - pipelined
  - configure the sorts direction (invert the compare operation)
  - sort arbirary data according to some key

  The networks require the inputs be a power of two.

 *)

(** {3 data path} *)

(** internal sorting direction *)
type dirn

(** A [Swap] module defines the datapath of the sorting network.

    The functors which implement the specific sorting network arrange
    values of type [t] with an appropriate set of [compare_swap] operations
    to create the sorted outputs *)
module type Swap =
sig
  (** type of data to be sorted *)
  type t

  (** The main sorting operation.  [compare_swap Up a b] will 
      return [(min a b, max a b)].  [compare_swap Down a b] will
      sort the opposite way. *)
  val compare_swap : dirn -> t -> t -> t * t

  (** {i hardware implementation only} delays inserted appropriately 
      into the datapath so each stage can be pipelined *)
  val delay : t -> t

end

(** Sort lists of integers *)
module SwapInt : (Swap with type t = int)

(** Sort in hardware ([Bits] or [Signals]) *)
module SwapHw(B : HardCaml.Comb.S) : (Swap with type t = B.t)

(** Pipelined sort *)
module SwapReg : (Swap with type t = HardCaml.Signal.Comb.t)

(** {3 Sorting network implementation} *)

(** A sorting network *)
module type Sort = 
sig
  (** type of data to be sorted (see {! Swap.t}) *)
  type t

  (** sorting function *)
  val sort : t list -> t list
end 

(** Bitonic sort *)
module Bitonic(S : Swap) : (Sort with type t = S.t)

(** Odd-even merge sort *)
module OddEvenMerge(S : Swap) : (Sort with type t = S.t)

