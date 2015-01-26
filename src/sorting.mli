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

  {4 Idea; number of elements to sort}

  The network structure requires the number of elements to sort is
  a power of two.
  
  An easy way to get round this limitiation is pad an arbitrary
  length list with the maximum value to the nearest power of 2 length,
  then drop these values from the output (since they always will be 
  sorted to the top of the list).  Because these values are then
  unreferenced a portion of the network will get removed automatically.

  That said a potentially large number of internal nodes will still be
  left behind and we know something about them - they are bigger than
  all the other values being sorted.  We could therefore encode
  this into the data being sorted with an option data type where none 
  represents a value bigger than all others;

{[
  type t = data_type option
]}

  and a compare function

{[
  let compare_swap dir a b = 
    match dir, a, b with
    | _, None, None -> a,b
    | Up, None, Some d | _, Some d, None -> Some d, None
    | Down, None, Some d | _, Some d, None -> None, Some d
    | .... (* and so on *)
]}

  It would be very interesting to see if this could evaluate an optimal sorting
  network.

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

(** {2 Design} *)

module Design : Framework.Design


