open HardCaml
open Utils

type dirn = Up | Down

module type Swap = sig
  type t
  val compare_swap : dirn -> t -> t -> t * t
  val delay : t -> t
end

(* for testing - sort a list of integers (slowly!) *)
module SwapInt = struct
  type t = int
  (* just the identity function *)
  let delay d = d
  let compare_swap dirn a b = 
    match dirn with
    | Up -> if a < b then a,b else b,a
    | Down -> if a < b then b,a else a,b
end

module SwapHw(B : HardCaml.Comb.S) = struct
  open B
  type t = B.t
  (* no delay *)
  let delay d = d
  (* swap using a comparator and muxs *)
  let compare_swap dirn a b =
    let c = match dirn with Up -> a >: b | Down -> b >: a in
    mux2 c b a, mux2 c a b
end

module SwapReg = struct
  (* since we are using registers, we must use signals *)
  open HardCaml.Signal
  module S = SwapHw(Comb)
  include S
  (* register each stage *)
  let delay d = Seq.(reg r_sync Comb.empty d)
end

module type Sort = sig
  type t
  val sort : t list -> t list
end 

(* split list into 2 halves *)
let halve l = 
  let length = List.length l in
  lselect l 0 (length/2 - 1), lselect l (length/2) (length-1)

module Bitonic(S : Swap) = struct

  type t = S.t

  let compare dirn l = 
    let l0,l1 = halve l in
    let l = List.map2 (S.compare_swap dirn) l0 l1 in
    (List.map fst l) @ (List.map snd l)

  let rec merge dirn l = 
    let len = List.length l in
    if len = 1 then l
    else
      let l = List.map S.delay (compare dirn l) in
      let l0,l1 = halve l in
      let l0 = merge dirn l0 in
      let l1 = merge dirn l1 in
      l0 @ l1

  let rec sort' dirn l = 
    let len = List.length l in
    if len = 1 then l
    else
      let l0,l1 = halve l in
      let l0 = sort' Up l0 in
      let l1 = sort' Down l1 in
      merge dirn (l0 @ l1)

  let sort = sort' Up

end

module OddEvenMerge(S : Swap) = struct

  type t = S.t

  let oddeven l = 
    let rec f l e o = 
      match l with
      | [] -> e,o
      | e'::o'::t -> f t (e'::e) (o'::o)
      | _ -> failwith "expecting even length list"
    in
    let e,o = f l [] [] in
    List.rev e, List.rev o

  let rec merge l = 
    let rec compare_list o e = 
      match o,e with
      | o1::[],[] ->
        [o1]
      | o1::ot,e2::et ->
        let a0,a1 = S.compare_swap Up o1 e2 in
        a0::a1::compare_list ot et
      | _ -> failwith "compare list failed"
    in
    match l with
    | a0::a1::[] ->
      let a0,a1 = S.compare_swap Up a0 a1 in
      List.map S.delay [a0;a1]
    | _ -> 
      let e,o = oddeven l in
      let e,o = merge e, merge o in
      List.map S.delay (List.hd e :: compare_list o (List.tl e))

  let rec sort l = 
    if List.length l > 1 then
      let l,h = halve l in
      merge (sort l @ sort h)
    else
      l

end

module Design = struct
  open Framework
  open Param

  let name = "sorting_network"
  let desc = "*Bitonic* and *Odd-even* merge sorting networks."

  module Hw_config = struct
    include interface bits size network pipeline end
    let params = {
      bits = Int 8, "Data width";
      size = Int 8, "Size of sorting network";
      network = Symbol(["bitonic";"odd-even"], "bitonic"), "Type of sorting network";
      pipeline = Flag false, "Pipeline network";
    }
  end

  module Tb_config = struct
    include interface cycles end
    let params = C.({ cycles = Int 32, "Number of cycles to test" })
  end

  let validate hw tb = 
    Hw_config.(gt hw.bits 0 >> gt hw.size 1 >> pow2 hw.size) >>
    Tb_config.(gt tb.cycles 0)

  module Make
    (B : Comb.S)
    (H : Params with type 'a t = 'a Hw_config.t)
    (T : Params with type 'a t = 'a Tb_config.t) = struct

    open Hw_config
    open Tb_config
    let bits = get_int H.params.bits
    let size = get_int H.params.size
    let network = get_string H.params.network
    let pipeline = get_bool H.params.pipeline
    let cycles = get_int T.params.cycles

    module I = interface d{size}[bits] end
    module O = interface q{size}[bits] end

    let wave_cfg = 
      Some(["clock",Display.B; "clear", Display.B] @
           I.(to_list (map Display.hex t)) @ 
           O.(to_list (map Display.hex t)))  

    let hw i = 
      (* apply functors according to configuration *)
      let module Swap = 
        (val 
          (if pipeline then 
            (module SwapReg : Swap with type t = Signal.Comb.t) 
          else 
            (module SwapHw(Signal.Comb)))) 
      in
      let module Sort = 
        (val
          (if network = "bitonic" then
            (module Bitonic(Swap) : Sort with type t = Signal.Comb.t)
          else
            (module OddEvenMerge(Swap))))
      in
      O.({ q = Sort.sort i.I.d })

    let tb sim i o _ = 
      let open I in
      let open O in
      let module S = Cyclesim.Api in
      S.reset sim;
      for j=0 to cycles-1 do
        for k=0 to size-1 do
          (List.nth i.d k) := B.srand bits;
        done;
        S.cycle sim;
      done

  end
end

