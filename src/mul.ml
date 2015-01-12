(*
  From wikipedia:

  The Wallace/Dadda tree has three steps:

  1. Multiply (that is - AND) each bit of one of the arguments, by each bit of the other, yielding n2 results. 
     Depending on position of the multiplied bits, the wires carry different weights, for example wire of bit 
     carrying result of a2b3 is 32.
  2. Reduce the number of partial products to two by layers of full and half adders.
  3. Group the wires in two numbers, and add them with a conventional adder.

  They differ in stage 2 in how the results are combined

  wallace:
    * Take any three wires with the same weights and input them into a full adder. The result will be an output 
      wire of the same weight and an output wire with a higher weight for each three input wires.
    * If there are two wires of the same weight left, input them into a half adder.
    * If there is just one wire left, connect it to the next layer.

  dadda
    * Take any 3 wires with the same weights and input them into a full adder. The result will be an output wire 
      of the same weight and an output wire with a higher weight for each 3 input wires.
    * If there are 2 wires of the same weight left, and the current number of output wires with that weight is 
      equal to 2 (modulo 3), input them into a half adder. Otherwise, pass them through to the next layer.
    * If there is just 1 wire left, connect it to the next layer.

    However, when a layer carries at most 3 input wires for any weight, that layer will be the last one. 
    In this case, the Dadda tree will use half adder more aggressively (but still not as much as in a 
    Wallace multiplier), to ensure that there are only two outputs for any weight. Then, the second rule 
    above changes as follows:

    * If there are 2 wires of the same weight left, and the current number of output wires with that weight is 
    equal to 1 or 2 (modulo 3), input them into a half adder. Otherwise, pass them through to the next layer.
*)

open HardCaml

module Make(B : Comb.S) = 
struct 

  open B
  module A = Add.Make(B)

  (** partial full adder *)
  let pfa x y c = 
    let p = x ^: y in
    let g = x &: y in
    let s = p ^: c in
    p, g, s

  let unzip l = 
    let rec unzip l0 l1 l = 
      match l with
      | [] -> List.rev l0, List.rev l1
      | (a,b)::t -> unzip (a::l0) (b::l1) t
    in
    unzip [] [] l

  (** (wallace/dadda) multiply each bit in "a" by each bit in "b" sorted by weight. *)
  let build_weights a b =
    let wa = width a in
    let wb = width b in
    let max_weight = wa + wb - 2 in

    (* get wires of each weight *)
    let rec build_weights w =
      if w > max_weight then []
      else
        let make_weight w = 
          List.map (fun x -> match x with None -> failwith "" | Some x -> x)
            (List.filter ((<>) None) 
               (List.concat 
                  (List.map (fun i -> 
                       List.map (fun j -> 
                           if i+j = w then Some (bit a i &: bit b j) 
                           else None) 
                         (Utils.range wb)
                     ) (Utils.range wa))
               ) 
            )
        in
        make_weight w :: build_weights (w+1) in 
    build_weights 0

  (** (wallace/dadda) final sum of weights *)
  let build_weight_sum w = 
    let rec b0 w a b =
      match w with
      | [] -> concat a, concat b
      | hd :: tl ->
        match hd with
        | a'::b'::_ -> b0 tl (a'::a) (b'::b)
        | a'::_ -> b0 tl (a'::a) (gnd::b)
        | _ -> b0 tl (gnd::a) (gnd::b) 
    in
    let a, b = b0 w [] [] in
    a +: b

  (** wallace tree multiplier *)
  let wallace a b = 
    (* wallace tree single weight optimiser *)
    let rec optimise_weight w x xn = 
      match w with
      | a::b::c::tl -> let c,s = A.fa a b c in optimise_weight tl (s::x) (c::xn)
      | a::b::tl -> let c,s = A.ha a b in optimise_weight tl (s::x) (c::xn)
      | a::tl -> optimise_weight tl (a::x) xn
      | [] -> x, xn 
    in

    (* optimise all current weights at current reduction layer *)
    let optimise_layer weights =
      let weights_next = List.map (fun w -> optimise_weight w [] []) weights in
      (* merge the new weight lists *)
      let wc, wn = unzip weights_next in
      let wc, wn = wc @ [ [] ], [] :: wn in
      let weights = List.map2 (fun x y -> x @ y) wc wn in
      weights 
    in

    (* recursively optimise until all weights have 2 or less wires *)
    let rec optimise w = 
      let max_wires = List.fold_left 
          (fun a l -> 
             if a < List.length l then
               List.length l else a) 0 w 
      in
      if max_wires <= 2 then w
      else optimise (optimise_layer w) 
    in

    let weights = build_weights a b in
    let weights = optimise weights in
    build_weight_sum weights

  let dadda a b = 
    (* dadda tree single weight optimiser.  must count current number of output weights as we optimise, including those from the previous level *)
    let opt2_1 x = (x mod 3) = 2 in   (* optimise with half adders if not final stage *)
    let opt2_2 x = (x mod 3) <> 0 in  (* optimise with half adders if final stage *)
    let rec optimise_weight opt2 prev w x xn = 
      match w with
      | a::b::c::tl -> let c,s = A.fa a b c in optimise_weight opt2 prev tl (s::x) (c::xn)
      | a::b::tl -> 
        if opt2 (List.length x + prev) then
          let c,s = A.ha a b in
          optimise_weight opt2 prev tl (s::x) (c::xn)
        else
          optimise_weight opt2 prev tl (a::b::x) xn 
      | a::tl -> optimise_weight opt2 prev tl (a::x) xn
      | [] -> x, xn 
    in

    (* optimise all current weights at current reduction layer *)
    let optimise_layer optimise_weight weights =
      let _, weights_next = 
        List.fold_left (fun (carry, wl) w -> 
            let w,wn = optimise_weight carry w [] [] in
            List.length wn, wl @ [(w, wn)]
          ) (0,[]) weights 
      in
      (* merge the new weight lists *)
      let wc, wn = unzip weights_next in
      let wc, wn = wc @ [ [] ], [] :: wn in
      let weights = List.map2 (fun x y -> x @ y) wc wn in
      weights 
    in

    (* recursively optimise until all weights have 2 or less wires *)
    let rec optimise w = 
      let max_wires = List.fold_left 
          (fun a l -> 
             if a < List.length l then List.length l 
             else a) 0 w in
      match max_wires with
      | 0 | 1 | 2 -> w
      | 3 -> optimise (optimise_layer (optimise_weight opt2_2) w) 
      | _ -> optimise (optimise_layer (optimise_weight opt2_1) w) 
    in

    let weights = build_weights a b in
    let weights = optimise weights in
    build_weight_sum weights

end

