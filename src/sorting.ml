open HardCaml
open Utils

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

module SwapInt = 
struct
    type t = int
    let up, down = 1, 0
    let delay d = d
    let compare_swap up a b = 
        if (a > b) <> (up=1) then a,b
        else b,a
end

module SwapHw(B : HardCaml.Comb.S) = 
struct
    open B
    type t = B.t
    let up, down = vdd, gnd
    let delay d = d
    let compare_swap up a b = 
        let c = (a >: b) ==: up in
        mux2 c b a, mux2 c a b
end

(* split list into 2 halves *)
let half l = 
    let length = List.length l in
    lselect l 0 (length/2 - 1), lselect l (length/2) (length-1)

module Bitonic(S : Swap) = 
struct

    type t = S.t

    let compare up l = 
        let l0,l1 = half l in
        let l = List.map2 (S.compare_swap up) l0 l1 in
        (List.map fst l) @ (List.map snd l)

    let rec merge up l = 
        let len = List.length l in
        if len = 1 then l
        else
            let l = List.map S.delay (compare up l) in
            let l0,l1 = half l in
            let l0 = merge up l0 in
            let l1 = merge up l1 in
            l0 @ l1

    let rec sort' up l = 
        let len = List.length l in
        if len = 1 then l
        else
            let l0,l1 = half l in
            let l0 = sort' S.up l0 in
            let l1 = sort' S.down l1 in
            merge up (l0 @ l1)

    let sort = sort' S.up

end

module BitonicInt = Bitonic(SwapInt)
module BitonicHw = Bitonic( SwapHw( HardCaml.Signal.Comb ) )

module OddEvenMerge(S : Swap) = 
struct

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
                let a0,a1 = S.compare_swap S.up o1 e2 in
                a0::a1::compare_list ot et
            | _ -> failwith "compare list failed"
        in
        match l with
        | a0::a1::[] ->
            let a0,a1 = S.compare_swap S.up a0 a1 in
            List.map S.delay [a0;a1]
        | _ -> 
            let e,o = oddeven l in
            let e,o = merge e, merge o in
            List.map S.delay (List.hd e :: compare_list o (List.tl e))

    let rec sort l = 
        if List.length l > 1 then
            let l,h = half l in
            merge (sort l @ sort h)
        else
            l

end

module OddEvenMergeInt = OddEvenMerge(SwapInt)
module OddEvenMergeHw = OddEvenMerge(SwapHw(HardCaml.Signal.Comb))

