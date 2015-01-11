module Make(B : HardCaml.Comb.S) = struct

    open B

    let fa x y z = 
        let s = (x ^: y) ^: z in
        let c = (x &: y) |: (x &: z) |: (y &: z) in
        c, s
      
    let ha x y = 
        let s = x ^: y in
        let c = x &: y in
        c, s

    let fs x y z = 
        let s = (x ^: y) ^: z in
        let b = ( (~: x) &: (y |: z) ) |: (x &: y &: z) in
        b, s

    module Prefix = 
    struct

        type 'a f = ('a -> 'a -> 'a) -> 'a list -> 'a list

        let serial (+:) l = 
            let h,t = List.hd l, List.tl l in
            let o = List.fold_left (fun a b -> (b +: (List.hd a)) :: a) [h] t in
            List.rev o

        let split_pow2 l = 
            let open HardCaml in
            let w = List.length l in
            if w = 0 then [],[]
            else if w = 1 then l,[]
            else
                let x = 1 lsl ((Utils.clog2 w)-1) in
                Utils.lselect l 0 (x-1), Utils.lselect l x (w-1)

        let rec sklansky (+:) l = 
            match l with
            | [] -> failwith "sklansky"
            | [a] -> [a]
            | _ ->
                let s,t = split_pow2 l in
                let s = sklansky (+:) s in
                let t = sklansky (+:) t in
                let s' = List.hd (List.rev s) in
                let t = List.map (fun t -> t +: s') t in
                s @ t

        let rec brent_kung (+:) l = 
            match l with 
            | [] -> failwith "brent_kung"
            | [a] -> [a]
            | _ ->
                let p = HardCaml.Utils.pairs l in
                let l = List.map (fun (a,b) -> b +: a) p in
                let l = brent_kung (+:) l in
                let p = List.map fst p in
                let ph,pt = List.hd p, List.tl p in
                let lt,lh = 
                    let l = List.rev l in
                    List.rev (List.tl l), List.hd l
                in
                let o = List.flatten (List.map2 (fun a b -> [a; b +: a]) lt pt) in
                ph :: (o @ [lh])

        let kogge_stone (+:) l = 
            let open HardCaml.Utils in
            let l' = List.length l in (* must be power of two *)
            let rec b n l = 
                if n=0 then l 
                else
                    let l = b (n/2) l in
                    let l0,l1,l2 = 
                        lselect l 0 (n-1), lselect l n (l'-1), 
                        lselect l 0 (l'-n-1)
                    in
                    l0 @ (List.map2 (+:) l1 l2)
            in
            b (l' / 2) l

        let add f a b cin = 
            let gp (gi,pi) (gk,pk) = (gi |: (pi &: gk), pi &: pk) in
            let a,b = List.rev (bits a), List.rev (bits b) in
            let i = List.map2 ha a b in
            let o = f gp i in
            let c = List.map (fun (g,p) -> g |: (p &: cin)) o in
            let s = List.map2 (fun (_,p) c -> p ^: c) (i@[gnd,gnd]) (cin::c) in
            concat (List.rev s)

    end

end

