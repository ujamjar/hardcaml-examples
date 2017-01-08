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

let to_dot os network n = 
  let module S = Set.Make(struct type t = int let compare = compare end) in
  let n_nodes = ref 0 in
  let id = fun () -> incr n_nodes; !n_nodes-1 in
  let (+:) a b = `node(a, b, id()) in
  let input i = `input(i) in
  let outputs = List.mapi (fun i s -> `output(s, i)) 
    (network (+:) (Array.to_list (Array.init n input))) 
  in
  let str = function
    | `output(s, i) -> "o"^string_of_int i
    | `node(_, _, i) -> "n"^string_of_int i
    | `input(i) -> "i"^string_of_int i
  in
  let conn a b = os ("  " ^ str a ^ " -- " ^ str b ^ ";\n") in
  let rec traverse set x = 
    match x with
    | `output(s, i) -> 
        conn x s; traverse set s
    | `node(a, b, i) -> 
        if S.exists ((=) i) set then set
        else begin
          conn x a; conn x b; 
          let set = S.add i set in
          let set = traverse set a in
          let set = traverse set b in
          set
        end
    | `input(i) -> set
  in
  os "graph prefix_network {\n";
  os "  rankdir=BT;\n";
  for i=0 to n-1 do
    os ("  i" ^ string_of_int i ^ " [shape=box];\n");
    os ("  o" ^ string_of_int i ^ " [shape=box];\n")
  done;
  for i=0 to !n_nodes-1 do
    os ("  n" ^ string_of_int i ^ " [shape=circle];\n")
  done;
  os "  {rank=same; "; for i=0 to n-1 do os ("i" ^ string_of_int i ^ " ") done; os "}\n";
  os "  {rank=same; "; for i=0 to n-1 do os ("o" ^ string_of_int i ^ " ") done; os "}\n";
  let _ = List.fold_left traverse S.empty outputs in
  os "}\n"

module Adder(B : HardCaml.Comb.S) = struct

  module A = Add.Make(B)
  open B

  let add f a b cin = 
    let gp (gi,pi) (gk,pk) = (gi |: (pi &: gk), pi &: pk) in
    let a,b = List.rev (bits a), List.rev (bits b) in
    let i = List.map2 A.ha a b in
    let o = f gp i in
    let c = List.map (fun (g,p) -> g |: (p &: cin)) o in
    let s = List.map2 (fun (_,p) c -> p ^: c) (i@[gnd,gnd]) (cin::c) in
    concat (List.rev s)

end

module Design = struct
  open HardCaml
  open HardCamlFramework.Framework
  open Param

  let name = "prefix"
  let desc = "**Parallel prefix adder**

Adder architectures which trade circuit area to reduce critical path.
4 progressive larger/faster architectures are provided; _serial_, _sklansky_
_brent-kung_ and _kogge-stone_."

  module Hw_config = struct
    include struct
      type 'a t = { bits : 'a; network : 'a; graph : 'a; }[@@deriving hardcaml]
    end
    let params = {
      bits = Int 8, "Data width";
      network = Symbol(["serial"; "sklansky"; "brent-kung"; "kogge-stone"], "sklansky"), 
        "Type of prefix network";
      graph = File(""), "Create graphviz representation of network";
    }
  end

  module Tb_config = struct
    include struct
      type 'a t = { cycles : 'a; }[@@deriving hardcaml]
    end
    let params = {
      cycles = Int 10, "Number of cycles to test";
    }
  end

  let validate hw tb = 
    let open Hw_config in
    gt hw.bits 0 >>
    (if get_string hw.network = "kogge-stone" then pow2 hw.bits else Ok)

  module Make
    (B : Comb.S)
    (H : Params with type 'a t = 'a Hw_config.t)
    (T : Params with type 'a t = 'a Tb_config.t) = struct

    open Hw_config
    open Tb_config
    let bits = get_int H.params.bits
    let network () = 
      match get_string H.params.network with
      | "serial" -> serial
      | "sklansky" -> sklansky
      | "brent-kung" -> brent_kung
      | "kogge-stone" -> kogge_stone
      | _ -> failwith "not a valid network"
    let graph = get_string H.params.graph
    let cycles = get_int T.params.cycles

    module I = struct
      type 'a t = { a : 'a[@bits bits]; b : 'a[@bits bits]; cin : 'a[@bits 1]; }[@@deriving hardcaml]
    end
    module O = struct
      type 'a t = { c : 'a[@bits bits+1]; }[@@deriving hardcaml]
    end

    let wave_cfg = 
      let open Display in
      let i = I.(map uint t) in
      let o = O.(map uint t) in
      Some( I.(to_list { i with cin = bin i.cin }) @ O.(to_list o ))

    let () = 
      if graph <> "" then begin
        let f = open_out graph in
        to_dot (output_string f) (network()) bits;
        close_out f
      end

    module A = Adder(Signal.Comb)
    let hw i = O.({ c = A.add (network()) i.I.a i.I.b i.I.cin })

    let tb sim i o _ = 
      let open I in
      let open O in
      let module S = Cyclesim.Api in
      S.reset sim;     
      for j=0 to cycles - 1 do
        i.a := B.srand bits;
        i.b := B.srand bits;
        i.cin := B.srand 1;
        S.cycle sim;
      done

  end

end

