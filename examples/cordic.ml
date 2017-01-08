type system = 
  | Circular 
  | Linear 
  | Hyperbolic 

type mode = 
  | Rotation 
  | Vectoring 
  | Inverse of float

(* (1/2) * log2 ((1+t) / (1-t)) *)
let atanh t = 
  0.5 *. (log ((1.0 +. t) /. (1.0 -. t))) 

let cordic_iter ~iters ~f a = 
  let rec g i ih k a = 
    if i=iters then a
    else
      let a = f i ih a in
      if ih=k then g (i+1) ih ((k*3)+1) a
      else g (i+1) (ih+1) k a
  in
  g 0 1 4 a

let gain ~iters = cordic_iter ~iters 
  ~f:(fun i _ a -> a *. sqrt (1. +. (2. ** (2. *. (float_of_int (- i)))))) 1.

let gainh ~iters = cordic_iter ~iters 
  ~f:(fun _ i a -> a *. sqrt (1. -. (2. ** (2. *. (float_of_int (- i)))))) 1.

module Double = struct

  let cordic ~system ~mode ~iters ~x ~y ~z =
    cordic_iter ~iters
      ~f:(fun i ih (x,y,z) ->
        let t i = 2. ** float_of_int (-i) in
        let t, m, e = 
          match system with 
          | Circular -> let t = t i in t, 1., atan t
          | Linear -> let t = t i in t, 0., t
          | Hyperbolic -> let t = t ih in t, -1., atanh t
        in
        let d = 
          match mode with 
          | Rotation -> if z < 0. then -1. else 1.
          | Vectoring -> if y < 0. then 1. else -1.
          | Inverse c -> if y < c then 1. else -1.
        in
        let x' = x -. (m *. y *. d *. t) in
        let y' = y +. (x *. d *. t) in
        let z' = z -. (d *. e) in
        x', y', z')
      (x, y, z)

  module Fns(I : sig val iters : int end) = struct

    let iters = I.iters
    let gain = gain ~iters 
    let gainh = gainh ~iters

    (* range |pi/2| *)
    let cos_sin angle = 
      let x, y, _ = cordic ~system:Circular ~mode:Rotation ~iters 
        ~x:(1. /. gain) ~y:0. ~z:angle 
      in
      x, y

    let polar_to_rect mag phase = 
      let x, y, _ = cordic ~system:Circular ~mode:Rotation ~iters 
        ~x:(mag /. gain) ~y:0. ~z:phase 
      in
      x, y

    let rotate_vector x y angle = 
      let x, y, _ = cordic ~system:Circular ~mode:Rotation ~iters 
        ~x ~y ~z:angle 
      in
      x /. gain, y /. gain

    (* no particular range restriction *)
    let atan a = 
      let _, _, z = cordic ~system:Circular ~mode:Vectoring ~iters 
        ~x:1.0 ~y:a ~z:0. 
      in
      z

    let atan2 y x = 
      let _, _, z = cordic ~system:Circular ~mode:Vectoring ~iters 
        ~x ~y ~z:0. 
      in
      z

    let rect_to_polar x y = 
      let x, _, z = cordic ~system:Circular ~mode:Vectoring ~iters 
        ~x ~y ~z:0. 
      in
      x /. gain, z

    (* range ~|0.98| *)
    let asin a = 
      let x, y, z = cordic ~system:Circular ~mode:(Inverse (abs_float a)) ~iters 
        ~x:(1. /. gain) ~y:0. ~z:0. 
      in
      if a < 0. then z else -. z

    let mul a b = 
      let x, y, z = cordic ~system:Linear ~mode:Rotation ~iters 
        ~x:a ~y:0. ~z:b 
      in
      y

    let div a b =
      let x, y, z = cordic ~system:Linear ~mode:Vectoring ~iters 
        ~x:b ~y:a ~z:0. 
      in
      z

    let cosh_sinh a =
      let x, y, z = cordic ~system:Hyperbolic ~mode:Rotation ~iters 
        ~x:(1. /. gainh) ~y:0. ~z:a
      in
      x, y

    (* range ~|0.8| *)
    let atanh a = 
      let x, y, z = cordic ~system:Hyperbolic ~mode:Vectoring ~iters 
        ~x:1. ~y:a ~z:0.
      in
      z

  end

end

open HardCaml

module type Fixpt = sig
  val w : int
  val fp : int
end

module Unrolled(B : Comb.S)(P : Fixpt) = struct

  let rnd x = int_of_float (if x < 0. then (x -. 0.5) else (x +. 0.5))
  let constf x = B.consti P.w (rnd (x *. (2. ** float_of_int P.fp)))

  let atan iters = 
    Array.init iters (fun i -> constf (atan (2. ** float_of_int (-i))))

  let atanh iters = 
    Array.of_list (List.rev (cordic_iter ~iters 
      ~f:(fun _ ih l -> constf (atanh (2. ** float_of_int (-ih))) :: l) []))

  let t iters = 
    Array.init iters (fun i -> B.srl (constf 1.) i)

  let step ~x ~xsft ~y ~ysft ~z ~d ~m ~e = 
    let open B in
    let x' = mux2 (msb m) x (mux2 ((lsb m) ^: d) (x +: ysft) (x -: ysft)) in
    let y' = mux2 d (y -: xsft) (y +: xsft) in
    let z' = mux2 d (z +: e) (z -: e) in
    x', y', z'

  (* system *)
  let circular = B.const "00"
  let hyperbolic = B.const "01"
  let linear = B.const "10"

  (* modes *)
  let rotation = B.const "00"
  let vectoring = B.const "01"
  let inverse = B.const "11"

  let cordic ?pipe ~system ~mode ~iters ~c ~x ~y ~z = 
    let open B in
    let p = match pipe with None -> (fun p -> p) | Some(p) -> p in
    assert (width x = width y);
    assert (width x = width z);
    
    let m = system in
    let e = [ atan iters; atanh iters; t iters ] in
    let is_hyper = system ==: hyperbolic in

    cordic_iter ~iters 
      ~f:(fun i ih (x,y,z) ->
        let e = mux system (List.map (fun e -> e.(i)) e) in
        let d = mux mode [ z <+. 0; y >=+. 0; y >=+ c ] in
        let xsft = mux2 is_hyper (sra x ih) (sra x i) in
        let ysft = mux2 is_hyper (sra y ih) (sra y i) in
        let x, y, z = step ~x ~xsft ~y ~ysft ~z ~d ~m ~e:e in
        (p x), (p y), (p z)) (x, y, z)

end

module Iterative(P : Fixpt) = struct

  module S = Signal.Comb
  (*module S = Const_prop.Comb*)
  module C = Unrolled(S)(P)
  open S
  open Signal.Seq

  let hyper_iter ~ld ~system ~iter =
    let is_hyper = system ==: C.hyperbolic in
    let iterh = wire (width iter) -- "iterh" in
    let k = wire (width iter + 2) -- "k" in
    let repeat = (k ==: ((zero 2) @: iterh)) -- "repeated_step" in
    let upd v init t f = 
      v <== reg r_sync enable (mux2 ld (consti (width t) init) (mux2 repeat t f)) 
    in
    let () = upd k 4 (k +: (sll k 1) +:. 1) k in 
    let () = upd iterh 1 iterh (iterh +:. 1) in
    (mux2 is_hyper iterh iter) -- "iter_sel"

  let cordic ~ld ~system ~mode ~iters ~c ~x ~y ~z = 
    let wi = Utils.nbits (iters-1) in

    let iter = reg_fb r_sync enable wi (fun d -> mux2 ld (zero wi) (d +:. 1)) -- "iter" in

    let atan = mux iter (Array.to_list (C.atan iters)) in
    let atanh = mux iter (Array.to_list (C.atanh iters)) in
    let t = mux iter (Array.to_list (C.t iters)) in

    let xw, yw, zw = wire P.w, wire P.w, wire P.w in

    let m = system in
    let e = mux system [ atan; atanh; t ] -- "e" in
    let d = mux mode [ zw <+. 0; yw >=+. 0; yw >=+ c ] in

    let iter = hyper_iter ~ld ~system ~iter in

    let xsft = log_shift sra xw iter in
    let ysft = log_shift sra yw iter in
    let xs, ys, zs = C.step ~x:xw ~xsft ~y:yw ~ysft ~z:zw ~d ~m ~e in

    xw <== (reg r_sync enable (mux2 ld x xs));
    yw <== (reg r_sync enable (mux2 ld y ys));
    zw <== (reg r_sync enable (mux2 ld z zs));

    xw, yw, zw

end

module Design = struct

  open HardCaml
  open HardCamlFramework.Framework
  open Param

  let name = "CORDIC"
  let desc = "CORDIC"

  (* for now we dont support the 'asin' function as it complicates the 
     generator code.  The generic cores do support it though *)

  module Hw_config = struct
    include struct
      type 'a t = { arch : 'a; bits : 'a; fp : 'a; system : 'a; mode : 'a; iters : 'a; funct : 'a; }[@@deriving hardcaml]
    end
    let params = {
      arch = Symbol(["comb"; "pipe"; "iter"], "comb"), "Combinatorial, pipelined or iterative architecture";
      bits = Int 20, "CORDIC data path width";
      fp = Int 16, "Fixed point";
      system = Symbol(["generic"; "circular"; "linear"; "hyperbolic"], "generic"),
        "Coordinate system";
      mode = Symbol(["generic"; "rotation"; "vectoring"; "inverse"], "generic"),
        "Rotation mode";
      funct = Symbol(
        [ "generic"; "cos-sin"; "polar-to-rect"; "rotate-vector"; "atan"; "atan2";
          "rect-to-polar"; "mul"; "div"; "cosh-sinh"; "atanh" ],
        "generic"), "Configure circuit for specific function";
      iters = Int 16, "Max Number of iterations";
    }
  end

  module Tb_config = struct
    include struct
      type 'a t = { x : 'a; y : 'a; z : 'a; c : 'a; }[@@deriving hardcaml]
    end
    let params = {
      x = Float 0.0, "x input";
      y = Float 0.0, "y input";
      z = Float 0.0, "z input";
      c = Float 0.0, "Inverse mode decision target value";
    }
  end

  let validate hw tb = Ok

  module Make
    (B : Comb.S)
    (H : Params with type 'a t = 'a Hw_config.t)
    (T : Params with type 'a t = 'a Tb_config.t) = struct

    module S = Signal.Comb
    open Hw_config
    open Tb_config

    let arch = get_string H.params.arch
    let bits = get_int H.params.bits
    let fp = get_int H.params.fp
    let arch = get_string H.params.arch
    let system = get_string H.params.system
    let mode = get_string H.params.mode
    let funct = get_string H.params.funct
    let iters = get_int H.params.iters

    let x = get_float T.params.x
    let y = get_float T.params.y
    let z = get_float T.params.z
    let c = get_float T.params.c

    module I = struct
      type 'a t = { 
        enable : 'a[@bits 1]; 
        ld : 'a[@bits 1]; 
        system : 'a[@bits 2]; 
        mode : 'a[@bits 2]; 
        c : 'a[@bits bits]; 
        x : 'a[@bits bits]; 
        y : 'a[@bits bits]; 
        z : 'a[@bits bits]; 
      }[@@deriving hardcaml]
    end
    module O = struct
      type 'a t = { 
        xo : 'a[@bits bits]; 
        yo : 'a[@bits bits]; 
        zo : 'a[@bits bits]; 
      }[@@deriving hardcaml]
    end

    let wave_cfg = 
      let open Display in
      Some (
        ["clock",B; "reset",B; "clear",B; "enable",B] @
        I.(to_list (map2 (fun (n,_) d -> n,d) t 
        {
          enable = B;
          ld = B;
          system = B;
          mode = B;
          c = F fp;
          x = F fp;
          y = F fp;
          z = F fp;
        })) @
      ["iter",U; "iterh",U; "iter_sel",U; "k",U; "e",F fp] @
      O.(to_list (map (fun (n,_) -> n,F fp) t)))

    module Fp = struct
      let w = bits
      let fp = fp
    end
    module Uc = Unrolled(Signal.Comb)(Fp)
    module Ic = Iterative(Fp)

    let constf = Uc.constf 
    let system = 
      match system with
      | "circular" -> Some Uc.circular
      | "linear" -> Some Uc.linear
      | "hyperbolic" -> Some Uc.hyperbolic
      | _ -> None
    let mode = 
      match mode with
      | "rotation" -> Some Uc.rotation
      | "vectoring" -> Some Uc.vectoring
      | "inverse" -> Some Uc.inverse
      | _ -> None

    let sconst x = S.constibl (List.map B.to_int (B.bits x)) 
    let fixed x = Uc.rnd (x *. (2. ** float_of_int fp))

    let gain = gain ~iters 
    let gainh = gainh ~iters (* XXX no - I think we need the opposite of this *)

    (* configure special functions *)
    let system, mode, x', y', z', c', xo', yo', zo' = 
      let fx x =  `fix (B.consti bits (fixed x)) in
      match funct with
      | "cos-sin" -> 
        Some Uc.circular, Some Uc.rotation, 
        fx (1. /. gain), fx 0., `none, fx 0.,
        `none, `none, `none
      | "polar-to-rect" -> 
        Some Uc.circular, Some Uc.rotation, 
        `igain, fx 0., `none, fx 0.,
        `none, `none, `none
      | "rotate-vector" -> 
        Some Uc.circular, Some Uc.rotation, 
        `none, `none, `none, fx 0.,
        `igain, `igain, `none
      | "atan" -> 
        Some Uc.circular, Some Uc.vectoring, 
        fx 1., `none, fx 0., fx 0.,
        `none, `none, `none
      | "atan2" ->
        Some Uc.circular, Some Uc.vectoring, 
        `none, `none, fx 0., fx 0.,
        `none, `none, `none
      | "rect-to-polar" ->
        Some Uc.circular, Some Uc.vectoring, 
        `none, `none, fx 0., fx 0.,
        `igain, `none, `none
      | "mul" ->
        Some Uc.linear, Some Uc.vectoring, 
        `none, fx 0., `none, fx 0.,
        `none, `none, `none
      | "div" ->
        Some Uc.linear, Some Uc.vectoring, 
        `none, `none, fx 0., fx 0.,
        `none, `none, `none
      | "cosh-sinh" ->
        Some Uc.hyperbolic, Some Uc.rotation, 
        fx (1. /. gainh), fx 0., `none, fx 0.,
        `none, `none, `none
      | "atanh" ->
        Some Uc.hyperbolic, Some Uc.vectoring, 
        fx 1., `none, fx 0., fx 0.,
        `none, `none, `none
      | _ -> 
        system, mode, 
        `none, `none, `none, `none, 
        `none, `none, `none

    let cordic ~ld ~system ~mode ~c ~x ~y ~z = 
      match arch with
      | "comb" -> 
        Uc.cordic ?pipe:None ~system ~mode ~iters ~c ~x ~y ~z 
      | "pipe" -> 
        Uc.cordic ~pipe:Signal.Seq.(reg r_sync S.enable) ~system ~mode ~iters ~c ~x ~y ~z 
      | "iter" -> 
        Ic.cordic ~ld ~system ~mode ~iters ~c ~x ~y ~z
      | _ -> failwith "bad arch"

    let hw i = 
      let open I in
      let system = match system with None -> i.system | Some(x) -> x in
      let mode = match mode with None -> i.mode | Some(x) -> x in

      (* inputs may be fixed or scaled by the cordic gain *)
      let igain = S.consti bits (fixed (1. /. gain)) in
      let iparm x ix = 
        match x with
        | `none -> ix
        | `fix(x) -> sconst x
        | `igain -> S.(ix *+ igain)
      in
      let x = iparm x' i.x in
      let y = iparm y' i.y in
      let z = iparm z' i.z in
      let c = iparm c' i.c in

      let xo, yo, zo = cordic ~ld:i.ld ~system ~mode ~c ~x ~y ~z in

      (* outputs may be scaled by the cordic gain *)
      let oparm o' o = 
        match xo' with
        | `none -> o
        | `igain -> S.(o *+ igain)
      in
      let xo = oparm xo' xo in
      let yo = oparm yo' yo in
      let zo = oparm zo' zo in
      O.({ xo; yo; zo })

    open I 
    open O 
    module Sim = Cyclesim.Api 

    let tb_comb sim i o _ = 
      Sim.reset sim;
      i.x := B.consti bits (fixed x);
      i.y := B.consti bits (fixed y);
      i.z := B.consti bits (fixed z);
      i.c := B.consti bits (fixed c);
      Sim.cycle sim

    let tb_pipe sim i o _ =
      Sim.reset sim;
      i.enable := B.vdd;
      i.x := B.consti bits (fixed x);
      i.y := B.consti bits (fixed y);
      i.z := B.consti bits (fixed z);
      i.c := B.consti bits (fixed c);
      for j=0 to iters-1 do
        Sim.cycle sim;
      done;
      i.enable := B.gnd;
      Sim.cycle sim

    let tb_iter sim i o _ =
      Sim.reset sim;
      i.enable := B.vdd;
      i.ld := B.vdd;
      i.x := B.consti bits (fixed x);
      i.y := B.consti bits (fixed y);
      i.z := B.consti bits (fixed z);
      i.c := B.consti bits (fixed c);
      Sim.cycle sim;
      i.ld := B.gnd;
      for j=0 to iters-1 do
        Sim.cycle sim;
      done;
      i.enable := B.gnd;
      Sim.cycle sim

    let tb : B.t HardCaml.Cyclesim.Api.cyclesim -> (* 4.03.0-beta2 compat *)
             B.t ref I.t -> B.t ref O.t -> B.t ref O.t -> unit = 
      match arch with
      | "comb" -> tb_comb
      | "pipe" -> tb_pipe
      | "iter" -> tb_iter
      | _ -> failwith "bad arch"

  end

end


