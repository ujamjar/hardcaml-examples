type system = 
  | Circular 
  | Linear 
  | Hyperbolic 

type mode = 
  | Rotation 
  | Vectoring 
  | Inverse of float

module Double = struct

  (* (1/2) * log2 ((1+t) / (1-t)) *)
  let atanh t = 
    0.5 *. (log ((1.0 +. t) /. (1.0 -. t))) 

  (* cordic gain *)
  let gain ~iters = 
    let rec f i = 
      if i=iters then 1. 
      else sqrt (1. +. (2. ** (2. *. (float_of_int (- i))))) *. f (i+1)
    in
    f 0

  (* cordic hyperbolic gain *)
  let gainh ~iters = 
    let rec f i k = 
      if i=iters then 1. 
      else 
        let x = sqrt (1. -. (2. ** (2. *. (float_of_int (- i))))) in
        x *. (if i=k then f i ((k*3)+1) else f (i+1) k)
    in
    f 1 4

  let cordic ~system ~mode ~iters ~x ~y ~z = 
    let rec iter i k x y z = 
      (*Printf.printf "%i %i\n" i k;*)
      if i = iters then x, y, z
      else
        let t = 2. ** float_of_int (-i) in
        let m, e = 
          match system with 
          | Circular -> 1., atan t 
          | Linear -> 0., t
          | Hyperbolic -> -1., atanh t
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
        if system = Hyperbolic && i = k then iter i ((3*k)+1) x' y' z'
        else iter (i+1) k x' y' z'
    in
    iter (if system=Hyperbolic then 1 else 0) 4 x y z  

  module Fns(I : sig val iters : int end) = struct

    let iters = I.iters
    let gain = gain ~iters 
    let gainh = gainh ~iters

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

  (* XXX hyperbolic repetition...FIXME ? *)
  let atanh iters = 
    Array.init iters (fun i -> constf (Double.atanh (2. ** float_of_int (-i))))

  let t iters = 
    Array.init iters (fun i -> B.srl (constf 1.) i)

  let step ~x ~y ~z ~d ~m ~i ~e = 
    let open B in
    let xsft = sra x i in
    let ysft = sra y i in
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

    let rec f i x y z = 
      if i = iters then x, y, z
      else
        let e = mux system (List.map (fun e -> e.(i)) e) in
        let d = mux mode [ z <+. 0; y >=+. 0; y >=+ c ] in
        let x, y, z = step ~x ~y ~z ~d ~m ~i ~e:e in
        f (i+1) (p x) (p y) (p z)
    in
    f 0 x y z

end

module Iterative(P : Fixpt) = struct

  module C = Unrolled(Signal.Comb)(P)
  open Signal.Comb
  open Signal.Seq

  let step ~x ~y ~z ~d ~m ~i ~e = 
    let xsft = log_shift sra x i in
    let ysft = log_shift sra y i in
    let x' = mux2 (msb m) x (mux2 ((lsb m) ^: d) (x +: ysft) (x -: ysft)) in
    let y' = mux2 d (y -: xsft) (y +: xsft) in
    let z' = mux2 d (z +: e) (z -: e) in
    x', y', z'

  let cordic ~ld ~system ~mode ~iter ~c ~x ~y ~z = 
    let iters = 1 lsl (width iter) in

    let atan = mux iter (Array.to_list (C.atan iters)) in
    let atanh = mux iter (Array.to_list (C.atanh iters)) in
    let t = mux iter (Array.to_list (C.t iters)) in

    let xw, yw, zw = wire P.w, wire P.w, wire P.w in

    let m = system in
    let e = mux system [ atan; atanh; t ] in
    let d = mux mode [ zw <+. 0; yw >=+. 0; yw >=+ c ] in

    let xs, ys, zs = step ~x:xw ~y:yw ~z:zw ~d ~m ~i:iter ~e in

    xw <== (reg r_sync enable (mux2 ld x xs));
    yw <== (reg r_sync enable (mux2 ld y ys));
    zw <== (reg r_sync enable (mux2 ld z zs));

    xw, yw, zw

end

module Design = struct

  open HardCaml
  open Framework
  open Param

  let name = "CORDIC"
  let desc = "CORDIC"

  (* for now we dont support the 'asin' functionas it complicates the 
     generator code.  The generic cores do support it though *)

  module Hw_config = struct
    include interface arch bits fp system mode iters funct end
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
    include interface x y z c end
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

    module I = interface enable[1] ld[1] system[2] mode[2] iter[Utils.nbits iters] c[bits] x[bits] y[bits] z[bits] end
    module O = interface xo[bits] yo[bits] zo[bits] end

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
          iter = U;
          c = F fp;
          x = F fp;
          y = F fp;
          z = F fp;
        })) @
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

    let gain = Double.gain ~iters 
    let gainh = Double.gainh ~iters 

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

    let cordic ~ld ~system ~mode ~iter ~c ~x ~y ~z = 
      match arch with
      | "comb" -> 
        Uc.cordic ?pipe:None ~system ~mode ~iters ~c ~x ~y ~z 
      | "pipe" -> 
        Uc.cordic ~pipe:Signal.Seq.(reg r_sync S.enable) ~system ~mode ~iters ~c ~x ~y ~z 
      | "iter" -> 
        Ic.cordic ~ld ~system ~mode ~iter ~c ~x ~y ~z
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

      let xo, yo, zo = cordic ~ld:i.ld ~system ~mode ~iter:i.iter ~c ~x ~y ~z in

      (* outputs may be scaled by the cordic gain *)
      let oparm o' o = 
        match xo' with
        | `none -> o
        | `igain -> S.(o *+ igain)
      in
      let xo = oparm xo' xo in
      let yo = oparm yo' yo in
      let zo = oparm zo' zo in
      O.{ xo; yo; zo }

    open I 
    open O 
    module Sim = Cyclesim.Api 

    let tb_comb sim i o = 
      Sim.reset sim;
      i.x := B.consti bits (fixed x);
      i.y := B.consti bits (fixed y);
      i.z := B.consti bits (fixed z);
      i.c := B.consti bits (fixed c);
      Sim.cycle sim

    let tb_pipe sim i o =
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

    let tb_iter sim i o =
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
        i.iter := (B.consti (Utils.nbits iters) j);
        Sim.cycle sim;
      done;
      i.enable := B.gnd;
      Sim.cycle sim

    let tb = 
      match arch with
      | "comb" -> tb_comb
      | "pipe" -> tb_pipe
      | "iter" -> tb_iter
      | _ -> failwith "bad arch"

  end

end


