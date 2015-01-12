open HardCaml

let pi = 3.1415926535897932384626433832795

module Hardware(B : Comb.S) = 
struct

  open B

  (*
   Cordic calculation type

   Cordic0 simplified cordic calculation (x is not updated)
   Cordic1 standard cordic equation
   Cordic2 hyperbolic
  *)
  type cordic_type_t =  Cordic0 | Cordic1 | Cordic2    

  (*
   Type of cordic circuit to generate

   CordicComb combinatorial
   CordicSeq Fully unrolled
   CordicIter Iterative
  *)
  type cordic_gen_t = 
      CordicComb of cordic_type_t   
    | CordicSeq of cordic_type_t    
    | CordicIter of cordic_type_t   

  (*
   Parameterized cordic circuit generation.

   Inputs:
     pre       Pre-rotation mode.  -1 none, 0 or 1 different rotation modes
     cdc_type  Type of cordic to build
     fix       fix point precision of arithmetic
     iters     number of iterations
     reg       register
     vld_in    input valid signal
     x         input data
     y         input data
     z         input data
     vecmode   vectoring mode

   Outputs:
     x         output data
     y         output data
     z         output data
     vld_out   output valid signal
  *)

  let cordic pre cdc_type fix iters reg vld_in x y z vecmode = 
    let (>>+) = sra in

    (* 2 to the power -i *)
    let atan i = atan (2.0 ** (float (-i)))  in

    (* (1/2) * log2 ((1+t) / (1-t)) *)
    let atanh i = 
      let t = (2.0 ** (float (-(i+1)))) in
      0.5 *. (log ((1.0 +. t) /. (1.0 -. t))) 
    in

    let pipeline x = 
      match cdc_type with 
      | CordicComb _ -> x 
      | _ -> reg x 
    in

    let bits = width x in
    let scale = 2.0 ** (float_of_int (bits-fix)) in
    let fconst f = consti bits (int_of_float (scale *. f)) in

    let shift_mux hyper addr v = 
      let range = 
        if hyper then List.tl (Utils.range (iters+1)) 
        else (Utils.range iters) 
      in
      mux addr (List.map (fun x -> v >>+ x) range) in

    (* iter -> index map for hyperbolic functions *)
    let index_map iters = 
      let rec imap i j =
        let iters_left = iters - i in
        if (i = iters) then []
        else if (j > 0) && ((j mod 3) = 0) && (iters_left > 1) then
          [ j; j ] @ imap (i+2) (j+1)
        else 
          [ j ] @ imap (i+1) (j+1) in
      imap 0 0 
    in

    let prerotate0 x y z =  
      (* XXX check this function :
          let n = neg (mux2 d x y) this was included ... looks
          like it was rewritten and left behind *)
      let bits = width x in
      let d = y <+ (zero bits) in
      let neg x = (zero bits) -: x in
      mux2 d (neg y) y, mux2 d x (neg x), 
      z -: (mux2 d (fconst (pi /. 2.0)) (fconst (pi /. (-2.0)))) in

    let prerotate1 x y z = 
      let bits = width x in
      let d = x <+ (zero bits) in
      let neg x = (zero bits) -: x in
      mux2 d (neg x) x, mux2 d (neg y) y, 
      mux2 d (z -: (fconst pi)) z 
    in

    (* pre-rotation mode *)
    let x,y,z = 
      match pre with
      | -1 -> x,y,z
      |  0 -> prerotate0 x y z
      |  1 -> prerotate1 x y z
      | _ -> failwith "Invalid pre-rotatation mode" 
    in

    let cordic0 x y z vecmode xt yt t = 
      let c = ( ( (vecmode >=+ (zero bits)) &: (y <+ (zero bits)) ) |:
                ( (vecmode <+ (zero bits)) &: (z >=+ (zero bits)) ) ) 
      in
      x, 
      (mux2 c (y +: xt) (y -: xt)), 
      (mux2 c (z -: t) (z +: t)) 
    in

    let cordic1 x y z vecmode xt yt t = 
      let c = ( ( (vecmode >=+ (zero bits)) &: (y <+ vecmode) ) |:
                ( (vecmode <+ (zero bits)) &: (z >=+ (zero bits)) ) ) 
      in
      (mux2 c (x -: yt) (x +: yt)), 
      (mux2 c (y +: xt) (y -: xt)), 
      (mux2 c (z -: t) (z +: t)) 
    in

    let cordic2 x y z vecmode xt yt t = 
      let c = ( ( (vecmode >=+ (zero bits)) &: (y <+ (zero bits)) ) |: 
                ( (vecmode <+ (zero bits)) &: (z >=+ (zero bits)) ) ) 
      in
      (mux2 c (x +: yt) (x -: yt)), 
      (mux2 c (y +: xt) (y -: xt)), 
      (mux2 c (z -: t) (z +: t)) 
    in

    let cordic_full cordic x y z vecmode tab hyper =
      let imap = 
        if hyper then (index_map iters) 
        else (Utils.range iters) 
      in
      let rec cf i vld x y z vecmode = 
        if i = iters then
          (x,y,z,vld)
        else
          let idx = List.nth imap i in
          let idx_sft = if hyper then idx + 1 else idx in
          let x,y,z = 
            cordic x y z vecmode 
              (x >>+ idx_sft) (y >>+ idx_sft) 
              (List.nth tab idx) 
          in
          let x,y,z,vld,vecmode = 
            pipeline x, pipeline y, 
            pipeline z, pipeline vld, 
            pipeline vecmode 
          in
          cf (i+1) vld x y z vecmode 
      in
      cf 0 vld_in x y z vecmode in

    let cordic_iter cordic ix iy iz vecmode tab hyper =
      let bits = width ix in
      let iter_bits = Utils.clog2 (iters-1) in
      (* counter used to control the iteration and the rom address *)
      let count_next = wire iter_bits in
      let count = pipeline (mux2 vld_in (zero iter_bits) count_next) in
      count_next <== count +: (one iter_bits);
      (* remap count for hyperbolic functions in needed *)
      let idx = 
        if hyper then 
          mux count 
            (List.map 
               (fun x -> consti (width count) x) 
               (index_map iters)) 
        else count 
      in
      (* cordic circuit *)
      let sx,sy,sz = wire bits, wire bits, wire bits in
      let rx,ry,rz = 
        pipeline (mux2 vld_in ix sx), 
        pipeline (mux2 vld_in iy sy), 
        pipeline (mux2 vld_in iz sz) 
      in
      let nx,ny = shift_mux hyper idx rx, shift_mux hyper idx ry in 
      let tab = mux idx tab in 
      let cx,cy,cz = cordic rx ry rz vecmode nx ny tab in
      sx <== cx; sy <== cy; sz <== cz; 
      let vld_out = (count ==:. (iters-1)) in
      cx, cy, cz, vld_out 
    in

    let tab0 = 
      List.map (fun x -> fconst (2.0 ** (float (-x)))) (Utils.range iters) 
    in
    let tab1 = 
      List.map (fun x -> fconst (atan x)) (Utils.range iters) 
    in
    let tab2 = 
      List.map (fun x -> fconst (atanh x)) (Utils.range iters) 
    in

    let fn, cdc, tab, hyper = 
      let sel_cdc fn typ = 
        match typ with 
        | Cordic0 -> fn, cordic0, tab0, false
        | Cordic1 -> fn, cordic1, tab1, false 
        | Cordic2 -> fn, cordic2, tab2, true 
      in
      match cdc_type with
      | CordicComb cdc -> sel_cdc cordic_full cdc
      | CordicSeq  cdc -> sel_cdc cordic_full cdc
      | CordicIter cdc -> sel_cdc cordic_iter cdc 
    in

    fn cdc x y z vecmode tab hyper

end

module Software = 
struct

  let rec icordic0 i iters x y z vecmode = 
    if i = iters then (x,y,z)
    else
      let t = (2.0 ** (float (-i))) in
      if ((vecmode >= 0.0 && y < 0.0) || 
          (vecmode < 0.0  && z >= 0.0)) then
        icordic0 (i+1) iters x (y +. (x *. t)) (z -. t) vecmode
      else 
        icordic0 (i+1) iters x (y -. (x *. t)) (z +. t) vecmode 

  let rec icordic1 i iters x y z vecmode = 
    (* 2 to the power -i (not complex) *)
    let atan i = atan (2.0 ** (float (-i))) in
    if i = iters then (x,y,z)
    else
      let t = (2.0 ** (float_of_int (-i))) in
      if ((vecmode >= 0.0 && y < vecmode) || 
          (vecmode < 0.0  && z >= 0.0)) then
        icordic1 (i+1) iters 
          (x -. (y *. t)) (y +. (x *. t)) (z -. (atan i)) vecmode
      else 
        icordic1 (i+1) iters 
          (x +. (y *. t)) (y -. (x *. t)) (z +. (atan i)) vecmode 

  let rec icordic2 i iters x y z vecmode = 
    (* (1/2) * log2 ((1+t) / (1-t)) *)
    let atanh i = 
      let t = (2.0 ** (float (-(i+1)))) in
      0.5 *. (log ((1.0 +. t) /. (1.0 -. t))) 
    in
    let cdc i x y z vecmode = 
      let t = (2.0 ** (float (-(i+1)))) in
      if ((vecmode >= 0.0 && y < 0.0) || 
          (vecmode < 0.0 && z >= 0.0)) then
        x +. (y *. t), y +. (x *. t), z -. (atanh i)
      else 
        x -. (y *. t), y -. (x *. t), z +. (atanh i) 
    in
    if i = iters then (x,y,z)
    else
      let x,y,z = 
        if (i > 0) && ((i mod 3) = 0) then 
          let x, y, z = cdc i x y z vecmode in
          cdc i x y z vecmode
        else
          cdc i x y z vecmode 
      in
      icordic2 (i+1) iters x y z vecmode 

  let cordic0 = icordic0 0 
  let cordic1 = icordic1 0 
  let cordic2 = icordic2 0 

  let x c = let x,y,z = c in x 
  let y c = let x,y,z = c in y 
  let z c = let x,y,z = c in z 

  let invGain1 iters = 1.0 /. (x (cordic1 iters 1.0 0.0 0.0 (-1.0))) 
  let invGain2 iters = 1.0 /. (x (cordic2 iters 1.0 0.0 0.0 (-1.0))) 

  let prerotate0 x y z = 
    let d = if y < 0.0 then 1.0 else -1.0 in
    -. (d *. y), (d *. x), (z -. (d *. pi /. 2.0))

  let prerotate1 x y z = 
    let d = if x < 0.0 then -1.0 else 1.0 in
    (d *. x), (d *. y), if x < 0.0 then z -. pi else z 

  (*let prerotate = prerotate0*)
  let prerotate x y z = x,y,z

  let mul iters a b = y (cordic0 iters a 0.0 b (-1.0))

  let div iters a b = y (cordic0 iters b a 0.0 0.0)

  let atan iters a = z (cordic1 iters 1.0 a 0.0 0.0)

  let sincos iters a = 
    let xn,yn,zn = prerotate (invGain1 iters) 0.0 a in
    let x,y,z = cordic1 iters xn yn zn (-1.0) in
    y, x 

  let tan iters a = 
    let sin,cos = sincos iters a in
    sin /. cos 

  let asin iters a = 
    let x,y,z = 
      cordic1 iters (invGain1 iters) 0.0 0.0 
        (if a < 0.0 then (-. a) else a) 
    in
    if a < 0.0 then (-. z) else z 

  let magphase iters x y = 
    let xn,yn,zn = prerotate x y 0.0 in
    let mag,_,phase = cordic1 iters xn yn zn 0.0 in
    mag, phase 

  let polar_to_rect iters m p = 
    let x,y,z = cordic1 iters m 0.0 p (-1.0) in
    y, x 

  let sinhcosh iters a = 
    let x,y,z = cordic2 iters (invGain2 iters) 0.0 a (-1.0) in
    y, x 

  let tanh iters a = 
    let sinh,cosh = sinhcosh iters a in
    sinh /. cosh 

  let atanh iters a = z (cordic2 iters 1.0 a 0.0 0.0) 

  let log iters a = 2.0 *. (z (cordic2 iters (a +. 1.0) (a -. 1.0) 0.0 0.0)) 

  let sqrt iters a = 
    (invGain2 iters) *. (x (cordic2 iters (a +. 0.25) (a -. 0.25) 0.0 0.0)) 

  let exp iters a = 
    let sinh,cosh = sinhcosh iters a in
    sinh +. cosh 

end

