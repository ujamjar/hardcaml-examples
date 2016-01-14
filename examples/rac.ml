open HardCaml

module Rom(B : Comb.S) = struct

  let rec norm x = 
    let open B in
    if width x = 1 then x
    else 
      let m,l = msb x, lsbs x in
      if m = msb l then norm l
      else x

  let make coefs =
    let open B in
    let n_coefs = List.length coefs in
    (* generate rom *)
    let sum i = 
      let (+:) a b = Signed.(to_signal (of_signal a +: of_signal b)) in
      reduce (+:) 
        (List.map2 (fun i c -> mux2 i c (zero (width c))) 
          (List.rev (bits i))
          coefs) 
    in
    let rec rom i = 
      let sum = sum i in
      if i = ones (width i) then [sum]
      else sum :: rom (i +:. 1)
    in
    (* resize coeffs *)
    let rom = List.map norm (rom (zero n_coefs)) in
    let w = List.fold_left max 0 (List.map width rom) in
    List.map (fun s -> sresize s w) rom

end

open Signal.Comb
open Signal.Seq

let rac ~fixed ~accbits ~romshift ~en ~ld ~addsub ~romcoefs ~x = 
  (* parallel in, serial out register *)
  let piso en ld d = 
    if fixed then 
      lsb (reg_fb r_sync en (width d) (fun d' -> mux2 ld d (srl d' 1))) 
    else
      msb (reg_fb r_sync en (width d) (fun d' -> mux2 ld d (sll d' 1))) 
  in
  (* rom address *)
  let addr = concat (List.rev (List.map (piso en ld) x)) -- "piso_addr" in
  (* build and index rom *)
  let coef = mux addr romcoefs -- "rom_coef" in
  (* accumulator *)
  reg_fb r_sync en accbits 
    (fun acc -> 
      let acc = (if fixed then sra else sll) acc 1 in
      let coef = sll (sresize coef accbits) romshift in
      mux2 ld 
        (zero accbits) 
        (mux2 addsub (acc -: coef) (acc +: coef))
    )

module Design = struct

  open HardCamlFramework.Framework
  open Param

  let name = "RAC"
  let desc = "**ROM-accumulator using distributed arithmetic**

The RAC computes **a0.x0 + a1.x1 ...** for fixed **a** and variable **x**.

The hardware design can be built in two ways depending on the *-fixed*
parameter; **fixed** or **integer** mode.

*Integer mode*

*-coefs* and *-x* are interpreted as integers (they are rounded if 
required).  *-xbits* gives the 2\\`s complement width of the inputs.
Internal data path widths are calculated automatically.

```
$ "^Sys.argv.(0)^" -coefs \"1,-1,7,-3\" -x \"-4,2,3,-6\" -tb
```

*Fixed mode*

*-coefs* and *-x* are specified as floating point numbers and
rounded to the precision provided by *-coefprec* and *-inpprec*.
The accumulator maintains *-guard* bits of precision for the result.

The input data width is specified with *-xbits*.  *-inpprec* 
affects how the testbench converts input and displays output values 
but not the actual hardware data path.

```
$ "^Sys.argv.(0)^" -coefs \"0.9,0.4,-1.4,0.1\" -x \"12.4,22.1,-3.346,10.223\" 
  -fixed -xbits 9 -inpprec 3 -coefprec 8 -guard 4 -tb
```

Note; overflow of input values is not checked by the testbench
and will lead to incorrect results."
  
  module Hw_config = struct
    include interface fixed xbits coefs coefprec guard config end
    let params = { 
      fixed=Flag false, "Fixed point or integer RAC";
      xbits=Int 16, "input width";
      coefs=Float_list [], "RAC coefficients";
      coefprec=Int 4, "Coefficient fixed point precision";
      guard=Int 4, "Accumulator guard bits";
      config=Flag false, "Show configuration info";
    }
  end

  module Tb_config = struct
    include interface inpprec x end
    let params = {
      inpprec=Int 4, "Input fixed point precision";
      x=Float_list [], "list of testbench input values";
    }
  end
  
  let validate hw tb = 
    let open Hw_config in
    let open Tb_config in
    let coefs, x = get_float_list hw.coefs, get_float_list tb.x in
    let n_coefs, n_x = List.length coefs, List.length x in
    (if 0 = n_coefs then 
      Error["no coefficients specified"] else Ok) >>
    (if 0 <> n_x && n_coefs <> n_x then
      Error["number of coefs does not match number of inputs"] else Ok)

  module Make
    (B : Comb.S)
    (H : Params with type 'a t = 'a Hw_config.t)
    (T : Params with type 'a t = 'a Tb_config.t) = struct
    
    module R = Rom(B)

    (* get configuration parameters *)
    let fixed = get_bool H.params.Hw_config.fixed
    let xbits = get_int H.params.Hw_config.xbits
    let coefs = get_float_list H.params.Hw_config.coefs
    let n_coefs = List.length coefs
    let x = get_float_list T.params.Tb_config.x
    let coefprec = get_int H.params.Hw_config.coefprec
    let inpprec = get_int T.params.Tb_config.inpprec
    let guard = get_int H.params.Hw_config.guard
    let config = get_bool H.params.Hw_config.config

    let rnd x = int_of_float (if x>0. then x +. 0.5 else x -. 0.5)

    (* number of bits required to represent the given integer as two's complement *)
    let nbits_of_2compl n = 
      let open Utils in
      if n = 0 then 1
      else if n < 0 then (nbits (-(n+1))) + 1
      else (nbits n) + 1

    (* bits -> signal conversion *)
    let sconst x = constibl (List.map B.to_int (B.bits x)) 

    let eval = try List.fold_left2 (fun a c x -> a +. (c *. x)) 0. coefs x with _ -> 0.0

    let constf prec f = 
      let f = rnd (f *. (2. ** (float_of_int prec))) in
      B.consti (nbits_of_2compl f) f

    let float_of_fixed prec b =
      let p = 2. ** (float_of_int prec) in
      let i = float_of_int (B.to_sint b) in
      (i /. p)

    (* RAC in integer mode *)
    module Integer = struct

      (* convert inputs and coeffs to integers *)
      let coefs = List.map rnd coefs
      let x = List.map rnd x

      (* compute the required number of accumulator bits by evaluating
         with max/min values of the inputs (depending on sign of coef) *)
      let accbits = 
        let xmin = - (1 lsl (xbits-1)) in
        let xmax = (1 lsl (xbits-1)) - 1 in
        let amin = List.fold_left (fun a c -> a + c * (if c<0 then xmax else xmin)) 0 coefs in
        let amax = List.fold_left (fun a c -> a + c * (if c>0 then xmax else xmin)) 0 coefs in
        let accbits = max (nbits_of_2compl amin) (nbits_of_2compl amax) in
        accbits

      (* rom coefficients *)
      let coefs = List.map (fun x -> B.consti (nbits_of_2compl x) x) coefs 
      let romcoefs = List.map sconst (R.make coefs) 

      let romshift = 0
      let inpprec = 0
      let outprec = 0
      let coefprec = 0
      let romsize = width (List.hd romcoefs)

      let x = List.map (fun x -> B.consti xbits x) x

    end

    module Fixed = struct

      (* rom coefficients *)
      let coefs = List.map (fun x -> constf coefprec x) coefs 
      let romcoefs = List.map sconst (R.make coefs) 
      let romsize = width (List.hd romcoefs)
      let accbits = 1 + romsize + guard
      let romshift = guard
      
      let coefprec = coefprec
      let inpprec = inpprec
      let outprec = coefprec + 1 + inpprec + guard - xbits

      (* input values *)
      let x = List.map (fun x -> (B.consti xbits (rnd (x *. (2. ** (float_of_int inpprec)))))) x 

    end

    let accbits, romshift, romcoefs, x, coefs, inpprec, outprec, coefprec, romsize = 
      if fixed then
        Fixed.(accbits, romshift, romcoefs, x, coefs, inpprec, outprec, coefprec, romsize)
      else 
        Integer.(accbits, romshift, romcoefs, x, coefs, inpprec, outprec, coefprec, romsize)

    let () = if config then begin
      Printf.printf "  accbits = %i\n" accbits;
      Printf.printf "  romshift = %i\n" romshift;
      Printf.printf "  inpprec = %i\n" inpprec;
      Printf.printf "  coefprec = %i\n" coefprec;
      Printf.printf "  romsize = %i\n" romsize;
      Printf.printf "  outprec = %i\n" outprec;
      Printf.printf "  coefs:\n";
      for i=0 to n_coefs - 1 do
        Printf.printf "    %s\n" (B.to_bstr (List.nth coefs i));
      done;
      Printf.printf "  inputs:\n";
      for i=0 to n_coefs - 1 do
        Printf.printf "    %s\n" (B.to_bstr (List.nth x i));
      done;
    end

    module I = interface
      en[1] ld[1] addsub[1] x{|n_coefs|}[xbits]
    end
    
    module O = interface
      q[accbits]
    end

    let wave_cfg = 
      let disp prec = if fixed then Display.F(prec) else Display.S in
      let open Display in
      let i = I.({ (map (fun _ -> disp inpprec) t) with en=B; ld=B; addsub=B; }) in
      let i = I.(to_list (map2 (fun (n,_) b -> n,b) t i)) in
      let o = O.(to_list (map (fun (n,_) -> n, disp outprec) t)) in 
      Some(["clock",Display.B; "clear", Display.B] @ i @ o @ [("rom_coef",disp coefprec)])

    let hw i =
      {
        O.q = 
          I.(rac ~fixed ~accbits ~romshift
            ~en:i.en ~ld:i.ld ~addsub:i.addsub 
            ~romcoefs:romcoefs ~x:(Array.to_list i.x))
      }

    let tb sim i o _ = 
      let open I in
      let open O in
      let module S = Cyclesim.Api in
      S.reset sim;
      i.en := B.vdd;
      for k=0 to n_coefs - 1 do
        i.x.(k) := (try List.nth x k with _ -> B.srand xbits)
      done;
      i.ld := B.vdd;
      S.cycle sim;
      i.ld := B.gnd;
      for j=0 to xbits-1 do
        if (not fixed && j=0) || (fixed && j=xbits-1) then begin
          i.addsub := B.vdd
        end;
        S.cycle sim;
        i.addsub := B.gnd
      done;
      let q = float_of_int (B.to_sint !(o.q)) /. (2. ** float_of_int outprec) in
      Printf.printf "expected=%f got=%f\n" eval q

  end

end


