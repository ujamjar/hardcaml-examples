open HardCaml

module C = Bits.Comb.IntbitsList
type coef = C.t

module Rom(B : Comb.S) = struct

  let rec norm x = 
    let open C in
    if width x = 1 then x
    else 
      let m,l = msb x, lsbs x in
      if m = msb l then norm l
      else x

  let make coefs =
    let open C in
    let n_coefs = List.length coefs in
    (* generate rom *)
    let sum i = 
      let (+:) a b = C.Signed.(to_signal (of_signal a +: of_signal b)) in
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
    List.map (fun s -> B.constibl (sresize s w)) rom

end

open Signal.Comb
open Signal.Seq

module R = Rom(Signal.Comb)

let rac ~accbits ~romshift ~en ~ld ~last ~coefs ~x = 
  (* parallel in, serial out register *)
  let piso en ld d = lsb (reg_fb r_sync en (width d) (fun d' -> mux2 ld d (srl d' 1))) in
  (* rom address *)
  let addr = concat (List.rev (List.map (piso en ld) x)) in
  (* build and index rom *)
  let coef = mux addr (R.make coefs) in
  (* accumulator *)
  reg_fb r_sync en accbits 
    (fun acc -> 
      let acc = sra acc 1 in
      let coef = sll (sresize coef accbits) romshift in
      mux2 ld 
        (zero accbits) 
        (mux2 last (acc -: coef) (acc +: coef))
    )

module Design = struct

  open Framework

  let name = "RAC"
  let desc = "ROM-accumulator using distributed arithmetic"
  
  module Hw_config = struct
    open Param
    module C = interface xbits accbits romshift n_coefs end
    let params = C.{ 
      xbits=Int 16; 
      accbits=Int 20; 
      romshift=Int 0;
      n_coefs=Int 4;
    }
    let desc = C.{
      xbits = "input width";
      accbits = "accumulator width";
      romshift = "rom shift";
      n_coefs = "number of coefficients";
    }
    let validate _ = None
  end

  module Tb_config = struct
    module C = Interface.Empty
    let params = Interface.Empty.None
    let desc = Interface.Empty.None
    let validate _ = None
  end
  
  module Make
    (B : Comb.S)
    (H : Params with type 'a C.t = 'a Hw_config.C.t)
    (T : Params with type 'a C.t = 'a Tb_config.C.t) = struct
    
    (* get configuration parameters *)
    open Param
    let xbits = get_int H.params.Hw_config.C.xbits
    let accbits = get_int H.params.Hw_config.C.accbits
    let romshift = get_int H.params.Hw_config.C.romshift
    let n_coefs = get_int H.params.Hw_config.C.n_coefs

    (* declare interfaces *)
    module I = interface
      en[1] ld[1] last[1] x{n_coefs}[xbits]
    end
    
    module O = interface
      q[accbits]
    end

    (* hardware design *)
    let hw i =
      let coefs = List.map (C.consti 8) [ 12;13;56;4 ] in (* XXX *)
      {
        O.q = 
          rac ~accbits:accbits ~romshift:romshift 
            ~en:i.I.en ~ld:i.I.ld ~last:i.I.last ~coefs:coefs ~x:i.I.x
      }

    (* testbench *)
    let tb sim i o = ()

  end

end


