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
  open Param

  let name = "RAC"
  let desc = "ROM-accumulator using distributed arithmetic"
  
  module Hw_config = struct
    include interface xbits accbits romshift coefs end
    let params = { 
      xbits=Int 16, "input width";
      accbits=Int 20, "accumulator width";
      romshift=Int 0, "rom shift";
      coefs=Int_list [0;0;0;0], "list of coefs";
    }
  end

  module Tb_config = Params_none
  
  let validate _ _ = Ok

  module Make
    (B : Comb.S)
    (H : Params with type 'a t = 'a Hw_config.t)
    (T : Params with type 'a t = 'a Tb_config.t) = struct
    
    (* get configuration parameters *)
    let xbits = get_int H.params.Hw_config.xbits
    let accbits = get_int H.params.Hw_config.accbits
    let romshift = get_int H.params.Hw_config.romshift
    let coefs = get_int_list H.params.Hw_config.coefs
    let n_coefs = List.length coefs

    (* declare interfaces *)
    module I = interface
      en[1] ld[1] last[1] x{n_coefs}[xbits]
    end
    
    module O = interface
      q[accbits]
    end

    let wave_cfg = None

    (* hardware design *)
    let hw i =
      let coefs = List.map (C.consti 32) coefs in (* XXX *)
      {
        O.q = 
          rac ~accbits:accbits ~romshift:romshift 
            ~en:i.I.en ~ld:i.I.ld ~last:i.I.last ~coefs:coefs ~x:i.I.x
      }

    (* testbench *)
    let tb sim i o = 
      let open I in
      let open O in
      let module S = Cyclesim.Api in
      S.reset sim;     
      for j=0 to 1 do
        S.cycle sim;
      done

  end

end


