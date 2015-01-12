(* todo *)
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

(* generate the interface to the rac *)
module type Params = sig
  val xbits : int
  val accbits : int
  val romshift : int
  val coefs : coef list
end

module Make(P : Params) = struct
  let n_coefs = List.length P.coefs 

  module I = interface
    en[1] ld[1] last[1] x{n_coefs}[P.xbits]
  end
  
  module O = interface
    q[P.accbits]
  end

  let f i = 
    {
      O.q = 
        rac ~accbits:P.accbits ~romshift:P.romshift 
          ~en:i.I.en ~ld:i.I.ld ~last:i.I.last ~coefs:P.coefs ~x:i.I.x
    }

end

module Test = struct
  (* todo... *)
end


