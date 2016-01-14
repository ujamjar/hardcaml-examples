open HardCaml.Utils

(* xapp052  2..168 *)
let taps = [|
  []; []; [2;1]; [3;2]; [4;3]; [5;3]; [6;5]; [7;6]; [8;6;5;4]; [9;5]; [10;7];
  [11;9]; [12;6;4;1]; [13;4;3;1]; [14;5;3;1]; [15;14]; [16;15;13;4]; [17;14];
  [18;11]; [19;6;2;1]; [20;17]; [21;19]; [22;21]; [23;18]; [24;23;22;17];
  [25;22]; [26;6;2;1]; [27;5;2;1]; [28;25]; [29;27]; [30;6;4;1]; [31;28];
  [32;22;2;1]; [33;20]; [34;27;2;1]; [35;33]; [36;25]; [37;5;4;3;2;1];
  [38;6;5;1]; [39;35]; [40;38;21;19]; [41;38]; [42;41;20;19]; [43;42;38;37];
  [44;43;18;17]; [45;44;42;41]; [46;45;26;25]; [47;42]; [48;47;21;20]; [49;40];
  [50;49;24;23]; [51;50;36;35]; [52;49]; [53;52;38;37]; [54;53;18;17]; [55;31];
  [56;55;35;34]; [57;50]; [58;39]; [59;58;38;37]; [60;59]; [61;60;46;45];
  [62;61;6;5]; [63;62]; [64;63;61;60]; [65;47]; [66;65;57;56]; [67;66;58;57];
  [68;59]; [69;67;42;40]; [70;69;55;54]; [71;65]; [72;66;25;19]; [73;48];
  [74;73;59;58]; [75;74;65;64]; [76;75;41;40]; [77;76;47;46]; [78;77;59;58];
  [79;70]; [80;79;43;42]; [81;77]; [82;79;47;44]; [83;82;38;37]; [84;71];
  [85;84;58;57]; [86;85;74;73]; [87;74]; [88;87;17;16]; [89;51]; [90;89;72;71];
  [91;90;8;7]; [92;91;80;79]; [93;91]; [94;73]; [95;84]; [96;94;49;47];
  [97;91]; [98;87]; [99;97;54;52]; [100;63]; [101;100;95;94]; [102;101;36;35];
  [103;94]; [104;103;94;93]; [105;89]; [106;91]; [107;105;44;42]; [108;77];
  [109;108;103;102]; [110;109;98;97]; [111;101]; [112;110;69;67]; [113;104];
  [114;113;33;32]; [115;114;101;100]; [116;115;46;45]; [117;115;99;97];
  [118;85]; [119;111]; [120;113;9;2]; [121;103]; [122;121;63;62]; [123;121];
  [124;87]; [125;124;18;17]; [126;125;90;89]; [127;126]; [128;126;101;99];
  [129;124]; [130;127]; [131;130;84;83]; [132;103]; [133;132;82;81]; [134;77];
  [135;124]; [136;135;11;10]; [137;116]; [138;137;131;130]; [139;136;134;131];
  [140;111]; [141;140;110;109]; [142;121]; [143;142;123;122]; [144;143;75;74];
  [145;93]; [146;145;87;86]; [147;146;110;109]; [148;121]; [149;148;40;39];
  [150;97]; [151;148]; [152;151;87;86]; [153;152]; [154;152;27;25];
  [155;154;124;123]; [156;155;41;40]; [157;156;131;130]; [158;157;132;131];
  [159;128]; [160;159;142;141]; [161;143]; [162;161;75;74]; [163;162;104;103];
  [164;163;151;150]; [165;164;135;134]; [166;165;128;127]; [167;161];
  [168;166;153;151];
|]

let counterpart taps = 
  let n,t = List.hd taps, List.tl taps in
  let t = List.map (fun t -> n - t) t in
  n :: t

module type S = sig
  include HardCaml.Comb.S
  val lfsr : (t -> t -> t) -> int list -> t -> t
end

module Galois(B : HardCaml.Comb.S) = struct
  include B
  let lfsr (^:) poly = 
    let poly = List.rev (List.tl poly) in
    let rec f n poly lo state =
      match poly with 
      | [] -> [select_e state (width state - 1) n]
      | p :: poly ->
        select_e state (p-1) n :: 
        (lo ^: bit state p) :: 
        f (p+1) poly lo state
    in
    let mk state = 
      let l = bit state 0 in
      let s = f 0 poly l state in
      let s = concat_e (List.rev s) in
      l @: (msbs s)
    in
    mk
end

module Fibonacci(B : HardCaml.Comb.S) = struct

  include B
  let lfsr (^:) poly state =  
    let p = List.map (fun i -> bit state (i-1)) poly in
    let f = List.fold_left (fun a p -> a ^: p) (List.hd p) (List.tl p) in
    lsbs state @: f

end

module Make(B : S) = struct
  open B

  let lfsr = lfsr

  let lfsr_xor = lfsr (^:)
  let lfsr_xnor = lfsr (fun a b -> ~: (a ^: b))

  let seq lfsr state = 
    let n = width state in
    let rec f m st = 
      if (1 lsl n)-1 = m then []
      else
        let st = lfsr st in
        st :: f (m+1) st
    in
    f 0 state

  let seq_xor n = seq (lfsr_xor taps.(n)) (ones n)

  let seq_xnor n = seq (lfsr_xnor taps.(n)) (zero n)

end

module Design = struct

  open HardCaml
  open HardCamlFramework.Framework
  open Param

  let name = "LFSR"
  let desc = "Linear Feedback Shift Register"

  module Hw_config = struct
    include interface bits structure xnor counterpart end
    let params = {
      bits = Int 8, "LFSR width";
      structure = Symbol(["fibonacci"; "galois"], "fibonacci"), "LFSR structure";
      xnor = Flag false, "Build with XNOR rather then XOR gates";
      counterpart = Flag false, "Create counterpart tap structure";
    }
  end

  module Tb_config = struct
    include interface cycles end
    let params = {
      cycles = Int 10, "Number of cycles to test";
    }
  end

  let validate hw tb = 
    gt hw.Hw_config.bits 1 >>
    lt hw.Hw_config.bits 169

  module Make
    (B : Comb.S)
    (H : Params with type 'a t = 'a Hw_config.t)
    (T : Params with type 'a t = 'a Tb_config.t) = struct

    open Hw_config
    open Tb_config
    let bits = get_int H.params.bits
    let structure = get_string H.params.structure
    let xnor = get_bool H.params.xnor
    let counterpt = get_bool H.params.counterpart
    let cycles = get_int T.params.cycles

    module I = interface d[bits] end
    module O = interface q[bits] end

    let wave_cfg = None
  
    module F = Fibonacci(Signal.Comb)
    module G = Galois(Signal.Comb)

    let hw i = 
      let lfsr = 
        if structure = "galois" then G.lfsr
        else F.lfsr
      in
      let op = 
        let open Signal.Comb in
        if xnor then (fun a b -> ~: (a ^: b))
        else (^:)
      in
      let taps = if counterpt then counterpart taps.(bits) else taps.(bits) in
      O.({ q = lfsr op taps i.I.d })

    let tb sim i o _ = 
      let open I in
      let open O in
      let module S = Cyclesim.Api in
      S.reset sim;     
      i.d := B.consti bits (if xnor then 0 else 1);
      for j=0 to cycles - 1 do
        S.cycle sim;
        i.d := !(o.q);
      done

  end

end

