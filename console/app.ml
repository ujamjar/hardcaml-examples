(* application framework *)

open HardCamlFramework.Framework
open HardCaml

module Command_line(P : Params) = struct
  open Param

  let parse_list f l str = 
    try
      l := List.map f Str.(split (regexp ",") str)
    with _ ->
      raise (Arg.Bad "Failed to read list")

  let get, params = 
    let f = 
      (function
        | Flag(x) -> let x = ref x in (fun () -> Flag(!x)), Arg.Set(x), " "
        | Int(x) -> let x = ref x in (fun () -> Int(!x)), Arg.Set_int(x), "<int> "
        | Float(x) -> let x = ref x in (fun () -> Float(!x)), Arg.Set_float(x), "<float> "
        | String(x) -> let x = ref x in (fun () -> String(!x)), Arg.Set_string(x), "<str> "
        | File(x) -> let x = ref x in (fun () -> File (!x)), Arg.Set_string(x), "<file> "
        | Symbol(c,x) -> 
            let x = ref x in (fun () -> Symbol(c,!x)), Arg.Symbol(c,(fun s -> x := s)), " "
        | Int_list(x) -> 
            let x = ref x in 
            (fun () -> Int_list(!x)), Arg.String(parse_list int_of_string x), "<4,-1,> "
        | Float_list(x) -> 
            let x = ref x in 
            (fun () -> Float_list(!x)), Arg.String(parse_list float_of_string x), "<0.3,1.,> "
      ) 
    in
    
    let p = P.map2 (fun (n,_) (p,d) -> let f,p,x = f p in f, (("-"^n), p, x^d)) P.t P.params in

    (fun () -> P.map2 (fun (f,_) (n,_) -> f(), n) p P.t), 
    P.(to_list (map snd p))

end

module Circuit_gen(B : Comb.S)(I : Interface.S)(O : Interface.S) = struct

    module S = Cyclesim.Make(B)
    module Cs = Cyclesim.Api

    let make name logic sim_provider = 
        let outputs = logic I.(map (fun (n,b) -> Signal.Comb.input n b) t) in 
        let circuit = Circuit.make name 
            (O.to_list (O.map2 (fun (n,_) s -> Signal.Comb.output n s) O.t outputs))
        in
        let sim = sim_provider circuit in
        let inputs = I.(map (fun (n,_) -> try Cs.in_port sim n with _ -> ref B.empty) t) in
        let outputs = O.(map (fun (n,_) -> try Cs.out_port sim n with _ -> ref B.empty) t) in
        let next = O.(map (fun (n,_) -> try Cs.out_port_next sim n with _ -> ref B.empty) t) in
        circuit, sim, inputs, outputs, next

end

module Make(D : Design) = struct

  (*module B = Bits.Ext.Comb.BigarraybitsNativeint*)
  module B = Bits.Ext.Comb.ArraybitsNativeint
  (*module B = Bits.Ext.Comb.IntbitsList*)

  module Gtkwave = Vcd.Gtkwave(B)
  module Vcd = Vcd.Make(B)
  module ISim = Cyclesim.Interactive(B)
  module Waveterm_waves = HardCamlWaveTerm.Wave.Make(HardCamlWaveTerm.Wave.Bits(B))
  module Waveterm_sim = HardCamlWaveTerm.Sim.Make(B)(Waveterm_waves)
  module Waveterm_ui = HardCamlWaveTerm.Ui.Make(B)(Waveterm_waves)

  (* frame work command line arguments *)
  module Std_config = struct
    open Param
    include interface 
      vlog vhdl csim 
      tb llvm vpi checktb
      interactive vcd waveterm gtkwave 
    end
    let params = {
      vlog = File "", "generate verilog netlist";
      vhdl = File "", "generate vhdl netlist";
      csim = File "", "generate C simulation model";
      tb = Flag false, "run testbench";
      llvm = Flag false, "use LLVM backend to run testbench";
      vpi = Flag false, "use Icarus Verilog to run testbench";
      vcd = File "", "generate VCD file";
      waveterm = Flag false, "integrated waveform viewer";
      gtkwave = Flag false, "gtkwave waveform viewer";
      interactive = Flag false, "interactive text driven testbench mode";
      checktb = Flag false, "compare ocaml simulation with LLVM/VPI backend";
    }
    let validate _ = Ok
  end

  (* command line processing *)
  module Std_cl = Command_line(Std_config)
  module Hw_cl = Command_line(D.Hw_config)
  module Tb_cl = Command_line(D.Tb_config)
  let () = 
    let help = Omd_term.text_of_md (Omd.of_string D.desc) in
    Arg.parse
      (Arg.align (Std_cl.params @ Hw_cl.params @ Tb_cl.params))
      (fun _ -> failwith "Anonymous arguments not allowed")
      help
  let std_params = Std_cl.get ()
  let hw_params = Hw_cl.get ()
  let tb_params = Tb_cl.get ()

  (* validate *)
  let pair (n,_) v = (v, n) 
  let check = function
    | Param.Error(errs) -> begin
      Printf.eprintf "Parameter errors:\n"; 
      List.iter (fun e -> Printf.eprintf "%s\n" e) errs;
      exit 0
    end
    | Param.Ok -> ()

  let () = check (D.validate hw_params tb_params)

  (* build hardware with user configuration *)
  module H = D.Make(B)
    (struct
      include D.Hw_config
      let params = hw_params
    end)
    (struct
      include D.Tb_config
      let params = tb_params
    end)

  open Param
  open Std_config

  (* generate circuit *)
  module G = Circuit_gen(B)(H.I)(H.O)
  module Cs = Cyclesim.Make(B)
  (*module Cs_llvm = HardCamlLlvmsim.Sim.Make(B)*)
  module Cs_vpi = Cosim.Make(B)
  (* choose simulation backend.
     ocaml, llvm or vpi backend, with optional combining *)
  let circ, sim, i, o, n =
    (* options *)
    let tb = get_bool std_params.tb in
    let llvm = get_bool std_params.llvm in
    let vpi = get_bool std_params.vpi in
    let checktb = get_bool std_params.checktb in
    (* simulators *)
    let cs = Cs.make ?log:None ?inst:None ~internal:(Some(fun s -> Signal.Types.names s <> [])) in
    let cs_llvm c = 
      let () = Dynlink.allow_unsafe_modules true in
      let () = 
        try HardCamlDynlink.Sim_provider.load_provider_from_package "hardcaml-llvmsim" "HardCamlLlvmsim.cma"
        with Dynlink.Error e -> failwith (Dynlink.error_message e)
      in
      let sim = HardCamlDynlink.Sim_provider.get_provider "hardcaml-llvmsim" in
      let module F = (val sim : HardCamlDynlink.Sim_provider.S) in
      let module Cs_llvm = F(B) in
      Cs_llvm.make c in
    let cs_vpi = Cs_vpi.make ?dump_file:None in
    let combine = Cs.combine_strict in
    let open Printf in
    let sim = 
      match tb, checktb, llvm, vpi with 
      | true, false, true, false -> cs_llvm
      | true, false, false, true -> cs_vpi
      | true, false, true, true -> failwith "cannot specify llvm and vpi without checktb"
      | true, true, true, false -> (fun s -> combine (cs s) (cs_llvm s))
      | true, true, false, true -> (fun s -> combine (cs s) (cs_vpi s))
      | true, true, true, true -> (fun s -> combine (cs s) (combine (cs_vpi s) (cs_llvm s)))
      | _ -> cs
    in
    G.make D.name H.hw sim

  (* write netlists *)

  let with_out_file name f = 
    let file = open_out name in
    let res = f file in
    let () = close_out file in
    res
  let write file_name rtl = 
    if file_name <> "" then 
      with_out_file file_name
        (fun file -> rtl (output_string file) circ)

  let () = write (get_string std_params.vlog) Rtl.Verilog.write
  let () = write (get_string std_params.vhdl) Rtl.Vhdl.write
  let () = write (get_string std_params.csim) Rtl.C.write

  (* run testbench *)
  let out_file_fn name = 
    let file = open_out name in
    at_exit (fun () -> close_out file);
    (fun s -> output_string file s)

  let wave_cfg = 
    let str_of_fixed prec b =
      let p = 2. ** (float_of_int prec) in
      let i = float_of_int (B.to_sint b) in
      string_of_float (i /. p)
    in
    match H.wave_cfg with 
    | None -> None 
    | Some(l) -> 
      Some(List.map (fun (n,t) -> n,
        match t with
        | Display.B -> Waveterm_waves.B
        | Display.U -> Waveterm_waves.U
        | Display.S -> Waveterm_waves.S
        | Display.H -> Waveterm_waves.H
        | Display.F prec -> Waveterm_waves.F(str_of_fixed prec)) l)

  let () = 
    if get_bool std_params.tb then begin
      (* write vcd file *)
      let sim = 
        if get_string std_params.vcd <> "" then
          Vcd.wrap (out_file_fn (get_string std_params.vcd)) sim
        else sim
      in
      (* run gtkwave *)
      let sim = 
        if get_bool std_params.gtkwave then
          Gtkwave.gtkwave ~args:"" sim
        else sim
      in
      (* wrap sim for waveterm *)
      let sim, waves = 
        if get_bool std_params.waveterm then
          let sim, waves = Waveterm_sim.wrap ?cfg:wave_cfg sim in
          sim, Some(waves)
        else sim, None
      in
      let () = 
        if get_bool std_params.interactive then (* use hardcaml-waveterm when ready *)
          ISim.run stdin sim
        else
          H.tb sim i o n
      in
      (* display waveterm *)
      let () = 
        match waves with
        | None -> ()
        | Some(waves) -> 
          Lwt_main.run (Waveterm_ui.run Waveterm_waves.({ cfg=default; waves }))
      in
      let () = 
        if get_bool std_params.gtkwave then 
          (Printf.printf "press <return> to exit\n"; ignore(read_line()))
      in
      ()
    end

end


