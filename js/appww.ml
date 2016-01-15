(* webworker thread *)
open HardCamlFramework.Framework
open HardCaml

module Make(D : Design) = struct

  module Message = Message.Make(D)

  module B = Bits.Comb.IntbitsList
  (*module S = Interface.Gen(B)(M.I)(M.O)*)

  let get_rtl hw fn = 
    let module M = D.Make(B)
      (struct
        include D.Hw_config
        let params = hw
      end)
      (D.Tb_config)
    in
    let module C = Interface.Circ(M.I)(M.O) in
    let buf = Buffer.create 1024 in
    fn (Buffer.add_string buf) (C.make D.name M.hw);
    Buffer.contents buf

  let get_wave hw tb = 
    let module M = D.Make(B)
      (struct
        include D.Hw_config
        let params = hw
      end)
      (struct
        include D.Tb_config
        let params = tb
      end)
    in
    let module S = Interface.Gen(B)(M.I)(M.O) in
    let circ, sim, i, o, o' = S.make D.name M.hw in
    let sim, wave = HardCamlJS.Wave.wrap sim in
    M.tb sim i o o';
    wave

  let run_webworker() =
    let worker = Web_worker.Thread.worker in
    let try_err msg f = 
      try f () 
      with e -> worker##postMessage(Message.(str_of_ww (WError [msg; Printexc.to_string e])))
    in
    worker##onmessage <- Dom.handler (fun e ->
      let _ = match Message.main_of_str e##data with
      | Message.MVerilog hw -> 
        try_err "an error occured while generating Verilog" @@ fun () -> 
          worker##postMessage(Message.(str_of_ww (WVerilog (get_rtl hw Rtl.Verilog.write))))
      | Message.MVhdl hw -> 
        try_err "an error occured while generating VHDL" @@ fun () -> 
          worker##postMessage(Message.(str_of_ww (WVhdl (get_rtl hw Rtl.Vhdl.write))))
      | Message.MSimulate (hw,tb) -> 
        try_err "an error occured during simulation" @@ fun () -> 
          worker##postMessage(Message.(str_of_ww (WSimulate(get_wave hw tb))))
      in
      Js._false
    )

  let () = run_webworker ()

end
