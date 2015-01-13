(* application framework *)

open Framework
open HardCaml

module Command_line(P : Params) = struct
  open Param
  let get, params = 
    let v = P.C.map 
      (function
        | Flag(x) -> let x = ref x in (fun () -> Flag(!x)), Arg.Set(x)
        | Int(x) -> let x = ref x in (fun () -> Int(!x)), Arg.Set_int(x)
        | String(x) -> let x = ref x in (fun () -> String(!x)), Arg.Set_string(x)) 
      P.params
    in
    let p = P.C.map2 (fun (n,_) (_,p) -> ("-"^n), p) P.C.t v in
    let p = P.C.map2 (fun (n,p) d -> n, p, d) p P.desc in
    (fun () -> P.C.map (fun (f,_) -> f()) v), (P.C.to_list p)
end

module Make(D : Design) = struct

  (* frame work command line arguments *)
  module Std_config = struct
    open Param
    module C = interface vlog vhdl csim testbench end
    let params = C.{
      vlog = String "";
      vhdl = String "";
      csim = String "";
      testbench = Flag false;
    }
    let desc = C.{
      vlog = "generate verilog netlist";
      vhdl = "generate vhdl netlist";
      csim = "generate C simulation model";
      testbench = "run testbench";
    }
    let validate _ = None
  end

  (* command line processing *)
  module Std_cl = Command_line(Std_config)
  module Hw_cl = Command_line(D.Hw_config)
  module Tb_cl = Command_line(D.Tb_config)
  let () = Arg.parse
    (Std_cl.params @ Hw_cl.params @ Tb_cl.params)
    (fun _ -> failwith "Anonymous arguments not allowed")
    (D.name ^ "\n\n" ^ D.desc ^ "\n")
  let std_params = Std_cl.get ()
  let hw_params = Hw_cl.get ()
  let tb_params = Tb_cl.get ()

  (* validate *)
  let () = match D.Hw_config.validate hw_params with None -> () | Some(x) -> failwith x
  let () = match D.Tb_config.validate tb_params with None -> () | Some(x) -> failwith x

  (* build hardware with user configuration *)
  module B = Bits.Comb.IntbitsList
  module H = D.Make(B)
    (struct
      include D.Hw_config
      let params = hw_params
    end)
    (struct
      include D.Tb_config
      let params = tb_params
    end)

  module Vcd = Vcd.Make(B)
  module Wave = Vcd_ext.Make(B)

  (* generate circuit *)
  module G = Interface.Gen(B)(H.I)(H.O)
  let circ, sim, i, o = G.make D.name H.hw

  let with_out_file name f = 
    let file = open_out name in
    let res = f file in
    let () = close_out file in
    res

  (* write netlists *)
  open Param
  open Std_config.C
  let write file_name rtl = 
    if file_name <> "" then 
      with_out_file file_name
        (fun file -> rtl (output_string file) circ)
  let () = write (get_string std_params.vlog) Rtl.Verilog.write
  let () = write (get_string std_params.vhdl) Rtl.Vhdl.write
  let () = write (get_string std_params.csim) C.write

  (* run testbench *)
  let () = 
    if get_flag std_params.testbench then
      H.tb sim i o

end


