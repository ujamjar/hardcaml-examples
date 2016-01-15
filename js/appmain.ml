(* javascript hardcaml app framwork *)

(* split into 2 parts - the main thread which runs the user interface, and
 * a webworker which constructs circuits and runs simulations *)

open HardCamlFramework.Framework

module D = Dom_html

let license = "
Copyright (C) 2016 by MicroJamJar Ltd.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the \"Software\"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
"

module Js_utils = struct
  let debug = true
  let jlog s = if debug then Firebug.console##log(s)
  let log s = if debug then jlog (Js.string s)

  let jstr = Js.string
  let jstri x = jstr (string_of_int x)
  let ostr = Js.to_string

  let cast s = (s :> Dom.element Js.t) 

  let get_element e = 
    let d = D.document in
    Js.Opt.get 
      (d##getElementById (Js.string e))
      (fun () -> failwith "getElementById")

  let get_attr d a = 
    ostr @@ Js.Opt.get
      (d##getAttribute(Js.string a))
      (fun () -> failwith "getAttribute")

  let bold s = String.concat "" [ "<b>"; s; "</b>" ]
  let italic s = String.concat "" [ "<i>"; s; "</i>" ]
  let h1 s = String.concat "" [ "<h1>"; s; "</h1>" ]

  let mk_div () = 
    let div = D.createDiv D.document in
    div

  let mk_p s = 
    let d = D.document in
    let t = D.createP d in
    t##innerHTML <- Js.string s;
    t

  let mk_button label = 
    let b = D.createButton D.document in
    b##innerHTML <- Js.string label;
    b##className <- Js.string "pure-button";
    b

  let mk_text init = 
    let d = D.document in
    let t = D.createInput ~_type:(jstr "text") d in
    t##value <- jstr init;
    t

  let mk_integer x = 
    let d = D.document in
    let t = D.createInput ~_type:(jstr "number") d in
    t##value <- jstr (string_of_int x);
    t

  let mk_real x = 
    let d = D.document in
    let t = D.createInput ~_type:(jstr "number") d in
    t##value <- jstr (Printf.sprintf "%f" x);
    t

  (* create a checkbox with initial value *)
  let mk_check ?(className="") b = 
    let d = D.document in
    let i = D.createInput ~_type:(jstr "checkbox") d in
    i##checked <- Js.bool b;
    i

  let mk_dropdown n = 
    let d = D.document in
    let s = D.createSelect d in
    let o = List.map (fun n ->
      let o = D.createOption d in
      o##value <- jstr n;
      o##innerHTML <- jstr n;
      Dom.appendChild s o;
      o
    ) n in
    s, o 

  let mk_table hdr elts =
    let d = D.document in
    let table = D.createTable d in
    table##className <- Js.string "pure-table-striped";
    if hdr <> [||] then begin
      let thdr = D.createThead d in
      Dom.appendChild table thdr;
      let trow = D.createTr d in
      Array.iter 
        (fun elt ->
          let th = D.createTh d in
          Dom.appendChild th elt;
          Dom.appendChild trow th;
        ) hdr;
      Dom.appendChild thdr trow;
    end;
    let tbody = D.createTbody d in
    Dom.appendChild table tbody;
    Array.iter 
      (fun elts -> 
        let trow = D.createTr d in
        Array.iter
          (fun elt ->
            let telt = D.createTd d in
            Dom.appendChild telt elt;
            Dom.appendChild trow telt)
          elts;
        Dom.appendChild tbody trow)
      elts;
    table

  let rec delete_kids par = 
    let kid = par##firstChild in
    match Js.Opt.to_option kid with
    | Some(kid) ->  begin
      Dom.removeChild par kid;
      delete_kids par
    end
    | None -> ()

  let mk_textbuffer ?(rows=20) ?(cols=80) par buffer = 
    let d = D.document in
    let button = mk_button "select code" in
    let text = D.createTextarea d in
    button##innerHTML <- jstr "select code";
    button##onclick <- D.handler (fun _ -> text##select(); Js._false);
    button##title <- jstr "Click here to select the code.";

    text##className <- jstr "textbuf-area";
    text##rows <- rows;
    text##cols <- cols;
    text##defaultValue <- jstr buffer;
    text##readOnly <- Js._true;

    Dom.appendChild par button;
    Dom.appendChild par (D.createBr d);
    Dom.appendChild par text

end

module Options(P : Params) = struct

  open Js_utils
  open Param
  
  let parse_list name f str = 
    try
      List.map f Regexp.(split (regexp ",") str)
    with _ ->
      raise (Arg.Bad (bold name ^ ": failed to read list"))

  (* create a ui element for the parameter, and return an accessor function *)
  let ui_elt name = function
    | Flag(x) -> 
      let c = mk_check x in
      cast c, "", (fun () -> Flag(Js.to_bool c##checked))
    | Int(x) -> 
      let t = mk_integer x in
      cast t, "integer", (fun () -> Int(int_of_string (ostr t##value)))
    | Float(x) -> 
      let t = mk_real x in
      cast t, "real", (fun () -> Float(float_of_string (ostr t##value)))
    | String(x) -> 
      let t = mk_text x in
      cast t, "string", (fun () -> String(ostr t##value))
    | Symbol(c,x) ->
      let d, o = mk_dropdown c in
      let rec selected c o = 
        match c, o with
        | c::c', o::o' -> if Js.to_bool o##selected then c else selected c' o'
        | _ -> failwith "dropdown not selected"
      in
      List.iter2 (fun c o -> if c=x then o##selected <- Js._true) c o;
      d##required <- Js._true;
      cast @@ d, "", (fun () -> Symbol(c,selected c o))
    | Int_list(x) -> 
      let t = mk_text "" in
      cast t, "integer list", (fun () -> Int_list(parse_list name int_of_string (ostr t##value)))
    | Float_list(x) -> 
      let t = mk_text "" in
      cast t, "real list", (fun () -> Float_list(parse_list name float_of_string (ostr t##value)))
    | File _ -> 
      (cast @@ mk_p "files not supported!"), "", (fun () -> File "")
  
  let try_arg_bad name f = 
    try f ()
    with Arg.Bad s -> raise (Arg.Bad s)
       | e -> raise (Arg.Bad(bold name ^ ": " ^ Printexc.to_string e))

  let ui_rows, get = 
    let f (n,_) (p,d) = 
      let u,t,g = ui_elt n p in 
      let label s  = cast @@ mk_p s in
      let desc d t = if t="" then label d else label (d ^ " (" ^ italic t ^ ")") in
      [| label @@ bold n; desc d t; u |], 
      (fun () -> try_arg_bad n g) 
    in
    let x = P.map2 f P.t P.params in
    (P.to_list @@ P.map fst x), P.map snd x

end

module Make(D : Design) = struct

  module Message = Message.Make(D)

  (* standard options *)
  module Std_config = struct
    open Param
    include interface
      vlog vhdl csim tb
    end
    let params = {
      vlog = Flag false, "generate verilog netlist";
      vhdl = Flag false, "generate vhdl netlist";
      csim = Flag false, "generate C simulation model";
      tb = Flag false, "run testbench";
    }
    let validate _ = Ok
  end

  (*module Std_cl = Options(Std_config)*)
  module Hw_cl = Options(D.Hw_config)
  module Tb_cl = Options(D.Tb_config)

  (* build table with options *)
  let build_parameters_table () = 
    let rows = 
      Array.of_list @@ 
      List.concat [ Hw_cl.ui_rows; Tb_cl.ui_rows; ] 
    in
    let hdr = Array.map (fun x -> Js_utils.(cast @@ mk_p @@ bold x))
      [| "Parameter"; "Description"; "Value" |]
    in
    Js_utils.mk_table hdr rows

  let build_desc () = 
    Js_utils.h1 D.name ^ Omd.to_html @@ Omd.of_string D.desc

  exception ParamValidationErrors of string list

  let get_hw_params () = D.Hw_config.(map2 (fun f (n,_) -> f(), n) Hw_cl.get t)
  let get_tb_params () = D.Tb_config.(map2 (fun f (n,_) -> f(), n) Tb_cl.get t)

  let get_params () = 
    let hw, tb = get_hw_params (), get_tb_params () in
    let check = function
      | Param.Error(errs) -> raise (ParamValidationErrors errs)
      | Param.Ok -> ()
    in
    check (D.validate hw tb);
    hw, tb

  (* run *)
  let run_main () = Dom_html.window##onload <- Dom_html.handler (fun _ ->
    let main_div = Js_utils.get_element "hardcaml-framework-webapp" in
    let jsfile = Js_utils.get_attr main_div "data-hcww" in

    let add_div () = 
      let div = Js_utils.mk_div () in
      div##style##marginBottom <- Js_utils.jstr "20px";
      Dom.appendChild main_div div;
      div
    in
    let descr_div = add_div () in
    let params_div = add_div () in
    let std_div = add_div () in
    let err_div = add_div () in
    let rtl_div = add_div () in
    let sim_div = add_div () in

    let show_error errs =
      Js_utils.delete_kids err_div;
      List.iter (fun e -> Dom.appendChild err_div (Js_utils.mk_p e)) errs
    in

    (* start webworker *)
    let worker = Web_worker.Main.worker (Js.string jsfile) in

    (* generate control messages *)
    let bverilog, bvhdl, bsimulate = 
      Js_utils.mk_button "Verilog", Js_utils.mk_button "VHDL", Js_utils.mk_button "Simulate"
    in

    let build_std_options () = 
      let div = Js_utils.mk_div () in
      Dom.appendChild div bverilog; Dom.appendChild div bvhdl; Dom.appendChild div bsimulate;
      div
    in

    let enable_buttons b = 
      bverilog##disabled <- if b then Js._false else Js._true;
      bvhdl##disabled <- if b then Js._false else Js._true;
      bsimulate##disabled <- if b then Js._false else Js._true;
    in

    let verilog _ = 
      begin try 
        show_error [];
        worker##postMessage(Message.(str_of_main (MVerilog (fst @@ get_params()))));
        enable_buttons false;
      with ParamValidationErrors errs -> show_error ("parameter validation error"::errs)
         | e -> show_error [ "an error occured while generating Verilog"; Printexc.to_string e ]
      end;
      Js._false
    in

    let vhdl _ = 
      begin try 
        show_error [];
        worker##postMessage(Message.(str_of_main (MVhdl (fst @@ get_params()))));
        enable_buttons false;
      with ParamValidationErrors errs -> show_error ("parameter validation error"::errs)
         | e -> show_error [ "an error occured while generating VHDL"; Printexc.to_string e ]
      end;
      Js._false 
    in

    let simulate _ = 
      begin try 
        show_error [];
        worker##postMessage(Message.(str_of_main (MSimulate (get_params()))));
        enable_buttons false;
      with ParamValidationErrors errs -> show_error ("parameter validation error"::errs)
         | e -> show_error [ "an error occured while setting up the simulation"; 
                             Printexc.to_string e ]
      end;
      Js._false 
    in

    bverilog##onclick <- Dom_html.handler verilog;
    bvhdl##onclick <- Dom_html.handler vhdl;
    bsimulate##onclick <- Dom_html.handler simulate;

    let write_rtl rtl = 
      Js_utils.delete_kids rtl_div;
      Js_utils.mk_textbuffer rtl_div rtl
    in

    let show_sim wave = 
      Js_utils.delete_kids sim_div;
      HardCamlJS.Wave.Gui.mk_wave_table sim_div 400 20 wave
    in

    (* respond to webworker message *)
    worker##onmessage <- Dom.handler (fun e ->
      let _ = match Message.ww_of_str e##data with
      | Message.WVerilog vlog -> write_rtl vlog
      | Message.WVhdl vhdl -> write_rtl vhdl
      | Message.WSimulate wave -> show_sim wave
      | Message.WError errs -> show_error errs
      in
      enable_buttons true;
      Js._false);

    let () = descr_div##innerHTML <- Js.string (build_desc()) in
    let () = Dom.appendChild params_div (build_parameters_table ()) in
    let () = Dom.appendChild std_div (build_std_options ()) in

    Js._false)

  let () = run_main ()

end

