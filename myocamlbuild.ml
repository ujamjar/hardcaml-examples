open Ocamlbuild_plugin

let targets env build = 
  let targets = env "%.targets" in
  let targets = 
    let t = string_list_of_file targets in
    List.concat (List.map (fun t -> 
        [ 
          ["hc_" ^ t ^ ".native"];
          ["hcjs_" ^ t ^ ".js"];
          ["hcww_" ^ t ^ ".js"];
        ]) t)
  in
  let _ = List.map Outcome.good (build targets) in
  Nop

let () = dispatch @@ function
  | Before_options -> Options.use_ocamlfind := true
  | After_rules -> begin
      rule " -> hc_%.ml"
        ~prods:["hc_%.ml"]
        (fun env _ ->
          let m = module_name_of_pathname (env "%") in
          Cmd (S[
            A"echo"; 
              A("module A = HardCamlFrameworkConsole.App.Make(HardCamlExamples."^m^".Design)");
              Sh">";
              A(env "hc_%.ml")
          ])
        );
      rule " -> hcww_%.ml"
        ~prods:["hcww_%.ml"]
        (fun env _ ->
          let m = module_name_of_pathname (env "%") in
          Cmd (S[
            A"echo"; 
              A("module A = HardCamlFrameworkJS.Appww.Make(HardCamlExamples."^m^".Design)");
              Sh">";
              A(env "hcww_%.ml")
          ])
        );
      rule " -> hcjs_%.ml"
        ~prods:["hcjs_%.ml"]
        (fun env _ ->
          let m = module_name_of_pathname (env "%") in
          Cmd (S[
            A"echo"; 
              A("module A = HardCamlFrameworkJS.Appmain.Make(HardCamlExamples."^m^".Design)");
              Sh">";
              A(env "hcjs_%.ml")
          ])
        );
      rule "%.byte -> %.js"
        ~deps:["%.byte"]
        ~prods:["%.js"]
        (fun env _ ->
          Cmd (S[ 
            A"js_of_ocaml"; A"+nat.js"; A(env "%.byte")
          ])
        );
      rule "%.targets -> %.top"
        ~dep:"%.targets"
        ~stamp:"%.top"
        targets;
  end
  | _ -> ()

