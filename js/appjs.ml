(* javascript hardcaml app framwork *)

open HardCamlFramework.Framework

let run_main () = Dom_html.window##onload <- Dom_html.handler (fun e ->
  let worker = Web_worker.Main.worker (Js.string "appww.js") in
  worker##postMessage(Js.string "hello from main");
  worker##onmessage <- Dom.handler (fun e ->
    Firebug.console##log(Js.string "Well, got the good ole message back!");
    Js._false
  );
  Js._false)

