(* webworker thread *)

let run_webworker() =
  let worker = Web_worker.Thread.worker in
  worker##onmessage <- Dom.handler (fun e ->
    worker##postMessage (Js.string "hello from thread");
    Js._false
  )

