class type ['target] worker_event = object
    inherit ['target] Dom.event 
    method data : Js.js_string Js.t Js.readonly_prop
end

module Main : sig 
    class type worker = object('self)
        method terminate : unit Js.meth
        method postMessage : Js.js_string Js.t -> unit Js.meth
        method onmessage : ('self Js.t, worker worker_event Js.t) 
            Dom_html.event_listener Js.writeonly_prop
    end

    val worker : Js.js_string Js.t -> worker Js.t
end

module Thread : sig
    class type worker = object('self)
        method close : unit Js.meth
        method postMessage : Js.js_string Js.t -> unit Js.meth
        method onmessage : ('self Js.t, worker worker_event Js.t) 
            Dom_html.event_listener Js.writeonly_prop
    end

    val worker : worker Js.t
end


