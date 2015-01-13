(** Arg.parse parameter parser *)
module Command_line(P : Framework.Params) : sig
  val get : unit -> Framework.Param.t P.C.t
  val params : (string * Arg.spec * string) list
end

(** called for it's side effects *)
module Make(D : Framework.Design) : sig end

