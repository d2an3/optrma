module Block : sig
  type binstr =
    (Syntax.var Bddapron.Syntax.expr,
    Syntax.var Bddapron.Syntax.expr list) Cfg.binstr
  val simplify : binstr list -> binstr list
end
val procedure :
  removed:Syntax.point PSette.t ref ->
  internalize:bool ->
  truebranch:bool ->
  basicblock:bool ->
  scheduling:[ `Cooperative | `Preemptive ] ->
  writefilter:(Syntax.var -> bool) ->
  readfilter:(Syntax.var -> bool) ->
   (Cfg.syntax_t, Dbconcur.readwrite) Db.program ->
  Cfg.syntax_t Db.procedure ->
  unit
val program :
  removed:Syntax.point PSette.t ref ->
  internalize:bool ->
  truebranch:bool ->
  basicblock:bool ->
  scheduling:[ `Cooperative | `Preemptive ] ->
  (Cfg.syntax_t, Dbconcur.readwrite) Db.program -> unit
