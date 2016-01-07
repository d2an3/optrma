(** Converting from [Syntax.program] to [Db.program] *)

val env_add_vars :
  Syntax.var Bddapron.Env.t ->
  ([< Syntax.var],Syntax.var Bddapron.Env.typ) Mappe.t -> 
  Syntax.var Bddapron.Env.t

val program : 
  env0:Syntax.var Bddapron.Env.t ->
  cond0:Syntax.var Bddapron.Cond.t ->
  Syntax.var Syntax.program -> (unit, unit) Db.program
