module Dynamic : sig
  val activethread_of_tpoint :
    ('a,'b) Cfg.t array -> Syntax.point array -> int option
  val forward :
    (Cfg.syntax_t, 'a) Db.program -> Equation.dynamic Equation.equation
  val succ_hedge :
     ('a,'b) Cfg.t array ->
    Equation.hedge -> Equation.vertex
end

module Cfg : sig
    val thread :
      cond0: Syntax.var Bddapron.Cond.t ->
      (Cfg.syntax_t, 'a) Db.thread -> Cfg.bddapron_t
  end

val static_of_dynamic :
  Equation.dynamic Equation.equation ->
  ('a,'b,'c) Equation.static ->
  ('a,'b,'c) Equation.static Equation.equation
val backward_of_forward :
  switchfb:(Equation.vertex -> 'a -> 'a) ->
  ('a, bool, 'c) Equation.static Equation.equation ->
  ('a, bool, 'c) Equation.static Equation.equation
