(** Solving the equations *)

(* This file is part of the ConcurInterproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Bertrand Jeannet 2009.
*)

module Solving1 : sig
  type 'a abstract = (Syntax.var, 'a) Bddapron.Domain1.t
  type 'a manager = (Equation.vertex, Equation.hedge, 'a abstract, bool) Fixpoint.manager
  type 'a output = ('a abstract, bool, Fixpoint.stat) Equation.static

  val make_fpmanager :
    debug:int ->
    equation:'a Equation.equation ->
    abstract_init:((Syntax.var, 'b, 'c, 'd) Bddapron.Domain1.man ->
      Equation.vertex -> 'd abstract) ->
    apply:(
      (Syntax.var, 'b, 'c, 'd) Bddapron.Domain1.man ->
	Equation.hedge -> 'd abstract array -> bool * 'd abstract
    ) ->
    (Syntax.var, 'b, 'c, 'd) Bddapron.Domain1.man -> 'd manager

  val make_fpmanager_static :
    debug:int ->
    analysis:[ `Backward | `Forward ] ->
    equation:('d abstract, 'e, 'f) Equation.static Equation.equation ->
    (Syntax.var, 'b, 'c, 'd) Bddapron.Domain1.man -> 'd manager

  val compute_combination :
    fmt:Format.formatter ->
    removed:Syntax.point PSette.t ->
    prog:Syntax.var Syntax.program ->
    Equation.dynamic Equation.equation ->
    (Syntax.var, 'a, 'b, 'c) Bddapron.Domain1.man ->
    (Equation.vertex, Equation.hedge, 'c abstract, bool) Fixpoint.output
end
module Solving2 : sig
  type 'a abstract = (Syntax.var, 'a) Bddapron.Domain1.t array
  type 'a manager =
      (Equation.vertex, Equation.hedge, 'a abstract, bool) Fixpoint.manager
  type 'a output = ('a abstract, bool, Fixpoint.stat) Equation.static

  val compute_combination :
    fmt:Format.formatter ->
    removed:Syntax.point PSette.t ->
    prog:Syntax.var Syntax.program ->
    Equation.dynamic Equation.equation ->
    (Syntax.var, 'a, 'b, 'c) Bddapron.Domain1.man ->
    (Equation.vertex, Equation.hedge, 'c abstract, bool) Fixpoint.output
end
