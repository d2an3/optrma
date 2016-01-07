(** Abstract semantics of CFG instructions, cartesian product version *)

(* This file is part of the ConcurInterproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Bertrand Jeannet 2009.
*)

open Equation

type 'a abstract = 'a Abssemantic1.abstract array

val print_abstract :
  (Syntax.var, 'b, 'c, 'd) Bddapron.Domain1.man ->
  Format.formatter -> 'd abstract -> unit

val is_bottom :
  (Syntax.var, 'b, 'c, 'd) Bddapron.Domain1.man -> 'd abstract -> bool

val bottom_of_vertex :
  'a Equation.equation ->
  (Syntax.var, 'b, 'c, 'd) Bddapron.Domain1.man ->
  Equation.vertex -> 'd abstract

val top_of_vertex :
  'a Equation.equation ->
  (Syntax.var, 'b, 'c, 'd) Bddapron.Domain1.man ->
  Equation.vertex -> 'd abstract

val apply_instr :
  analysis:[ `Backward | `Forward ] ->
  (Cfg.syntax_t, unit) Db.program ->
  int ->
  Cfg.bddapron_instr ->
  (Syntax.var, 'a, 'b, 'c) Bddapron.Domain1.man ->
  'c abstract array ->
  Syntax.var Bddapron.Env.t -> 'c abstract option -> 'c abstract
