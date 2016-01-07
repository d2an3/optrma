(** *)

(* This file is part of the Interproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Bertrand Jeannet 2009.
*)

val compute_and_display :
  fmt:Format.formatter ->
  removed:Syntax.point PSette.t ref ->
  prog:Syntax.var Syntax.program ->
  Equation.dynamic Equation.equation ->
  (Syntax.var, 'a, 'b, 'c) Bddapron.Domain1.man -> unit

val analyze_and_display :
  Format.formatter -> Syntax.var Syntax.program -> unit
