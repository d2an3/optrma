(** Inferring thresholds *)

(* This file is part of the ConcurInterproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Bertrand Jeannet 2012.
*)

module Abssemantic1 : sig
  val compute_thresholds :
    fmt:Format.formatter ->
    removed:Syntax.point PSette.t ->
    prog:Syntax.var Syntax.program ->
    ('a, 'b, 'c, 'd) Bddapron.Domain1.man ->
    ((Syntax.var, 'e) Bddapron.Domain1.t, 'f, 'g) Equation.static Equation.equation -> 
    unit
end
