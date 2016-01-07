(** Output the result of analyses *)

(* This file is part of the ConcurInterproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Bertrand Jeannet 2012.
*)

type 'a threadoutput = (Syntax.point, (Syntax.point array * 'a) list) PHashhe.t

val threadoutput_of_output :
  is_bottom:('a -> bool) ->
  equation:'b Equation.equation ->
  (Equation.vertex, 'c, 'a, 'd, 'e) PSHGraph.t -> string -> 'a threadoutput
val print_output :
  analysis:[< `Backward | `Forward ] ->
  print_abstract:(Format.formatter -> 'a -> unit) ->
  Format.formatter -> (Equation.vertex, 'b, 'a, 'c, 'd) PSHGraph.t -> unit

val print_threadoutput :
  analysis:[< `Backward | `Forward ] ->
  removed:Syntax.point PSette.t ->
  print_abstract:(Format.formatter -> 'a -> unit) ->
  prog:Syntax.var Syntax.program ->
  Format.formatter ->
  string -> (Syntax.point, (Syntax.point array * 'a) list) PHashhe.t -> unit

val print_result :
  is_bottom:'a ->
  print_abstract:(Format.formatter -> 'b -> unit) ->
  threadoutput_of_output:(is_bottom:'a ->
			  equation:'c Equation.equation ->
			  (Equation.vertex, 'd, 'b, 'e, Fixpoint.stat) PSHGraph.t ->
			   string -> 'f) ->
  threadoutput_project:(remove_var0:bool ->
			remove_concurrency:bool ->
			equation:'c Equation.equation ->
			(Syntax.var, 'g, 'h, 'i) Bddapron.Domain1.man ->
			'f ->
			 string ->
			 (Syntax.point, (Syntax.point array * 'b) list) PHashhe.t) ->
  removed:Syntax.point PSette.t ->
  analysis:[< `Backward | `Forward ] ->
  prog:Syntax.var Syntax.program ->
  equation:'c Equation.equation ->
  (Syntax.var, 'g, 'h, 'i) Bddapron.Domain1.man ->
  Format.formatter ->
  (Equation.vertex, 'd, 'b, 'e, Fixpoint.stat) PSHGraph.t -> unit
