(** Abstract semantics of CFG instructions, standard version *)

(* This file is part of the ConcurInterproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Bertrand Jeannet 2009.
*)


type 'd abstract = (Syntax.var,'d) Bddapron.Domain1.t

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

val meet_odest :
  (Syntax.var,'b,'c,'d) Bddapron.Domain1.man ->
  'd abstract -> 'd abstract option ->
  'd abstract

val apply_forget :
  (Syntax.var,'b,'c,'d) Bddapron.Domain1.man ->
  'd abstract -> Syntax.var list -> 'd abstract option ->
  'd abstract

val apply_assign :
  analysis:[`Backward | `Forward ] ->
  (Syntax.var,'b,'c,'d) Bddapron.Domain1.man ->
  'd abstract ->
  Syntax.var list -> Syntax.var Bddapron.Expr2.List.t ->
  'd abstract option ->
  'd abstract

val apply_binstr :
  analysis:[`Backward | `Forward ] ->
  (Syntax.var,'b,'c,'d) Bddapron.Domain1.man ->
  'd abstract -> Cfg.bddapron_binstr -> 'd abstract option ->
  'd abstract

val apply_block :
  analysis:[`Backward | `Forward ] ->
  (Syntax.var,'b,'c,'d) Bddapron.Domain1.man ->
  'd abstract ->   Cfg.bddapron_binstr list -> 'd abstract option ->
  'd abstract

module Forward : sig
  val apply_push :
    (Cfg.syntax_t,unit) Db.thread ->
    Cfg.callinstr ->
    (Syntax.var, 'a, 'b, 'c) Bddapron.Domain1.man ->
    'c abstract -> Syntax.var Bddapron.Env.t -> 'c abstract option ->
    'c abstract
  val apply_call :
    instrum:bool ->
    (Cfg.syntax_t,unit) Db.thread ->
    Cfg.callinstr ->
    (Syntax.var, 'a, 'b, 'c) Bddapron.Domain1.man ->
    'c abstract -> Syntax.var Bddapron.Env.t -> 'c abstract option ->
    'c abstract
  val apply_return :
    (Cfg.syntax_t,unit) Db.program -> (Cfg.syntax_t,unit) Db.thread ->
    Cfg.callinstr ->
    (Syntax.var, 'a, 'b, 'c) Bddapron.Domain1.man ->
    'c abstract -> 'c abstract -> Syntax.var Bddapron.Env.t -> 'c abstract option ->
    'c abstract
end
module Backward : sig
  val apply_push :
    (Cfg.syntax_t,unit) Db.program -> (Cfg.syntax_t,unit) Db.thread ->
    Cfg.callinstr ->
    (Syntax.var, 'a, 'b, 'c) Bddapron.Domain1.man ->
    'c abstract -> Syntax.var Bddapron.Env.t -> 'c abstract option ->
    'c abstract
  val apply_call :
    (Cfg.syntax_t,unit) Db.program -> (Cfg.syntax_t,unit) Db.thread ->
    Cfg.callinstr ->
    (Syntax.var, 'a, 'b, 'c) Bddapron.Domain1.man ->
    'c abstract -> 'c abstract -> Syntax.var Bddapron.Env.t -> 'c abstract option ->
    'c abstract
  val apply_return :
    instrum:bool ->
    (Cfg.syntax_t,unit) Db.thread ->
    Cfg.callinstr ->
    (Syntax.var, 'a, 'b, 'c) Bddapron.Domain1.man ->
    'c abstract -> Syntax.var Bddapron.Env.t -> 'c abstract option ->
    'c abstract
end

val apply_instr :
  analysis:[`Backward | `Forward ] ->
  (Cfg.syntax_t,unit) Db.program -> (Cfg.syntax_t,unit) Db.thread ->
  Cfg.bddapron_instr ->
  (Syntax.var,'b,'c,'d) Bddapron.Domain1.man ->
  'd abstract array ->
  Syntax.var Bddapron.Env.t ->
  'd abstract option ->
  'd abstract
