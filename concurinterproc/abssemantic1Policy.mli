(** Abstract semantics of CFG instructions, standard version *)

(* This file is part of the ConcurInterproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Bertrand Jeannet 2009.
   Copyright (C) Bertrand Jeannet, Pascal Sotin 2011.
*)


type 'd abstract = (Syntax.var,'d) Bddapron.Domain1.t

type 'f binstr_policy = 'f option
type 'f block_policy = 'f binstr_policy list
type 'f instr_policy = 'f block_policy option

val print_binstr_policy :
  ('a, 'b, 'c, 'd, 'e, 'f) Bddapron.Policy.Domain1.man ->
  'a Bddapron.Env.t -> 'a Bddapron.Cond.t ->
  Format.formatter -> 'f binstr_policy -> unit
val print_block_policy :
  ('a, 'b, 'c, 'd, 'e, 'f) Bddapron.Policy.Domain1.man ->
  'a Bddapron.Env.t -> 'a Bddapron.Cond.t ->
  Format.formatter -> 'f block_policy -> unit
val print_instr_policy :
  ('a, 'b, 'c, 'd, 'e, 'f) Bddapron.Policy.Domain1.man ->
  'a Bddapron.Env.t -> 'a Bddapron.Cond.t ->
  Format.formatter -> 'f instr_policy -> unit


val apply_instr :
  analysis:[ `Backward | `Forward ] ->
  (Cfg.syntax_t, unit) Db.program -> (Cfg.syntax_t, unit) Db.thread ->
  Cfg.bddapron_instr ->
  (Syntax.var,'b,'c,'d,'e,'f) Bddapron.Policy.Domain1.man ->
  'f instr_policy ->
  'd abstract array ->
  Syntax.var Bddapron.Env.t ->
  'd abstract option ->
  'd abstract
val improve_instr :
  analysis:[ `Backward | `Forward ] ->
  (Cfg.syntax_t, unit) Db.program -> (Cfg.syntax_t, unit) Db.thread ->
  Cfg.bddapron_instr ->
  (Syntax.var,'b,'c,'d,'e,'f) Bddapron.Policy.Domain1.man ->
  'f instr_policy option ->
  'd abstract array ->
  Syntax.var Bddapron.Env.t ->
  'd abstract option ->
  'f instr_policy
