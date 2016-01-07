(** Solving the equations *)

(* This file is part of the ConcurInterproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Bertrand Jeannet, Pascal Sotin 2011.
*)

open Format
open Db
open Cfg
open Equation
open Solving

(*  ********************************************************************** *)
(** {2 Method 1: one abstract value} *)
(*  ********************************************************************** *)

module Solving1 = struct

  type 'd abstract = 'd Solving.Solving1.abstract
  type 'd manager = 'd Solving.Solving1.manager

  type 'f instr_policy = 'f Abssemantic1Policy.instr_policy
  type 'f program_policy = (Equation.hedge,'f instr_policy) PHashhe.t

  let man_of_pman =
    Bddapron.Policy.Domain0.manager_get_manager

  (*  ********************************************************************** *)
  (** {2 Main transfer function} *)
  (*  ********************************************************************** *)

  let apply_hedge_static
      ~analysis
      ~(policy : 'd program_policy ref)
      ~(equation : ('d abstract,'e,'f) static equation)
      (pman:(Syntax.var,_,_,_,_,_) Bddapron.Policy.Domain1.man)
      (hedge:hedge)
      (tabs:'d abstract array)
      :
      bool * 'd abstract
      =
    let man = man_of_pman pman in
    let succ = PSHGraph.succvertex equation.graph hedge in
    let res =
      try
	let db = equation.db in
	let index = hedge.index in
	let thread = db.threads.(index) in
	let cfg = equation.tcfgs.(index) in
	let instr = PSHGraph.attrhedge cfg hedge.hedge in
	let instr_policy = PHashhe.find !policy hedge in
	let dest = PSHGraph.attrvertex equation.graph succ.(0) in
	let nenv = Bddapron.Domain1.get_env dest in
	Abssemantic1Policy.apply_instr ~analysis
	  db thread instr pman
	  instr_policy tabs nenv (Some dest)
      with Not_found ->
	Abssemantic1.bottom_of_vertex equation man succ.(0)
    in
    (not (Abssemantic1.is_bottom man res),res)

  (*  ********************************************************************* *)
  (** {2 Building the manager} *)
  (*  ********************************************************************* *)

  (** Build a fixpoint manager (for module [Fixpoint]) given:
      - an equation graph (forward or backward)
      - optionally, the result of a previous, dual analysis
      - a function [apply graph output manager hyperedge tabstract]
      - a function [get_init]
      - an APRON manager;
      - a debug level
  *)

  let make_fpmanager
      ~(debug:int)
      ~analysis
      ~(policy : 'd program_policy ref)
      ~(equation : _ Equation.equation)
      (pman:(Syntax.var,_,_,_,_,_) Bddapron.Policy.Domain1.man)
      :
      'd manager
      =
    let man = man_of_pman pman in
    let fpmanager =
      Solving.Solving1.make_fpmanager_static
	~debug ~analysis ~equation man
    in
    { fpmanager with
	Fixpoint.apply = (begin fun hedge tx -> apply_hedge_static ~analysis ~policy ~equation pman hedge tx end)
    }

  (*  ********************************************************************** *)
  (** {2 Compute (post)fixpoint} *)
  (*  ********************************************************************** *)

  let print_prog_policy pman equation fmt p =
    PHashhe.iter
      (begin fun hedge p ->
	let tpred = PSHGraph.predvertex equation.graph hedge in
	assert((Array.length tpred)=1);
	let env = Equation.env_of_vertex equation tpred.(0) in
	Format.fprintf fmt "Edge %a, policy %a@."
	  Equation.print_hedge hedge
	  (Abssemantic1Policy.print_instr_policy pman env equation.Equation.db.Db.cond0) p
      end)
      !p

end
