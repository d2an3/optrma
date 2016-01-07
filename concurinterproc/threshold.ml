(** Inferring thresholds *)

(* This file is part of the ConcurInterproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Bertrand Jeannet 2012.
*)

open Format
open Db
open Cfg
open Equation

module Threshold = struct
  type t = Syntax.var Bddapron.Apronexpr.Condition.t

  let ttrue =
    let open Bddapron.Apronexpr in
    (Condition.SUPEQ,one)

  let compare = Bddapron.Apronexpr.Condition.compare
  let print = Bddapron.Apronexpr.Condition.print
  let init symbol = PSette.singleton (compare symbol) ttrue

  let empty symbol = PSette.empty (compare symbol)

  let of_apron (set:t PSette.t) env (abs:'a Apron.Abstract0.t)
      :
      t PSette.t
      =
    let open Apron in
    let open Bddapron in
    let apron = Abstract0.manager abs in
    let symbol = env.Bdd.Env.symbol in

    assert(not (Abstract0.is_bottom apron abs));
    let tlincons0 = Abstract0.to_lincons_array apron abs in
    if tlincons0<>[||] then begin
      let apronenv = Env.apron env in
      Array.fold_left
	(begin fun set lincons0 ->
	  let expr =
	    Apronexpr.of_linexpr0 symbol apronenv lincons0.Lincons0.linexpr0
	  in
	  let typ = lincons0.Lincons0.typ in
	  if typ=Lincons0.SUPEQ || typ=Lincons0.SUP || typ=Lincons0.EQ then
	    let condition = (typ,expr) in
	    PSette.add condition set
	  else
	    set
	end)
	set
	tlincons0
    end else begin
      PSette.add ttrue set
    end

  let of_disjunctionapron
      (set:t PSette.t)
      env
      (abs:'a Apron.Disjunction.t Apron.Abstract0.t)
      :
      t PSette.t
      =
    let man = Apron.Abstract0.manager abs in
    let tabs = Apron.Disjunction.decompose man abs in
    Array.fold_left
      (begin fun set abs -> of_apron set env abs end)
      set
      tabs

  let of_bddapron
      (set:t PSette.t)
      (man:(Syntax.var,_,_,_) Bddapron.Domain1.man)
      (abs1:(Syntax.var,_) Bddapron.Domain1.t)
      :
      t PSette.t
      =
  let open Bddapron in
  let env = Domain1.get_env abs1 in
  let abs0 = Domain1.to_level0 abs1 in
  let lbddabs0 = Domain0.to_bddapron man abs0 in
  List.fold_left
    (begin fun set (bdd,abs0) ->
      of_disjunctionapron set env abs0
    end)
    set
    lbddabs0

  let to_bddapron
      (man:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
      env
      (threshold:t)
      :
      (Syntax.var,'d) Bddapron.Domain1.t
      =
    let open Apron in
    let open Bddapron in
    let apronenv = Env.apron env in
    let apronman = Domain1.man_get_apron man in
    let top1 = Abstract1.top apronman apronenv in
    let top0 = Abstract1.abstract0 top1 in
    let lincons0 = match Apronexpr.Condition.to_apron0 env.Bdd.Env.symbol apronenv threshold with
      | `Lin x -> x
      | _ -> failwith ""
    in
    Apron.Abstract0.meet_lincons_array_with apronman top0 [|lincons0|];
    assert (not (Apron.Abstract0.is_bottom apronman top0));
    let abs0 = Domain0.of_bddapron man env [(Cudd.Bdd.dtrue env.Bdd.Env.cudd, top0)] in
    Domain1.of_level0 env abs0

  let to_lbddapron
      (man:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
      env
      (threshold:t PSette.t)
      :
      (Syntax.var,'d) Bddapron.Domain1.t list
      =
    PSette.fold
      (fun threshold res -> (to_bddapron man env threshold)::res)
      threshold []

  let split_equalities (set:t PSette.t) : t PSette.t =
    PSette.fold
      (begin fun threshold res ->
	let open Bddapron.Apronexpr in
	let (typ,expr) = threshold in
	if typ = Condition.EQ then
	  let ta = (Condition.SUPEQ,expr) in
	  let tb = (Condition.SUPEQ,negate expr) in
	  PSette.add ta (PSette.add tb res)
	else begin
	  let res = PSette.add threshold res in
	  if typ = Condition.SUPEQ && not (threshold=ttrue) then
	    PSette.add (Condition.SUPEQ,negate expr) res
	  else
	    res
	end
      end)
      set
      (PSette.empty set.PSette.compare)
end

let earray1_of_llincons0 env llincons0 =
  let earray0 = Array.of_list llincons0 in
  let open Apron.Lincons1 in
  {
    lincons0_array = earray0;
    array_env = env
  }

let print_apron_disjunction string_of_dim fmt abs =
  let man = Apron.Abstract0.manager abs in
  let tabs = Apron.Disjunction.decompose man abs in
  Print.array
    (Apron.Abstract0.print string_of_dim)
    fmt tabs


(*  ********************************************************************** *)
(** {2 Method 1: one abstract value} *)
(*  ********************************************************************** *)
module Abssemantic1 = struct

  type 'a abstract = 'a Apron.Disjunction.t Abssemantic1.abstract

(*
  let print_abstract man fmt abs =
    Bddapron.Domain1.print ~print_apron:print_apron_disjunction man fmt abs
*)

  let list_fold_nested f acc l1 l2 =
    let acc = ref acc in
    List.iter
      (fun e1 ->
	List.iter
	  (fun e2 -> acc := f !acc e1 e2)
	  l2
      )
      l1
    ;
    !acc

  let is_call_relation db thread callinstr (threshold:Threshold.t) =
    let symbol = db.Db.env.Bdd.Env.symbol in
    let supp = Bddapron.Apronexpr.Condition.support symbol threshold in
    let caller = Mappe.find callinstr.Cfg.caller thread.Db.tprocedures in
    let memsupp = fun var -> PSette.mem (var:>Syntax.var) supp in
    false || (
      ((List.exists memsupp thread.Db.lglobalinput) ||
	(List.exists memsupp caller.Db.linputcst))
      &&
	((List.exists memsupp db.Db.lglobal) ||
	  (List.exists memsupp callinstr.Cfg.cinput))
    )

  let is_ret_relation db thread callinstr (threshold:Threshold.t) =
    let symbol = db.Db.env.Bdd.Env.symbol in
    let supp = Bddapron.Apronexpr.Condition.support symbol threshold in
    let callee = Mappe.find callinstr.Cfg.callee thread.Db.tprocedures in
    let memsupp = fun var -> PSette.mem (var:>Syntax.var) supp in
    false || (
      ((List.exists memsupp thread.Db.lglobalinput) ||
	(List.exists memsupp callee.Db.linputcst))
      &&
	((List.exists memsupp db.Db.lglobal) ||
	  (List.exists memsupp callee.Db.loutput))
    )
  let apply_instr
      ~analysis
      (db:(Cfg.syntax_t,unit) Db.program)
      (thread:(Cfg.syntax_t,unit) Db.thread)
      (instr:Cfg.bddapron_instr)
      (manager:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
      tenv
      (tabs:Threshold.t PSette.t array)
      nenv
      :
      Threshold.t PSette.t
      =
    let symbol = nenv.Bdd.Env.symbol in
    let res =
      match (analysis,instr) with
      | `Forward, (Call(ExitReturn,callinstr)) ->
	  assert((Array.length tabs)=2 && (Array.length tenv)=2);
	  let callt =
	    PSette.filter (is_call_relation db thread callinstr) tabs.(0)
	  in
	  let rett =
	    PSette.filter (is_ret_relation db thread callinstr) tabs.(1)
	  in
	  let labscall = Threshold.to_lbddapron manager tenv.(0) callt in
	  let labsret = Threshold.to_lbddapron manager tenv.(1) rett in
	  list_fold_nested
	    (begin fun res abscall absret ->
	      let nabs =
		Abssemantic1.Forward.apply_return
		  db thread callinstr
		  manager abscall absret nenv None
	      in
	      Threshold.of_bddapron res manager nabs
	    end)
	    (Threshold.empty symbol) labscall labsret
      | `Backward, (Call(CallStart,callinstr)) ->
	  failwith ""
      | _ ->
	  if false then
	    printf "@.apply_instr instr=%a@.thresholds=%a@.tenv=%a@."
	      Cfg.print_instr_bddapron instr
	      (PSette.print (Threshold.print symbol)) tabs.(0)
	      Bddapron.Env.print tenv.(0)
	  ;
	  assert((Array.length tabs)=1 && (Array.length tenv)=1);
	  let labs = Threshold.to_lbddapron manager tenv.(0) tabs.(0) in
	  List.fold_left
	    (begin fun res abs ->
	      let nabs =
		Abssemantic1.apply_instr ~analysis
		  db thread instr
		  manager [|abs|] nenv None
	      in
	      if false then printf "abs=%a  nabs=%a@."
		(Abssemantic1.print_abstract manager) abs
		(Abssemantic1.print_abstract manager) nabs
	      ;
	      Threshold.of_bddapron res manager nabs
	    end)
	    (Threshold.empty symbol) labs
    in
    Threshold.split_equalities res

  let apply_hedge_static
      ~analysis
      ~(equation : (_,_,_) static equation)
      (man:(_,_,_,_) Bddapron.Domain1.man)
      (hedge:hedge)
      (tabs:Threshold.t PSette.t array)
      :
      Threshold.t PSette.t
      =
    let symbol = equation.Equation.db.Db.env.Bdd.Env.symbol in
    let tsucc = PSHGraph.succvertex equation.graph hedge in
    let tpred = PSHGraph.predvertex equation.graph hedge in
    let res =
      try
	let dest = PSHGraph.attrvertex equation.graph tsucc.(0) in
	let nenv = Bddapron.Domain1.get_env dest in
	let tenv =
	  Array.map
	    (begin fun pred ->
	      let org = PSHGraph.attrvertex equation.graph pred in
	      Bddapron.Domain1.get_env org
	    end)
	    tpred
	in
	let index = hedge.index in
	let cfg = equation.tcfgs.(index) in
	let instr = PSHGraph.attrhedge cfg hedge.hedge in
	let db = equation.db in
	let thread = db.threads.(index) in
	apply_instr ~analysis db thread instr man tenv tabs nenv
      with Not_found ->
	Threshold.empty symbol
    in
    res

  let compute_thresholds
      ~(fmt:Format.formatter)
      ~(removed:Syntax.point PSette.t)
      ~(prog:Syntax.var Syntax.program)
      man
      (fstatic:('a,'b,'c) Equation.static Equation.equation)
      =
    let debug = !Option.debug in

    if false && debug>4 then begin
      printf "equation=@.%a@."
	Equation.print_static_equation fstatic
    end;
    let man =
      let apron = Bddapron.Domain0.man_get_apron man in
      let mandisj = Apron.Disjunction.manager_alloc apron in
      let man = Bddapron.Domain0.make_bdd mandisj in
      man
    in
    let fpmanager =
      Equation.make_fpmanager_common
	~debug
	~equation:fstatic
    in
    let symbol = fstatic.Equation.db.Db.env.Bdd.Env.symbol in
    let parameter =
      let open FixpointThreshold in {
	compare = Threshold.compare symbol;
	print = Threshold.print symbol;
	init = begin fun _ -> Threshold.init symbol end;
	apply = begin
	  apply_hedge_static
	    ~analysis:`Forward ~equation:fstatic
	    man
	end;
	iteration_nb = 2;
      }
    in
    let strategy =
      Fixpoint.make_strategy_default
	~depth:2
	~vertex_dummy:vertex_dummy
	~hedge_dummy:hedge_dummy
	~widening_start:max_int
	~widening_descend:max_int
	fstatic.Equation.graph fstatic.Equation.sinit
    in
    let hash_vertex_thresholds =
      FixpointThreshold.inference
	fpmanager parameter fstatic.Equation.graph
	strategy
    in
    let hash_vertex_tlincons1 =
      PHashhe.map
	(begin fun vertex set ->
	  let env = Equation.env_of_vertex fstatic vertex in
	  let apronenv = Bddapron.Env.apron env in
	  let llincons0 =
	    PSette.fold
	      (begin fun condition res ->
		if condition = Threshold.ttrue then
		  res
		else begin
		  let apron0 =
		    Bddapron.Apronexpr.Condition.to_apron0 symbol apronenv condition
		  in
		  match apron0 with
		  | `Lin lincons0 -> lincons0::res
		  | _ -> failwith ""
		end
	      end)
	      set []
	  in
	  earray1_of_llincons0 apronenv llincons0
	end)
	hash_vertex_thresholds
    in
    fstatic.Equation.threshold <- hash_vertex_tlincons1;
    if debug>=1 then begin
      printf "thresholds=@.";
      PHashhe.iter
	(begin fun vertex earray1 ->
	  let env = Equation.env_of_vertex fstatic vertex in
	  fprintf fmt "%a -> %a@."
	    Equation.print_vertex vertex
	    (Print.array (Apron.Lincons0.print (Bddapron.Env.string_of_aprondim env))) earray1.Apron.Lincons1.lincons0_array
	end)
	fstatic.Equation.threshold
    end;
    ()
end
