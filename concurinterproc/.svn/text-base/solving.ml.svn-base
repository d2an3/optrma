(** Solving the equations *)

(* This file is part of the ConcurInterproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Bertrand Jeannet 2009.
*)

open Format
open Db
open Cfg
open Equation

(*  ********************************************************************* *)
(** {2. Common function} *)
(*  ********************************************************************* *)

let update_static_equation ~print_abstract ~is_bottom ~switch ~equation ~output
    =
  let seth = ref (PSette.empty Pervasives.compare) in
  PSHGraph.iter_hedge output
    (begin fun hedge attr ~pred ~succ ->
      if not attr then seth := PSette.add hedge !seth
    end)
  ;
  PSette.iter (PSHGraph.remove_hedge equation.graph) !seth;

  let setv = ref (PSette.empty Equation.vertex_compare) in
  PSHGraph.iter_vertex equation.graph
    (begin fun vertex attr ~pred ~succ ->
      try
	let abs = PSHGraph.attrvertex output vertex in
	if is_bottom abs then raise Not_found;
	PSHGraph.replace_attrvertex equation.graph vertex (switch vertex abs)
      with Not_found ->
	setv := PSette.add vertex !setv
    end);
  PSette.iter (PSHGraph.remove_vertex equation.graph) !setv;
  equation.sinit <- PSette.diff equation.sinit !setv;
  ()

let analysis_static
    ~make_fpmanager_static
    ~analysis ~equation man
    =
  let debug = !Option.debug in
  let fpmanager = make_fpmanager_static ~debug ~analysis ~equation man in
  let output =
    if !Option.iteration_guided then begin
      let db = equation.db in
      let cudd = db.env.Bdd.Env.cudd in
      let i = ref 0 in
      Fixpoint.analysis_guided fpmanager equation.graph equation.sinit
	(fun filter ->
	  incr i;
	  printf "@.Step %i@." !i;
	  Cudd.Man.reduce_heap cudd Cudd.Man.REORDER_SIFT 10000;
	  let strategy =
	    Fixpoint.make_strategy_default
	      ~priority:(PSHGraph.Filter filter)
	      ~depth:(!Option.iteration_depth)
	      ~widening_start:(!Option.widening_start)
	      ~widening_descend:(!Option.widening_descend)
	      ~vertex_dummy:vertex_dummy ~hedge_dummy:hedge_dummy
	      equation.graph equation.sinit
	  in
	  strategy
	)
    end
    else begin
      Fixpoint.analysis_std fpmanager equation.graph equation.sinit
	(let strategy =
	  Fixpoint.make_strategy_default
	    ~vertex_dummy:vertex_dummy
	    ~hedge_dummy:hedge_dummy
	    ~depth:(!Option.iteration_depth)
	    ~widening_start:(!Option.widening_start)
	    ~widening_descend:(!Option.widening_descend)
	    equation.graph equation.sinit
	in
	strategy
	)
    end
  in
  output

let compute_combination
    ~make_fpmanager_dynamic
    ~make_fpmanager_static
    ?(compute_thresholds=None)
    ~top_of_vertex ~top_of_vertexd ~is_bottom ~print_abstract
    ~threadoutput_of_output ~threadoutput_project
    ~switchfb ~switchbf
    ~(fmt:Format.formatter)
    ~(removed:Syntax.point PSette.t)
    ~(prog:Syntax.var Syntax.program)
    (fdynamic:Equation.dynamic Equation.equation)
    (man:(Syntax.var,'b,'c,'d) Bddapron.Domain0.man)
    =
  let debug = !Option.debug in

  if debug>4 then begin
    printf "equation=@.%a@."
      Equation.print_dynamic_equation fdynamic
  end;
  let db = fdynamic.db in
  let cudd = db.env.Bdd.Env.cudd in

  (* Initial forward analysis *)
  let (output,fstatic) =
    if !Option.analysis_dynamic then begin
      let fpmanager = make_fpmanager_dynamic ~debug ~equation:fdynamic man in
      let i = ref 0 in
      let output =
	Fixpoint.analysis_dyn Equation.graph_compare
	  ~guided:!Option.iteration_guided
	  fpmanager fdynamic.graph.succvertex fdynamic.sinit
	  (fun graph ->
	    incr i;
	    printf "@.Step %i@." !i;
	    Cudd.Man.reduce_heap cudd Cudd.Man.REORDER_SIFT 10000;
	    let strategy =
	      Fixpoint.make_strategy_default
		~vertex_dummy:vertex_dummy ~hedge_dummy:hedge_dummy
		~depth:(!Option.iteration_depth)
		~widening_start:(!Option.widening_start)
		~widening_descend:(!Option.widening_descend)
		graph fdynamic.sinit
	    in
	    strategy
	  )
      in
      let fstatic = Db2equation.static_of_dynamic fdynamic output in
      fprintf fmt "dynamic=true, before thresholds@.";
      Output.print_result
	~is_bottom ~print_abstract
	~threadoutput_of_output ~threadoutput_project
	~removed ~analysis:`Forward
	~prog ~equation:fstatic man fmt output
      ;
      begin match compute_thresholds with
      | Some f when !Option.analysis_threshold ->
	  fprintf fmt "dynamic=false, inference of thresholds@.";
	  let time = ref 0.0 in
	  let res =
	    Time.wrap_duration time
	      (fun () -> f ~fmt ~removed ~prog man fstatic)
	  in
	  fprintf fmt "Time = %f@." !time;
	  res
      | _ -> ()
      end;
      (output,fstatic)
    end else begin
      let graph =
	Fixpoint.graph_of_equation
	  Equation.graph_compare
	  ~make_attrvertex:(fun vertex -> top_of_vertexd fdynamic man vertex)
	  ~make_attrhedge:(fun hedge -> false)
	  ~info:(let open Fixpoint in { time = 0.0; ascending=({nb=0;stable=false},[]); descending=({nb=0;stable=false},[]) })
	  fdynamic.graph.succvertex
	  fdynamic.Equation.sinit
      in
      let fstatic = Db2equation.static_of_dynamic fdynamic graph in
      begin match compute_thresholds with
      | Some f when !Option.analysis_threshold ->
	  let time = ref 0.0 in
	  let res =
	    Time.wrap_duration time
	      (fun () -> f ~fmt ~removed ~prog man fstatic)
	  in
	  fprintf fmt "Time = %f@." !time;
	  res
      | _ -> ()
      end;
      let output =
	analysis_static
	  ~make_fpmanager_static
	  ~analysis:`Forward ~equation:fstatic man
      in
      let fstatic = Db2equation.static_of_dynamic fdynamic output in
      Output.print_result
	~is_bottom ~print_abstract
	~threadoutput_of_output ~threadoutput_project
	~removed ~analysis:`Forward
	~prog ~equation:fstatic man fmt output
      ;
      (output,fstatic)
    end
  in
  let bstatic = Db2equation.backward_of_forward ~switchfb fstatic in
  let lastoutput = ref output in
  List.iter
    (begin fun analysis ->
      if false then printf "analysis %s@." (match analysis with `Forward -> "Foward" | `Backward -> "Backward");
      let equation = match analysis with
	| `Forward -> fstatic
	| `Backward -> bstatic
      in
      let output = analysis_static
	~make_fpmanager_static
	~analysis ~equation man
      in
      begin match analysis with
      | `Forward ->
	  Output.print_result
	    ~is_bottom ~print_abstract
	    ~threadoutput_of_output ~threadoutput_project
	    ~removed ~analysis
	    ~prog ~equation:fstatic man fmt output
	  ;
	  update_static_equation ~print_abstract ~is_bottom ~switch:(fun x y -> y) ~equation:fstatic ~output;
	  update_static_equation ~print_abstract ~is_bottom ~switch:switchfb ~equation:bstatic ~output;
      | `Backward ->
	  Output.print_result
	    ~is_bottom ~print_abstract
	    ~threadoutput_of_output ~threadoutput_project
	    ~removed ~analysis
	    ~prog ~equation:bstatic man fmt output
	  ;
	  update_static_equation ~print_abstract ~is_bottom ~switch:switchbf ~equation:fstatic ~output;
	  update_static_equation ~print_abstract ~is_bottom ~switch:(fun x y -> y) ~equation:bstatic ~output;
      end;
      lastoutput := output
    end)
    (if compute_thresholds<>None && !Option.analysis_threshold && !Option.analysis_dynamic then !Option.analysis else List.tl !Option.analysis)
  ;
  !lastoutput


(*  ********************************************************************** *)
(** {2 Method 1: one abstract value} *)
(*  ********************************************************************** *)

module Solving1 = struct
  type 'a abstract = (Syntax.var, 'a) Bddapron.Domain1.t
  type 'a manager = (vertex, hedge, 'a abstract, bool) Fixpoint.manager
  type 'a output = ('a abstract, bool, Fixpoint.stat) Equation.static

  let apply_hedge_dynamic
      ~(equation : 'a equation)
      (man:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
      (hedge:hedge) (tabs:'d abstract array)
      :
      bool * 'd abstract
      =
    let db = equation.db in
    let index = hedge.index in
    let thread = db.threads.(index) in
    let cfg = equation.tcfgs.(index) in
    let instr = PSHGraph.attrhedge cfg hedge.hedge in
    let nenv = Equation.env_of_vertex equation (Db2equation.Dynamic.succ_hedge equation.tcfgs hedge) in
    let res =
      Abssemantic1.apply_instr ~analysis:`Forward db thread instr man tabs nenv None
    in
    (not (Abssemantic1.is_bottom man res),res)

  let apply_hedge_static
      ~analysis
      ~(equation : ('d abstract,'e,'f) static equation)
      (man:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
      (hedge:hedge)
      (tabs:'d abstract array)
      :
      bool * 'd abstract
      =
    let succ = PSHGraph.succvertex equation.graph hedge in
    let res =
      try
	let db = equation.db in
	let index = hedge.index in
	let thread = db.threads.(index) in
	let cfg = equation.tcfgs.(index) in
	let instr = PSHGraph.attrhedge cfg hedge.hedge in
	let dest = PSHGraph.attrvertex equation.graph succ.(0) in
	let nenv = Bddapron.Domain1.get_env dest in
	Abssemantic1.apply_instr ~analysis
	  db thread instr man tabs nenv (Some dest)
      with Not_found ->
	Abssemantic1.bottom_of_vertex equation man succ.(0)
    in
    (not (Abssemantic1.is_bottom man res),res)

  let make_fpmanager
      ~(debug:int)
      ~(equation : 'a Equation.equation)
      ~(abstract_init : (Syntax.var,'b,'c,'d) Bddapron.Domain1.man -> Equation.vertex -> 'd abstract)
      ~(apply : (Syntax.var,'b,'c,'d) Bddapron.Domain1.man -> Equation.hedge -> 'd abstract array -> bool * 'd abstract)
      (man:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
      :
      'd manager
      =
    let manager = make_fpmanager_common ~debug ~equation in
    { manager with
      Fixpoint.bottom = begin fun vertex ->
	Abssemantic1.bottom_of_vertex equation man vertex
      end;
      Fixpoint.canonical = begin fun vertex abs -> () end;
      Fixpoint.is_bottom = begin fun vertex abs -> Abssemantic1.is_bottom man abs end;
      Fixpoint.is_leq = begin fun vertex abs1 abs2 ->
	Bddapron.Domain1.is_leq man abs1 abs2
      end;
      Fixpoint.join = begin fun vertex abs1 abs2 ->
	Bddapron.Domain1.join man abs1 abs2
      end;
      Fixpoint.join_list = begin fun vertex labs ->
	List.fold_left (Bddapron.Domain1.join man) (List.hd labs) (List.tl labs)
      end;
      Fixpoint.odiff = None;
      Fixpoint.widening = begin fun vertex abs1 abs2 ->
	try
	  let tlincons1 = PHashhe.find equation.threshold vertex in
	  Bddapron.Domain1.widening_threshold man abs1 abs2 tlincons1
	with Not_found ->
	  Bddapron.Domain1.widening man abs1 abs2
      end;
      Fixpoint.abstract_init = begin fun vertex -> abstract_init man vertex end;
      Fixpoint.apply = begin fun hedge tx -> apply man hedge tx end;
      Fixpoint.print_abstract = Abssemantic1.print_abstract man;
    }

  let make_fpmanager_dynamic ~debug ~equation man =
    make_fpmanager ~debug ~equation
      ~abstract_init:(begin fun man vertex ->
	let abs = Abssemantic1.top_of_vertex equation man vertex in
	let expr = Bddapron.Expr2.Bool.extend_environment equation.initial abs.Bddapron.Env.env in
	Bddapron.Domain1.meet_condition2 man abs expr
      end)
      ~apply:(apply_hedge_dynamic ~equation)
      man

  let make_fpmanager_static ~debug ~analysis ~equation man =
    make_fpmanager ~debug ~equation
      ~abstract_init:(fun man vertex -> PSHGraph.attrvertex equation.graph vertex)
      ~apply:(apply_hedge_static ~analysis ~equation)
      man

  let threadoutput_project
      ~(remove_var0:bool)
      ~(remove_concurrency:bool)
      ~(equation:'a equation)
      (man:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
      (threadoutput:'d abstract Output.threadoutput)
      (threadname:string)
      :
      'd abstract Output.threadoutput
      =
    let lvar0 = if remove_var0 then Db.lvaraux equation.db else [] in
    let i = Equation.index_of_threadname equation threadname in
    let tcfg = equation.tcfgs.(i) in
    let tinfo = PSHGraph.info tcfg in
    PHashhe.map
      (begin fun point ltpointabs ->
	let ltpointabs =
	  if remove_concurrency then begin
	    let labs =
	      List.map
		(begin fun (tpoint,abs) ->
		  let nenv = PHashhe.find tinfo.pointenv point in
		  let nabs = Bddapron.Domain1.change_environment man abs nenv in
		  nabs
		end)
		ltpointabs
	    in
	    let uabs = List.fold_left
	      (Bddapron.Domain1.join man)
	      (List.hd labs) (List.tl labs)
	    in
	    [([|point|],uabs)]
	  end
	  else
	    ltpointabs
	in
	let ltpointabs =
	  if remove_var0 then begin
	    List.map
	      (begin fun (tpoint,abs) ->
		let env = abs.Bddapron.Env.env in
		let nenv =
		  Bddapron.Env.remove_vars env
		    (List.filter (Bddapron.Env.mem_var env) lvar0)
		in
		let nabs = Bddapron.Domain1.change_environment man abs nenv in
		(tpoint,nabs)
	      end)
	      ltpointabs
	  end
	  else
	    ltpointabs
	in
	let ltpointabs = List.fast_sort
	  (fun (x,_) (y,_) -> Equation.tpoint_compare x y)
	  ltpointabs
	in
	ltpointabs
      end)
      threadoutput

  let switchfb x y = y
  let switchbf x y = y

  let compute_combination
      ~(fmt:Format.formatter)
      ~(removed:Syntax.point PSette.t)
      ~(prog:Syntax.var Syntax.program)
      (fdynamic:Equation.dynamic Equation.equation)
      (man:(Syntax.var,'b,'c,'d) Bddapron.Domain0.man)
      =
    let open Output in let open Abssemantic1 in
    let is_bottom = is_bottom man in
    let print_abstract = print_abstract man in
    compute_combination
      ~make_fpmanager_dynamic
      ~make_fpmanager_static
      ~compute_thresholds:(Some Threshold.Abssemantic1.compute_thresholds)
      ~top_of_vertex ~top_of_vertexd:top_of_vertex ~is_bottom ~print_abstract
      ~threadoutput_of_output
      ~threadoutput_project
      ~switchfb ~switchbf
      ~fmt ~removed ~prog fdynamic man


end

(*  ********************************************************************** *)
(** {2 Method 2: partially overlapping packs of abstract values} *)
(*  ********************************************************************** *)

module Solving2 = struct
  type 'a abstract = (Syntax.var, 'a) Bddapron.Domain1.t array
  type 'a manager = (vertex, hedge, 'a abstract, bool) Fixpoint.manager
  type 'a output = ('a abstract, bool, Fixpoint.stat) Equation.static

  let make_fpmanager
      ~(debug:int)
      ~(equation:'a Equation.equation)
      ~(abstract_init : (Syntax.var,'b,'c,'d) Bddapron.Domain1.man -> Equation.vertex -> 'd abstract)
      ~(apply : (Syntax.var,'b,'c,'d) Bddapron.Domain1.man -> Equation.hedge -> 'd abstract array -> bool * 'd abstract)
      (man:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
      :
      'd manager
      =
    let manager = make_fpmanager_common ~debug ~equation in
    { manager with
      Fixpoint.bottom = begin fun vertex ->
	Abssemantic2.bottom_of_vertex equation man vertex
      end;
      Fixpoint.canonical = begin fun vertex abs -> () end;
      Fixpoint.is_bottom = begin fun vertex abs -> Abssemantic2.is_bottom man abs end;
      Fixpoint.is_leq = begin fun vertex abs1 abs2 ->
	Equation.array_forall2 (Bddapron.Domain1.is_leq man) abs1 abs2
      end;
      Fixpoint.join = begin fun vertex abs1 abs2 ->
	Equation.array_map2 (Bddapron.Domain1.join man) abs1 abs2
      end;
      Fixpoint.join_list = begin fun vertex labs ->
	List.fold_left
	  (Equation.array_map2 (Bddapron.Domain1.join man))
	  (List.hd labs) (List.tl labs)
      end;
      Fixpoint.odiff = None;
      Fixpoint.widening = begin fun vertex abs1 abs2 ->
	Equation.array_map2 (Bddapron.Domain1.widening man) abs1 abs2
      end;
      Fixpoint.abstract_init = begin fun vertex -> abstract_init man vertex end;
      Fixpoint.apply = begin fun ghedge tx -> apply man ghedge tx end;
      Fixpoint.print_abstract = Print.array (Bddapron.Domain1.print man);
    }

  let apply_hedge_dynamic
      ~(equation : 'a equation)
      (man:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
      (hedge:hedge) (tabs:'d abstract array)
      :
      bool * 'd abstract
      =
    let db = equation.db in
    let index = hedge.index in
    let cfg = equation.tcfgs.(index) in
    let instr = PSHGraph.attrhedge cfg hedge.hedge in
    let nenv = Equation.env_of_vertex equation (Db2equation.Dynamic.succ_hedge equation.tcfgs hedge) in
    let res =
      Abssemantic2.apply_instr ~analysis:`Forward
	db index instr
	man tabs nenv None
    in
    (not (Abssemantic2.is_bottom man res),res)

  let apply_hedge_static
      ~analysis
      ~(equation : ('d abstract,'e,'f) static equation)
      (man:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man) (hedge:hedge)
      (tabs:'d abstract array)
      :
      bool * 'd abstract
      =
    let succ = PSHGraph.succvertex equation.graph hedge in
    let res =
      try
	let db = equation.db in
	let index = hedge.index in
	let cfg = equation.tcfgs.(index) in
	let instr = PSHGraph.attrhedge cfg hedge.hedge in
	let dest = PSHGraph.attrvertex equation.graph succ.(0) in
	let nenv = Bddapron.Domain1.get_env dest.(0) in
	Abssemantic2.apply_instr ~analysis
	  db index instr man tabs nenv (Some dest)
      with Not_found ->
	Abssemantic2.bottom_of_vertex equation man succ.(0)
    in
    (not (Abssemantic2.is_bottom man res),res)

  let make_fpmanager_dynamic ~debug ~equation man =
    make_fpmanager ~debug ~equation
      ~abstract_init:(begin fun man vertex ->
	let abs = Abssemantic2.top_of_vertex equation man vertex in
	Array.map
	  (begin fun abs ->
	    let expr =
	      Bddapron.Expr2.Bool.extend_environment equation.initial
		abs.Bddapron.Env.env
	    in
	    Bddapron.Domain1.meet_condition2 man abs expr
	  end)
	  abs
      end)
      ~apply:(apply_hedge_dynamic ~equation)
      man

  let make_fpmanager_static ~debug ~analysis ~equation man =
    make_fpmanager ~debug ~equation
      ~abstract_init:(fun man vertex -> PSHGraph.attrvertex equation.graph vertex)
      ~apply:(apply_hedge_static ~analysis ~equation)
      man

  let threadoutput_project
      ~(remove_var0:bool)
      ~(remove_concurrency:bool)
      ~(equation:'a equation)
      (manager:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
      (threadoutput:'d abstract Output.threadoutput)
      (threadname:string)
      :
      'd abstract Output.threadoutput
      =
    let lvar0 = if remove_var0 then Db.lvaraux equation.db else [] in
    let index = Equation.index_of_threadname equation threadname in
    let tcfg = equation.tcfgs.(index) in
    let tinfo = PSHGraph.info tcfg in
    PHashhe.map
      (begin fun point ltpointabs ->
	let ltpointabs =
	  if remove_concurrency then begin
	    let labs =
	      List.map
		(begin fun (tpoint,abs) ->
		  let nenv = PHashhe.find tinfo.pointenv point in
		  let abs = abs.(index) in
		  let nabs = Bddapron.Domain1.change_environment manager abs nenv in
		  nabs
		end)
		ltpointabs
	    in
	    let uabs = List.fold_left
	      (Bddapron.Domain1.join manager)
	      (List.hd labs) (List.tl labs)
	    in
	    [([|point|],[|uabs|])]
	  end
	  else
	    ltpointabs
	in
	let ltpointabs =
	  if remove_var0 then begin
	    List.map
	      (begin fun (tpoint,abs) ->
		let abs = abs.(index) in
		let env = abs.Bddapron.Env.env in
		let nenv =
		  Bddapron.Env.remove_vars env
		    (List.filter (Bddapron.Env.mem_var env) lvar0)
		in
		let nabs = Bddapron.Domain1.change_environment manager abs nenv in
		(tpoint,[|nabs|])
	      end)
	      ltpointabs
	  end
	  else
	    ltpointabs
	in
	let ltpointabs = List.fast_sort
	  (fun (x,_) (y,_) -> Equation.tpoint_compare x y)
	  ltpointabs
	in
	ltpointabs
      end)
      threadoutput

  let switchfb x y = y
  let switchbf x y = y

  let compute_combination
      ~(fmt:Format.formatter)
      ~(removed:Syntax.point PSette.t)
      ~(prog:Syntax.var Syntax.program)
      (fdynamic:Equation.dynamic Equation.equation)
      (man:(Syntax.var,'b,'c,'d) Bddapron.Domain0.man)
      =
    let open Output in let open Abssemantic2 in
    let is_bottom = is_bottom man in
    let print_abstract = print_abstract man in
    compute_combination
      ~make_fpmanager_dynamic
      ~make_fpmanager_static
      ~top_of_vertex ~top_of_vertexd:top_of_vertex ~is_bottom ~print_abstract
      ~threadoutput_of_output ~threadoutput_project
      ~switchfb ~switchbf
      ~fmt ~removed ~prog fdynamic man

end

(*
let abstract_switch_to_dual
    ~analysis
    ~(equation:'a Equation.equation)
    ~(forget_list:'b -> Syntax.var list -> 'b)
    (vertex:Equation.vertex)
    (abs:'b)
    :
    'b
    =
  let lglobaltmp = (equation.db.lglobaltmp:>Syntax.var list) in
  let nabs = match analysis with
    | `Backward -> abs
    | `Forward ->
	begin match vertex with
	  | Top _ -> abs
	  | Tail(i,point) -> forget_list abs lglobaltmp
	end
  in
  nabs
let forget_list man tabs lvar =
  Array.map
    (fun abs -> Bddapron.Domain1.forget_list man abs lvar)
    tabs

*)
