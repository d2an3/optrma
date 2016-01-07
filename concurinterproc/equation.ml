(** Representing equation system *)

(* This file is part of the Interproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Mathias Argoud, Gaël Lalire, Bertrand Jeannet 2007.
*)

open Format
open Db

(*  ********************************************************************* *)
(** {2 Types} *)
(*  ********************************************************************* *)

type vertex =
  | Top of Syntax.point array
  | Tail of int * Syntax.point
type hedge = {
  index : int;
  hedge : int;
  tpoint : Syntax.point array;
}
let vertex_dummy = Tail(-1,Syntax.point_dummy)
let hedge_dummy = { index = -1; hedge = -1; tpoint=[||]; }

(** Equation system *)
type ('a,'b,'c) static = (vertex,hedge,'a,'b,'c) PSHGraph.t
type dynamic = {
  texitpoints : (Syntax.point array, unit) PHashhe.t array;
  succvertex : (vertex,hedge) Fixpoint.equation;
}
type 'a equation = {
  db : (Cfg.syntax_t, unit) Db.program;
  initial : Syntax.var Bddapron.Expr2.Bool.t;
    (** Initial condition on global variables *)
  final : Syntax.var Bddapron.Expr2.Bool.t;
    (** Initial condition on program counters and global variables *)
  tcfgs : Cfg.bddapron_t array;
    (** Cfgs of each thread *)
  vertexenv : (vertex,Syntax.var Bddapron.Env.t) PHashhe.t;
    (** Memoize the association vertices to environments *)
  mutable sinit : vertex PSette.t;
    (** Set of initial vertices *)
  mutable graph : 'a;
    (** either [static] or [dynamic] *)
  mutable threshold : (vertex, Apron.Lincons1.earray) PHashhe.t;
    (** Array of thresholds associated to each vertex *)
}
    (** ['a] is either [('a,'b,'c) static] or [dynamic] *)

(*  ********************************************************************* *)
(** {2 Hashing and comparison Functions} *)
(*  ********************************************************************* *)

let tpoint_compare tx ty =
  let rec parcours i =
    if i<(Array.length tx) then begin
      let res = Syntax.point_compare tx.(i) ty.(i) in
      if res=0 then parcours (i+1) else res
    end
    else
      0
  in
  parcours 0
let tpoint_equal tx ty =
  let rec parcours i =
    if i<(Array.length tx) then
      (Syntax.point_equal tx.(i) ty.(i)) && parcours (i+1)
    else
      true
  in
  parcours 0
let tpoint_hash tx =
  Array.fold_left
    (fun res x -> 2*res + (Syntax.point_hash x))
    0 tx
let tpoint_hashhe_compare = {
  PHashhe.hash = tpoint_hash;
  PHashhe.equal = tpoint_equal;
}

let vertex_compare x y = match (x,y) with
  | (Tail _, Top _) -> -1
  | (Top _, Tail _) -> 1
  | (Top tx, Top ty) -> tpoint_compare tx ty
  | (Tail (x1,x2), Tail (y1,y2)) ->
      let r = x1 - y1 in
      if r<>0 then r else Syntax.point_compare x2 y2

let vertex_equal x y = match (x,y) with
  | (Tail _, Top _)
  | (Top _, Tail _) -> false
  | (Top tx, Top ty) -> tpoint_equal tx ty
  | (Tail (x1,y1), Tail (x2,y2)) ->
      x1=x2 && (Syntax.point_equal y1 y2)

let vertex_hash = function
  | Top(tx) -> tpoint_hash tx
  | Tail(i,p) -> abs(3*i) + Syntax.point_hash p

let vertex_hashhe_compare = {
  PHashhe.hash = vertex_hash;
  PHashhe.equal = vertex_equal;
}


let graph_compare = {
  PSHGraph.hashv = vertex_hashhe_compare;
  PSHGraph.hashh = {
    Hashhe.hash = Hashtbl.hash;
    Hashhe.equal = (=);
  };
  PSHGraph.comparev = vertex_compare;
  PSHGraph.compareh = Pervasives.compare;
}

(*  ********************************************************************* *)
(** {2 Printing functions} *)
(*  ********************************************************************* *)
let print_tpoint fmt (tpoint:Syntax.point array) =
  Print.array
    ~first:"(@[<h>" ~sep:",@," ~last:"@])"
    Syntax.print_point
    fmt
    tpoint

let print_vertex fmt vertex =
  match vertex with
  | Top(tpoint) ->
	print_tpoint fmt tpoint
  | Tail(index,point) ->
      fprintf fmt "(%a Tail %i)"
	Syntax.print_point point index

let print_hedge fmt (hedge:hedge) =
  fprintf fmt "%a: %i %i"
    print_tpoint hedge.tpoint hedge.index hedge.hedge

let print_unit fmt _ = ()

let print_static fmt static =
  PSHGraph.print
    print_vertex print_hedge
    print_unit print_unit print_unit
    fmt static

let print_dynamic fmt dynamic =
  fprintf fmt "{ texitpoints = %a }"
    (Print.array
      (PHashhe.print print_tpoint print_unit))
    dynamic.texitpoints

let print_equation print_graph fmt equation =
  fprintf fmt "{ @[<v>db=%a;@ initial=%a;@ tcfgs=%a;@ sinit = %a;@ graph = %a@] }"
    (Db.print_prog Cfg.print_syntax print_unit) equation.db
    Bddapron.Expr2.Bool.print equation.initial
    (Print.array Cfg.print_bddapron) equation.tcfgs
    (PSette.print print_vertex) equation.sinit
    print_graph equation.graph
  ;
  ()

let print_static_equation fmt equation =
  print_equation print_static fmt equation
let print_dynamic_equation fmt equation =
  print_equation print_dynamic fmt equation

let print_apron_scalar fmt scalar =
  let res = Apron.Scalar.is_infty scalar in
  if res<>0 then
    pp_print_string fmt (if res<0 then "-oo" else "+oo")
  else begin
    match scalar with
    | Apron.Scalar.Float _ | Apron.Scalar.Mpfrf _ ->
	Apron.Scalar.print fmt scalar
    | Apron.Scalar.Mpqf mpqf ->
	Apron.Scalar.print fmt (Apron.Scalar.Float (Mpqf.to_float mpqf))
  end
let print_apron_interval fmt itv =
  Format.fprintf fmt "[@[<hv>%a;@ %a@]]"
    print_apron_scalar itv.Apron.Interval.inf
    print_apron_scalar itv.Apron.Interval.sup
let print_apron_box string_of_dim fmt tinterval =
  let first = ref true in
  fprintf fmt "[|@[";
  Array.iteri
    (begin fun i interval ->
      if not (Apron.Interval.is_top interval) then begin
	if not !first then fprintf fmt ";@ ";
	fprintf fmt "%s in %a" (string_of_dim i)
	  print_apron_interval interval;
	first := false
      end;
    end)
    tinterval
  ;
  fprintf fmt "@]|]"
let print_apron string_of_dim fmt abs =
  let man = Apron.Abstract0.manager abs in
  if Apron.Abstract0.is_bottom man abs then
    fprintf fmt "bottom"
  else begin
    if !Option.print_box then begin
      let box = Apron.Abstract0.to_box man abs in
      print_apron_box string_of_dim fmt box;
    end;
    Apron.Abstract0.print string_of_dim fmt abs
  end

let print_bddapron_abstract1 man fmt abs =
  Bddapron.Domain1.print ~print_apron man fmt abs

let print_ltpointtabs ~print_abstract fmt ltpointabs =
  Print.list
    ~first:"@[<v>" ~sep:"@ " ~last:"@]"
    (fun fmt (tpoint,abs) ->
      fprintf fmt "%a@ %a"
	print_tpoint tpoint
	print_abstract abs
    )
    fmt
    ltpointabs


(** ********************************************************************* *)
(** {3 Environment associated to a vertex} *)
(** ********************************************************************* *)

let env_of_vertex2 equation vertex =
  let env_of_point i point =
    let tcfg = equation.tcfgs.(i) in
    let tinfo = PSHGraph.info tcfg in
    let env = PHashhe.find tinfo.Cfg.pointenv point in
    env
  in

  let db = equation.db in
  match vertex with
  | Top tpoint ->
      let rec parcours uenv i =
	if i<(Array.length tpoint) then
	  parcours (Bddapron.Env.lce uenv (env_of_point i tpoint.(i))) (i+1)
	else
	  uenv
      in
      let env = parcours (env_of_point 0 tpoint.(0)) 1 in
      if false then
	printf "env=%a@." Bddapron.Env.print env
      ;
      env
  | Tail(i,ppoint) ->
      let env =
	let point = match ppoint with
	  | Syntax.Push point -> point
	  | _ -> failwith ""
	in
	env_of_point i point
      in
      let tcfg = equation.tcfgs.(i) in
      let (coutput,coutputtmp) =
	let predhedge = PSHGraph.predhedge tcfg ppoint in
	let hedge = match PSette.elements predhedge with
	  | [x] -> x
	  | _ -> failwith ""
	in
	let instr = PSHGraph.attrhedge tcfg hedge in
	let open Cfg in
	match instr with
	| Call(Push,callinstr) ->
	    (callinstr.coutput,callinstr.coutputtmp)
	| _ -> failwith ""
      in
      let lvartyp =
	List.rev_map2
	  (fun var vartmp -> (vartmp, Mappe.find var db.gvars))
	  (db.lglobal:>Syntax.global list)
	  (db.lglobaltmp:>Syntax.var list)
      in
      let lvartyp =
	List.fold_left2
	  (begin fun res var vartmp ->
	    let typ = Bddapron.Env.typ_of_var env var in
	    (vartmp,typ)::res
	  end)
	  lvartyp
	  coutput coutputtmp
      in
      let nenv = Bddapron.Env.add_vars env lvartyp in
      if false then printf "ppoint=%a@.env=%a@.nenv=%a@."
	Syntax.print_point ppoint
	Bddapron.Env.print env Bddapron.Env.print nenv
      ;
      nenv

let env_of_vertex equation vertex =
  try
    PHashhe.find equation.vertexenv vertex
  with Not_found ->
    let env = env_of_vertex2 equation vertex in
    PHashhe.add equation.vertexenv vertex env;
    env

let index_of_threadname
  ~(equation:'a equation) (threadname:string)
  =
  let index = ref (-1) in
  try
    Array.iteri
      (fun i thread -> if thread.tname=threadname then (index := i; raise Exit))
      equation.db.threads;
    failwith (Print.sprintf "Unknown thread %s" threadname);
  with Exit ->
    !index

let make_fpmanager_common
    ~(debug:int)
    ~equation
    :
    (vertex,hedge,unit,bool) Fixpoint.manager
    =
  {
    Fixpoint.bottom = begin fun vertex -> () end;
    Fixpoint.canonical = begin fun vertex abs -> () end;
    Fixpoint.is_bottom = begin fun vertex abs -> false end;
    Fixpoint.is_leq = begin fun vertex abs1 abs2 -> false end;
    Fixpoint.join = begin fun vertex abs1 abs2 -> () end;
    Fixpoint.join_list = begin fun vertex labs -> () end;
    Fixpoint.odiff = None;
    Fixpoint.widening = begin fun vertex abs1 abs2 -> () end;
    Fixpoint.abstract_init = begin fun vertex -> () end;
    Fixpoint.arc_init = begin fun hedge -> false end;
    Fixpoint.apply = begin fun hedge tx -> (false,()) end;
    Fixpoint.print_abstract = begin fun fmt abs -> () end;
    Fixpoint.print_arc = pp_print_bool;
    Fixpoint.print_vertex = print_vertex;
    Fixpoint.print_hedge = begin fun fmt hedge ->
      fprintf fmt "%a: %i %i"
	print_tpoint hedge.tpoint
	hedge.index hedge.hedge;
    end;
    Fixpoint.accumulate = true;
    Fixpoint.print_fmt = Format.std_formatter;
    Fixpoint.print_analysis=debug>=1;
    Fixpoint.print_component=debug>=2;
    Fixpoint.print_step=debug>=3;
    Fixpoint.print_state=debug>=4;
    Fixpoint.print_postpre=debug>=5;
    Fixpoint.print_workingsets=debug>=6;
    Fixpoint.dot_fmt = !Option.dot_fmt;
    Fixpoint.dot_vertex = print_vertex;
    Fixpoint.dot_hedge = begin fun fmt hedge ->
      fprintf fmt "%a:%i,%i"
	print_tpoint hedge.tpoint
	hedge.index
	hedge.hedge
    end;
    Fixpoint.dot_attrvertex = begin fun fmt vertex ->
      begin match vertex with
	| Top tpoint ->
	    let tprocpoint =
	      Array.map
		(fun point ->
		  let proc = PHashhe.find equation.db.Db.pointproc point in
		  (proc,point))
		tpoint
	    in
	    Print.array
	      ~first:"(@[<h>" ~sep:",@," ~last:"@])"
	      (Print.pair ~first:"" ~sep:"," ~last:"" pp_print_string Syntax.print_point)
	      fmt
	      tprocpoint
	| Tail (index,((Syntax.Push point) as ppoint)) ->
	    let proc = PHashhe.find equation.db.Db.pointproc point in
	    fprintf fmt "(%s,%a Tail %i)"
	      proc Syntax.print_point ppoint index
      end
    end;
    Fixpoint.dot_attrhedge = begin fun fmt hedge ->
      let index = hedge.index in
      let hedge = hedge.hedge in
      let instr = PSHGraph.attrhedge equation.tcfgs.(index) hedge in
      fprintf fmt "%i: %a"
	index
	Cfg.print_instr_bddapron instr
    end;
  }

(*  ********************************************************************* *)
(** {2 General combinators} *)
(*  ********************************************************************* *)

let list_combine list1 list2 =
  List.combine (list1:>Syntax.var list) (list2:>Syntax.var list)

let list_combine_add res list1 list2 =
  List.fold_left2
    (fun res x y -> (x,y)::res)
    res (list1:>Syntax.var list) (list2:>Syntax.var list)

let array_mem tvar var0 : bool =
  try
    Array.iter
      (fun var -> if var=var0 then raise Exit)
      tvar
    ;
    false
  with Exit ->
    true

let array_forall f tab =
  let length = Array.length tab in
  let res = ref true in
  for i=0 to length-1 do
    res := !res && f tab.(i)
  done;
  !res
let array_forall2 f tab1 tab2 =
  let length1 = Array.length tab1 in
  if length1<>(Array.length tab2) then raise (Invalid_argument "");
  let res = ref true in
  for i=0 to length1-1 do
    res := !res && f tab1.(i) tab2.(i)
  done;
  !res

let array_map2 f tab1 tab2 =
  let length1 = Array.length tab1 in
  if length1<>(Array.length tab2) then raise (Invalid_argument "");
  Array.init
    length1
    (fun i -> f tab1.(i) tab2.(i))

let array_mapi2 f tab1 tab2 =
  let length1 = Array.length tab1 in
  if length1<>(Array.length tab2) then raise (Invalid_argument "");
  Array.init length1 (fun i -> f i tab1.(i) tab2.(i))

(*
let array_map3 f tab1 tab2 tab3 =
  let length1 = Array.length tab1 in
  if length1<>(Array.length tab2) ||
    length1<>(Array.length tab3)
  then raise (Invalid_argument "");
  Array.init length1 (fun i -> f tab1.(i) tab2.(i) tab3.(i))

let array_mapi3 f tab1 tab2 tab3 =
  let length1 = Array.length tab1 in
  if length1<>(Array.length tab2) ||
    length1<>(Array.length tab3)
  then raise (Invalid_argument "");
  Array.init length1 (fun i -> f i tab1.(i) tab2.(i) tab3.(i))

let rename_of_array2 (array1:Syntax.var array) (array2:Syntax.var array) : (Syntax.var*Syntax.var) list
  =
  let l1 = Array.length array1 in
  let l2 = Array.length array2 in
  if l1<>l2 then failwith(Print.sprintf "Equation.rename_of_array2 %a %a"
    (Print.array Syntax.print_var) array1 (Print.array Syntax.print_var) array2);
  let res = ref [] in
  for i=0 to l1-1 do
    res := (array1.(i),array2.(i)) :: !res
  done;
  !res
*)
