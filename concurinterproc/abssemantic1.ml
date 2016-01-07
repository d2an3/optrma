(** Abstract semantics of CFG instructions, standard version *)

(* This file is part of the ConcurInterproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Bertrand Jeannet 2009.
*)

open Format
open Cfg
open Db

type 'd abstract = (Syntax.var,'d) Bddapron.Domain1.t

let print_abstract = Equation.print_bddapron_abstract1

let print_apron string_of_dim fmt abs =
  let apron = Apron.Abstract0.manager abs in
(*
  if Apron.Disjunction.manager_is_disjunction apron then begin
    let apron = Apron.Disjunction.manager_to_disjunction apron in
    let abs = Apron.Disjunction.Abstract0.to_disjunction abs in
    let tabs = Apron.Disjunction.decompose apron abs in
    Print.array
      (Apron.Abstract0.print string_of_dim)
      fmt tabs
  end
  else
*)
  fprintf fmt "@[<v>%a@ %a@]"
    (Apron.Abstract0.print string_of_dim) abs
    (Print.array (Apron.Generator0.print string_of_dim)) (Apron.Abstract0.to_generator_array apron abs)

let bddapron_domain1_print man fmt abs =
  Bddapron.Domain1.print ~print_apron man fmt abs

let listexpr2_of_lvar
  (env:Syntax.var Bddapron.Env.t)
  (cond:Syntax.var Bddapron.Cond.t)
  list
  :
  Syntax.var Bddapron.Expr2.List.t
    =
  let lexpr0 = List.map
    (Bddapron.Expr0.var env cond)
    (list:>Syntax.var list)
  in
  Bddapron.Expr2.List.of_lexpr0 ~normalize:false env cond lexpr0

let map_remove_list map list
    =
  let map = (map:>(Syntax.var,Syntax.var Syntax.typ) Mappe.t) in
  let list = (list:>Syntax.var list) in
  List.fold_right Mappe.remove list map

let map_to_list map =
  let map = (map:>(Syntax.var,Syntax.var Syntax.typ) Mappe.t) in
  Mappe.fold (fun var typ res -> var::res) map []

let is_bottom = Bddapron.Domain1.is_bottom

let bottom_of_vertex equation man vertex : 'a abstract =
  Bddapron.Domain1.bottom man (Equation.env_of_vertex equation vertex)
let top_of_vertex equation man vertex : 'a abstract =
  Bddapron.Domain1.top man (Equation.env_of_vertex equation vertex)

(*  ********************************************************************** *)
(** {2 Common transfer functions} *)
(*  ********************************************************************** *)

(*  ====================================================================== *)
(** {3 Meet} *)
(*  ====================================================================== *)

let meet_odest
    (manager:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
    (abstract:'d abstract)
    (odest:'d abstract option)
    =
  let res = match odest with
    | None -> abstract
    | Some dest -> Bddapron.Domain1.meet manager abstract dest
  in
  res

(*  ====================================================================== *)
(** {3 Condition} *)
(*  ====================================================================== *)

let apply_condition
  (manager:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
  (abstract:'d abstract)
  (expr: Syntax.var Bddapron.Expr2.Bool.t)
  (odest:'d abstract option)
  :
  'd abstract
  =
  if Bddapron.Expr2.Bool.is_false expr then
    Bddapron.Domain1.bottom manager abstract.Bddapron.Env.env
  else
    let abstract = meet_odest manager abstract odest in
    if Bddapron.Expr2.Bool.is_true expr then
      abstract
    else begin
      let expr =
	if expr.Bdd.Cond.val1.Bddapron.Env.env !=
	  abstract.Bddapron.Env.env
	then
	  Bddapron.Expr2.Bool.extend_environment expr abstract.Bddapron.Env.env
	else
	  expr
      in
      Bddapron.Domain1.meet_condition2 manager abstract expr
    end

(*  ====================================================================== *)
(** {3 Change of dimension} *)
(*  ====================================================================== *)

let apply_intro
  (manager:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
  (abstract:'d abstract)
  lvartyp
  odest
    =
  let nenv = match odest with
    | None ->
	Bddapron.Env.add_vars abstract.Bddapron.Env.env lvartyp
    | Some dest ->
	dest.Bddapron.Env.env
  in
  let nabs = Bddapron.Domain1.change_environment manager abstract nenv in
  let nabs = meet_odest manager nabs odest in
  nabs

let apply_elim
  (manager:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
  (abstract:'d abstract)
  lvar
  odest
    =
  let nenv = match odest with
    | None ->
	Bddapron.Env.remove_vars abstract.Bddapron.Env.env lvar
    | Some dest ->
	dest.Bddapron.Env.env
  in
  let nabs = Bddapron.Domain1.change_environment manager abstract nenv in
  let nabs = meet_odest manager nabs odest in
  nabs

let apply_forget
  (manager:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
  (abstract:'d abstract)
  lvar
  odest
    =
  let nabs = meet_odest manager abstract odest in
  Bddapron.Domain1.forget_list manager nabs lvar

(*  ====================================================================== *)
(** {3 Assignement} *)
(*  ====================================================================== *)

let apply_assign
    ~analysis
    (manager:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
    (abstract:'d abstract)
    lvar listexpr2
    (odest:'d abstract option)
    =
  let listexpr2 =
    if
      listexpr2.Bdd.Cond.val1.Bddapron.Env.env !=
      abstract.Bddapron.Env.env
    then begin
      if false then begin
	printf "lvar=%a@.listexpr2=%a@.env=%a@.nenv=%a@."
	  (Print.list Syntax.print_var) lvar
	  Bddapron.Expr2.List.print listexpr2
	  Bddapron.Env.print listexpr2.Bddapron.Cond.val1.Bddapron.Env.env
	  Bddapron.Env.print abstract.Bddapron.Env.env
      end;
      Bddapron.Expr2.List.extend_environment listexpr2 abstract.Bddapron.Env.env
    end
    else
      listexpr2
  in
  let asssub = match analysis with
    | `Forward ->
	Bddapron.Domain1.assign_listexpr2
	  ~relational:true ~nodependency:false
    | `Backward ->
	Bddapron.Domain1.substitute_listexpr2
  in
  let res = asssub manager abstract lvar listexpr2 odest in
  if false then
    printf "org=%a@.lvar=%a listexpr2=%a@.odest=%t@.res=%a@."
      (bddapron_domain1_print manager) abstract
      (Print.list Syntax.print_var) lvar
      Bddapron.Expr2.List.print listexpr2
      (fun fmt -> match odest with
      | Some dest -> bddapron_domain1_print manager fmt dest
      | None -> fprintf fmt "None")
      (bddapron_domain1_print manager) res
  ;
  res

(*  ====================================================================== *)
(** {3 Basic Instruction and block of basic instructions} *)
(*  ====================================================================== *)

let apply_binstr
    ~analysis
    (manager:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
    (abs:'d abstract)
    binstr
    (odest:'d abstract option)
    :
    'd abstract
    =
  let res =
    match binstr with
    | Forget lvar ->
	apply_forget manager abs lvar odest
    | Intro decls when analysis=`Forward ->
	apply_intro manager abs decls odest
    | Elim decls when analysis=`Backward ->
	apply_intro manager abs decls odest
    | Intro decls
    | Elim decls ->
	apply_elim manager abs (List.map fst decls) odest
    | Assign(lvar,listexpr2) ->
	apply_assign ~analysis manager abs lvar listexpr2 odest
    | Condition cond ->
	apply_condition manager abs cond odest
  in
  if false then
    printf "      apply_hedge %a %a = %a@ @?"
      Cfg.print_binstr_bddapron binstr
      (bddapron_domain1_print manager) abs
      (bddapron_domain1_print manager) res
  ;
  res

let apply_block
    ~analysis
    (manager:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
    (abs:'d abstract)
    block
    (odest:'d abstract option)
    :
    'd abstract
    =
  let rec parcours abs = function
    | x::lx ->
	if lx=[] then
	  apply_binstr ~analysis manager abs x odest
	else
	  let nabs = apply_binstr ~analysis manager abs x None in
	  parcours nabs lx
    | [] -> failwith "Here 1"
  in
  parcours abs
    (begin match analysis with
    | `Forward -> block
    | `Backward -> List.rev block
    end)

(*  ********************************************************************** *)
(** {2 Forward interprocedural semantics} *)
(*  ********************************************************************** *)

(* We assume that actual input/output parameters are NEVER global variables *)

module Forward = struct

  (*  ==================================================================== *)
  (** {3 Call} *)
  (*  ==================================================================== *)

  let apply_call
      ~(instrum:bool)
      (thread:(Cfg.syntax_t,unit) Db.thread)
      (callinstr:callinstr)
      (manager:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
      (abstract:'d abstract)
      (nenv:Syntax.var Bddapron.Env.t)
      (odest:'d abstract option)
      =
    let caller = Mappe.find callinstr.caller thread.tprocedures in
    let callee = Mappe.find callinstr.callee thread.tprocedures in
    (* 1. We remove non-argument local variables from the current
       abstract value + globalinput and globaloutput *)
    let res =
      Bddapron.Domain1.forget_list manager abstract
	((@)
	  (thread.lglobalinput:>Syntax.var list)
	  (thread.lglobaloutput:>Syntax.var list))
    in
    let res =
      let vars = map_remove_list caller.vars callinstr.cinput in
      let lvar = map_to_list vars in
      let env = Bddapron.Env.remove_vars res.Bddapron.Env.env lvar in
      Bddapron.Domain1.change_environment manager res env
    in
    (* 2. We rename inargs in inputcopy *)
    let res =
      Bddapron.Domain1.rename manager res
	(Equation.list_combine callinstr.cinput callee.linput)
    in
    (* 3. We embed in callee environment *)
    let res = Bddapron.Domain1.change_environment manager res nenv in
    (* 4. We intersect with instrumentation condition and/or with destination *)
    let res =
      if instrum then
	let cond = match callee.instrumInput.icond with
	  | Some x -> x
	  | None -> failwith ""
	in
	apply_condition manager res cond odest
      else
	meet_odest manager res odest
    in
    res

  let apply_push
      (thread:(Cfg.syntax_t,unit) Db.thread)
      (callinstr:callinstr)
      (manager:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
      (abstract:'d abstract)
      nenv
      (odest:'d abstract option)
      =
    (* 1. We remove variables of other processes *)
    let res =
      Bddapron.Domain1.change_environment manager abstract nenv
    in
    (* 2. We intersect with destination *)
    let res = meet_odest manager res odest in
    res

  (*  ==================================================================== *)
  (** {3 Return} *)
  (*  ==================================================================== *)

  let apply_return
      (db:(Cfg.syntax_t,unit) Db.program)
      (thread:(Cfg.syntax_t,unit) Db.thread)
      (callinstr:callinstr)
      (manager:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
      (abstail:'d abstract) (abstop:'d abstract)
      nenv
      (odest:'d abstract option)
      =
    let callee = Mappe.find callinstr.callee thread.tprocedures in
    (* 1. We forget local variables in abstop, + possibly globaloutputcopy *)
    let res =
      let lvar =
	List.concat [
	  (callee.linputnotcst:>Syntax.var list);
	  (callee.loutputcopy:>Syntax.var list);
	  (thread.lglobaloutput:>Syntax.var list)
	]
      in
      if lvar<>[] then
	let env = Bddapron.Env.remove_vars abstop.Bddapron.Env.env lvar in
	Bddapron.Domain1.change_environment manager abstop env
      else
	abstop
    in
    if false then printf "1.res=%a@."
      (Bddapron.Domain1.print manager) res;
    (* 2. We rename in modified abstop
       (globalinput,inputfrozen,global,fr) by (global,inargs,globaltmp,frtmp)
    *)
    let res =
      let lpair = Equation.list_combine
	thread.lglobalinput db.lglobal
      in
      let lpair = Equation.list_combine_add lpair
	callee.linputcst callinstr.cinput
      in
      let lpair = Equation.list_combine_add lpair
	db.lglobal db.lglobaltmp
      in
      let lpair = Equation.list_combine_add lpair
	callee.loutput callee.loutputtmp
      in
      let res =
	Bddapron.Domain1.rename manager res lpair
      in
      if false then printf "lpair=%a@.2.res=%a@."
	(Print.list (Print.pair Syntax.print_var Syntax.print_var)) lpair
	(Bddapron.Domain1.print manager) res;
      res
    in
    (* 3. We unify the renamed top value and the tail value *)
    if false then printf "abstail=%a@.3.res=%a@."
      (Bddapron.Domain1.print manager) abstail
      (Bddapron.Domain1.print manager) res;
    let res = Bddapron.Domain1.unify manager res abstail in
    (* 4. We remove global variables, and then rename globaltmp in global *)
    let res =
      let env =
	Bddapron.Env.remove_vars res.Bddapron.Env.env
	  (db.lglobal:>Syntax.var list)
      in
      let res = Bddapron.Domain1.change_environment manager res env in
      let res = Bddapron.Domain1.rename manager res
	(Equation.list_combine db.lglobaltmp db.lglobal)
      in
      res
    in
    (* 4. We assign out parameters *)
    let res =
      let listexpr2 = listexpr2_of_lvar
	res.Bddapron.Env.env db.cond0 callee.loutputtmp
      in
      Bddapron.Domain1.assign_listexpr2 ~relational:true ~nodependency:true
	manager res callinstr.coutput listexpr2 None
    in
    (* 5. We remove the frtmp variables *)
    let res = Bddapron.Domain1.change_environment manager res nenv in
    (* 6. We possibly intersect with the result of a previous analysis *)
    let res = meet_odest manager res odest in
    res

end

(*  ********************************************************************** *)
(** {2 Backward interprocedural semantics} *)
(*  ********************************************************************** *)

module Backward = struct

  (*  ==================================================================== *)
  (** {3 Return} *)
  (*  ==================================================================== *)

  let apply_return
      ~(instrum:bool)
      (thread:(Cfg.syntax_t,unit) Db.thread)
      (callinstr:callinstr)
      (manager:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
      (abstract:'d abstract)
      nenv
      (odest:'d abstract option)
      =
    let caller = Mappe.find callinstr.caller thread.tprocedures in
    let callee = Mappe.find callinstr.callee thread.tprocedures in
     (* 1. We remove local variables not in cinput U coutput,
	and we forget gobalinput and globaloutput *)
    if false then printf "abstract=%a@."
      (Bddapron.Domain1.print manager) abstract
    ;
    let res =
      Bddapron.Domain1.forget_list manager abstract
	((@)
	  (thread.lglobalinput:>Syntax.var list)
	  (thread.lglobaloutput:>Syntax.var list))
    in
    let res =
      let vars = map_remove_list caller.vars callinstr.coutput in
      let vars = map_remove_list vars callinstr.cinput in
      let lvar = map_to_list vars in
      let env = Bddapron.Env.remove_vars res.Bddapron.Env.env lvar in
      Bddapron.Domain1.change_environment manager res env
    in
    (* 2. We rename coutput into fr, and possibly cinput\coutput
       into corresponding input *)
    if false then printf "res.1=%a@."
      (Bddapron.Domain1.print manager) res
    ;
    let res =
      let lpair = Equation.list_combine callinstr.coutput callee.loutput in
      let lpair =
	List.fold_left2
	  (begin fun res cinput inputcst ->
	    if List.mem cinput callinstr.coutput then
	      res
	    else
	      (cinput,(inputcst:>Syntax.var))::res
	    end)
	    lpair callinstr.cinput callee.linputcst
      in
      Bddapron.Domain1.rename manager res lpair
    in
    (* 3. We embed in callee environment *)
    if false then printf "res.2=%a@."
      (Bddapron.Domain1.print manager) res
    ;
    let res = Bddapron.Domain1.change_environment manager res nenv in
    (* 4. We intersect with instrumentation condition and/or with destination *)
    if false then printf "res.3=%a@."
      (Bddapron.Domain1.print manager) res
    ;
    let res =
      if instrum then
	let cond = match callee.instrumOutput.icond with
	  | Some x -> x
	  | None -> failwith ""
	in
	apply_condition manager res cond odest
      else
	meet_odest manager res odest
    in
    if false then printf "res.4=%a@."
      (Bddapron.Domain1.print manager) res
    ;
    res

  let apply_push
      (db:(Cfg.syntax_t,unit) Db.program)
      (thread:(Cfg.syntax_t,unit) Db.thread)
      (callinstr:callinstr)
      (manager:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
      (abstract:'d abstract)
      nenv
      (odest:'d abstract option)
      =
    (* 1. We remove variables of other processes and add
       globaltmp, coutputtmp *)
    if false then
      printf "abstract=%a@ " (Bddapron.Domain1.print manager) abstract;

    let res = Bddapron.Domain1.change_environment manager abstract nenv in
    if false then printf "Here@.env=%a@ nenv=%a@ res.1=%a@ "
      Bddapron.Env.print abstract.Bddapron.Env.env
      Bddapron.Env.print nenv
      (Bddapron.Domain1.print manager) res
    ;
    (* 2. We rename (global,coutput) in (globaltmp,coutputtmp) and conversely *)
    let res =
      let lpair = Equation.list_combine
	db.lglobal db.lglobaltmp
      in
      let lpair = Equation.list_combine_add lpair
	callinstr.coutput callinstr.coutputtmp
      in
      let lpair = Equation.list_combine_add lpair
	db.lglobaltmp db.lglobal
      in
      let lpair = Equation.list_combine_add lpair
	callinstr.coutputtmp callinstr.coutput
      in
      Bddapron.Domain1.rename manager res lpair
    in
    if false then
      printf "res.2=%a@ " (Bddapron.Domain1.print manager) res;
    (* 3. We possibly intersect *)
    let res = meet_odest manager res odest in
    if false then
      printf "res=%a@ " (Bddapron.Domain1.print manager) res;
    res

  (*  ==================================================================== *)
  (** {3 Call} *)
  (*  ==================================================================== *)

  let apply_call
      (db:(Cfg.syntax_t,unit) Db.program)
      (thread:(Cfg.syntax_t,unit) Db.thread)
      (callinstr:callinstr)
      (manager:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
      (abstail:'d abstract) (abstop:'d abstract)
      nenv
      (odest:'d abstract option)
      =
    let callee = Mappe.find callinstr.callee thread.tprocedures in
    (* 1. We possibly intersect with instrumentation condition *)
    if false then printf "abstail=%a@.abstop=%a@."
      (Bddapron.Domain1.print manager) abstail
      (Bddapron.Domain1.print manager) abstop
    ;
    let res =
	let cond = match callee.instrumInput.icond with
	  | Some x -> x
	  | None -> failwith ""
	in
	apply_condition manager abstop cond None
    in
    if false then printf "res.1=%a@."
      (Bddapron.Domain1.print manager) res
    ;
    (* 2. We forget local variables in abstop, + globalinputcopy *)
    let res =
      let lvar =
	List.concat
	  [(callee.linputfrozen:>Syntax.var list);
	  (callee.loutput:>Syntax.var list);
	  (thread.lglobalinput:>Syntax.var list)]
      in
      if lvar<>[] then
	let env = Bddapron.Env.remove_vars abstop.Bddapron.Env.env lvar in
	Bddapron.Domain1.change_environment manager res env
      else
	res
    in
    (* 3. We rename in modified abstop
       (globaloutput,outputcopy,input) by (globaltmp,coutputtmp,cinput)
    *)
    if false then printf "res.2=%a@."
      (Bddapron.Domain1.print manager) res
    ;
    let res =
      let lpair = Equation.list_combine
	thread.lglobaloutput db.lglobaltmp
      in
      let lpair = Equation.list_combine_add lpair
	callee.loutputcopy callinstr.coutputtmp
      in
      let lpair = Equation.list_combine_add lpair
	callee.linput callinstr.cinput
      in
      Bddapron.Domain1.rename manager res lpair
    in
    (* 4. We unify the renamed top value and the tail value *)
    if false then printf "res.3=%a@."
      (Bddapron.Domain1.print manager) res
    ;
    let res = Bddapron.Domain1.unify manager res abstail in
    (* 5. We remove globaltmp & frtmp variables *)
    if false then printf "res.4=%a@."
      (Bddapron.Domain1.print manager) res
    ;
    let res = Bddapron.Domain1.change_environment manager res nenv in
    (* 6. We possibly intersect with the result of a previous analysis *)
    if false then printf "res.5=%a@."
      (Bddapron.Domain1.print manager) res
    ;
    let res = meet_odest manager res odest in
    res

end

(*  ********************************************************************** *)
(** {2 General instruction} *)
(*  ********************************************************************** *)

let apply_instr
    ~analysis
    (db:(Cfg.syntax_t,unit) Db.program)
    (thread:(Cfg.syntax_t,unit) Db.thread)
    (instr:Cfg.bddapron_instr)
    (manager:(Syntax.var,'b,'c,'d) Bddapron.Domain1.man)
    (tabs:'d abstract array)
    nenv
    (odest:'d abstract option)
    :
    'd abstract
    =
  let abs =
    tabs.(0)
  in
  let res =
    match instr with
    | Block block ->
	apply_block ~analysis manager abs block odest
    | Call(CallStart,callinstr) ->
	begin match analysis with
	| `Forward ->
	    Forward.apply_call ~instrum:true thread callinstr manager abs nenv odest
	| `Backward ->
	    Backward.apply_call db thread callinstr manager abs tabs.(1) nenv odest
	end;
    | Call(ExitReturn,callinstr) ->
	begin match analysis with
	| `Forward ->
	    Forward.apply_return db thread callinstr manager abs tabs.(1) nenv odest
	| `Backward ->
	    Backward.apply_return ~instrum:true thread callinstr manager abs nenv odest
	end
    | Call(Push,callinstr) ->
	begin match analysis with
	| `Forward -> Forward.apply_push
	| `Backward -> Backward.apply_push db
	end
	  thread callinstr manager abs nenv odest
    | Call(CallReturn,_) -> failwith ""
  in
  res
