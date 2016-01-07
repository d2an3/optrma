(** Abstract semantics of CFG instructions, standard version *)

(* This file is part of the ConcurInterproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Bertrand Jeannet, Pascal Sotin 2011.
*)

open Format
open Cfg
open Db

type 'd abstract = (Syntax.var,'d) Bddapron.Domain1.t

type 'f binstr_policy = 'f option
    (* None if no policy attached, (Some p) otherwise *)
type 'f block_policy = 'f binstr_policy list
    (* A list matching the list of instructions *)
type 'f instr_policy = 'f block_policy option
    (* None if no policy attached, (Some p) otherwise *)

let man_of_pman = Bddapron.Policy.Domain1.manager_get_manager




let print_binstr_policy pman env cond fmt = function
  | None -> pp_print_string fmt "None"
  | Some p -> Bddapron.Policy.Domain1.print pman env cond fmt p
let print_block_policy pman env cond fmt (l:_ block_policy) =
  Print.list
    (print_binstr_policy pman env cond)
    fmt
    l
let print_instr_policy pman env cond fmt = function
  | None -> pp_print_string fmt "None"
  | Some p -> print_block_policy pman env cond fmt p


(*  ********************************************************************** *)
(** {2 Common transfer functions} *)
(*  ********************************************************************** *)

(*  ====================================================================== *)
(** {3 Condition} *)
(*  ====================================================================== *)

let apply_condition
  (pmanager:(Syntax.var,'b,'c,'d,'e,'f) Bddapron.Policy.Domain1.man)
  (policy:'f)
  (abstract:'d abstract)
  (expr: Syntax.var Bddapron.Expr2.Bool.t)
  (odest:'d abstract option)
  :
  'd abstract
    =
  let manager = man_of_pman pmanager in
  if Bddapron.Expr2.Bool.is_false expr then
    Bddapron.Domain1.bottom manager abstract.Bddapron.Env.env
  else
    let abstract = Abssemantic1.meet_odest manager abstract odest in
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
      Bddapron.Policy.Domain1.meet_condition2_apply pmanager policy abstract expr
    end

let improve_condition
  (pmanager:(Syntax.var,'b,'c,'d,'e,'f) Bddapron.Policy.Domain1.man)
  (opolicy:'f option)
  (abstract:'d abstract)
  (expr: Syntax.var Bddapron.Expr2.Bool.t)
  (odest:'d abstract option)
  :
  'f
    =
  let manager = man_of_pman pmanager in
  let abstract = Abssemantic1.meet_odest manager abstract odest in
  let expr =
    if expr.Bdd.Cond.val1.Bddapron.Env.env !=
      abstract.Bddapron.Env.env
    then
      Bddapron.Expr2.Bool.extend_environment expr abstract.Bddapron.Env.env
    else
      expr
  in
  Bddapron.Policy.Domain1.meet_condition2_improve pmanager opolicy abstract expr

(*  ====================================================================== *)
(** {3 Basic Instruction *)
(*  ====================================================================== *)

let apply_binstr
    ~analysis
    (pmanager:(Syntax.var,'b,'c,'d,'e,'f) Bddapron.Policy.Domain1.man)
    (opolicy:'f binstr_policy)
    (abs:'d abstract)
    binstr
    (odest:'d abstract option)
    :
    'd abstract
    =
  let manager = man_of_pman pmanager in
  let res =
    match (opolicy,binstr) with
    | (Some policy),(Condition cond) ->
	apply_condition pmanager policy abs cond odest
    | None,_ ->
	Abssemantic1.apply_binstr ~analysis manager abs binstr odest
    | _ -> failwith ""
  in
  res

let improve_binstr
    ~analysis
    (pmanager:(Syntax.var,'b,'c,'d,'e,'f) Bddapron.Policy.Domain1.man)
    (obinstr_policy:'f binstr_policy option)
    (abs:'d abstract)
    binstr
    (odest:'d abstract option)
    :
    'f binstr_policy
    =
  match binstr with
  | Condition cond ->
      let oldpolicy = match obinstr_policy with
	| None -> None
	| Some( (Some _) as p) -> p
	| Some None -> failwith ""
      in
      Some(improve_condition pmanager oldpolicy abs cond odest)
  | _ ->
      assert(obinstr_policy=None || obinstr_policy=(Some None));
      None

(*  ====================================================================== *)
(** {3 Block *)
(*  ====================================================================== *)

let apply_block
    ~analysis
    (manager:(Syntax.var,'b,'c,'d,'e,'f) Bddapron.Policy.Domain1.man)
    (block_policy:'f block_policy)
    (abs:'d abstract)
    block
    (odest:'d abstract option)
    :
    'd abstract
    =
  let rec parcours abs = function
    | (p::lp),(x::lx) ->
	if lp=[] && lx=[] then
	  apply_binstr ~analysis manager p abs x odest
	else if lp<>[] && lx<>[] then
	  let nabs = apply_binstr ~analysis manager p abs x None in
	  parcours nabs (lp,lx)
	else
	  failwith ""
    | _ -> failwith ""
  in
  parcours abs
    (begin match analysis with
    | `Forward -> (block_policy,block)
    | `Backward -> (List.rev block_policy, List.rev block)
    end)

let improve_block
    ~analysis
    (manager:(Syntax.var,'b,'c,'d,'e,'f) Bddapron.Policy.Domain1.man)
    (oblock_policy:'f block_policy option)
    (abs:'d abstract)
    (block:Cfg.bddapron_binstr list)
    (odest:'d abstract option)
    :
    'f block_policy
    =
  match oblock_policy with
  | None ->
      let rec parcours abs = function
	| x::lx ->
	    if lx=[] then
	      (improve_binstr ~analysis manager None abs x odest)::[]
	    else
	      let npolicy = improve_binstr ~analysis manager None abs x None in
	      let nabs = apply_binstr ~analysis manager npolicy abs x None in
	      npolicy::(parcours nabs lx)
	| [] -> failwith ""
      in
      parcours abs
	(begin match analysis with
	| `Forward -> block
	| `Backward -> List.rev block
	end)
  | Some(block_policy) ->
      let rec parcours abs = function
	| (p::lp),(x::lx) ->
	    if lp=[] && lx=[] then
	      (improve_binstr ~analysis manager (Some p) abs x odest)::[]
	    else if lp<>[] && lx<>[] then
	      let npolicy = improve_binstr ~analysis manager (Some p) abs x None in
	      let nabs = apply_binstr ~analysis manager npolicy abs x None in
	      npolicy::(parcours nabs (lp,lx))
	    else
	      failwith ""
	| _ -> failwith ""
      in
      parcours abs
	(begin match analysis with
	| `Forward -> (block_policy, block)
	| `Backward -> (List.rev block_policy, List.rev block)
	end)

(*  ====================================================================== *)
(** {3 General instruction} *)
(*  ====================================================================== *)

let apply_instr
    ~analysis
    (db:(Cfg.syntax_t,unit) Db.program)
    (thread:(Cfg.syntax_t,unit) Db.thread)
    (instr:Cfg.bddapron_instr)
    (pmanager:(Syntax.var,'b,'c,'d,'e,'f) Bddapron.Policy.Domain1.man)
    (instr_policy:'f instr_policy)
    (tabs:'d abstract array)
    nenv
    (odest:'d abstract option)
    :
    'd abstract
    =
  let res =
    match (instr_policy,instr) with
    | (Some block_policy),(Block block) ->
	apply_block ~analysis pmanager block_policy tabs.(0) block odest
    | (None,_) ->
	Abssemantic1.apply_instr ~analysis db thread instr (man_of_pman pmanager) tabs nenv odest
    | _ ->
	failwith ""
  in
  res

let improve_instr
    ~analysis
    (db:(Cfg.syntax_t,unit) Db.program)
    (thread:(Cfg.syntax_t,unit) Db.thread)
    (instr:Cfg.bddapron_instr)
    (pmanager:(Syntax.var,'b,'c,'d,'e,'f) Bddapron.Policy.Domain1.man)
    (oinstr_policy:'f instr_policy option)
    (tabs:'d abstract array)
    nenv
    (odest:'d abstract option)
    :
    'f instr_policy
    =
  match instr with
  | Block block ->
      let p = match oinstr_policy with
	| None -> None
	| Some( (Some _) as p) -> p
	| Some None -> failwith ""
      in
      Some(improve_block ~analysis pmanager p tabs.(0) block odest)
  | _ ->
      assert(oinstr_policy=None || oinstr_policy=(Some None));
      None
