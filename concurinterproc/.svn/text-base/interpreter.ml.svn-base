(** *)

exception Backtrack

type value = [
| `Bool of bool
| `Bint of int
| `Benum of Syntax.var
| `Int of Mpzf.t
| `Real of float
]

type env = (Syntax.var, value) PMappe.t
type cstate = (Syntax.var Bddapron.Env.t) * env * env
type act = {
  callinstr : Cfg.callinstr;
  bddapronenv : Syntax.var Bddapron.Env.t;
  point : Syntax.point;
  lenv : env;
}
type stack = act list
type state = {
  genv : env;
  threads : stack array;
}


let power2 n = 1 lsl n;;

(*  ====================================================================== *)
(** {2 Evaluation of expressions} *)
(*  ====================================================================== *)

let eval_cst bddapronenv (cst:Bddapron.Syntax.cst) : value =
  match cst with
    | `Apron coeff ->
	begin match coeff with
	  | Apron.Coeff.Scalar scalar ->
	      begin match scalar with
		| Apron.Scalar.Float f -> `Real f
		| Apron.Scalar.Mpqf mpqf ->
		    let den = Mpqf.get_den mpqf in
		    if (Mpzf.cmp_int den 1) = 0 then
		      `Int(Mpqf.get_num mpqf)
		    else
		      `Real (Mpqf.to_float mpqf)
		| Apron.Scalar.Mpfrf mpfrf ->
		    `Real (Mpfrf.to_float mpfrf)
	      end
	  | _ -> failwith ""
	end
    | `Bint((sign, size), a) -> `Bint a
    | `Bool b -> `Bool b

let rec eval_bool cstate e : bool =
  let v = eval cstate e in
  match v with
    | `Bool b -> b
    | _ -> failwith ""

and eval_bbinop cstate (op:Bddapron.Syntax.bbinop) e1 e2 : bool =
  begin match op with
    | `Or | `And | `EQ | `NEQ ->
	let v1 = eval_bool cstate e1 in
	let v2 = eval_bool cstate e2 in
	begin match op with
	  | `Or  -> v1 || v2
	  | `And -> v1 && v2
	  | `EQ  -> v1 = v2
	  | `NEQ -> v1 <> v2
	  | _ -> failwith ""
	end
    | `GT | `GEQ | `LEQ | `LT ->
	let v1 = eval cstate e1 in
	let v2 = eval cstate e2 in
	begin match op with
	  | `GT ->
	      begin match (v1,v2) with
		  (* add modulo stuff *)
		| (`Bint n1),(`Bint n2) -> n1 > n2
		| (`Int n1),(`Int n2) -> (Mpzf.cmp n1 n2) > 0
		| (`Real n1),(`Real n2) -> n1 > n2
		| _ -> failwith ""
	      end
	  | `GEQ ->
	      begin match (v1,v2) with
		  (* add modulo stuff *)
		| (`Bint n1),(`Bint n2) -> n1 >= n2
		| (`Int n1),(`Int n2) -> (Mpzf.cmp n1 n2) >= 0
		| (`Real n1),(`Real n2) -> n1 >= n2
		| _ -> failwith ""
	      end
	  | `LEQ ->
	      begin match (v1,v2) with
		  (* add modulo stuff *)
		| (`Bint n1),(`Bint n2) -> n1 <= n2
		| (`Int n1),(`Int n2) -> (Mpzf.cmp n1 n2) <= 0
		| (`Real n1),(`Real n2) -> n1 <= n2
		| _ -> failwith ""
	      end
	  | `LT ->
	      begin match (v1,v2) with
		  (* add modulo stuff *)
		| (`Bint n1),(`Bint n2) -> n1 < n2
		| (`Int n1),(`Int n2) -> (Mpzf.cmp n1 n2) < 0
		| (`Real n1),(`Real n2) -> n1 < n2
		| _ -> failwith ""
	      end
	  | _ -> failwith ""
	end
  end

and eval_unop cstate unop e1 : value =
  begin match unop with
    | `Not ->
	let v1 = eval_bool cstate e1 in
	`Bool (not v1)

    | `Apron(op,typ,rnd) ->
	let v1 = eval cstate e1 in
	begin match v1 with
	  | `Real n1 ->
	      let res = begin match op with
		| Apron.Texpr1.Neg -> (-.n1)
		| Apron.Texpr1.Cast -> failwith ""
		| Apron.Texpr1.Sqrt -> failwith ""
	      end
	      in
	      `Real res

	  | `Int n1 ->
	      let res = begin match op with
		| Apron.Texpr1.Neg -> Mpzf.neg n1
		| Apron.Texpr1.Cast -> failwith ""
		| Apron.Texpr1.Sqrt -> failwith ""
	      end
	      in
	      `Int res

	  | _ -> failwith ""
	end ;
  end

and eval_binop cstate (binop:Bddapron.Syntax.binop) e1 e2 : value =
  begin match binop with
    | `Bool op ->
	let ress = eval_bbinop cstate op e1 e2 in
	`Bool ress
    | `Apron(op,typ,rnd) ->
	let v1 = eval cstate e1 in
	let v2 = eval cstate e2 in
	begin match (v1, v2) with
	  | (`Real n1), (`Real n2) ->
	      let res = begin match op with
		| Apron.Texpr1.Add -> n1 +. n2
		| Apron.Texpr1.Sub -> n1 -. n2
		| Apron.Texpr1.Mul -> n1 *. n2
		| Apron.Texpr1.Div -> n1 /. n2
		| Apron.Texpr1.Mod -> failwith ""
	      end
	      in
	      `Real res
	  | (`Int n1), (`Int n2) ->
	      let res = begin match op with
		| Apron.Texpr1.Add -> Mpzf.add n1 n2
		| Apron.Texpr1.Sub -> Mpzf.sub n1 n2
		| Apron.Texpr1.Mul -> Mpzf.mul n1 n2
		| Apron.Texpr1.Div -> Mpzf.fdiv_q n1 n2
		| Apron.Texpr1.Mod -> failwith ""
	      end
	      in
	      `Int res
	  | _ -> failwith ""
	end
  end

and eval_if cstate e1 e2 e3 : value =
  let v1 = eval_bool cstate e1 in
  eval cstate (if v1 then e2 else e3)

and eval ((bddapronenv,_,_) as cstate) e : value =
  match e with
    | `Cst cst -> eval_cst bddapronenv cst
    | `Ref var -> failwith""
    | `Unop(op,e) -> eval_unop cstate op e
    | `Binop(op,e1,e2) -> eval_binop cstate op e1 e2
    | `If(e1,e2,e3) -> eval_if cstate e1 e2 e3
    | _ -> failwith ""

(*  ====================================================================== *)
(** {2 Instructions} *)
(*  ====================================================================== *)

let execute_forget_var (bddapronenv:Syntax.var Bddapron.Env.t) var : value =
  let typ = PMappe.find var bddapronenv.Bdd.Env.vartyp in
  match typ with
    | `Benum(typname) ->
	let typdef = PMappe.find typname bddapronenv.Bdd.Env.typdef in
	begin match typdef with
	  | `Benum tab ->
	      let length = Array.length tab in
	      let k = Random.int length in
	      `Benum tab.(k)
	end
    | `Bint(sign,size) ->
	let bound = power2 size in
	let n = Random.int bound in
	let res = if sign then n - (power2 (size-1)) else n in
	`Bint res
    | `Bool  -> `Bool(Random.bool ())
    | `Int   ->
	let n = Random.int max_int in
	let s = Random.bool () in
	let n = if s then n else (-n) in
	`Int(Mpzf.of_int n)
    | `Real  ->
	let n = Random.float max_float in
	let s = Random.bool () in
	let n = if s then n else (-.n) in
	`Real(n)


let execute_forget (bddapronenv,genv,lenv) lvar : cstate
    =
  let lvalue = List.map (fun var ->  execute_forget_var bddapronenv var) lvar in
  let (genv,lenv) =
    List.fold_left2
      (begin fun (genv,lenv) var value ->
	if PMappe.mem var lenv then
	  (genv, PMappe.add var value lenv)
	else
	  (PMappe.add var value genv, lenv)
      end)
      (genv,lenv)
      lvar lvalue
  in
  (bddapronenv,genv,lenv)

let execute_assign ((bddapronenv,genv,lenv) as cstate) lvar lexpr : cstate =
  let lvalue = List.map (eval cstate) lexpr in
  let (genv,lenv) =
    List.fold_left2
      (begin fun (genv,lenv) var value ->
	if PMappe.mem var lenv then
	  (genv, PMappe.add var value lenv)
	else
	  (PMappe.add var value genv, lenv)
      end)
      (genv,lenv)
    lvar lvalue
  in
  (bddapronenv,genv,lenv)

let execute_condition cstate cond : unit =
  let b = eval_bool cstate cond in
  if not b then raise Backtrack

let execute_call
    thread
    (genv:env)
    (stack:stack)
    callinstr
    :
    stack
    =
  let callact = List.hd stack in
  let calleebddapronenv =
    Bddapron.Env.remove_vars
      callact.bddapronenv
      (PSette.elements (Bddapron.Env.vars callact.bddapronenv))
  in
  let callee = Mappe.find callinstr.Cfg.callee thread.Db.tprocedures in
  let pstart = (PSHGraph.info callee.Db.pext).Cfg.pstart in
  let calleebddapronenv = Bddapron.Env.add_vars calleebddapronenv callee.Db.pinput in
  let calleebddapronenv = Bddapron.Env.add_vars calleebddapronenv callee.Db.poutput in
  let calleeenv = ref (PMappe.empty Syntax.compare_var) in
  List.iter2
    (begin fun callpar (fpar,typ) ->
      let newval = PMappe.find callpar callact.lenv in
      calleeenv := PMappe.add fpar newval !calleeenv
    end)
    callinstr.Cfg.cinput callee.Db.pinput
  ;
  List.iter
    (begin fun (var,typ) ->
      let value = execute_forget_var calleebddapronenv var in
      calleeenv := PMappe.add var value !calleeenv
    end)
    callee.Db.poutput
  ;
  let calleeact = {
    bddapronenv = calleebddapronenv;
    callinstr;
    point = pstart;
    lenv = !calleeenv;
  }
  in
  calleeact::stack

let execute_return
    thread
    (genv:env)
    (stack:stack)
    :
    stack
    =
  let calleeact = List.hd stack in
  let stack = List.tl stack in
  let calleract = List.hd stack in
  let stack = List.tl stack in

  let callinstr = calleeact.callinstr in
  let callee = Mappe.find callinstr.Cfg.callee thread.Db.tprocedures in
  let callerenv = ref calleract.lenv in
  List.iter2
    (begin fun retpar (fpar,typ) ->
      let value = PMappe.find fpar calleeact.lenv in
      callerenv := PMappe.add retpar value !callerenv
    end)
    callinstr.Cfg.coutput callee.Db.poutput
  ;
  let act = { calleract with point = callinstr.Cfg.retpoint; lenv = !callerenv } in
  act::stack

(*  ====================================================================== *)
(** {2 General instruction} *)
(*  ====================================================================== *)

let execute_binstr
    ((bddapronenv,genv,env) as cstate) (binstr:('a,'b) Cfg.binstr) : cstate
    =
  match binstr with
    | Cfg.Forget(lvar) ->
	execute_forget cstate lvar
    | Cfg.Assign(lvar, lexpr) ->
	execute_assign cstate lvar lexpr
    | Cfg.Condition(cond) ->
	execute_condition cstate cond;
	(bddapronenv,genv,env)
    | Cfg.Intro(lvartyp) ->
	let bddapronenv = Bddapron.Env.add_vars bddapronenv lvartyp in
	execute_forget (bddapronenv,genv,env) (List.map fst lvartyp)
    | Cfg.Elim(lvartyp) ->
	let lvar = List.map fst lvartyp in
	let bddapronenv = Bddapron.Env.remove_vars bddapronenv lvar in
	let env =
	  List.fold_left
	    (fun env var -> PMappe.remove var env)
	    env lvar
	in
	(bddapronenv,genv,env)

let execute_lbinstr
    cstate
    lbinstr
    :
    cstate
    =
  List.fold_left
    (fun cstate binstr -> execute_binstr cstate binstr)
    cstate
    lbinstr

(*
    | Cfg.Call(Cfg.CallReturn,callinstr) ->
	let callee = Mappe.find callinstr.Cfg.callee thread.Db.tprocedures in
	let calleecfg = callee.Db.pext in
	let infocfg = PSHGraph.info calleecfg in
	let pstart = infocfg.Cfg.pstart in
	let nstack =
	  execute_call genv stack
	    callee callinstr.Cfg.cinput
	    pstart
	in
	(genv,nstack)
    | _ -> failwith ""
*)


exception ResultThread of env*stack
let execute_step_thread
    prog thread
    genv stack
    :
    env * stack
    =
  let act = List.hd stack in
  let tail = List.tl stack in
  let pname = PHashhe.find prog.Db.pointproc act.point in
  let proc = Mappe.find pname thread.Db.tprocedures in
  let pcfg = proc.Db.pext in
  let pinfo = PSHGraph.info pcfg in
  if act.point=pinfo.Cfg.pexit then begin
    (genv, execute_return thread genv stack)
  end
  else begin
    let succhedge = PSHGraph.succhedge pcfg act.point in
    try
      PSette.iter
	(fun hedge ->
	  try
	    let instr = PSHGraph.attrhedge pcfg hedge in
	    let (genv,stack) =
	    match instr with
	      | Cfg.Block(list) ->
		  let (bddapronenv,genv,lenv) =
		    execute_lbinstr
		      (act.bddapronenv,genv,act.lenv)
		      list
		  in
		  let npoint = PSHGraph.succvertex pcfg hedge in
		  let act = { act with bddapronenv; lenv; point = npoint.(0) } in
		  (genv,act::tail)
	      | Cfg.Call(_,callinstr) ->
		  (genv,execute_call thread genv stack callinstr)
	    in
	    raise (ResultThread(genv,stack))
	  with Backtrack ->
	    ()
	)
	succhedge
      ;
      raise Backtrack
    with ResultThread(genv,stack) ->
      (genv,stack)
end

exception End
exception ResultProg of state
let execute_step_prog
    prog
    state
    :
    state
    =
  let possible = ref Sette.empty in
  begin try
    Array.iteri
      (begin fun i thread ->
	let stack = state.threads.(i) in
	if stack<>[] then begin
	  let act = (List.hd state.threads.(i)) in
	  let pname = PHashhe.find prog.Db.pointproc act.point in
	  let proc = Mappe.find pname thread.Db.tprocedures in
	  let pcfg = proc.Db.pext in
	  let attrpoint = PSHGraph.attrvertex pcfg act.point in
	  if attrpoint=`Internal then begin
	    possible := Sette.singleton i;
	    raise Exit
	  end
	  else if attrpoint=`Commute then
	    possible := Sette.add i !possible
	end
      end)
      prog.Db.threads
    ;
  with Exit -> ()
  end;
  if !possible=Sette.empty then raise End;
  begin try
    Sette.iter
      (begin fun i ->
	begin try
	  let (genv,stack) = execute_step_thread
	    prog prog.Db.threads.(i)
	    state.genv state.threads.(i)
	  in
	  let threads = Array.copy state.threads in
	  threads.(i) <- stack;
	  let state = { genv; threads } in
	  raise (ResultProg state)
	with Backtrack ->
	  ()
	end;
      end)
      !possible;
      raise Backtrack
  with ResultProg state -> state
  end
