(** Converting from [Syntax.program] to [Db.program] *)

open Syntax

let env_add_vars (env:Syntax.var Bddapron.Env.t) map =
  Bddapron.Env.add_vars env (Mappe.bindings (map:>(Syntax.var,'a) Mappe.t))

let procedure ((pname,proc):string * Syntax.var Syntax.procedure) : unit Db.procedure
    =
  let (linput:[`Local of string * string * [`Input]] list) =
    List.map
      (begin fun (v,t) ->
	match v with
	| (`Local(_,_,`Input)) as v -> v
	| _ -> failwith ""
      end)
      proc.pinput
  in
  let (loutput:[`Local of string * string * [`Output]] list) =
    List.map
      (begin fun (v,t) ->
	match v with
	| (`Local(_,_,`Output)) as v -> v
	| _ -> failwith ""
      end)
      proc.poutput
  in
  let vars = Mappe.empty in
  let vars =
    List.fold_left2
      (fun res v (_,t) -> Mappe.add (v:>Syntax.local) t res)
      vars
      linput proc.pinput
  in
  let vars =
    List.fold_left2
      (fun res v (_,t) -> Mappe.add (v:>Syntax.local) t res)
      vars
      loutput proc.poutput
  in
  let dbproc = {
    Db.pname = pname;
    Db.pinput = proc.pinput;
    Db.poutput = proc.poutput;
    Db.pcode = proc.pcode;
    Db.vars = vars;
    Db.instrumInput = { Db.instrum_dummy with Db.icond = None };
    Db.instrumOutput = { Db.instrum_dummy with Db.icond = None };
    Db.linput = linput;
    Db.loutput = loutput;
    Db.linputtmp = List.map (fun (`Local(t,v,`Input)) -> `Local(t,v,`Tmp)) linput;
    Db.loutputtmp = List.map (fun (`Local(t,v,`Output)) -> `Local(t,v,`Tmp)) loutput;
    Db.linputfrozen = [];
    Db.linputcst = [];
    Db.linputnotcst = [];
    Db.loutputcopy = [];
    Db.pext = ()
  }
  in
  dbproc

let thread0
    (prog:Syntax.var Syntax.program) lglobal
    :
    (unit,unit) Db.thread
    =
  let thread = {
    Db.tname = "";
    Db.tindex = -1;
    Db.tvars = Mappe.empty;
    Db.lglobalinput = List.rev_map (fun (`Global(v,`Current)) -> `Global(v,`Input "")) lglobal;
    Db.lglobaloutput = List.rev_map (fun (`Global(v,`Current)) -> `Global(v,`Output "")) lglobal;
    Db.tprocedures =
      List.fold_left
	(begin fun res (pname,proc) ->
	  Mappe.add pname (procedure (pname,proc)) res
	end)
	Mappe.empty
	prog.procedures
    ;
    Db.proccallsites = Hashhe.create 7;
    Db.text = ();
    }
    in
  thread

let program
    ~env0
    ~cond0
    (prog:Syntax.var Syntax.program)
    :
    (unit,unit) Db.program
    =
  let (lglobal,gvars,pcs) =
    List.fold_left
      (begin fun (lglobal,gvars,pcs) (v,t) ->
	match v with
	| (`Global(_,`Current)) as v ->
	    (v::lglobal, Mappe.add (v:>Syntax.global) t gvars, pcs)
	| (`Pc _) as v -> (lglobal,gvars, Mappe.add v t pcs)
	| _ -> failwith ""
      end)
      ([],Mappe.empty,Mappe.empty) prog.global
  in
  let thread0 = thread0 prog lglobal in
  let prog = {
    Db.typenumdef = prog.typenumdef;
(*
    Db.global = prog.global;
*)
    Db.gvars = gvars;
    Db.pcs = pcs;
    Db.lglobal = List.rev lglobal;
    Db.lglobaltmp = List.rev_map (fun (`Global(v,`Current)) -> `Global(v,`Tmp)) lglobal;
    Db.initial = prog.initial;
    Db.final = prog.final;
    Db.lthreads = prog.threads;
    Db.threads = [|thread0|];
    Db.env = begin
      let env = Bddapron.Env.copy env0 in
      List.iter
	(begin fun (name,labels) ->
	  Bddapron.Env.add_typ_with env name (`Benum labels)
	  end)
	prog.typenumdef
      ;
      env_add_vars env gvars
    end;
    Db.cond0 = cond0;
    Db.startexit = PDHashhe.create_compare Syntax.point_hashhe_compare Syntax.point_hashhe_compare 7;
    Db.callret = PDHashhe.create_compare Syntax.point_hashhe_compare Syntax.point_hashhe_compare 7;
    Db.pointproc = PHashhe.create_compare Syntax.point_hashhe_compare 7;
  }
  in
  Mappe.iter
    (begin fun pname proc ->
      let pcode = proc.Db.pcode in
      let pstart = pcode.bpoint in
      let pexit = exit_of_block pcode in
      PDHashhe.add prog.Db.startexit pstart pexit;
      iter_block
	(begin fun decl bpoint instr ->
	  PHashhe.replace prog.Db.pointproc bpoint pname;
	  PHashhe.add prog.Db.pointproc instr.ipoint pname;
	  match instr.instruction with
	  | CALL(_,callee,_) ->
	      PDHashhe.add prog.Db.callret bpoint instr.ipoint
	  | _ -> ()
	end)
	[] pcode
    end)
    thread0.Db.tprocedures
  ;
  prog
