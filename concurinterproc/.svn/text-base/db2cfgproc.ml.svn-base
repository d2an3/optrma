open Format
open Syntax
open Cfg
open Db

(*  ********************************************************************** *)
(** {2 Cfg of block} *)
(*  ********************************************************************** *)

let rec cfg_of_block ~scheduling cfg tname pname env block =
  if block.Syntax.instrs=[] then begin
    let point = block.Syntax.bpoint in
    let typ = match scheduling with
      | `Cooperative -> `Internal
      | `Preemptive -> `Commute
    in
    if not (PSHGraph.is_vertex cfg point) then begin
      PSHGraph.add_vertex cfg point typ;
      let info = PSHGraph.info cfg in
      PHashhe.add info.pointenv point env
    end
  end
  else
    ignore (
      List.fold_left
	(begin fun bpoint instr ->
	  cfg_of_instr ~scheduling cfg tname pname env bpoint instr;
	  instr.Syntax.ipoint
	end)
	block.Syntax.bpoint
	block.Syntax.instrs
    )

and cfg_of_instr ~scheduling cfg tname pname env point (instr:Syntax.var Syntax.instr) =
(*
  if false then printf "instr@.env=%a@.instr=%a@."
    Bddapron.Env.print env
    (Syntax.print_instr Syntax.print_var Syntax.print_point) instr
  ;
*)
  let add_point point =
    let typ = match scheduling with
      | `Cooperative -> `Internal
      | `Preemptive -> `Commute
    in
    if not (PSHGraph.is_vertex cfg point) then begin
      PSHGraph.add_vertex cfg point typ;
      let info = PSHGraph.info cfg in
      PHashhe.add info.pointenv point env
    end;
    ()
  in
  let add_edge2 org lbinstr dest =
    PSHGraph.add_hedge cfg
      (Cfg.unique()) (Cfg.Block lbinstr) ~pred:[|org|] ~succ:[|dest|]
  in
  let add_edge org binstr dest =
    add_edge2 org [binstr] dest
  in

  let ipoint = instr.ipoint in
  add_point point;
  add_point ipoint;
  begin match instr.instruction with
  | YIELD ->
      add_edge2 point [Cfg.Condition Syntax.etrue] ipoint;
      begin match scheduling with
      | `Cooperative ->
	  PSHGraph.replace_attrvertex cfg point `Commute
      | `Preemptive -> ()
      end
  | SKIP ->
      add_edge point (Cfg.Condition Syntax.etrue) ipoint
  | HALT ->
      ()
  | FAIL ->
      PSHGraph.replace_attrvertex cfg point `Fail;
  | ASSUME(expr) ->
      add_edge point (Cfg.Condition expr) ipoint
  | ASSIGN(lvar,loexpr) ->
      let (lvar,lexpr,lvarrandom) =
	List.fold_right2
	  (fun var oexpr (lvar,lexpr,lvarrandom) ->
	    match oexpr with
	    | None -> (lvar, lexpr, var::lvarrandom)
	    | Some expr -> (var::lvar, expr::lexpr, lvarrandom)
	  )
	  lvar loexpr ([],[],[])
      in
      if lvar=[] then add_edge point (Cfg.Forget lvarrandom) ipoint
      else if lvarrandom=[] then add_edge point (Cfg.Assign(lvar,lexpr)) ipoint
      else add_edge2 point [(Cfg.Assign (lvar,lexpr));(Cfg.Forget lvarrandom)] ipoint
  | IF(ocond,block1,oblock2) ->
      cfg_of_block ~scheduling cfg tname pname env block1;
      let (cond,ncond) = match ocond with
	| None -> (Syntax.etrue,Syntax.etrue)
	| Some cond -> (cond, `Unop(`Not,cond))
      in
      add_edge point (Cfg.Condition cond) block1.bpoint;
      add_edge (Syntax.exit_of_block block1) (Cfg.Condition Syntax.etrue) ipoint;
      begin match oblock2 with
      | None ->
	  add_edge point (Cfg.Condition ncond) ipoint
      | Some(block2) ->
	  cfg_of_block ~scheduling cfg tname pname env block2;
	  add_edge point (Cfg.Condition ncond) block2.bpoint;
	  add_edge (Syntax.exit_of_block block2) (Cfg.Condition Syntax.etrue) ipoint
      end
  | LOOP(ocond,block1) ->
      cfg_of_block ~scheduling cfg tname pname env block1;
      let (cond,ncond) = match ocond with
	| None -> (Syntax.etrue,Syntax.etrue)
	| Some cond -> (cond, `Unop(`Not,cond))
      in
      add_edge point (Cfg.Condition cond) block1.bpoint;
      add_edge point (Cfg.Condition ncond) ipoint;
      add_edge (Syntax.exit_of_block block1) (Cfg.Condition Syntax.etrue) point
  | GOTO(dpoint) ->
      add_point dpoint;
      add_edge point (Cfg.Condition Syntax.etrue) dpoint;
  | CALL(pout,name,pin) ->
      let callinstr = {
	Cfg.callpoint = point;
	Cfg.retpoint = ipoint;
	Cfg.caller = pname;
	Cfg.callee = name;
	Cfg.cinput = pin;
	Cfg.coutput = pout;
	Cfg.coutputtmp =
	  List.map
	    (function
	      |  `Local(tname,vname,kind) -> `Local(tname,vname,`Tmp)
	      | _ -> failwith ""
	    )
	    pout;
      }
      in
      PSHGraph.add_hedge cfg
	(Cfg.unique()) (Cfg.Call(Cfg.CallReturn,callinstr)) ~pred:[|point|] ~succ:[|ipoint|]
  | ATOMIC(block) ->
      cfg_of_block ~scheduling cfg tname pname env block;
      add_edge point (Cfg.Condition Syntax.etrue) block.bpoint;
      add_edge (Syntax.exit_of_block block) (Cfg.Condition Syntax.etrue) ipoint;
      begin match scheduling with
      | `Cooperative -> ()
      | `Preemptive ->
	  Syntax.iter_block
	    (begin fun _ point instr ->
	      PSHGraph.replace_attrvertex cfg point `Internal;
	      PSHGraph.replace_attrvertex cfg instr.ipoint `Internal;
	    end)
	    [] block
      end
  | LOCAL(_,decls,block) ->
      let nenv = Bddapron.Env.add_vars env decls in
      cfg_of_block ~scheduling cfg tname pname nenv block;
      if decls<>[] then begin
	add_edge point (Cfg.Intro decls) block.bpoint;
	add_edge (Syntax.exit_of_block block) (Cfg.Elim decls) ipoint
      end
      else begin
	add_edge point (Cfg.Condition Syntax.etrue) block.bpoint;
	add_edge (Syntax.exit_of_block block) (Cfg.Condition Syntax.etrue) ipoint
      end;
      ()
  end

(*  ********************************************************************** *)
(** {2 Cfg of procedure and thread} *)
(*  ********************************************************************** *)

let map_instrum env cond0 instrum =
  let expr2 = Bddapron.Syntax.to_boolexpr2
    ~normalize:true env (Bddapron.Cond.copy cond0) instrum.isyncond 
  in
  { instrum with icond = Some(expr2) }

let translate_procedure ~scheduling tname env cond0 proc =
  let env = Syn2db.env_add_vars env proc.vars in
  let nproc = { proc with
    Db.instrumInput = map_instrum env cond0 proc.instrumInput;
    Db.instrumOutput = map_instrum env cond0 proc.instrumOutput;
    Db.pext = begin
      let cfg = Cfg.make() in
      cfg_of_block ~scheduling cfg tname proc.pname env proc.pcode;
      let info = PSHGraph.info cfg in
      info.pstart <- proc.pcode.bpoint;
      info.pexit <- exit_of_block proc.pcode;
      cfg
    end
  }
  in
  nproc

let translate_thread ~scheduling env cond0 thread =
  let tenv = Syn2db.env_add_vars env thread.tvars in
  let nthread = { thread with
    tprocedures = Mappe.map (translate_procedure ~scheduling thread.tname tenv cond0) thread.tprocedures;
  }
  in
  nthread

(*  ********************************************************************** *)
(** {2 Program } *)
(*  ********************************************************************** *)

let translate_program
    ~scheduling
    prog
    =
  let nprog = { prog with
    threads = Array.map (translate_thread ~scheduling prog.env prog.cond0) prog.threads
  }
  in
  nprog
