(** Adding auxiliary variables for interprocedural analysis *)

open Syntax
open Db

let block_inputw (block:Syntax.var Syntax.block) =
  let set = ref Sette.empty in
  Syntax.iter_block
    (begin fun _ _ instr ->
      match instr.instruction with
      | ASSIGN(lvar,_) | CALL(lvar,_,_) ->
	  List.iter
	    (function
	      | `Local("",var,`Input) ->
		  set := Sette.add var !set
	      | _ -> ()
	    )
	    lvar
      | _ -> ()
    end)
    []
    block
  ;
  !set

let procedure_add_inputcopy proc
    =
  let inputw = block_inputw proc.Db.pcode in
  let instrum = proc.Db.instrumInput in
  List.iter
    (begin fun (`Local(tname,vname,`Input) as var) ->
      if Sette.mem vname inputw then begin
	let varfrozen = `Local(tname,vname,`InputFrozen) in
	let typ = Mappe.find var proc.vars in
	proc.vars <- Mappe.add varfrozen typ proc.vars;
	proc.linputnotcst <- var::proc.linputnotcst;
	proc.linputcst <- varfrozen::proc.linputcst;
	proc.linputfrozen <- varfrozen::proc.linputfrozen;
	instrum.isyncond <- Syntax.eand instrum.isyncond (Syntax.eeq var varfrozen)
      end
      else begin
	proc.linputcst <- var::proc.linputcst;
      end
    end)
    (List.rev proc.linput)
  ;
  ()

let procedure_add_outputcopy proc
    =
  let instrum = proc.instrumOutput in
  List.iter
    (begin fun ((`Local(tname,vname,`Output)) as var) ->
      let varcopy = `Local(tname,vname,`OutputCopy) in
      let typ = Mappe.find var proc.vars in
      proc.vars <- Mappe.add varcopy typ proc.vars;
      proc.loutputcopy <- varcopy::proc.loutputcopy;
      instrum.isyncond <- Syntax.eand instrum.isyncond (Syntax.eeq var varcopy)
    end)
    (List.rev proc.loutput)
  ;
  ()

let add_globalcopy prog thread0 tag =
  let condition =
    Mappe.fold
      (begin fun nvar ntyp res ->
	match nvar with
	| `Global(var,`Current) ->
	    let vartag = `Global(var,tag) in
	    thread0.tvars <- Mappe.add vartag ntyp thread0.tvars;
	    Syntax.eand res (Syntax.eeq nvar vartag)
	| _ ->
	    res
      end)
      prog.gvars Syntax.etrue
  in
  (condition:>Syntax.var Bddapron.Syntax.expr)

let prog_add_IOcopy prog
    =
  let thread0 = prog.threads.(0) in
  let gcondInput = add_globalcopy prog thread0 (`Input "") in
  let gcondOutput = add_globalcopy prog thread0 (`Output "") in
  Mappe.iter
    (begin fun pname proc ->
      proc.instrumInput.isyncond <- gcondInput;
      procedure_add_inputcopy proc;
      proc.instrumOutput.isyncond <- gcondOutput;
      procedure_add_outputcopy proc;
    end)
    thread0.tprocedures
