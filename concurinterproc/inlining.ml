(** Inlining procedures (after checking, before instrumenting procedures) *)

open Format
open Syntax

(*  ====================================================================== *)
(** {2 Utilities} *)
(*  ====================================================================== *)

(** Compute the set of input variables written *)

let procedure_inputw (proc:Syntax.var Syntax.procedure) : Syntax.var Sette.t =
  let set = ref Sette.empty in
  Syntax.iter_block
    (begin fun _ _ instr ->
      match instr.instruction with
      | ASSIGN(lvar,_) | CALL(lvar,_,_) ->
	  List.iter
	    (function
	      | `Local("",var,`Input) as x ->
		  set := Sette.add x !set
	      | _ -> ()
	    )
	    lvar
      | _ -> ()
    end)
    []
    proc.pcode
  ;
  !set

(** Find a local variable with same name, not already in the environment *)

let rec newlocalvar (env:Syntax.var Sette.t) (var:Syntax.local) : Syntax.var
    =
  let `Local(tname,vname,lkind) = var in
  begin match lkind with
  | `Local tab ->
      if Sette.mem (var:>Syntax.var) env then begin
	let ntab = if tab=[||] then [|0|] else Array.copy tab in
	let var = `Local(tname,vname,`Local ntab) in
	begin
	  try
	    for i=ntab.(0) to max_int do
	      ntab.(0) <- i;
	      if not (Sette.mem var env) then raise Exit
	    done;
	    failwith ""
	  with Exit ->
	    (var:>Syntax.var)
	end
      end
      else
	(var:>Syntax.var)
  | `Input -> newlocalvar env (`Local(tname,vname,`Local [|0|]))
  | _ -> failwith ""
  end

(*  ====================================================================== *)
(** {2 Renaming the code of a procedure when inlined inside another one} *)
(*  ====================================================================== *)

let block_rename index (env,map) block =
  Syntax.block_substitute
    ~env:(env,map)
    ~name:(begin fun (env,map) var ->
      match var with
      | `Local(_,vname,_) ->
	  begin
	    try Mappe.find var map
	    with Not_found -> var
	  end
      | _ -> var
    end)
    ~point:(fun tab -> Array.append [|index|] tab)
    ~scope:(fun (env,map) decl ->
      List.fold_left
	(begin fun (env,map) (var,typ) ->
	  match var with
	  | `Local(tname,vname,`Local tab) ->
	      let nvar = `Local(tname,vname,`Local (Array.append [|0|] tab)) in
	      (Sette.add nvar env, Mappe.add var nvar map)
	  | _ -> failwith ""
	end)
	(env,map) decl
    )
    block

(*  ====================================================================== *)
(** {2 Inlining inside a procedure the code of callee procedures} *)
(*  ====================================================================== *)

let procedure_expanse
    (inlined_callee:(string, Syntax.var procedure) Mappe.t)
    (procedure:Syntax.var Syntax.procedure)
    =
  let calleeindex = Mappe.map (fun proc -> ref 0) inlined_callee in
  let rec map_instruction
      (env:Syntax.var Sette.t)
      bpoint (instruction:Syntax.var instruction)
      =
    match instruction with
    | YIELD
    | SKIP
    | HALT
    | FAIL
    | FLUSH
    | ASSUME _
    | ASSIGN _
    | GOTO _ -> instruction
    | IF(e,b,ob) ->
	let b = map_block env b in
	let ob = match ob with
	  | None -> None
	  | Some(b) -> Some(map_block env b)
	in
	IF(e,b,ob)
    | LOOP(e,b) -> LOOP(e, map_block env b)
    | ATOMIC(b) -> ATOMIC(map_block env b)
    | LOCAL(flag,decl,b) ->
	let nenv =
	  List.fold_left
	    (fun res (var,typ) -> Sette.add var res)
	    env decl
	in
	LOCAL(flag,decl,map_block nenv b)
    | CALL(pout,pname,pin) ->
	if not (Mappe.mem pname inlined_callee) then
	  instruction
	else begin
	  let callee = Mappe.find pname inlined_callee in
	  let index = Mappe.find pname calleeindex in
	  let inputw = procedure_inputw callee in
	  let rename =
	    List.fold_left2
	      (begin fun res (formal,_) actual ->
		Mappe.add formal actual res
	      end)
	      Mappe.empty callee.poutput pout
	  in
	  let (env,rename,assign) =
	    List.fold_left2
	      (begin fun (env,rename,assign) (formal,typ) (actual:Syntax.var) ->
		if Sette.mem formal inputw then
		  let actualw =
		    match actual with
		    | `Local(tname,vname,_) as l -> newlocalvar env l
		    | _ -> failwith ""
		  in
		  ((Sette.add actualw env),
		  (Mappe.add formal actualw rename),
		  (actualw,actual,typ)::assign)
		else
		  (env,
		  (Mappe.add formal actual rename),
		  assign)
	      end)
	      (env,rename,[]) callee.pinput pin
	  in
	  let nbpoint =
	    Syntax.point_substitute
	      (fun tab -> Array.append [|!index|] tab) bpoint
	  in
	  let body = block_rename !index (env,rename) callee.pcode in
	  let nbody =
	    if assign=[] then
	      LOCAL(true,[],body)
	    else begin
	      let decl = List.map (fun (actw,act,typ) -> (actw,typ)) assign in
	      let instr =
		let cond =
		  List.fold_left
		    (fun res (actw,act,typ) -> Syntax.eand res (Syntax.eeq actw act))
		    Syntax.etrue
		    assign
		in
		{ instruction = ASSUME(cond); ipoint = body.bpoint }
	      in
	      let nbody = { bpoint = nbpoint; instrs = instr :: body.instrs } in
	      LOCAL(true,decl,nbody)
	    end
	  in
	  incr index;
	  nbody
	end
  and map_instr env bpoint instr =
    { instr with instruction = map_instruction env bpoint instr.instruction }
  and map_block env block =
    { block with instrs =
	let (_,acc) =
	  List.fold_left
	    (fun (bpoint,acc) instr ->
	      let ninstr = map_instr env bpoint instr in
	      (instr.ipoint,ninstr::acc)
	    )
	    (block.bpoint,[]) block.instrs
	in
	List.rev acc
    }
  in
  let env = Sette.empty in
  let env =
    List.fold_left
      (fun env (var,typ) -> Sette.add var env)
      env procedure.pinput
  in
  let env =
    List.fold_left
      (fun env (var,typ) -> Sette.add var env)
      env procedure.poutput
  in
  { procedure with pcode = map_block env procedure.pcode }

(*  ====================================================================== *)
(** {2 Compute the ordered list of inlinable procedures (bottom-up)} *)
(*  ====================================================================== *)

let inlinable (prog:Syntax.var Syntax.program) =
  let g = SHGraph.create 17 () in
  List.iter
    (begin fun (pname,proc) ->
      SHGraph.add_vertex g pname ()
    end)
    prog.procedures
  ;
  List.iter
    (begin fun (caller,proc) ->
      Syntax.iter_block
	(begin fun vardecls point instr ->
	  match instr.instruction with
	  | CALL(pout,callee,pin) ->
	      if not (SHGraph.is_hedge g (caller,callee)) then
		SHGraph.add_hedge g
		  (caller,callee) () ~pred:[|caller|] ~succ:[|callee|]
	  | _ -> ()
	end)
	[]
	proc.pcode
    end)
    prog.procedures
  ;
  let cfc =
    SHGraph.cfc_multi "+" ("+","+") g
      (List.fold_right Sette.add prog.threads Sette.empty)
  in
  let inlinable = ref [] in
  List.iter
    (begin fun component ->
      match component with
      | [x] -> inlinable := x :: !inlinable
      | _ -> ()
    end)
    cfc
  ;
  !inlinable

(*  ====================================================================== *)
(** {2 Main} *)
(*  ====================================================================== *)

let main
    (prog:Syntax.var Syntax.program)
    (tobeinlined:string Sette.t)
    :
    Syntax.var Syntax.program
    =
  let inlinable = inlinable prog in
  let inlined_callee =
    List.fold_left
      (begin fun map pname ->
	if Sette.mem pname tobeinlined then
	  let proc = List.assoc pname prog.procedures in
	  let nproc = procedure_expanse map proc in
	  if false then begin
	    printf "proc=%a@."
	      (Syntax.print_procedure Syntax.print_var Syntax.print_point)
	      proc
	    ;
	    printf "nproc=%a@."
	      (Syntax.print_procedure Syntax.print_var Syntax.print_point)
	      nproc
	    ;
	  end;
	  Mappe.add pname nproc map
	else
	  map
      end)
      Mappe.empty inlinable
  in
  let nprog =
    { prog with procedures =
	List.map
	  (begin fun (pname,proc) ->
	    let nproc =
	      try
		Mappe.find pname inlined_callee
	      with Not_found ->
		procedure_expanse inlined_callee proc
	    in
	    (pname,nproc)
	  end)
	  prog.procedures
    }
  in
  nprog
