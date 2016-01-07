(** Duplicate and rename procedures in each thread *)

open Format
open Db

type readwrite = {
  mutable globalread : string Sette.t;
  mutable globalwrite : string Sette.t;
  mutable globalreadother : string Sette.t;
  mutable globalwriteother : string Sette.t;
}

(*  ********************************************************************** *)
(** {2 Variable renaming} *)
(*  ********************************************************************** *)

let rename_global (tname:string) (var:[`Global of string * [> ]]) =
  match var with
  | `Global(var,`Input x) -> `Global(var,`Input tname)
  | `Global(var,`Output x) -> `Global(var,`Output tname)
  | `Global _ -> var
let rename_local (tname:string) var =
  match var with
  | `Local((x:string),(var:string),kind) -> `Local(tname,var,kind)

let rename tname var =
  match var with
  | `Local _ as x -> ((rename_local tname x):>Syntax.var)
  | `Global _ as x -> ((rename_global tname x):>Syntax.var)
  | `String _
  | `Pc _ -> var

let rename_decls (tname:string) decls =
  List.map (fun (v,t) -> (rename tname v,t)) decls

let rename_lvar (tname:string) lvar =
  List.map (rename tname) lvar

let rename_expr (tname:string) expr =
  Syntax.substitute (rename tname) expr
let rename_oexpr (tname:string) = function
  | None -> None
  | Some(e) -> Some(rename_expr tname e)

let rename_mappe_global tname map =
  Mappe.fold
    (fun var typ res -> Mappe.add (rename_global tname var) typ res)
    map Mappe.empty
let rename_mappe_local tname map =
  Mappe.fold
    (fun var typ res -> Mappe.add (rename_local tname var) typ res)
    map Mappe.empty

let rename_instrum tname instrum =
  let isyncond = rename_expr tname instrum.isyncond in
  {
    isyncond = isyncond;
    icond = None;
  }

(*  ********************************************************************** *)
(** {2 Identify variables read and written by procedures and threads} *)
(*  ********************************************************************** *)

let sette_add ?filter res var =
  let b = match filter with
    | None -> true
    | Some f -> f var
  in
  if b then Sette.add var res else res

let block_readwritevar row ?filter res block =
  let set = ref res in
  Syntax.iter_block
    (begin fun decl point instr ->
      match instr.Syntax.instruction with
      | Syntax.ASSUME(expr)
      | Syntax.IF(Some(expr),_,_)
      | Syntax.LOOP(Some(expr),_) ->
	  begin match row with
	  | `Read ->
	      set := Syntax.support ?filter !set expr
	  | `Write ->
	      ()
	  end
      | Syntax.CALL(pout,pname,pin) ->
	  let lvar = match row with
	      | `Read -> pin
	      | `Write -> pout
	  in
	  set :=
	    List.fold_left
	      (fun set var -> 
		let takeit = match filter with
		  | Some(f) -> f var
		  | None -> true
		in
		if takeit then Sette.add var set else set
	      )
	      !set lvar
      | Syntax.ASSIGN(lvar,loexpr) ->
	  begin match row with
	  | `Read ->
	      List.iter
		(begin function
		  | None -> ()
		  | Some expr ->
		      set := Syntax.support ?filter !set expr
		end)
		loexpr
	  | `Write ->
	      set := List.fold_left
		(fun set var -> 
		  let takeit = match filter with
		    | Some(f) -> f var
		    | None -> true
		  in
		  if takeit then Sette.add var set else set
		)
		!set lvar
	  end
      | _ -> ()
    end)
    [] block;
  !set

let procedure_readwritevar row ?filter res proc =
  block_readwritevar row ?filter res proc.pcode

let thread_readwritevar row ?filter thread =
  Mappe.fold
    (begin fun pname procedure res ->
      procedure_readwritevar row ?filter res procedure
    end)
    thread.tprocedures
    Sette.empty

(*  ********************************************************************** *)
(** {2 Procedure} *)
(*  ********************************************************************** *)

let rename_procedure tname proc =
  let nproc = { proc with
    Db.pinput = rename_decls tname proc.Db.pinput;
    Db.poutput = rename_decls tname proc.Db.poutput;
    Db.pcode = Syntax.block_substitute
      ~env:()
      ~name:(fun env s -> rename tname s)
      ~point:(fun x -> x)
      ~scope:(fun env decl -> env)
      proc.Db.pcode;
    Db.vars = rename_mappe_local tname proc.vars;
    Db.instrumInput = rename_instrum tname proc.Db.instrumInput;
    Db.instrumOutput = rename_instrum tname proc.Db.instrumOutput;
    Db.linput = List.map (rename_local tname) proc.Db.linput;
    Db.loutput = List.map (rename_local tname) proc.Db.loutput;
    Db.linputtmp = List.map (rename_local tname) proc.Db.linputtmp;
    Db.loutputtmp = List.map (rename_local tname) proc.Db.loutputtmp;
    Db.linputfrozen = List.map (rename_local tname) proc.Db.linputfrozen;
    Db.linputcst = List.map (rename_local tname) proc.Db.linputcst;
    Db.linputnotcst = List.map (rename_local tname) proc.Db.linputnotcst;
    Db.loutputcopy = List.map (rename_local tname) proc.Db.loutputcopy;
    Db.pext = ()
  }
  in
  nproc

(*  ********************************************************************** *)
(** {2 Threads} *)
(*  ********************************************************************** *)

let collect_procedures thread0 pname =
  let callsites = Hashhe.create 11 in
  let mapproc = ref Mappe.empty in

  let rec collect pname =
    let proc = Mappe.find pname thread0.tprocedures in
    if not (Mappe.mem pname !mapproc) then begin
      mapproc := Mappe.add pname proc !mapproc;
      Syntax.iter_block
	(begin fun _ bpoint instr ->
	  match instr.Syntax.instruction with
	  | Syntax.CALL(_,pname,_) ->
	      begin
		let set =
		  try Hashhe.find callsites pname
		  with Not_found -> PSette.empty Syntax.point_compare
		in
		let nset = PSette.add bpoint set in
		Hashhe.replace callsites pname nset
	      end;
	      collect pname
	  | _ -> ()
	end)
	[]
	proc.Db.pcode
    end
  in
  collect pname;
  (!mapproc,callsites)

let rename_thread thread0 tname tindex =
  let (tprocedures,callsites) = collect_procedures thread0 tname in
  let tprocedures =
    Mappe.map
      (rename_procedure tname)
      tprocedures
  in
  let readwrite = {
    globalread = Sette.empty;
    globalwrite = Sette.empty;
    globalreadother = Sette.empty;
    globalwriteother = Sette.empty;
  }
  in
  let thread = {
    Db.tname = tname;
    Db.tindex = tindex;
    Db.tvars = rename_mappe_global tname thread0.tvars;
    Db.lglobalinput = List.map (fun (`Global(v,`Input _)) -> `Global(v,`Input tname)) thread0.lglobalinput;
    Db.lglobaloutput = List.map (fun (`Global(v,`Output _)) -> `Global(v,`Output tname)) thread0.lglobaloutput;
    Db.tprocedures = tprocedures;
    Db.proccallsites = callsites;
    Db.text = readwrite;
  }
  in
  let filter = function
    | `Global _ -> true 
    | _ -> false
  in
  let globalread = thread_readwritevar `Read ~filter thread in
  let globalwrite = thread_readwritevar `Write ~filter thread in
  let filter set =
    Sette.fold
      (fun var res ->
	match var with
	| `Global(x,`Current) -> Sette.add x res
	| _ as x -> 
	    failwith (Print.sprintf "Bizarre: %a@." Syntax.print_var x)
      )
      set
      Sette.empty
  in
  readwrite.globalread <- filter globalread;
  readwrite.globalwrite <- filter globalwrite;
  thread

(*  ********************************************************************** *)
(** {2 Program } *)
(*  ********************************************************************** *)

let rename_program prog
    =
  let thread0 = prog.threads.(0) in
  assert((Array.length prog.threads) = 1);
  assert(thread0.tindex = -1);
  let tthreads = Array.of_list prog.lthreads in
  let threads =
    Array.mapi
      (begin fun i tname ->
	rename_thread thread0 tname i
      end)
      tthreads
  in
  Array.iteri
    (begin fun i threadi ->
      let readwrite = threadi.text in
      Array.iteri
	(begin fun j threadj ->
	  if i!=j then begin
	    readwrite.globalreadother <-
	      Sette.union
	      readwrite.globalreadother
	      threadj.text.globalread;
	    readwrite.globalwriteother <-
	      Sette.union
	      readwrite.globalwriteother
	      threadj.text.globalwrite;
	  end
	end)
	threads
    end)
    threads
  ;
  let nprog = { prog with threads=threads } in
  nprog
