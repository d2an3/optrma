(** Spl language: parsing, building AST and checking it is well-formed *)

open Format
open Syntax

(*  ********************************************************************** *)
(** {2 Parsing input file} *)
(*  ********************************************************************** *)

let parse_lexbuf
    (fmt:Format.formatter)
    (lexbuf:Lexing.lexbuf)
    :
    string Syntax.program
    =
  let prog =
    try
      Parser.program Lexer.token lexbuf
    with
    | Parsing.Parse_error ->
	let startp = Lexing.lexeme_start_p lexbuf
	and endp = Lexing.lexeme_end_p lexbuf
	in
	Format.fprintf fmt
	  "Syntaxical error: %s, line %i, characters %i-%i: '%s'.@."
	  startp.Lexing.pos_fname
	  startp.Lexing.pos_lnum
	  (startp.Lexing.pos_cnum - startp.Lexing.pos_bol)
	  (endp.Lexing.pos_cnum - endp.Lexing.pos_bol)
	  (Lexing.lexeme lexbuf);
	raise Exit;
    | Syntax.Error str ->
	let startp = Lexing.lexeme_start_p lexbuf
	and endp = Lexing.lexeme_end_p lexbuf
	in
	Format.fprintf fmt "Lexical error: %s, line %i, character %i-%i: '%s': %s@."
	  startp.Lexing.pos_fname
	  startp.Lexing.pos_lnum
	  (startp.Lexing.pos_cnum - startp.Lexing.pos_bol)
	  (endp.Lexing.pos_cnum - endp.Lexing.pos_bol)
	  (Lexing.lexeme lexbuf)
	  str;
	raise Exit
  in
  prog

(*  ********************************************************************** *)
(** {2 Utility functions} *)
(*  ********************************************************************** *)

(** Checks that a list does not contain two identical names *)
let check_uniqueness print list : unit
  =
  let rec parcours = function
    | s1::((s2::_) as l) ->
	if Pervasives.compare s1 s2 = 0 then
	  error "%a" print s1
	else
	  parcours l
    | _ -> ()
  in
  let list = List.fast_sort Pervasives.compare list in
  parcours list

(** Checks disjointness of two lists *)
let check_disjointness (assoc1:(string * 'a) list) (assoc2:(string * 'a) list) : unit
    =
  let set1 =
    List.fold_left (fun res (str,_) -> SetteS.add str res)
      SetteS.empty assoc1
  in
  List.iter
    (begin fun (str2,_) ->
      if SetteS.mem str2 set1 then raise (Error str2)
    end)
    assoc2;
  ()

(** Renaming types *)
let (maptyp : string Syntax.typ -> Syntax.var Syntax.typ) = function
  | `Benum s -> `Benum (`String s)
  | `Bool as x -> x
  | (`Bint _) as x -> x
  | `Int as x -> x
  | `Real as x -> x

(*  ********************************************************************** *)
(** {2 Renaming strings by values of type [Syntax.var]} *)
(*  ********************************************************************** *)

type env = (string, (Syntax.var * (Syntax.var Syntax.typ))) Mappe.t
  (** Maps strings to a pair (symbol, type) *)

let typ_of_var ~(env:env) var =
  try
    snd (Mappe.find var env)
  with Not_found ->
    error "unkown variable/label %s" var

(** Rename any string *)
let rename ~(env:env) var =
  try
    fst (Mappe.find var env)
  with Not_found ->
    error "unkown symbol %s" var

(** Rename a string denoting a variable (and not a label) *)
let rename_var ~(env:env) var =
  try
    let (var,_) = Mappe.find var env in
    begin match var with
    | `String s ->
	error "cannot assign the label %s belonging to an enumerated type" s
    | _ -> ()
    end;
    var
  with Not_found ->
    error "unkown symbol %s" var

(** Substitution in expressions *)

let expr ~(env:env) expr =
  Syntax.substitute (rename ~env) expr
let oexpr ~env oexpr =
  match oexpr with
  | None -> None
  | Some e -> Some(expr ~env e)

(*  ********************************************************************** *)
(** {2 Rewriting instructions} *)
(*  ********************************************************************** *)

type typproc = (string, Syntax.var Bddapron.Env.typ list * Syntax.var Bddapron.Env.typ list) Mappe.t
  (** Association table procedure name -> (type input parameters, type output parameters *)

(** Completing positions *)
let posid = ref 0
let rename_pos ~pname pos =
  incr posid;
  {
    pos with
      id = !posid;
      fid = pname;
  }

(** Completing points *)
let rename_point ~pname = function
  | Pos(p,a) ->
      assert(a=[||]);
      Pos(rename_pos ~pname p, a)
  | Lab _ as x -> x
  | Push _ -> failwith ""

(** Substitute label and variable names, also disambiguate
    variables in local scopes, and checks that control lbales have
    been defined. *)

let rec instruction
    ~(typproc:typproc)
    ~(pname:string)          (** Enclosing procedure name *)
    ~(env:env)               (** Environment of variables and labels *)
    ~(clabel:string Sette.t) (** Set of defined control labels *)
    ~(index:int)             (** Depth of current local scope *)
    (instruction:string Syntax.instruction)
    :
    Syntax.var Syntax.instruction
    =
  match instruction with
  | YIELD -> YIELD
  | SKIP -> SKIP
  | HALT -> HALT
  | FAIL -> FAIL
  | FLUSH -> FLUSH
  | GOTO(Lab(s,tab,b)) ->
      assert(tab=[||] && b);
      if Sette.mem s clabel then GOTO(Lab(s,tab,b))
      else error "unknown label %s in goto instruction" s s
  | GOTO _ -> failwith ""
  | ASSUME e -> ASSUME(expr ~env e)
  | ASSIGN(lvar,loexpr) ->
      let lvar = List.map (rename_var ~env) lvar in
      let loexpr = List.map (oexpr ~env) loexpr in
      ASSIGN(lvar,loexpr)
  | IF(oe,b,ob) ->
      let oe = oexpr ~env oe in
      let b = block ~typproc ~pname ~env ~clabel ~index b in
      let ob = Syntax.omap (block ~typproc ~pname ~env ~clabel ~index) ob in
      IF(oe,b,ob)
  | LOOP(oe,b) ->
      let oe = oexpr ~env oe in
      let b = block ~typproc ~pname ~env ~clabel ~index b in
      LOOP(oe,b)
  | ATOMIC(b) ->
      let b = block ~typproc ~pname ~env ~clabel ~index b in
      ATOMIC(b)
  | LOCAL(s,decls,b) ->
      if false then
	printf "env=%a@."
	  (Mappe.print pp_print_string (Syntax.print_declaration Syntax.print_var))
	  env
      ;
      if false then
	printf "decls=%a@."
	  (Syntax.print_declarations ~var:false pp_print_string)
	  decls
      ;
      let (ndecls,nenv) =
	List.fold_left
	  (begin fun (decls,env) (var,typ) ->
	    let tab = if Mappe.mem var env then [|index+1|] else [||] in
	    let nvar = `Local("",var,(`Local tab)) in
	    let ntyp = maptyp typ in
	    let ndecl = (nvar,ntyp) in
	    (ndecl::decls, Mappe.add var ndecl env)
	  end)
	  ([],env) decls
      in
      if false then
	printf "nenv=%a@."
	  (Mappe.print pp_print_string (Syntax.print_declaration Syntax.print_var))
	  nenv
      ;
      let ndecls = ((List.rev ndecls):>Syntax.var Syntax.declaration list) in
      let b = block ~typproc ~pname ~env:(nenv) ~clabel ~index:(index+1) b in
      LOCAL(s,ndecls,b)
  | CALL(lout,pname,lin) ->
      if not (Mappe.mem pname typproc) then
	error "call to an unknown procedure %s"
	  pname
      ;
      let (typin,typout) = Mappe.find pname typproc in
      let nb_lin = List.length lin in
      let nb_lout = List.length lout in
      let nb_fpi = List.length typin in
      let nb_fpo = List.length typout in
      if nb_lin<>nb_fpi then
	error "wrong number of input parameters in call of procedure %s: %i instead of expected %i"
	  pname nb_lin nb_fpi
      ;
      if nb_lout<>nb_fpo then
	error "wrong number of output parameters in call of procedure %s: %i instead of expected %i"
	  pname nb_lout nb_fpo
      ;
      List.iter2
	(begin fun pin tin ->
	  let typ = typ_of_var ~env pin in
	  if typ<>tin then
	    error "wrong type of parameter %s in call of procedure %s"
	      pin pname
	  ;
	end)
	lin typin
      ;
      List.iter2
	(begin fun pout tout ->
	  let typ = typ_of_var ~env pout in
	  if typ<>tout then
	    error "wrong type of parameter %s in call of procedure %s"
	      pout pname
	  ;
	end)
	lout typout
      ;
      let lout = List.map (rename_var ~env) lout in
      let lin = List.map (rename_var ~env) lin in
      begin try
	check_uniqueness Syntax.print_var lout;
	check_uniqueness Syntax.print_var lin;
      with Error s ->
	error "variable %s appearing twice either in input or output parameters in call of procedure call %s" s pname
      end;
      if List.exists (function `Global _ -> true | _ -> false) (lout@lin) then
	error "global variable appearing in input or output parameters in call of %s" pname
      ;
      CALL(lout,pname,lin)

and instr ~typproc ~pname ~env ~clabel ~index i =
  try
    {
      instruction = instruction ~typproc ~pname ~env ~clabel ~index i.instruction;
      ipoint = rename_point ~pname i.ipoint
    }
  with Error s ->
    error "just before point %a: %s"
      print_point i.ipoint s

and block ~typproc ~pname ~env ~clabel ~index b =
  {
    bpoint = rename_point ~pname b.bpoint;
    instrs = List.map (instr ~typproc ~pname ~env ~clabel ~index) b.instrs;
  }

(*  ********************************************************************** *)
(** {2 Computing the set of control labels and checking uniqueness} *)
(*  ********************************************************************** *)

let controllabels prog =
  let (all,list) =
    List.fold_left
      (begin fun (all,list) (pname,proc) ->
	let local = ref Sette.empty in
	Syntax.iter_block
	  (begin fun _ point instr ->
	    begin match point with
	    | Lab(l,tab,_) -> assert(tab=[||]); local := Sette.add l !local
	    | _ -> ()
	    end;
	    begin match instr.ipoint with
	    | Lab(l,tab,_) -> assert(tab=[||]); local := Sette.add l !local
	    | _ -> ()
	  end;
	end)
	[]
	proc.pcode
	;
	let inter = Sette.inter all !local in
	if not (Sette.is_empty inter) then
	  error "In procedure %s: control label %s already defined"
	    pname (Sette.choose inter)
	;
	((Sette.union all !local),
	(!local::list))
      end)
      (Sette.empty, []) prog.procedures
  in
  (all, List.rev list)

(*  ********************************************************************** *)
(** {2 Rewriting procedures} *)
(*  ********************************************************************** *)

let procedure ~(typproc:typproc) ~(env:env) ~(clabel:string Sette.t) p =
  try
    begin try
      check_uniqueness pp_print_string (List.map fst p.pinput);
      check_uniqueness pp_print_string (List.map fst p.poutput);
      check_disjointness p.pinput p.poutput
    with Error s ->
      error "two formal input or output parameter with same name %s" s
    end;
    let env = ref env in
    let pinput =
      List.map
	(begin fun (var,typ) ->
	  let nvar = `Local("",var,`Input) in
	  let ntyp = maptyp typ in
	  env := Mappe.add var (nvar,ntyp) !env;
	  (nvar, maptyp typ)
	end)
	p.pinput
    in
    let poutput =
      List.map
	(begin fun (var,typ) ->
	  let nvar = `Local("",var,`Output) in
	  let ntyp = maptyp typ in
	  env := Mappe.add var (nvar,ntyp) !env;
	  (nvar, maptyp typ)
	end)
	p.poutput
    in
    let p = {
      pname = p.pname;
      pinput = (pinput :> Syntax.var Syntax.declaration list);
      poutput = (poutput :> Syntax.var Syntax.declaration list);
      pcode = block ~typproc ~pname:p.pname ~env:(!env) ~clabel ~index:0 p.pcode;
    }
    in
    p

  with Error s ->
    error "In procedure %s: %s" p.pname s

let program
    (prog : string Syntax.program)
    :
    Syntax.var Syntax.program
    =
  (** Checking uniqueness of named types *)
  begin try
    check_uniqueness
      pp_print_string (fst (List.split prog.Syntax.typenumdef))
  with Error s ->
    error "Error: two types named %s" s
  end;

  (** Checking uniqueness of enumerated labels and building the environment *)
  let env = ref (Mappe.empty:env) in
  let typenumdef =
    List.map
      (begin fun (typ,tlabels) ->
	let ntyp = `String typ in
	let nntyp = `Benum ntyp in
	let ntlabels =
	  Array.map
	    (begin fun label ->
	      let (nlabel:Syntax.var) = `String label in
	      if Mappe.mem label !env then
		error "2 labels (of enumerated types) with same name %s" label
	      ;
	      env := Mappe.add label (nlabel,nntyp) !env;
	      nlabel
	    end)
	    tlabels
	in
	(ntyp,ntlabels)
      end)
      prog.Syntax.typenumdef
  in

  (** Checking uniqueness of global variables and adding them to the environment *)
  begin try
    check_uniqueness pp_print_string (List.map fst prog.Syntax.global)
  with Error s ->
    error "two global variables with same name %s" s
  end;
  let global =
    List.fold_left
      (begin fun global (var,typ) ->
	let nvar = `Global(var,`Current) in
	let ntyp = maptyp typ in
	env := Mappe.add var (nvar,ntyp) !env;
	(nvar,ntyp)::global
      end)
      []
      prog.Syntax.global
  in
  let global = List.rev global in

  (* Types of procedures *)
  let typproc =
    List.fold_left
      (begin fun res (pname,p) ->
	if Mappe.mem pname res then
	  error "threads or procedures with same name %s" pname
	;
	let ptyp = (
	  (List.map (fun vartyp -> maptyp (snd vartyp)) p.pinput),
	  (List.map (fun vartyp -> maptyp (snd vartyp)) p.poutput)
	)
	in
	Mappe.add pname ptyp res
      end)
      Mappe.empty
      prog.procedures
  in

  (* Checking uniqueness of control labels and set of labels per procedure *)
  let (clabel, lclabel) = controllabels prog in

  (** Adding an enumerated type for program counter(s) and program counter(s) *)
  let typenumdef = begin
    let typname = `Pc "" in
    let typ = `Benum typname in
    let llabels =
      Sette.fold
	(begin fun label llabels ->
	  let (nlabel:Syntax.var) = `Pc label in
	  env := Mappe.add (label^"$") (nlabel,typ) !env;
	  nlabel::llabels
	end)
	clabel []
    in
    let tlabels = Array.of_list llabels in
    (typname,tlabels)::typenumdef
  end
  in
  let global = 
    List.fold_left
      (begin fun global thread ->
	let pc = `Pc thread in
	let typname = `Pc "" in
	let typ = `Benum typname in	
	env := Mappe.add (thread^"@") (pc,typ) !env;
	(pc,typ)::global
      end)
      global prog.threads
  in

  let synprog = {
    typenumdef = typenumdef;
    Syntax.global = global;
    initial = oexpr ~env:(!env) prog.initial;
    final = oexpr ~env:(!env) prog.final;
    procedures =
      List.map2
	(fun (pname,p) clabel ->
	  (pname, procedure ~typproc ~env:(!env) ~clabel p))
	prog.procedures lclabel;
    threads = prog.threads
  }
  in
  synprog

(*  ********************************************************************** *)
(** {2 Main function} *)
(*  ********************************************************************** *)

let parse_and_check
    (fmt:Format.formatter)
    (lexbuf:Lexing.lexbuf)
    :
    Syntax.var Syntax.program
    =
  let prog = parse_lexbuf fmt lexbuf in
  let res =
    begin try
      program prog
    with Syntax.Error s ->
      Format.fprintf fmt "%s@." s;
      raise Exit
    end
  in
  res
