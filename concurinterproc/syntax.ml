(** Spl language Abstract Syntax tree *)

(* This file is part of the Interproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Mathias Argoud, Ga�l Lalire, Bertrand Jeannet 2007.
*)

open Format

(*  ********************************************************************* *)
(** {2 Control point in the program} *)
(*  ********************************************************************* *)

type pos = {
  id   : int;    (** Unique (positive) identifier *)
  file : string; (** Name of the file (maybe [""]) *)
  fid  : string; (** Name of the function *)
  char : int;
  line : int;
  col : int;
}

let pos_dummy = {
  id = 0;
  file = "";
  fid = "";
  char = -1;
  line = -1;
  col = -1;
}
let pos_compare p1 p2 = p1.id - p2.id
let pos_hash p = p.id

type point =
  | Pos of pos * int array
  | Lab of string * int array * bool
      (* If Boolean is true, user label to be kept when compressing CFG *)
  | Push of point

let point_dummy = Pos(pos_dummy,[||])
let rec point_compare p1 p2 = match (p1,p2) with
  | (Pos(p1,a1),Pos(p2,a2)) ->
      let res = pos_compare p1 p2 in
      if res<>0 then res else Pervasives.compare a1 a2
  | (Pos _, _) -> -1
  | (Lab _, Pos _) -> 1
  | (Lab _, Lab _) -> Pervasives.compare p1 p2
  | (Lab _, Push _) -> -1
  | (Push(p1), Push (p2)) -> point_compare p1 p2
  | (Push _, _) -> 1
let rec point_equal p1 p2 = match (p1,p2) with
  | (Pos(p1,a1),Pos(p2,a2)) ->
      (pos_compare p1 p2==0) && a1=a2
  | (Lab(l1,a1,_), Lab (l2,a2,_)) -> l1=l2 && a1=a2
  | (Push(p1), Push (p2)) -> point_equal p1 p2
  | _ -> false
let point_hash p =
  let rec hash = function
    | Pos(p,a) -> 3*(p.id) + Hashtbl.hash a
    | Lab(l,a,_) -> 7*(Hashtbl.hash l) + Hashtbl.hash a
    | Push(p) -> 5 + hash p
  in
  abs(hash p)

let point_hashhe_compare = {
  PHashhe.hash = point_hash;
  PHashhe.equal = point_equal;
}

(*  ********************************************************************* *)
(** {2 Variables and types} *)
(*  ********************************************************************* *)

(** Variable *)
type var0 = string

(** Variable and type declaration.  ['a] is the generic type of
    symbols.  It will be instanciated first with [var0] after
    parsing, then with [var] defined below.  *)
type 'a typ = 'a Bddapron.Env.typ
type 'a typdef = 'a Bddapron.Env.typdef
type 'a declaration = 'a * 'a typ

(*  ********************************************************************* *)
(** {2 Instructions and programs} *)
(*  ********************************************************************* *)

(** Instruction *)
type 'a instruction =
  | YIELD
    (** Pass the control to other threads (in cooperative scheduling) *)
  | SKIP
    (** Do nothing *)
  | HALT
    (** Halt the execution *)
  | FAIL
    (** Halt the execution, but also indicate a final point for backward
	analysis *)
  | FLUSH
  	(** Flush all the remote pending operations triggered by this process *)
  | ASSUME of 'a Bddapron.Syntax.expr
    (** Semantically equivalent to [if expr then skip; else halt;] *)
  | ASSIGN of ('a list) * ('a Bddapron.Syntax.expr option list)
    (** Assignement of a (numerical) variable *)
  | IF     of 'a Bddapron.Syntax.expr option * 'a block * 'a block option
    (** If-then(-else) instruction *)
  | LOOP   of 'a Bddapron.Syntax.expr option * 'a block
    (** While instruction *)
  | GOTO   of point
    (** Goto instruction *)
  | CALL   of 'a list * string * 'a list
    (** Procedure call [(x,y) = f(a,b)] *)
  | LOCAL  of bool * 'a declaration list * 'a block
    (** Variable declaration in local scope.
	False indicates beginning of a procedure/thread (different syntax) *)
  | ATOMIC of 'a block
    (** Atomic section *)

(** Labelled instruction *)
and 'a instr = {
  instruction: 'a instruction; (** instruction *)
  ipoint : point;              (** label *)
}

(** Sequence of instructions *)
and 'a block = {
  bpoint : point;         (** label preceding the first
			      instruction of the sequence *)
  instrs : 'a instr list; (** Labelled instruction list *)
}

(** Procedure declaration *)
type 'a procedure = {
  pname : string;
  (** Procedure name *)
  pinput : 'a declaration list;
  (** List of input paramaeters *)
  poutput : 'a declaration list;
  (** List of output parameters *)
  pcode : 'a block;
  (** Code of the procedure *)
}

(** Program *)
type 'a program = {
  typenumdef : ('a  * 'a array) list;
  (** Enumerated types definition. A definition is a pair
      [(name, list of labels)] *)
  global : 'a declaration list;
  (** Global variable declarations *)
  initial : 'a Bddapron.Syntax.expr option;
  (** Initial condition on global variables *)
  final : 'a Bddapron.Syntax.expr option;
  (** Final condition on program counters and global variables *)
  procedures : (string * 'a procedure) list;
  (** List of the procedures/threads of the program. *)
  threads : string list;
  (** List of threads of the program. *)
}

(*  ********************************************************************* *)
(** {2 Exceptions} *)
(*  ********************************************************************* *)

let start_of_comment = ref Lexing.dummy_pos

exception Error of string
exception ErrorLoc of point * point * string
exception Unterminated_comment of Lexing.position

let error format =
  let buffer = Buffer.create 128 in
  let fmt = Format.formatter_of_buffer buffer in
  Format.kfprintf
    (begin fun fmt ->
      Format.pp_print_flush fmt ();
      let s = Buffer.contents buffer in
      Buffer.clear buffer;
      raise (Error s)
    end)
    fmt
    format

let errorloc point1 point2 s = raise (ErrorLoc(point1,point2,s))

(*  ********************************************************************* *)
(** {2 Variables again} *)
(*  ********************************************************************* *)

type gkind = [
  | `Current
  | `Input of string
  | `Output of string
  | `Tmp
]
type lkind = [
  | `Input
  | `InputFrozen
  | `Output
  | `OutputCopy
  | `Tmp
  | `Local of int array
]
type symbol = [`String of string]
type global = [`Global of string * gkind]
type globalcurrent = [`Global of string * [`Current]]
type local = [`Local of string * string * lkind]
type var2 = [ global | local ]
type var = [
  | `String of string
  | `Pc of string
  | var2
]

let mylength str pos char =
  try
    let pos2 = String.index_from str pos char in
    pos2 - pos
  with Not_found ->
    (String.length str) - pos

let mylength_sharp str pos = mylength str pos '#'
let mylength_underscore str pos = mylength str pos '_'

let string_of_gkind = function
  | `Current -> "C"
  | `Input s -> "I#"^s
  | `Output s -> "O#"^s
  | `Tmp -> "T"
let gkind_of_string str pos =
  let l = mylength_sharp str pos in
  assert(l=1);
  match String.get str pos with
  | 'C' -> `Current
  | 'T' -> `Tmp
  | 'I' ->
      let l = mylength_sharp str (pos+2) in
      `Input(String.sub str (pos+2) l)
  | 'O' ->
      let l = mylength_sharp str (pos+2) in
      `Output(String.sub str (pos+2) l)
  | _-> failwith ""

let print_tab fmt tab =
  if tab<>[||] then
    fprintf fmt "%a" (Print.array ~first:"_" ~sep:"_" ~last:"" pp_print_int) tab
let string_of_tab =
  Print.string_of_print print_tab
let tab_of_string str =
  let length = String.length str in
  let pos = ref 0 in
  let list = ref [] in
  while !pos < length do
    let l = mylength_underscore str !pos in
    let s = String.sub str !pos l in
    list := (int_of_string s)  :: !list;
    pos := !pos + l + 1;
  done;
  Array.of_list (List.rev !list)

let string_of_lkind = function
  | `Input -> "I"
  | `InputFrozen -> "IF"
  | `Output -> "O"
  | `OutputCopy -> "OC"
  | `Tmp -> "T"
  | `Local tab -> "L"^(string_of_tab tab)

let lkind_of_string = function
  | "I" -> `Input
  | "IF" -> `InputFrozen
  | "O" -> `Output
  | "OC" -> `OutputCopy
  | "T" -> `Tmp
  | "L" -> `Local [||]
  | _ as s when s<>"" && (String.get s 0) = 'L' ->
      let tab = tab_of_string (String.sub s 2 ((String.length s) - 2)) in
      `Local tab
  | _ -> failwith ""

let print_var fmt var =
  let var = (var:>var) in
  match var with
  | `String var ->
      pp_print_string fmt var
  | `Pc thread ->
      fprintf fmt "%s@@" thread
  | `Global(var,gkind) ->
      pp_print_string fmt var;
      begin match gkind with
      | `Input thread ->
	  fprintf fmt "#I%s"
	    (if thread="" then "" else "@"^thread)
      | `Output thread ->
	  fprintf fmt "#O%s"
	    (if thread="" then "" else "@"^thread)
      | `Tmp -> pp_print_string fmt "#T"
      | `Current -> ()
      end
  | `Local(thread,var,lkind) ->
      if thread<>"" then fprintf fmt "%s@@" thread;
      pp_print_string fmt var;
      pp_print_string fmt
	begin match lkind with
	| `InputFrozen -> "#I"
	| `OutputCopy -> "#O"
	| `Tmp -> "#T"
	| `Local tab -> string_of_tab tab
	| _ -> ""
	end;
      ()

let unmarshal_var str : var =
  let l = mylength_sharp str 0 in
  assert(l=1);
  let var = match String.get str 0 with
    | 'S' -> `String (String.sub str 2 (mylength_sharp str 2))
    | 'P' -> `Pc (String.sub str 2 (mylength_sharp str 2))
    | 'G' ->
	let l = mylength_sharp str 2 in
	`Global((String.sub str 2 l),gkind_of_string str (l+3))
    | 'L' ->
	let pos1 = 2 in
	let l1 = mylength_sharp str pos1 in
	let thread = String.sub str pos1 l1 in
	let pos2 = pos1+l1+1 in
	let l2 = mylength_sharp str pos2 in
	let var = String.sub str pos2 l2 in
	let pos3 = pos2+l2+1 in
	let lkind = lkind_of_string (String.sub str pos3 ((String.length str) - pos3)) in
	`Local(thread,var,lkind)
    | _ -> failwith ""
  in
  var

let marshal_var (var:[< var]) =
  let str = match var with
    | `String var -> "S#"^var
    | `Pc thread -> "P#"^thread
    | `Global(var,gkind) ->
	sprintf "G#%s#%s" var (string_of_gkind gkind)
    | `Local(thread,var,lkind) ->
	sprintf "L#%s#%s#%s"
	  thread var (string_of_lkind lkind)
  in
(*
  assert(
    if true then begin
      if (var:>var) <> (unmarshal_var str) then
	(printf "@.var=%a@.str=%s@.var2=%a@." print_var var str print_var (unmarshal_var str); false)
      else
	true
    end
    else
      true
  );
*)
  str

let compare_var (s1:[< var] as 'a) (s2:'a) =
  match (s1,s2) with
  | ((#var2 as v1), (#var2 as v2)) ->
      begin match (v1,v2) with
      | (`Global p1, `Global p2) -> Pervasives.compare p1 p2
      | (`Local p1, `Local p2) -> Pervasives.compare p1 p2
      | (`Global _, `Local _) -> -1
      | (`Local _, `Global _) -> 1
      end
  | (_, #var2) -> -1
  | (#var2, _) -> 1
  | (_,_) -> Pervasives.compare s1 s2


(*  ********************************************************************* *)
(** {2 Utility functions} *)
(*  ********************************************************************* *)
let etrue = `Cst(`Bool true)
let efalse = `Cst(`Bool false)
let is_true = function
  | `Cst(`Bool true)
  | `Unop(`Not,`Cst(`Bool false)) -> true
  | _ -> false
let is_false = function
  | `Cst(`Bool false)
  | `Unop(`Not,`Cst(`Bool true)) -> true
  | _ -> false
let eeq var1 var2 = `Binop((`Bool `EQ),`Ref(var1),`Ref(var2))
let eand e1 e2 =
  if is_true e1 then e2
  else if is_true e2 then e1
  else if is_false e1 || is_false e2 then efalse
  else `Binop((`Bool `And),e1,e2)
let enot = function
  | `Unop(`Not,e) -> e
  | _ as e -> `Unop(`Not,e)

let rec support ?filter res expr =
  match expr with
  | `Cst _ -> res
  | `Ref var ->
      let b = match filter with
	| Some f -> f var
	| None -> true
      in
      if b
      then Sette.add var res
      else res
  | `Unop(_,e) -> support ?filter res e
  | `Binop(_,e1,e2) -> support ?filter (support ?filter res e1) e2
  | `If(e1,e2,e3) -> support ?filter (support ?filter (support ?filter res e1) e2) e3
  | `In(e,le) ->
      List.fold_left
	(support ?filter)
	(support ?filter res e) le

let rec substitute f (e:'a Bddapron.Syntax.expr) : 'b Bddapron.Syntax.expr
    =
  match e with
  | `Cst x -> `Cst x
  | `Ref var -> `Ref (f var)
  | `Unop(op,e) -> `Unop(op, substitute f e)
  | `Binop(op,e1,e2) -> `Binop(op, (substitute f e1), (substitute f e2))
  | `If(e1,e2,e3) -> `If((substitute f e1),(substitute f e2),(substitute f e3))
  | `In(e,le) -> `In((substitute f e),List.map (substitute f) le)

(** Last element of a list *)
let rec last_of_list = function
  | [] -> failwith ""
  | [x] -> x
  | x::l -> last_of_list l

(** Exit point of a block *)
let exit_of_block block =
  if block.instrs=[] then
    block.bpoint
  else begin
    let instr = last_of_list block.instrs in
    instr.ipoint
  end

(** Optional arguments *)
let omap f = function
  | None -> None
  | Some x -> Some(f x)
let oiter f = function
  | None -> ()
  | Some x -> f x
let oget = function
  | None -> failwith "Syntax.oget"
  | Some x -> x

(** Iterating on instructions *)

let rec iter_block f decl block
    =
  ignore begin
    List.fold_left
      (begin fun point instr ->
	f decl point instr;
	begin match instr.instruction with
	| IF(expr,block1,oblock2) ->
	    iter_block f decl block1;
	    oiter (iter_block f decl) oblock2
	| LOOP(_,block)
	| ATOMIC(block) ->
	    iter_block f decl block
	| LOCAL(_,ndecl,block) ->
	    iter_block f (ndecl@decl) block
	| YIELD
	| SKIP
	| HALT
	| FAIL
 	| FLUSH
	| ASSUME _
	| ASSIGN _
	| GOTO _
	| CALL _ ->
	    ()
	end;
	instr.ipoint
      end)
      block.bpoint
      block.instrs
  end
  ;
  ()

let typ_substitute f (x:'a Bddapron.Env.typ) : 'b Bddapron.Env.typ =
  match x with
  | `Benum s -> `Benum(f s)
  | `Bool as x -> x
  | `Bint(_) as x -> x
  | `Int as x -> x
  | `Real as x -> x

let oexpr_substitute f = function
  | None -> None
  | Some e -> Some (substitute f e)

let point_substitute g = function
  | Pos(pos,tab) -> Pos(pos,g tab)
  | Lab(name,tab,b) -> Lab(name,g tab,b)
  | _ -> failwith ""

let rec instruction_substitute ~env ~name ~point ~scope = function
  | YIELD -> YIELD
  | SKIP -> SKIP
  | HALT -> HALT
  | FAIL -> FAIL
  | FLUSH -> FLUSH
  | GOTO(Pos(pos,tab)) -> GOTO(Pos(pos,(point tab)))
  | GOTO(Lab(s,tab,b)) -> GOTO(Lab(s,(point tab),b))
  | GOTO(Push _) -> failwith ""
  | ASSUME e -> ASSUME(substitute (name env) e)
  | ASSIGN(lvar,loexpr) ->
      let lvar = List.map (name env) lvar in
      let loexpr = List.map (oexpr_substitute (name env)) loexpr in
      ASSIGN(lvar,loexpr)
  | IF(oe,b,ob) ->
      let oe = oexpr_substitute (name env) oe in
      let b = block_substitute ~env ~name ~point ~scope b in
      let ob = omap (block_substitute ~env ~name ~point ~scope) ob in
      IF(oe,b,ob)
  | LOOP(oe,b) ->
      let oe = oexpr_substitute (name env) oe in
      let b = block_substitute ~env ~name ~point ~scope b in
      LOOP(oe,b)
  | CALL(lout,pname,lin) ->
      let lout = List.map (name env) lout in
      let lin = List.map (name env) lin in
      CALL(lout,pname,lin)
  | ATOMIC(b) ->
      ATOMIC(block_substitute ~env ~name ~point ~scope b)
  | LOCAL(b,decl,block) ->
      let nenv = scope env decl in
      LOCAL(
	b,
	(List.map (fun (v,t) -> (name nenv v, typ_substitute (name nenv) t)) decl),
	(block_substitute ~env:nenv ~name ~point ~scope block)
      )

and instr_substitute ~env ~name ~point ~scope i =
    {
      instruction = instruction_substitute ~env ~name ~point ~scope i.instruction;
      ipoint = point_substitute point i.ipoint
    }
and block_substitute ~env ~name ~point ~scope b =
  {
    bpoint = point_substitute point b.bpoint;
    instrs = List.map (instr_substitute ~env ~name ~point ~scope) b.instrs
  }

(*  ********************************************************************* *)
(** {2 Printing functions} *)
(*  ********************************************************************* *)

let print_typdef
    print_var (fmt:Format.formatter) (typdef:'a typdef) =
  match typdef with
  | `Benum array ->
      fprintf fmt "enum { %a }"
	(Print.array ~first:"@[" ~sep:",@ " ~last:"@]" print_var)
	array
  (*| `Bint(sign,size) -> fprintf fmt "%cint[%i]" (if sign then 's' else 'u') size*)
let string_of_typdef print_var = Print.string_of_print (print_typdef print_var)

let print_typ
    print_var
    (fmt:Format.formatter)
    (typ:[<'a typ])
    :
    unit
    =
  match typ with
  | `Bool -> pp_print_string fmt "bool"
  | `Bint(sign,size) -> fprintf fmt "%cint[%i]" (if sign then 's' else 'u') size
  | `Benum s -> print_var fmt s
  | `Int -> pp_print_string fmt "int"
  | `Real -> pp_print_string fmt "real"

let string_of_typ print_var = Print.string_of_print (print_typ print_var)

let print_typenumdef print_var fmt ltypdef =
  if ltypdef<>[] then begin
    fprintf fmt "typedef %a@ "
      (Print.list ~first:"@[<v>" ~sep:"@ " ~last:"@]"
	(fun fmt (typ,tlabel) ->
	  fprintf fmt "%a = enum {%a};"
	    print_var typ
	    (Print.array ~first:"@[<hov>" ~sep:",@ " ~last:"@]"
	      print_var)
	    tlabel
	)
      )
      ltypdef
  end

let print_oexpr
  (print_var: Format.formatter -> 'a -> unit)
  fmt
  =
  function
  | None -> pp_print_string fmt "random"
  | Some e -> Bddapron.Syntax.print_expr print_var fmt e

let print_oinitial
    (print_var: Format.formatter -> 'a -> unit)
    fmt oinitial =
  match oinitial with
  | Some expr ->
      fprintf fmt "initial %a;@ @ "
      (Bddapron.Syntax.print_expr print_var) expr
  | None -> ()

let print_ofinal
    (print_var: Format.formatter -> 'a -> unit)
    fmt oinitial =
  match oinitial with
  | Some expr ->
      fprintf fmt "final %a;@ @ "
      (Bddapron.Syntax.print_expr print_var) expr
  | None -> ()

let print_pos fmt point
  =
  fprintf fmt "L%iC%i"
    point.line point.col

let rec print_point fmt point
  =
  let tab = match point with
    | Pos(pos,tab) -> print_pos fmt pos; tab
    | Lab(lab,tab,_) -> pp_print_string fmt lab; tab
    | Push(p) -> fprintf fmt "p%a" print_point p; [||]
  in
  if tab<>[||] then
    fprintf fmt "_%a" print_tab tab
  ;
  ()

let print_poutput print_var fmt (l:'a list) =
  match l with
  | [x] -> print_var fmt x
  | _ ->
      Print.list
      ~first:"(@[<h>" ~sep:", " ~last:"@])"
      print_var fmt l

let print_pinput print_var fmt (l:'a list) =
  Print.list
    ~first:"@[<h>" ~sep:", " ~last:"@]"
    print_var fmt l

let print_declaration print_var fmt (var,typ) =
  fprintf fmt "%a : %a"
    print_var var
    (print_typ print_var) typ

let print_declarations ?(global=false) ~var print_var fmt list =
  if list<>[] then begin
    fprintf fmt "%s%a"
      (if var then "var " else "")
      (Print.list ~first:"@[<hov>" ~sep:",@ " ~last:"@]"
	(print_declaration print_var))
      list
    ;
    if global then pp_print_char fmt ';'
  end

let rec print_instruction print_var print_comment fmt instruction =
  begin match instruction with
  | YIELD ->
      pp_print_string fmt "yield;"
  | SKIP ->
      pp_print_string fmt "skip;"
  | HALT ->
      pp_print_string fmt "halt;"
  | FAIL ->
      pp_print_string fmt "fail;"
  | FLUSH ->
      pp_print_string fmt "flush;"
  | ASSUME(expr) ->
      fprintf fmt "assume %a;"
	(Bddapron.Syntax.print_expr print_var) expr
  | ASSIGN(lvar,lexpr) ->
      begin match lvar,lexpr with
      | [var],[expr] ->
	  fprintf fmt "%a = %a;"
	    print_var var
	    (print_oexpr print_var) expr
      | _ ->
	  fprintf fmt "%a = %a;"
	    (print_poutput print_var) lvar
	    (Print.list ~first:"(@[<hov>" ~sep:",@ " ~last:"@])"
	      (print_oexpr print_var))
	    lexpr
      end
  | IF (expr,block,oblock) ->
      fprintf fmt "if %a then@   %a@ "
	(print_oexpr print_var) expr
	(print_block print_var print_comment) block
      ;
      begin match oblock with
      | None -> fprintf fmt "endif;";
      | Some block ->
	  fprintf fmt "else@   %a@ endif;"
	    (print_block print_var print_comment) block
      end;
  | LOOP(expr,block) ->
      fprintf fmt "while %a do@   %a@ done;"
	(print_oexpr print_var) expr
	(print_block print_var print_comment) block
  | LOCAL(b,decl,block) ->
      if decl<>[] then
	if b then
	  fprintf fmt "%a in "
	    (print_declarations print_var ~var:true) decl
	else
	  fprintf fmt "%a;@ "
	    (print_declarations print_var ~var:true) decl
      ;
      fprintf fmt "begin@   %a@ end;"
	(print_block print_var print_comment) block
  | ATOMIC(block) ->
      fprintf fmt "atomic begin@   %a@ end;"
	(print_block print_var print_comment) block
  | GOTO(point) ->
      fprintf fmt "goto %a;" print_point point
  | CALL(pout, id, pin) ->
      fprintf fmt "%a = %s(%a);"
	(print_poutput print_var) pout
	id
	(print_pinput print_var) pin
  end

and print_instr print_var print_comment fmt instr
    =
  fprintf fmt "%a /* %a */"
    (print_instruction print_var print_comment)
    instr.instruction
    print_comment
    instr.ipoint
  ;
  begin match instr.ipoint with
  | Lab(lab,tab,_) ->
      fprintf fmt "@ %s%a$" lab print_tab tab
  | Pos _ | Push _ -> ()

  end

and print_block print_var print_comment fmt block =
  fprintf fmt "@[<v>/* %a */"
    print_comment block.bpoint
  ;
  begin match block.bpoint with
  | Lab(lab,tab,_) -> fprintf fmt "@ %s%a$" lab print_tab tab
  | Pos _ | Push _ -> ()
  end;
  begin match block.instrs with
  | [] -> ()
  | _ ->
      Print.list ~first:"@ @[<v>" ~sep:"@ " ~last:"@]"
	(print_instr print_var print_comment) fmt block.instrs
  end;
  fprintf fmt "@]"

let print_code print_var print_comment fmt block =
  match block.instrs with
  | [{instruction=LOCAL(false,_,_); ipoint=_}] ->
      fprintf fmt "%a"
	(print_block print_var print_comment) block
  | _ ->
      fprintf fmt "begin@   %a@ end"
	(print_block print_var print_comment) block

let print_procedure print_var print_comment fmt proc =
  if proc.pname<>"" then
    fprintf fmt "proc %s @[<hv>(%a) returns@ (%a)@]@ %a"
      proc.pname
      (print_declarations print_var ~var:false) proc.pinput
      (print_declarations print_var ~var:false) proc.poutput
      (print_code print_var print_comment) proc.pcode

let print_thread print_var print_comment fmt thread =
  if thread.pname<>"" then
    fprintf fmt "thread %s:@ " thread.pname
  ;
  fprintf fmt "%a@ " (print_code print_var print_comment) thread.pcode

let print_program print_var print_comment fmt prog =
  fprintf fmt "%a%a@ %a"
    (print_typenumdef print_var) prog.typenumdef
    (print_declarations ~global:true ~var:true print_var) prog.global
    (print_oinitial print_var) prog.initial
  ;
  Print.list ~first:"@[<v>" ~sep:"@ " ~last:""
    (fun fmt (pname,proc) ->
      let print =
	if List.mem proc.pname prog.threads
	then print_thread
	else print_procedure
      in
      print print_var print_comment fmt proc
    )
    fmt
    prog.procedures
  ;
  print_ofinal print_var fmt prog.final
