(** Control Flow Graphs *)

open Format
open Syntax

(*  ********************************************************************** *)
(** {2 Types} *)
(*  ********************************************************************** *)

type ('a,'b) binstr =
  | Condition of 'a
  | Assign of (Syntax.var list) * 'b
  | Forget of Syntax.var list
  | Intro of Syntax.var Syntax.declaration list
  | Elim of Syntax.var Syntax.declaration list
type call =
  | Push
  | CallReturn
  | CallStart
  | ExitReturn
type callinstr = {
  callpoint : Syntax.point;
  retpoint : Syntax.point;
  caller : string;
  callee : string;
  cinput : Syntax.var list;
  coutput : Syntax.var list;
  coutputtmp : Syntax.var list;
}
type ('a,'b) instr =
  | Block of ('a,'b) binstr list
  | Call of call * callinstr

type attr = [
  | `Commute
  | `Internal
  | `Fail
  | `Push
]

type info = {
  mutable pstart : Syntax.point;
  mutable pexit : Syntax.point;
  pointenv : (Syntax.point, Syntax.var Bddapron.Env.t) PHashhe.t;
}
type ('a,'b) t = (Syntax.point, int, attr, ('a,'b) instr, info) PSHGraph.t

type bddapron_binstr = (Syntax.var Bddapron.Expr2.Bool.t, Syntax.var Bddapron.Expr2.List.t) binstr
type bddapron_instr = (Syntax.var Bddapron.Expr2.Bool.t, Syntax.var Bddapron.Expr2.List.t) instr
type syntax_t = (Syntax.var Bddapron.Syntax.expr, Syntax.var Bddapron.Syntax.expr list) t
type bddapron_t = (Syntax.var Bddapron.Expr2.Bool.t, Syntax.var Bddapron.Expr2.List.t) t

let compare = {
  PSHGraph.hashv = {
    PHashhe.hash = Syntax.point_hash;
    PHashhe.equal = Syntax.point_equal;
  };
  PSHGraph.hashh = {
    PHashhe.hash = begin fun x -> x end;
    PHashhe.equal = ( = )
  };
  PSHGraph.comparev = Syntax.point_compare;
  PSHGraph.compareh = ( - );
}

let unique_c = ref (-1)
let unique () = incr unique_c; !unique_c

let make () : ('a,'b) t =
  PSHGraph.create compare 5
    {
      pstart = Syntax.point_dummy;
      pexit = Syntax.point_dummy;
      pointenv = PHashhe.create_compare Syntax.point_hashhe_compare 13;
    }

let dummy = make()

let identity x = x
let identity2 x y = y
let copy cfg = PSHGraph.copy identity2 identity2 (fun x -> { x with pstart = x.pstart }) cfg

(*  ********************************************************************** *)
(** {2 Printing} *)
(*  ********************************************************************** *)

let print_lvar fmt lvar =
  Print.list ~first:"(@[" ~sep:",@," ~last:"@])"
    Syntax.print_var fmt lvar
let my_print_lvar fmt = function
  | [var] -> Syntax.print_var fmt var
  | _ as lvar -> print_lvar fmt lvar

let print_bool_syntax fmt (x:Syntax.var Bddapron.Syntax.expr) =
  Bddapron.Syntax.print_expr Syntax.print_var fmt x
let print_list_syntax fmt list =
  (Print.list ~first:"(@[" ~sep:",@ " ~last:"@])"
    print_bool_syntax)
    fmt list

let print_bool_bddapron = Bddapron.Expr2.Bool.print
let print_list_bddapron = Bddapron.Expr2.List.print

let print_binstr print_bool print_list fmt binstr =
  match binstr with
  | Condition(e) ->
      fprintf fmt "IF %a" print_bool e
  | Assign(lvar,lexpr) ->
      fprintf fmt "@[<hv 8>%a =@ %a@]"
	my_print_lvar lvar print_list lexpr
  | Forget lvar ->
      fprintf fmt "forget%a" print_lvar lvar
  | Intro(decl) ->
      fprintf fmt "intro %a" (Syntax.print_declarations ~var:false Syntax.print_var) decl
  | Elim(decl) ->
      fprintf fmt "elim %a"
	(Print.list ~first:"@[" ~sep:",@ " ~last:"@]" Syntax.print_var)
	(List.map fst decl)

let print_binstr_syntax fmt x = print_binstr print_bool_syntax print_list_syntax fmt x

let print_binstr_bddapron fmt x = print_binstr print_bool_bddapron print_list_bddapron fmt x

let print_call fmt call =
  pp_print_string fmt (match call with
  | CallReturn -> ""
  | CallStart -> "CALL "
  | ExitReturn -> "RET "
  | Push -> "PUSH "
  )

let print_callinstr fmt callinstr =
  fprintf fmt "@[<hv 8>%a =@ %s%a@]"
    print_lvar callinstr.coutput
    callinstr.callee
    print_lvar callinstr.cinput

let print_instr print_bool print_list fmt instr =
  match instr with
  | Block list ->
      Print.list ~first:"@[<v>" ~sep:";@ " ~last:"@]"
	(print_binstr print_bool print_list) fmt list
  | Call(call,callinstr) ->
      fprintf fmt "%a%a"
	print_call call
	print_callinstr callinstr

let print_instr_syntax fmt x = print_instr print_bool_syntax print_list_syntax fmt x

let print_instr_bddapron fmt x = print_instr print_bool_bddapron print_list_bddapron fmt x

let print_attr fmt (attr:attr) =
    pp_print_string fmt (match attr with
    | `Internal -> "i"
    | `Commute -> "c"
    | `Fail -> "f"
    | `Push -> "p"
    )

let print_pointattr cfg fmt point =
  fprintf fmt "%a%a"
    Syntax.print_point point
    print_attr (PSHGraph.attrvertex cfg point)

let print_info fmt info =
  fprintf fmt "@[start=%a; exit = %a;@ pointenv = %a@]"
    Syntax.print_point info.pstart
    Syntax.print_point info.pexit
    (PHashhe.print Syntax.print_point Bddapron.Env.print) info.pointenv

let print print_bool print_list fmt (cfg:('a,'b) t) =
  PSHGraph.print
    Syntax.print_point
    pp_print_int
    print_attr
    (print_instr print_bool print_list)
    print_info
    fmt
    cfg

let print_syntax fmt cfg = print print_bool_syntax print_list_syntax fmt cfg
let print_bddapron fmt cfg = print print_bool_bddapron print_list_bddapron fmt cfg

let print_dot ~title print_bool print_list dotfmt (cfg:('a,'b) t) =
  let print_instr fmt instr =
    let str = Print.sprintf "%a@ " (print_instr print_bool print_list) instr in
    let str = Print.escaped ~linebreak:'l' str in
    pp_print_string fmt str
  in
  PSHGraph.print_dot ~title
    ~style:"size=\"7.5,10\";ratio=\"auto\";center=true;ranksep=0.1;nodesep=0.1;"
    ~hedgestyle:"shape=plaintext,fontsize=12,height=0.01,width=0.01"
    ~vertexstyle:"shape=box,fontsize=10,height=0.01,width=0.01"
    Syntax.print_point
    pp_print_int
    (fun fmt point attr -> fprintf fmt "%a%a" Syntax.print_point point print_attr attr)
    (fun fmt hedge instr -> fprintf fmt "%i: %a" hedge print_instr instr)
    dotfmt
    cfg
  ;
  fprintf dotfmt "@.";
  ()

let print_dot_syntax = print_dot print_bool_syntax print_list_syntax
let print_dot_bddapron = print_dot print_bool_bddapron print_list_bddapron
