(** Control Flow Graphs *)

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

val dummy : syntax_t

val compare : (Syntax.point,int) PSHGraph.compare

val unique : unit -> int
val make : unit -> ('a,'b) t
val copy : ('a,'b) t -> ('a,'b) t

val print_binstr :
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter -> ('a, 'b) binstr -> unit
val print_binstr_syntax :
  Format.formatter ->
  (Syntax.var Bddapron.Syntax.expr, Syntax.var Bddapron.Syntax.expr list)
  binstr -> unit
val print_binstr_bddapron :
  Format.formatter ->
  ('a Bddapron.Expr2.Bool.t, 'b Bddapron.Expr2.List.t) binstr -> unit
val print_call : Format.formatter -> call -> unit
val print_callinstr : Format.formatter -> callinstr -> unit
val print_instr :
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter -> ('a, 'b) instr -> unit
val print_instr_syntax :
  Format.formatter ->
  (Syntax.var Bddapron.Syntax.expr, Syntax.var Bddapron.Syntax.expr list)
  instr -> unit
val print_instr_bddapron :
  Format.formatter ->
  ('a Bddapron.Expr2.Bool.t, 'b Bddapron.Expr2.List.t) instr -> unit
val print_attr : Format.formatter -> attr -> unit
val print_pointattr :
  (Syntax.point, 'a, attr, 'b, 'c) PSHGraph.t ->
  Format.formatter -> Syntax.point -> unit
val print_info : Format.formatter -> info -> unit

val print :
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) -> Format.formatter -> ('a, 'b) t -> unit
val print_syntax :
  Format.formatter ->
  (Syntax.var Bddapron.Syntax.expr, Syntax.var Bddapron.Syntax.expr list) t ->
  unit
val print_bddapron :
  Format.formatter ->
  ('a Bddapron.Expr2.Bool.t, 'a Bddapron.Expr2.List.t) t -> unit
val print_dot :
  title:string ->
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) -> Format.formatter -> ('a, 'b) t -> unit
val print_dot_syntax :
  title:string ->
  Format.formatter ->
  (Syntax.var Bddapron.Syntax.expr, Syntax.var Bddapron.Syntax.expr list) t ->
  unit
val print_dot_bddapron :
  title:string ->
  Format.formatter ->
  ('a Bddapron.Expr2.Bool.t, 'a Bddapron.Expr2.List.t) t -> unit
