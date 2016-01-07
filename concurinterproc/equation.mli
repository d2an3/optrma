(** Representing equation system *)

(* This file is part of the Interproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Mathias Argoud, Gaël Lalire, Bertrand Jeannet 2007.
*)

(*  ********************************************************************* *)
(** {2 Types} *)
(*  ********************************************************************* *)

type vertex =
  | Top of Syntax.point array
  | Tail of int * Syntax.point
type hedge = {
  index : int;
  hedge : int;
  tpoint : Syntax.point array;
}
val vertex_dummy : vertex
val hedge_dummy : hedge

(** Equation system *)
type ('a,'b,'c) static = (vertex,hedge,'a,'b,'c) PSHGraph.t
type dynamic = {
  texitpoints : (Syntax.point array, unit) PHashhe.t array;
  succvertex : (vertex,hedge) Fixpoint.equation;
}
type 'a equation = {
  db : (Cfg.syntax_t, unit) Db.program;
  initial : Syntax.var Bddapron.Expr2.Bool.t;
    (** Initial condition on global variables *)
  final : Syntax.var Bddapron.Expr2.Bool.t;
    (** Final condition on program counters and global variables *)
  tcfgs : Cfg.bddapron_t array;
    (** Cfgs of each thread *)
  vertexenv : (vertex,Syntax.var Bddapron.Env.t) PHashhe.t;
    (** Memoize the association vertices to environments *)
  mutable sinit : vertex PSette.t;
    (** Set of initial vertices *)
  mutable graph : 'a;
    (** either [static] or [dynamic] *)
  mutable threshold : (vertex, Apron.Lincons1.earray) PHashhe.t;
    (** Array of thresholds associated to each vertex *)
}
    (** ['a] is either [('a,'b,'c) static] or [dynamic] *)

(*  ********************************************************************* *)
(** {2 Hashing and comparison Functions} *)
(*  ********************************************************************* *)

val tpoint_compare : Syntax.point array -> Syntax.point array -> int
val tpoint_equal : Syntax.point array -> Syntax.point array -> bool
val tpoint_hash : Syntax.point array -> int
val tpoint_hashhe_compare : Syntax.point array PHashhe.compare

val vertex_compare : vertex -> vertex -> int
val vertex_equal : vertex -> vertex -> bool
val vertex_hash : vertex -> int
val vertex_hashhe_compare : vertex PHashhe.compare

val graph_compare : (vertex,hedge) PSHGraph.compare

(*  ********************************************************************* *)
(** {2 Printing functions} *)
(*  ********************************************************************* *)

val print_tpoint : Format.formatter -> Syntax.point array -> unit
val print_vertex : Format.formatter -> vertex -> unit
val print_hedge : Format.formatter -> hedge -> unit
val print_static : Format.formatter -> ('a, 'b, 'c) static -> unit
val print_dynamic : Format.formatter -> dynamic -> unit
val print_equation : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a equation -> unit
val print_static_equation : Format.formatter -> ('a,'b,'c) static equation -> unit
val print_dynamic_equation : Format.formatter -> dynamic equation -> unit

val print_apron_scalar : Format.formatter -> Apron.Scalar.t -> unit
val print_apron_interval : Format.formatter -> Apron.Interval.t -> unit
val print_apron_box :
  (int -> string) -> Format.formatter -> Apron.Interval.t array -> unit
val print_apron :
  (int -> string) -> Format.formatter -> 'a Apron.Abstract0.t -> unit
val print_bddapron_abstract1 :
  ('a, 'b, 'c, 'd) Bddapron.Domain1.man ->
  Format.formatter -> ('a, 'd) Bddapron.Domain1.t -> unit
val print_ltpointtabs :
  print_abstract:(Format.formatter -> 'a -> unit) ->
  Format.formatter -> (Syntax.point array * 'a) list -> unit

(*  ********************************************************************* *)
(** {2 Various utility functions} *)
(*  ********************************************************************* *)

val env_of_vertex : 'a equation -> vertex -> Syntax.var Bddapron.Env.t
val index_of_threadname : equation:'a equation -> string -> int
val make_fpmanager_common :
  debug:int -> equation:'a equation ->
  (vertex, hedge, unit, bool) Fixpoint.manager

(*  ********************************************************************* *)
(** {2 General combinators} *)
(*  ********************************************************************* *)

val list_combine :
  [< `Global of string * [< Syntax.gkind ]
   | `Local of string * string * [< Syntax.lkind ]
   | `String of string
   | `Pc of string ]
  list ->
  [< `Global of string * [< Syntax.gkind ]
   | `Local of string * string * [< Syntax.lkind ]
   | `String of string
   | `Pc of string ]
  list -> (Syntax.var * Syntax.var) list
val list_combine_add :
  (Syntax.var * Syntax.var) list ->
  [< `Global of string * [< Syntax.gkind ]
   | `Local of string * string * [< Syntax.lkind ]
   | `String of string
   | `Pc of string ]
  list ->
  [< `Global of string * [< Syntax.gkind ]
   | `Local of string * string * [< Syntax.lkind ]
   | `String of string
   | `Pc of string ]
  list -> (Syntax.var * Syntax.var) list

val array_mem : 'a array -> 'a -> bool
val array_forall : ('a -> bool) -> 'a array -> bool
val array_forall2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool
val array_map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
(*
val array_map3 :
  ('a -> 'b -> 'c -> 'd) -> 'a array -> 'b array -> 'c array -> 'd array
val array_mapi3 :
  (int -> 'a -> 'b -> 'c -> 'd) ->
  'a array -> 'b array -> 'c array -> 'd array
*)
val array_mapi2 : (int -> 'a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
(*
val rename_of_array2 : Syntax.var array -> Syntax.var array -> (Syntax.var * Syntax.var) list
*)
