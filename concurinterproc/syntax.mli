(** Spl language Abstract Syntax tree *)

(* This file is part of the ConcurInterproc analyzer, released
   under GPL license.  Please read the COPYING file packaged in
   the distribution.

   Copyright (C) Bertrand Jeannet 2008.
*)

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
type point =
  | Pos of pos * int array
  | Lab of string * int array * bool
      (* If Boolean is true, user label to be kept when compressing CFG *)
  | Push of point

val pos_dummy : pos
val pos_compare : pos -> pos -> int
val pos_hash : pos -> int
val point_dummy : point
val point_compare : point -> point -> int
val point_equal : point -> point -> bool
val point_hash : point -> int
val point_hashhe_compare : point PHashhe.compare

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

val start_of_comment : Lexing.position ref
exception Unterminated_comment of Lexing.position
  (** Raised during lexical analysis *)
exception Error of string
exception ErrorLoc of point * point * string
  (** Lexical or syntaxical analysis *)
val error : ('a, Format.formatter, unit, 'b) format4 -> 'a
val errorloc : point -> point -> string -> 'a
  (** Raises an error *)

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
type var = [
  | `String of string
  | `Pc of string
  | `Global of string * gkind
  | `Local of string * string * lkind
]

val compare_var : ([< var > `Global `Local] as 'a) -> 'a -> int
val marshal_var : [< var] -> string
val unmarshal_var : string -> var

val print_var : Format.formatter -> [< var] -> unit

(*  ********************************************************************* *)
(** {2 Utility functions} *)
(*  ********************************************************************* *)
val etrue : 'a Bddapron.Syntax.expr
val efalse : 'a Bddapron.Syntax.expr
val eeq :  'a -> 'a ->  'a Bddapron.Syntax.expr
val enot :  'a Bddapron.Syntax.expr -> 'a Bddapron.Syntax.expr
val eand : 'a Bddapron.Syntax.expr -> 'a Bddapron.Syntax.expr -> 'a Bddapron.Syntax.expr
val is_true : 'a Bddapron.Syntax.expr -> bool
val is_false : 'a Bddapron.Syntax.expr -> bool
val support : ?filter:('a -> bool) -> 'a Sette.t -> 'a Bddapron.Syntax.expr -> 'a Sette.t

val substitute : ('a -> 'b) -> 'a Bddapron.Syntax.expr -> 'b Bddapron.Syntax.expr

val last_of_list : 'a list -> 'a
val exit_of_block : 'a block -> point

val omap : ('a -> 'b) -> 'a option -> 'b option
val oiter : ('a -> unit) -> 'a option -> unit
val oget : 'a option -> 'a

val iter_block :
  ('a declaration list -> point -> 'a instr -> unit) ->
  'a declaration list -> 'a block -> unit

val point_substitute : (int array -> int array) -> point -> point

val instruction_substitute :
  env:'a ->
  name:('a -> 'b -> 'c) ->
  point:(int array -> int array) ->
  scope:('a -> 'b declaration list -> 'a) ->
  'b instruction -> 'c instruction
val instr_substitute :
  env:'a ->
  name:('a -> 'b -> 'c) ->
  point:(int array -> int array) ->
  scope:('a -> 'b declaration list -> 'a) ->
  'b instr -> 'c instr
val block_substitute :
  env:'a ->
  name:('a -> 'b -> 'c) ->
  point:(int array -> int array) ->
  scope:('a -> 'b declaration list -> 'a) ->
  'b block -> 'c block

(*  ********************************************************************* *)
(** {2 Printing functions} *)
(*  ********************************************************************* *)

val print_typdef :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a typdef -> unit
val print_typ :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> [<'a typ] -> unit
val print_oexpr :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a Bddapron.Syntax.expr option -> unit

(** {3 Simple printing functions} *)

val print_pos : Format.formatter -> pos -> unit
val print_point : Format.formatter -> point -> unit
val print_poutput :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a list -> unit
val print_pinput : (
  Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a list -> unit

(** {3 Other printing functions} *)

(** The following functions takes as first argument a printing function for
  type ['a point]. *)

val print_instruction :
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> point -> unit) ->
  Format.formatter -> 'a instruction -> unit
val print_instr :
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> point -> unit) ->
  Format.formatter -> 'a instr -> unit
val print_block :
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> point -> unit) ->
  Format.formatter -> 'a block -> unit
val print_declaration :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a * 'a typ -> unit
val print_declarations :
  ?global:bool -> var:bool ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> ('a * 'a typ) list -> unit
val print_procedure :
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> point -> unit) ->
  Format.formatter -> 'a procedure -> unit
val print_thread :
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> point -> unit) ->
  Format.formatter -> 'a procedure -> unit
val print_program :
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> point -> unit) ->
  Format.formatter -> 'a program -> unit
