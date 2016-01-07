(**  Argument, Options and Parsing of command line *)

(* This file is part of the Interproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Mathias Argoud, Gaël Lalire, Bertrand Jeannet 2007.
*)

val inputfilename : string ref
  (** input filename *)
val debug : int ref
  (** debug level *)
val margin : int ref
  (** margin for display *)

(*  ---------------------------------------------------------------------- *)
(** {3 Display style} *)
(*  ---------------------------------------------------------------------- *)

val texttags : Format.formatter_tag_functions
val colortags : Format.formatter_tag_functions
val htmltags : Format.formatter_tag_functions

val displaytags : Format.formatter_tag_functions ref

(*  ---------------------------------------------------------------------- *)
(** {3 Printing options} *)
(*  ---------------------------------------------------------------------- *)

val print_raw : bool ref
val print_thread : string ref
val print_auxvar : bool ref
val print_concurrency : bool ref
val print_box : bool ref

val xml_cout	      : (out_channel option) ref
val xml_formatter     : (Format.formatter option) ref
(*  ---------------------------------------------------------------------- *)
(** {3 Choice of abstract domain} *)
(*  ---------------------------------------------------------------------- *)

type domain =
    Box
  | Octagon
  | PolkaLoose
  | PolkaStrict
  | PolkaEq
  | Taylor1plus
  | PplPolyLoose
  | PplPolyStrict
  | PplGrid
  | PolkaGrid
val assocnamedomain : (string * domain) list
val domain : domain ref

type bddapron = Bdd | Mtbdd
val assocnamebddapron : (string * bddapron) list
val bddapron : bddapron ref

(*  ---------------------------------------------------------------------- *)
(** {3 Choice of analysis type} *)
(*  ---------------------------------------------------------------------- *)

val analysis : [`Forward | `Backward] list ref

val analysis_method : [`Solving1 | `Solving2 ] ref

val analysis_dynamic : bool ref

val analysis_threshold : bool ref

val assocnamesched : (string * [> `Cooperative | `Preemptive ]) list
val scheduling : [`Preemptive | `Cooperative] ref

val instrumCompl : bool ref

(*  ---------------------------------------------------------------------- *)
(** {3 Fixpoint iteration} *)
(*  ---------------------------------------------------------------------- *)

val iteration_depth : int ref
  (** Depth of recursion in iteration. If the depth is deeper, one tries to
      stabilize inner loops first before propagating to enclosing loops.*)

val iteration_guided : bool ref
  (** Guided iteration technique *)

val widening_start : int ref
  (** Number of steps without widening *)

val widening_descend : int ref
  (** Number of descending iterations *)

val dot_fmt : Format.formatter option ref
  (** Optional dot output *)

(*  ---------------------------------------------------------------------- *)
(** {3 Program transformation} *)
(*  ---------------------------------------------------------------------- *)

val inline : string list option ref
val unroll : int list ref
val compress_truebranch : bool ref
val compress_basicblock : bool ref
val compress_internalize : bool ref

(*  ---------------------------------------------------------------------- *)
(** {3 Speclist} *)
(*  ---------------------------------------------------------------------- *)

val speclist : Arg2.entry list
val t : Arg2.t
