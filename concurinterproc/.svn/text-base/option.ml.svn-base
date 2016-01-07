(**  Argument, Options and Parsing of command line *)

(* This file is part of the Interproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Mathias Argoud, Gaël Lalire, Bertrand Jeannet 2007.
*)

(** input filename *)
let inputfilename = ref ""

(** debug level *)
let debug = ref 0

(** margin for display *)
let margin = ref 78

(*  ---------------------------------------------------------------------- *)
(** {3 Display style} *)
(*  ---------------------------------------------------------------------- *)


let (texttags:Format.formatter_tag_functions) = {
  Format.mark_open_tag = (fun tag -> "");
  Format.mark_close_tag = (fun tag -> "");
  Format.print_open_tag = (fun tag -> ());
  Format.print_close_tag = (fun tag -> ());
}
let (colortags:Format.formatter_tag_functions)  = {
  texttags with
  Format.mark_open_tag = (function
    | "B" -> "\027[32m"
    | "R" -> "\027[31m"
    | "G" -> "\027[34m"
    | _ -> failwith "Bad tag specified");
  Format.mark_close_tag = (fun tag -> "\027[m");
}
let (htmltags:Format.formatter_tag_functions)  = {
  texttags with
    Format.mark_open_tag = (function
      | "B" -> "<span style=\"color:blue;\">"
      | "R" -> "<span style=\"color:red;\">"
      | "G" -> "<span style=\"color:green;\">"
      | _ -> failwith "Bad tag specified");
    Format.mark_close_tag = (fun tag -> "</span>");
}

let (assocnamedisplaytags : (string * Format.formatter_tag_functions) list) =
  [
    ("text",texttags);
    ("color",colortags);
    ("html",htmltags);
  ]
let (lnamedisplaytags : string list) =
  List.map
    (fun (name,_) -> name)
    assocnamedisplaytags

let displaytags = ref colortags


(*  ---------------------------------------------------------------------- *)
(** {3 Printing options} *)
(*  ---------------------------------------------------------------------- *)

let print_thread = ref ""
let print_raw = ref true
let print_auxvar = ref true
let print_concurrency = ref true
let print_box = ref false

let xml_cout = ref None
let xml_formatter = ref None

(*  ---------------------------------------------------------------------- *)
(** {3 Choice of abstract domain} *)
(*  ---------------------------------------------------------------------- *)

type domain =
  | Box
  | Octagon
  | PolkaLoose
  | PolkaStrict
  | PolkaEq
  | Taylor1plus
  | PplPolyLoose
  | PplPolyStrict
  | PplGrid
  | PolkaGrid
let (assocnamedomain : (string * domain) list) =
  [
    ("box",Box);
    ("octagon",Octagon);
    ("polka",PolkaLoose);
    ("polkastrict",PolkaStrict);
    ("polkaeq",PolkaEq);
    ("taylor1plus",Taylor1plus);
    ("ppl",PplPolyLoose);
    ("pplstrict",PplPolyStrict);
    ("pplgrid",PplGrid);
    ("polkagrid",PolkaGrid)
  ]
let (lnamedomain : string list) =
  List.map
    (fun (name,_) -> name)
    assocnamedomain
let domain = ref PolkaLoose (** abstract domain to use *)

type bddapron = Bdd | Mtbdd
let (assocnamebddapron : (string * bddapron) list) =
  [
    ("bdd",Bdd);
    ("mtbdd",Mtbdd)
  ]
let (lnamebddapron : string list) =
  List.map
    (fun (name,_) -> name)
    assocnamebddapron
let bddapron = ref Mtbdd

(*  ---------------------------------------------------------------------- *)
(** {3 Choice of analysis type} *)
(*  ---------------------------------------------------------------------- *)

let (analysis:[< `Backward | `Forward ] list ref) = ref [`Forward]

let instrumCompl = ref false

(*  ---------------------------------------------------------------------- *)
(** {3 Choice of analysis method} *)
(*  ---------------------------------------------------------------------- *)

let assocnamemethod =
  [
    ("full",`Solving1);
    ("partial",`Solving2);
  ]
let lnamemethod =
  List.map
    (fun (name,_) -> name)
    assocnamemethod
let (analysis_method:[ `Solving1 | `Solving2] ref) = ref `Solving1

let analysis_dynamic = ref true

let analysis_threshold = ref false

(*  ---------------------------------------------------------------------- *)
(** {3 Fixpoint iteration} *)
(*  ---------------------------------------------------------------------- *)

let iteration_depth = ref 2
let iteration_guided = ref false
let widening_start = ref 1
let widening_descend = ref 2
let dot_fmt = ref None

(*  ---------------------------------------------------------------------- *)
(** {3 Scheduling} *)
(*  ---------------------------------------------------------------------- *)

let assocnamesched =
  [
    ("preemptive",`Preemptive);
    ("cooperative",`Cooperative);
  ]
let lnamesched =
  List.map
    (fun (name,_) -> name)
    assocnamesched
let (scheduling:[ `Cooperative | `Preemptive] ref) = ref `Preemptive

(*  ---------------------------------------------------------------------- *)
(** {3 Program transformation} *)
(*  ---------------------------------------------------------------------- *)

let inline = ref (Some [])

let unroll = ref []

let compress_truebranch = ref true
let compress_basicblock = ref true
let compress_internalize = ref true

(*  ---------------------------------------------------------------------- *)
(** {3 Main specification list} *)
(*  ---------------------------------------------------------------------- *)

let (speclist:Arg2.entry list) =
     [
      (
	"debug",
	Arg2.Set_int(debug),
	"<int>", (" : debug level, from 0 to 5 (default:0)":Arg2.doc)
      );
      (
	"dot",
	Arg2.String(begin fun filename ->
	  let dotfile = open_out filename in
	  let dotfmt = Format.formatter_of_out_channel dotfile in
	  dot_fmt := Some dotfmt;
	end),
	"<filename>" ,(" : @[activate DOT output to the given file@ (depends on the -debug option)@]":Arg2.doc)
      );
      (
	"margin",
	Arg2.Set_int(margin),
	"<int>", (" : right margin to use for display":Arg2.doc)
      );
      (
	"domain",
	Arg2.Symbol(
	  lnamedomain,
	  (fun name -> domain := List.assoc name assocnamedomain)
	),
	"",(" : numerical abstract domain to use (default: polka)":Arg2.doc)
      );
      (
	"bddapron",
	Arg2.Symbol(
	  lnamebddapron,
	  (fun name -> bddapron := List.assoc name assocnamebddapron)
	),
	"",(" : @[Boolean/numerical domains combination to use@ (default: mtbdd)@]":Arg2.doc)
      );
      (
	"sched",
	Arg2.Symbol(
	  lnamesched,
	  (fun name -> scheduling := List.assoc name assocnamesched)
	),
	"",(" : @[preemptive or cooperative scheduling ?@ (default: preemptive)@]":Arg2.doc)
      );
      (
	"compress",
	Arg2.Tuple([
	  Arg2.Bool (fun b -> compress_internalize := b);
	  Arg2.Bool (fun b -> compress_truebranch := b);
	  Arg2.Bool (fun b -> compress_basicblock := b);
	]),
	"<bool><bool><bool>", (" : @[compress CFG (internalize point, true branch, basic block) ?@ (default: true true true)@]":Arg2.doc)
      );
      (
	"inline",
	Arg2.String(begin fun str ->
	  if str="all" then
	    inline := None
	  else if str="" then
	    inline := Some []
	  else begin
	    let res = ref [] in
	    let i = ref 0 in
	    while !i < (String.length str) do
	      let index =
		try String.index_from str !i ','
		with Not_found -> String.length str
	      in
	      res := (String.sub str !i (index - !i)) :: !res;
	      i := index+1
	    done;
	    inline := Some !res;
	  end
	end),
	"\"<proc1,proc2,...>\"|all",
	(" : inline the given procedures, or all of them (default: \"\")":Arg2.doc)
      );
      (
	"unroll",
	Arg2.String(begin fun str ->
	  unroll := [];
	  let i = ref 0 in
	  while !i < (String.length str) do
	    let index =
	      try String.index_from str !i ','
	      with Not_found -> String.length str
	    in
	    let substr = String.sub str !i (index - !i) in
	    begin
	      try
		let nb = int_of_string substr in
		unroll := nb :: !unroll
	      with
	      | Failure _ ->
		  raise (Arg2.Bad ("Wrong argument `"^substr^"'; option `-unroll' expects a list of (positive) integers"))
	      | _ as exn -> raise exn
	    end;
	    i := index+1
	  done;
	  unroll := List.rev !unroll;
	  let rec check = function
	    | prefix::body::l ->
		if prefix<0 then
		  raise (Arg2.Bad ("Wrong argument to option -unroll: `"^str^"'; `prefixN' should be positive or null"))
		;
		if body<1 then
		  raise (Arg2.Bad ("Wrong argument to option -unroll: `"^str^"'; `bodyN' should be strictly positive"))
		;
		check l;
	    | [] -> ()
	    | _ ->
		raise (Arg2.Bad ("Wrong argument `"^str^"'; option `-loopunroll' expects a list of even length"))
	  in
	  check !unroll
	end),
	"\"<prefix1,body1,prefix2,body2,...>\"",
	(" : unroll the loops of depth 1,2,...":Arg2.doc)
      );
      (
	"depth",
	Arg2.Int(begin fun n ->
	  if n<2 then
	    raise (Arg2.Bad ("Wrong argument `"^(string_of_int n)^"'; option `-depth' expects an integer >= 2"))
	  else
	    iteration_depth := n
	end),
	"<int>", (" : @[depth of recursive iterations@ (default 2, may only be more)@]":Arg2.doc)
      );
      (
	"dynamic",
	Arg2.Bool(begin fun b -> analysis_dynamic := b end),
	"<bool>", (" : in first analysis, dynamic expliration of CFG (default: true)":Arg2.doc)
      );
      (
	"guided",
	Arg2.Bool(begin fun b -> iteration_guided := b end),
	"<bool>", (" : guided analysis of Gopand and Reps (default: false)":Arg2.doc)
      );
      (
	"threshold",
	Arg2.Bool(begin fun b -> analysis_threshold := b end),
	"<bool>", (" : infers thresholds and then uses widneing with thresholds (default: false)":Arg2.doc)
      );
      (
	"widening",
	Arg2.Tuple([
	  Arg2.Int(begin fun n ->
	    if n<0 then
	      raise (Arg2.Bad ("Wrong argument `"^(string_of_int n)^"'; option `-widening' expects a positive integer for its `widening start' argument"))
	    else
	      widening_start := n
	  end);
	  Arg2.Int(begin fun n ->
	    if n<0 then
	      raise (Arg2.Bad ("Wrong argument `"^(string_of_int n)^"'; option `-widening' expects a positive integer for its `descending' argument"))
	    else
	      widening_descend := n
	  end)
	]),
	"<int><int>", (" : @[specifies usage of delay,@ and nb. of descending steps@ (default: 1 2)@]":Arg2.doc)
      )
      ;
      (
	"display",
	Arg2.Symbol(
	  lnamedisplaytags,
	  begin fun name -> displaytags := List.assoc name assocnamedisplaytags end
	),
	"", (" : display style (default: color)":Arg2.doc)
      );
      (
	"thread",Arg2.String(fun s -> print_raw := false; print_thread := s),
	"<string>", (" : project analysis results on the given thread":Arg2.doc)
      );
      (
	"print_box",Arg2.Bool(fun b -> print_box := b),
	"<bool>", (" : @[display also bounding boxes when displaying numerical abstract values@ (default: false)@]":Arg2.doc)
      );
      (
	"print_auxvar",Arg2.Bool(fun b -> print_auxvar := b),
	"<bool>", (" : @[keep in analysis results frozen variables@ introduced by the method@ (default: true)@]":Arg2.doc)
      );
      (
	"print_conc",Arg2.Bool(fun b -> print_concurrency := b),
	"<bool>", (" : @[keep in analysis results variables and control points@ from other threads@ (default: true)@]":Arg2.doc)
      );
      (
	"analysis",
	Arg2.String(begin fun str ->
	  analysis := [];
	  String.iter
	    (begin fun chr ->
	      match chr with
	      | 'f' ->
		  analysis := `Forward :: !analysis
	      | 'b' ->
		  analysis := `Backward :: !analysis
	      | _ ->
		  raise (Arg2.Bad ("Wrong argument `"^str^"'; option `-analysis' expects only 'f' or 'b' characters in its argument string"))
	    end)
	    str;
	  analysis := List.rev !analysis;
	  if !analysis=[] then analysis := [`Forward];
	end),
	"<('f'|'b')+>", (" : @[sequence of forward and backward analyses to perform@ (default \"f\")@]":Arg2.doc)
      );
      (
	"instrum",Arg2.Bool(fun b -> instrumCompl := b),
	"<bool>", (" : @[keep in backward analysis instrumentation variables@ of forward analysis,@ and conversely@ (default: false)@]":Arg2.doc)
      );
      (
	"method",
	Arg2.Symbol(
	  lnamemethod,
	  (fun name -> analysis_method := List.assoc name assocnamemethod)
	),
	"", (" : @[full(y) or partial(ly) relational method ?@ (default: full)@]":Arg2.doc)
      );
      (
	"xml",
	Arg2.String(fun str ->
		      let cout = open_out str in
		      let fmt  = Format.formatter_of_out_channel cout in
			xml_cout := Some cout;
			xml_formatter := Some (fmt);
			Format.fprintf fmt "<newspeak_interproc>"
		   ),
	"<string>", (" : name of the XML containing the output":Arg2.doc)
      );
     ]

let t = (("concurinterproc <options> <inputfile>":Arg2.usage_msg),10,speclist,(fun name -> inputfilename := name))
