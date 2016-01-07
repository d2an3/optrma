(** CGI-Interface for the concurinterproc analyzer *)

(* This file is part of the ConcurInterproc analyzer, released under GPL
   license.  Please read the COPYING file packaged in the distribution.

   Copyright (C) Bertrand Jeannet 2008.
*)

open Html
open Http
open Cgi
open Sscookie
open Date

(* you will need to change the following address *)
module Link = struct
  let (url:string) =
    Html.link
      "http://pop-art.inrialpes.fr/interproc/concurinterprocweb.cgi"
      "concurinterprocweb"
  let (apron:string) =
    Html.link
      "http://apron.cri.ensmp.fr/library/"
      "APRON Numerical Abstract Domains Library"
  let (bddapron:string) =
    Html.link
      "http://pop-art.inrialpes.fr/~bjeannet/bjeannet-forge/bddapron/index.html"
      "BDDAPRON Logico-Numerical Abstract Domain Library"
  let (fixpoint:string) =
    Html.link
      "http://pop-art.inrialpes.fr/people/bjeannet/bjeannet-forge/fixpoint/index.html"
      "Fixpoint Solver Library"
  let (concurinterproc:string) =
    Html.link
      "http://pop-art.inrialpes.fr/people/bjeannet/bjeannet-forge/concurinterproc/manual.html"
      "ConcurInterproc"
  let (simple_syntax:string) =
    Html.link
      "http://pop-art.inrialpes.fr/people/bjeannet/bjeannet-forge/concurinterproc/manual_syntax.html"
      "``Simple'' language syntax"
  let (concur_benchmark:string) =
    Html.link
      "http://pop-art.inrialpes.fr/people/bjeannet/bjeannet-forge/concurinterproc/manual_benchmark.html"
      "Benchmarks"
  let (program_examples:string) =
    Print.sprintf "%s %s %s %s %s %s %s %s %s %s %s %s"
      (Html.link
	"incr.txt" "incrr")
      (Html.link
	"maccarthy91.txt" "maccarthy91")
      (Html.link
	"maccarthy91b.txt" "maccarthy91b")
      (Html.link
	"heapsort.txt" "heapsort")
      (Html.link
	"symmetricalstairs.txt" "symmetricalstairs")
      (Html.link
	"numerical.txt" "numerical")
      (Html.link
	"finitetype.txt" "finitetype")
      (Html.link
	"concurinterproc_examples/barrier_counter/global2.txt" "barrier_counter")
      (Html.link
	"concurinterproc_examples/peterson/peterson2.txt" "Peterson mutual exclusion")
      (Html.link
	"concurinterproc_examples/kessel/kessel2.txt" "Kessel mutual exclusion")
      (Html.link
	"concurinterproc_examples/BlueToothQadeerWu_PLDI2004/prog.txt" "BlueToothQadeerWu_PLDI2004")
      (Html.link
	"concurinterproc_examples/QuadeerRajamaniRehof_POPL04/prog.txt" "QuadeerRajamaniRehof_POPL04")

  let (ocamlhtml:string) =
    Html.link
      "http://www.eleves.ens.fr/home/mine/ocamlhtml/"
      "OCamlHtml library"
end

(* ********************************************************************** *)
(* analyze *)
(* ********************************************************************** *)
let myescape_html s =
  let buf = Buffer.create 16 in
  let i = ref 0 in
  while !i < String.length s do
    if s.[!i]='<' then begin
      if (String.sub s (!i+1) 11)="span style=" then begin
	let index_end = String.index_from s (!i + 12) '>' in
	Buffer.add_string buf
	  (String.sub s !i (index_end + 1 - !i));
	i := index_end + 1;
      end
      else if (String.sub s (!i+1) 6)="/span>" then begin
	Buffer.add_string buf "</span>";
	i := !i + 7;
      end
      else begin
	Buffer.add_string buf "&lt;";
	incr i;
      end
    end
    else begin
      if s.[!i]='>' then Buffer.add_string buf "&gt;"
      else if s.[!i]='&' then Buffer.add_string buf "&amp;"
      else if s.[!i]='"' then Buffer.add_string buf "&quot;"
      else Buffer.add_char buf s.[!i]
      ;
      incr i;
    end;
  done;
  Buffer.contents buf

let analyze (progtext:string) =
  let d = Date.get_date ()
  and e = Date.get_date ()
  in
  Date.add_minutes e 15;

  Html.p ("Run "^Link.url^" ?");

  Html.h1 "Source";
  Html.pre progtext;

  Html.h1 "Analysis Result";

  let buffer = Buffer.create (String.length progtext) in
  let (output:Format.formatter) = Format.formatter_of_buffer buffer in
  begin try
    Option.displaytags := Option.htmltags;
    Format.pp_set_margin output !Option.margin;
    Format.pp_set_tags output true;
    Format.pp_set_formatter_tag_functions output !Option.displaytags;
    (* Parsing the program *)
    let lexbuf = Lexing.from_string progtext in
    let prog = Check.parse_and_check output lexbuf in
    (* Computing solution *)
    Frontend.analyze_and_display output prog;
    ()
  with
  | Exit -> ()
  | Failure s ->
      Html.h2 "Source";
      Html.pre progtext;

      Html.p (Html.escape_html s)
  end;

   print_string "<pre>\r\n";
  print_string (myescape_html (Buffer.contents buffer));
  Buffer.clear buffer;
  print_string "</pre>\r\n";

  Html.p ("Run "^Link.url^" ?");
  ()

(* ********************************************************************** *)
(* frontpage *)
(* ********************************************************************** *)

let frontpage () =
  Html.h1 "The ConcurInterproc Analyzer";
  Html.p
    (Printf.sprintf "\
This is a web interface to the %s analyzer connected \
to the BDDAPRON and the %s."
      Link.concurinterproc
      Link.fixpoint
    );
  Html.form_begin ~meth:Multipart "concurinterprocweb.cgi";

  Html.h2 "Arguments";
  Html.p ("\
Please type a program, upload a file from your hard-drive, \
or choose one the provided examples:"
  );
  Html.form_file ~maxlength:32768 "file";
  Html.br ();
  Html.form_menu "example"
    [
      Option (None,"none",         "user-supplied",  true);
      Option (None,"concurinterproc_examples/concur.txt",     "Concur",     false);
      Menu(
	"synchronisation barrier",
	[
	  Option (None, "concurinterproc_examples/barrier_counter/PC1.txt", "PC1", false);
	  Option (None, "concurinterproc_examples/barrier_counter/PC2.txt", "PC2", false);
	  Option (None, "concurinterproc_examples/barrier_counter/PC3.txt", "PC3", false);
	  Option (None, "concurinterproc_examples/barrier_counter/global2.txt", "global2", false);
	  Option (None, "concurinterproc_examples/barrier_counter/local2.txt", "local2", false);
	  Option (None, "concurinterproc_examples/barrier_counter/local2inline.txt", "local2inline", false);
	]
      );
      Menu(
	"Peterson mutual exclusion",
	[
	  Option (None, "concurinterproc_examples/peterson/peterson2.txt", "peterson2", false);
	  Option (None, "concurinterproc_examples/peterson/peterson2inline.txt", "peterson2inline", false);
	]
      );
      Menu(
	"Kessel mutual exclusion",
	[
	  Option (None, "concurinterproc_examples/kessel/kessel2.txt", "kessel2", false);
	  Option (None, "concurinterproc_examples/kessel/kessel2inline.txt", "kessel2inline", false);
	]
      );
      Option (None, "concurinterproc_examples/QuadeerRajamaniRehof_POPL04/prog.txt", "QuadeerRajamaniRehof_POPL04", false);
      Option (None, "concurinterproc_examples/BlueToothQadeerWu_PLDI2004/prog.txt", "BlueToothQadeerWu_PLDI2004", false);
      Menu(
	"tested with thresholds (to be executed without guided, and with thresholds",
	[
	  Option (None, "concurinterproc_examples/thresholds/loop1.txt", "loop1", false);
	  Option (None, "concurinterproc_examples/thresholds/loop_nondet.txt", "loop_nondet", false);
	  Option (None, "concurinterproc_examples/thresholds/loop_reset.txt", "loop_reset", false);
	  Option (None, "concurinterproc_examples/thresholds/loop2.txt", "loop2", false);
	  Option (None, "concurinterproc_examples/thresholds/gopanreps.txt", "gopanreps", false);
	  Option (None, "concurinterproc_examples/thresholds/loop2Bis.txt", "loop2Bis", false);
	  Option (None, "concurinterproc_examples/thresholds/gopanrepsBis.txt", "gopanrepsBis", false);
	  Option (None, "concurinterproc_examples/thresholds/nestedLoop.txt", "nestedLoop", false);
	  Option (None, "concurinterproc_examples/thresholds/sipma91.txt", "sipma91", false);
	  Option (None, "concurinterproc_examples/thresholds/car.txt", "car", false);
	  Option (None, "concurinterproc_examples/thresholds/concurrent_loop.txt", "concurrent_loop (options preemptive and dynamic)", false);
	  Option (None, "concurinterproc_examples/thresholds/loop2_TLM.txt", "loop2_TLM (options inline all, cooperative and dynamic)", false);
	  Option (None, "concurinterproc_examples/thresholds/barrier_counter_2.txt", "barrier_counter_2 (options preemptive and dynamic)", false);
	  Option (None, "concurinterproc_examples/thresholds/loop2_rec.txt", "loop2_rec", false);
	  Option (None, "concurinterproc_examples/thresholds/gopanreps_rec.txt", "gopanreps_rec", false);
	  Option (None, "concurinterproc_examples/thresholds/loop2Bis_rec.txt", "loop2Bis_rec", false);
	  Option (None, "concurinterproc_examples/thresholds/gopanrepsBis_rec.txt", "gopanrepsBis_rec", false);
	  Option (None, "concurinterproc_examples/thresholds/loop2_TLM.txt", "loop2_TLM_rec (options no inlining, cooperative and dynamic", false);
	]
      );
    ]
  ;
  Html.br ();
  Html.form_textarea ~default:"/* type your program here ! */" "text" 15 60;

  print_string "<br>Hit the OK button to proceed: ";
  Html.form_submit ~label:"OK !" ();
  Html.form_reset ~label:"Reset" ();

  print_string "<br><br>Program source: ";
  print_string "scheduling policy ";
  Html.form_menu "sched"
    [
      Option (None, "none", "Choose a scheduling policy ", false);
      Option (None, "cooperative", "cooperative (with yield)", false);
      Option (None, "preemptive", "preemptive", true);
    ];
  print_string " Inlining (`all', or comma-separated list of proc. names) ";
  Html.form_text
    ~size:6
    ~maxlength:10
    ~default:""
    "inline"
  ;
  print_string "<br><br>Kind of Analysis:<br>";
  Html.form_text
    ~size:6
    ~maxlength:6
    ~default:"f"
    "analysis"
  ;
  print_string "sequence of <u>f</u>orward and <u>b</u>ackward analysis";
  print_string "<br>Abstract Domains:<br>Numerical ";
  Html.form_menu "domain"
    [
      Option (None, "none", "Choose: ", false);
      Option (None, "box", "box", false);
      Option (None, "octagon", "octagon", false);
      Option (None, "polka", "convex polyhedra (polka)", true);
      Option (None, "polkastrict", "strict convex polyhedra (polka)", false);
      Option (None, "taylor1plus", "zonotopes (taylor1plus)", false);
      Option (None, "ppl", "convex polyhedra (PPL)", false);
      Option (None, "pplstrict", "strict convex polyhedra (PPL)", false);
      Option (None, "polkaeq", "linear equalities (polka)", false);
      Option (None, "pplgrid", "linear congruences (PPL)", false);
      Option (None, "polkagrid", "convex polyhedra + linear congruences", false);
    ];
  print_string " Logico-Numerical ";
  Html.form_menu "bddapron"
    [
      Option (None, "none", "Choose a combination technique:", false);
      Option (None, "bdd", "lists of BDDs and APRON values", true);
      Option (None, "mtbdd", "MYBDDs", false);
    ];
  print_string "<br><br>Iterations/Widening options:<br>";
  Html.form_checkbox
    ~checked:true
    "dynamic"
  ;
  print_string "dynamic CFG exploration (concurrent programs) ";
  Html.form_checkbox
    ~checked:true
    "guided"
  ;
  print_string "guided iterations ";
  Html.form_checkbox
    ~checked:false
    "threshold"
  ;
  print_string "threshold inference and analysis with them<br>";
  Html.form_text
    ~size:2
    ~maxlength:2
    ~default:"1"
    "widening_start"
  ;
  print_string " widening delay ";
  Html.form_text
    ~size:2
    ~maxlength:2
    ~default:"2"
    "descending"
  ;
  print_string "descending steps";
  print_string "<br><br>Results options:<br>";
  print_string "Print bounding boxes ";
  Html.form_checkbox
    ~checked:false
    "print_box"
  ;
  print_string "<br>Project on thread ";
  Html.form_text
    ~size:6
    ~maxlength:6
    ~default:""
    "print_thread"
  ;
  Html.form_checkbox
    ~checked:true
    "print_auxvar"
  ;
  print_string "Keep frozen variables ";
  Html.form_checkbox
    ~checked:true
    "print_conc"
  ;
  print_string "Keep information about other threads";

  Html.form_end ();


  Html.h2 Link.simple_syntax;

  Html.h2 Link.concur_benchmark;

  Html.p (Print.sprintf "Here are other program examples: %s" Link.program_examples);

  Html.h2 "Results";
  Html.p ~style:"note" "\
In order not to flood our web-server, analysis computation time is \
limited to 1min in this demonstration. Also note that result files are \
temporary files stored on our server that have a very short life-time."
  ;
  Html.p "The analysis computes an invariant at each program point.";
  Html.h2 "Informations";
  Html.p (Print.sprintf "\
The %s is freely available. It is written in C, with a OCaml binding. \
The %s analyzer and the %s are freely available, \
and are written in OCaml."
    Link.concurinterproc
    Link.apron
    Link.fixpoint);
  Html.p
    (Print.sprintf "\
This CGI-WEB interface is written in OCaml using the %s, \
freely available"
      Link.ocamlhtml);
  ()


(* ********************************************************************** *)
(* main *)
(* ********************************************************************** *)

let main () =
  try
    let args = Cgi.get_cgi_args () in
(*
    Format.bprintf Format.stdbuf
      "%a"
      (Print.list
	(fun fmt (str1,ostr2) ->
	  Format.fprintf fmt "(%s,%s)"
	  str1
	  (begin match ostr2 with
	  | None -> "None"
	  | Some s -> "Some "^s
	  end)
	))
      args
    ;
    print_string "<pre>\r\n";
    print_string (myescape_html (Format.flush_str_formatter ()));
    print_string "</pre>\r\n";
*)
    let (text,args) = match args with
      | ("file",Some "")::
	  ("filecontent",Some "")::
	  ("example",Some "none")::
	  ("text",Some text)::
	  args
	->
	  (text,args)
      | ("file",_)::
	  ("filecontent",Some text)::
	  ("example",Some "none")::
	  ("text",_)::
	  args
	->
	  (text,args)
      | ("file",_)::
	  ("filecontent",_)::
	  ("example",Some filename)::
	  ("text",_ )::
	  args
	->
	  let file = open_in filename in
	  let buffer = Buffer.create 1024 in
	  begin
	    try
	      while true do
		let line = input_line file in
		Buffer.add_string buffer line;
		Buffer.add_string buffer "\r\n";
	      done
	    with
	    | End_of_file -> close_in file
	  end;
	  let text = Buffer.contents buffer in
	  (text,args)
      | _ -> raise Exit
    in

    Option.iteration_guided := false;

    List.iter
      (begin function
	| ("domain",Some name) ->
	    Option.domain := List.assoc name Option.assocnamedomain;
	| ("bddapron",Some name) ->
	    Option.bddapron := List.assoc name Option.assocnamebddapron;
	| ("inline", Some str) ->
            if str="all" then
	      Option.inline := None
	    else if str="" then
	      Option.inline := Some []
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
	      Option.inline := Some !res;
	    end
	| ("analysis", Some text) ->
	    Option.analysis := [];
	    String.iter
	      (begin fun chr ->
		match chr with
		| 'f' ->
		    Option.analysis := `Forward :: !Option.analysis
		| 'b' ->
		    Option.analysis := `Backward :: !Option.analysis
	      | _ ->
		  raise (Arg.Bad ("Wrong argument `"^text^"'; option `-analysis' expects only 'f' or 'b' characters in its argument string"))
	    end)
	    text;
	    Option.analysis := List.rev !Option.analysis;
	    if !Option.analysis=[] then
	      Option.analysis := [`Forward]
	    ;
	| ("sched", Some name) ->
	    Option.scheduling := List.assoc name Option.assocnamesched;
	| ("print_box", Some text) ->
	    Option.print_box := (text="on")
	| ("print_thread", Some text) ->
	    if text="" then
	      Option.print_raw := true
	    else begin
	      Option.print_raw := false;
	      Option.print_thread := text
	    end
	| ("print_auxvar", Some text) ->
	    Option.print_auxvar := (text="on")
	| ("print_conc", Some text) ->
	    Option.print_concurrency := (text="on")
	| ("dynamic",Some text) ->
	    Option.analysis_dynamic := (text="on");
	| ("guided",Some text) ->
	    Option.iteration_guided := (text="on");
	| ("threshold",Some text) ->
	    Option.analysis_threshold := (text="on")
	| ("widening_start",Some text) ->
	    Option.widening_start := int_of_string text;
	| ("descending",Some text) ->
	    Option.widening_descend := int_of_string text;
	| _ -> ()
      end)
      args
    ;
    analyze text
  with
  | Exit ->
      frontpage ()
  | exc ->
      print_string "<pre>\r\n";
      print_string (myescape_html (Printexc.to_string exc));
      print_string "</pre>\r\n"

let _ =
  Cgi.set_timeout 15;

  Sscookie.clean_cookies "concurinterprochtml";

  Http.http_header ();

  Html.html_begin
    ~lang:"en"
    ~author:"Bertrand Jeannet"
    ~desc:"\
CGI interface to the ConcurInterproc static analyzer"
    "ConcurInterproc Analyzer"
  ;
  main ();
  html_end
    ~author:"Bertrand Jeannet"
    ~email:"bjeannet@NOSPAM inrialpes.fr"
    ()
  ;
  ()
