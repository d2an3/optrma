(** Master module *)

(* This file is part of the Interproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Mathias Argoud, Gaël Lalire, Bertrand Jeannet 2007.
*)

open Format
open Option

let main () =
  (* Parsing the command line *)
  Arg2.parse Option.t;
  Format.pp_set_margin Format.std_formatter !Option.margin;
  Format.pp_set_tags Format.std_formatter true;
  Format.pp_set_formatter_tag_functions std_formatter !Option.displaytags;

  (* Parsing the program *)
  let input = open_in !Option.inputfilename in
  let lexbuf = Lexing.from_channel input in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with
      Lexing.pos_fname = !Option.inputfilename;
    };
  let prog = Check.parse_and_check Format.err_formatter lexbuf in
  close_in input;
  (* Computing solution *)
  Frontend.analyze_and_display Format.std_formatter prog;
  ()

let _ =
(*
  Gc.set { (Gc.get()) with Gc.verbose=0x2FF };
*)
  Printexc.record_backtrace true;
  begin try
    main ()
  with Failure s ->
    Format.eprintf "@.Failure exception:@.%s%s"
      s (Printexc.get_backtrace())
    ;
  end;
  Gc.full_major();
  ()
