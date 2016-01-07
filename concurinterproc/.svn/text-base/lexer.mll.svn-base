(* This file is part of the Interproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Bertrand Jeannet 2009.
*)

{
open Syntax
open Parser

(* Keywords *)
let keywords = Hashtbl.create 53
let _ =
  Array.iter
    (fun (keyword,token) -> Hashtbl.add keywords keyword token)
    [|
      ("initial", TK_INITIAL);
      ("yield", TK_YIELD);
      ("typedef", TK_TYPEDEF);
      ("enum", TK_ENUM);
      ("proc", TK_PROC);
      ("thread", TK_THREAD);
      ("var", TK_VAR);
      ("true", TK_TRUE);
      ("false",TK_FALSE);
      ("assume", TK_ASSUME);
      ("halt", TK_HALT);
      ("fail", TK_FAIL);
      ("skip", TK_SKIP);
      ("while",TK_WHILE);
      ("returns", TK_RETURNS);
      ("done",   TK_DONE);
      ("endif",  TK_ENDIF);
      ("if",   TK_IF);
      ("int",  TK_INT);
      ("real",  TK_REAL);
      ("not",  TK_NOT);
      ("and",  TK_AND);
      ("or",   TK_OR);
      ("random", TK_RANDOM);
      ("in", TK_IN);
      ("bool", TK_BOOL);
      ("sint", TK_SINT);
      ("uint", TK_UINT);
      ("goto", TK_GOTO);
      ("inline", TK_INLINE);
      ("atomic", TK_ATOMIC);
    |]

let newline (lexbuf:Lexing.lexbuf) : unit
  =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum;
  };
  ()

let point lexbuf offset : Syntax.pos
  =
  let pos = lexbuf.Lexing.lex_curr_p in
  let line = pos.Lexing.pos_lnum in
  let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  {
    Syntax.id = -1;
    Syntax.file = pos.Lexing.pos_fname;
    Syntax.fid = "";
    Syntax.line = line;
    Syntax.col = col + offset;
    Syntax.char = pos.Lexing.pos_cnum;
  }

let attributes_of_string (pos:int) lexbuf =
  let str = Lexing.lexeme lexbuf in
  let length = (String.length str) - pos in
  let suffix = String.sub str pos length in
  let index = ref 0 in
  if length=0 then
    (Apron.Texpr1.Real, Apron.Texpr1.Rnd)
  else begin
    if String.get suffix !index <> '_' || length = 1 then
      raise (Error "")
    ;
    incr index;
    let round_typ =
      match String.get suffix !index with
      | 'i' -> Apron.Texpr1.Int
      | 'f' -> Apron.Texpr1.Single
      | 'd' -> Apron.Texpr1.Double
      | 'l' -> Apron.Texpr1.Extended
      | 'q' -> Apron.Texpr1.Quad
      | _ -> raise (Error "");
    in
    if length = 2 then
      (round_typ, Apron.Texpr1.Rnd)
    else begin
      incr index;
      if (String.get suffix !index) <> ',' || length = 3 then
	raise (Error "")
      ;
      incr index;
      let round_dir =
	match String.sub suffix !index (length - !index) with
       | "n" -> Apron.Texpr1.Near
	| "0" -> Apron.Texpr1.Zero
	| "+oo" -> Apron.Texpr1.Up
	| "-oo" -> Apron.Texpr1.Down
	| "?" -> Apron.Texpr1.Rnd
	|  _ -> raise (Error "")
      in
      (round_typ, round_dir)
    end
  end
}

rule token = parse
    (* ignore spaces, tabs, and \r *)
      ['\r' ' ' '\t']+   { token lexbuf }

    (* line count *)
    | '\n'
    { newline lexbuf; token lexbuf }

    (* comments: nested /* */, and // are allowed *)
    | "/*" { Syntax.start_of_comment := Lexing.lexeme_start_p lexbuf;
	     comment lexbuf ;
	     token lexbuf }
    | "//"([^'\n']*)     { token lexbuf }

    (* numbers *)
    | (['0'-'9'])+ ('/'['0'-'9']+)?
	{
	  let str = Lexing.lexeme lexbuf in
	  TK_MPQF(Mpqf.of_string str)
	}
    | ['0'-'9']* ('.' ['0'-'9']+) (['e' 'E'] ['+' '-']? ['0'-'9']+)?
	{
	  let str = Lexing.lexeme lexbuf in
	  TK_FLOAT(float_of_string str)
	}

    (* keywords *)
    | "do"     { TK_DO (point lexbuf 0) }
    | "then"   { TK_THEN (point lexbuf 0) }
    | "else"   { TK_ELSE (point lexbuf 0) }
    | "begin"  { TK_BEGIN (point lexbuf 0) }
    | "end"    { TK_END (point lexbuf 0) }
    | ";"      { TK_SEMICOLON (point lexbuf 0) }
    | ":"      { TK_COLON }
    | ","      { TK_COMMA }
    | "("      { TK_LPAR }
    | ")"      { TK_RPAR }
    | "{"      { TK_LBRACE }
    | "}"      { TK_RBRACE }
    | "["      { TK_LBRACKET }
    | "]"      { TK_RBRACKET }
    | "<="     { TK_LEQ }
    | ">="     { TK_GEQ }
    | "<"      { TK_LT }
    | ">"      { TK_GT }
    | "!="     { TK_NEQ }
    | "=="     { TK_EQ }
    | "="      { TK_AF }

    (* Arithmetic operations *)
    | "+"(('_'['i''f''d''l''q'])(','("n"|"0"|"+oo"|"-oo"|"?"))?)?
	{ TK_ADD(attributes_of_string 1 lexbuf) }
    | "-"(('_'['i''f''d''l''q'])(','("n"|"0"|"+oo"|"-oo"|"?"))?)?
	{ TK_SUB(attributes_of_string 1 lexbuf) }
    | "*"(('_'['i''f''d''l''q'])(','("n"|"0"|"+oo"|"-oo"|"?"))?)?
	{ TK_MUL(attributes_of_string 1 lexbuf) }
    | "/"(('_'['i''f''d''l''q'])(','("n"|"0"|"+oo"|"-oo"|"?"))?)?
	{ TK_DIV(attributes_of_string 1 lexbuf) }
    | "%"(('_'['i''f''d''l''q'])(','("n"|"0"|"+oo"|"-oo"|"?"))?)?
	{ TK_MODULO(attributes_of_string 1 lexbuf) }
    | "cast"(('_'['i''f''d''l''q'])(','("n"|"0"|"+oo"|"-oo"|"?"))?)?
	{ TK_CAST(attributes_of_string 4 lexbuf) }
    | "sqrt"(('_'['i''f''d''l''q'])(','("n"|"0"|"+oo"|"-oo"|"?"))?)?
	{ TK_SQRT(attributes_of_string 4 lexbuf) }

    (* labels *)
    | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* ['$']
      {
	let id = Lexing.lexeme lexbuf in
	let id = String.sub id 0 ((String.length id)-1) in
	TK_LABEL(id)
      }
    (* identifiers *)
    | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
      {
	let id = Lexing.lexeme lexbuf in
	try Hashtbl.find keywords id
	with Not_found -> TK_ID id }

    (* end of file *)
    | eof         { TK_EOF }
    | '\004'      { TK_EOF }


    (* handles nested /* */ comments *)

and comment = parse
    "/*"   { comment lexbuf; comment lexbuf }
  | "*/"   { () }
  | '\n'
      { newline lexbuf; comment lexbuf }
  | [^'\n' '*' '/']+ { comment lexbuf }
  | _              { comment lexbuf }
  | eof            { raise (Unterminated_comment !Syntax.start_of_comment) }
