/* This file is part of the Interproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Mathias Argoud, Gaël Lalire, Bertrand Jeannet 2007.
*/

%{
open Bddapron.Syntax
open Syntax

let pos_minus_begin pos =
  assert(pos.Syntax.col>=5);
  { pos with
    Syntax.col = pos.Syntax.col - 5;
    Syntax.char = pos.Syntax.char - 5;
  }
%}

/* tokens */
%token TK_BOOL TK_UINT TK_SINT TK_IN TK_LPAR TK_RPAR TK_LBRACE TK_RBRACE TK_LBRACKET TK_RBRACKET TK_RANDOM TK_ASSUME TK_HALT TK_FAIL TK_VAR TK_DONE TK_ENDIF TK_SKIP TK_YIELD TK_INITIAL TK_INLINE TK_ATOMIC
%token TK_TYPEDEF TK_ENUM TK_PROC TK_THREAD TK_WHILE TK_IF TK_GOTO
%token <(Apron.Texpr1.typ * Apron.Texpr1.round)> TK_MUL
%token <(Apron.Texpr1.typ * Apron.Texpr1.round)> TK_ADD
%token <(Apron.Texpr1.typ * Apron.Texpr1.round)> TK_SUB
%token <(Apron.Texpr1.typ * Apron.Texpr1.round)> TK_DIV
%token <(Apron.Texpr1.typ * Apron.Texpr1.round)> TK_MODULO
%token <(Apron.Texpr1.typ * Apron.Texpr1.round)> TK_CAST
%token <(Apron.Texpr1.typ * Apron.Texpr1.round)> TK_SQRT
%token <Mpqf.t> TK_MPQF
%token <float> TK_FLOAT

%token TK_PLUS TK_MINUS TK_TIMES TK_LEQ TK_GEQ TK_LT TK_GT TK_EQ TK_NEQ TK_AF
%token TK_AND TK_OR TK_NOT
%token TK_COMMA TK_RETURNS TK_COLON
%token TK_INT TK_REAL
%token <string> TK_LABEL
%token <string> TK_ID
%token <Syntax.pos> TK_DO TK_THEN TK_ELSE TK_BEGIN TK_END TK_SEMICOLON
%token TK_TRUE TK_FALSE
%token TK_EOF

/* precedence */
/*
%nonassoc TK_SEMICOLON
%nonassoc TK_WHILE TK_DO TK_DONE
%nonassoc TK_IF TK_THEN
%nonassoc TK_ELSE
%nonassoc TK_LPAR TK_RPAR TK_BEGIN TK_END
%left TK_OR
%left TK_AND
%nonassoc TK_NOT
%nonassoc TK_LEQ TK_GEQ TK_L TK_G TK_EQ TK_NEQ TK_AF
*/

/* types */
%type <string Syntax.program> program
%type <string Bddapron.Syntax.expr option> expr
%type <int> num
%type <string list> variables

%start program
%%

coeff:
  scalar { Apron.Coeff.Scalar $1 }
| interval { Apron.Coeff.Interval $1 }
scalar:
  TK_MPQF { Apron.Scalar.Mpqf($1) }
| TK_FLOAT { Apron.Scalar.Float($1) }
interval:
  TK_LBRACKET scalar TK_COMMA scalar TK_RBRACKET
   { Apron.Interval.of_scalar $2 $4 }
cst:
| TK_TRUE  { `Bool(true) }
| TK_FALSE { `Bool(false) }
| bint num {
    `Bint($1,$2) }
| bint TK_SUB num { `Bint($1,-$3) }
| coeff { `Apron $1 }
num: TK_MPQF
{
  let mpqf = $1 in
  if Mpzf.cmp_int (Mpqf.get_den mpqf) 1 = 0 then
    let mpz = Mpqf.get_num mpqf in
    Mpz.get_int mpz
  else
    raise (Error (Print.sprintf "Error: expecting integer here"))
}

list_expr:
  expr { [$1] }
| expr TK_COMMA list_expr { $1::$3 }

expr:
  TK_RANDOM { None }
| expr0 { Some $1 }

list_expr0:
  expr0 { [$1] }
| expr0 TK_COMMA list_expr0 { $1::$3 }

expr0:
  expr1 { $1 }
| TK_IF expr0 TK_THEN expr0 TK_ELSE expr0 { `If($2,$4,$6) }
expr1:
  expr2 { $1 }
| expr1 TK_OR expr2   { `Binop(`Bool `Or,$1,$3) }
expr2:
  expr3 { $1 }
| expr2 TK_AND expr3  { `Binop(`Bool `And,$1,$3) }
expr3:
  expr4 { $1 }
| expr3 TK_NEQ expr4  { `Binop((`Bool `NEQ),$1,$3) }
| expr3 TK_EQ expr4   { `Binop((`Bool `EQ),$1,$3) }
| expr6 TK_IN TK_LBRACE list_expr0 TK_RBRACE  { `In($1,$4) }
expr4:
  expr5 { $1 }
| TK_NOT expr4 { `Unop(`Not,$2) }
expr5:
  expr6 { $1 }
| expr6 TK_GEQ expr6 { `Binop(`Bool `GEQ,$1,$3) }
| expr6 TK_GT expr6 { `Binop(`Bool `GT,$1,$3) }
| expr6 TK_LEQ expr6 { `Binop(`Bool `LEQ,$1,$3) }
| expr6 TK_LT expr6 { `Binop(`Bool `LT,$1,$3) }
expr6:
  expr7 { $1 }
| expr6 TK_ADD expr7
    { let (t,r) = $2 in `Binop(`Apron(Apron.Texpr1.Add,t,r), $1,$3) }
| expr6 TK_SUB expr7
    { let (t,r) = $2 in `Binop(`Apron(Apron.Texpr1.Sub,t,r), $1,$3) }
expr7:
  expr8 { $1 }
| expr7 TK_MUL expr8
    { let (t,r) = $2 in `Binop(`Apron(Apron.Texpr1.Mul,t,r), $1,$3) }
| expr7 TK_DIV expr8
    { let (t,r) = $2 in `Binop(`Apron(Apron.Texpr1.Div,t,r), $1,$3) }
| expr7 TK_MODULO expr8
    { let (t,r) = $2 in `Binop(`Apron(Apron.Texpr1.Mod,t,r), $1,$3) }
expr8:
  TK_CAST expr8
    { let (t,r) = $1 in `Unop(`Apron(Apron.Texpr1.Cast,t,r),$2) }
| TK_SQRT expr8
    { let (t,r) = $1 in `Unop(`Apron(Apron.Texpr1.Sqrt,t,r),$2) }
| TK_SUB expr8
    { let (t,r) = $1 in `Unop(`Apron(Apron.Texpr1.Neg,t,r),$2) }
| TK_LPAR expr0 TK_RPAR
    { $2 }
| cst
   { `Cst $1 }
| TK_ID
    { `Ref $1 }

label: TK_LABEL { $1 }

tk_then:
  TK_THEN label { Lab($2,[||],true) }
| TK_THEN { Pos($1,[||]) }
tk_else:
  TK_ELSE label { Lab($2,[||],true) }
| TK_ELSE { Pos($1,[||]) }
tk_do:
  TK_DO label { Lab($2,[||],true) }
| TK_DO { Pos($1,[||]) }
tk_begin:
  TK_BEGIN label { Lab($2,[||],true) }
| TK_BEGIN { Pos($1,[||]) }
tk_begin2:
  TK_BEGIN label { (Pos(pos_minus_begin $1,[||]), Lab($2,[||],true)) }
| TK_BEGIN { (Pos(pos_minus_begin $1,[||]), Pos($1,[||])) }

tk_end:
TK_END { Pos($1,[||]) }

instruction:
  TK_YIELD
      { YIELD }
| TK_SKIP
      { SKIP }
| TK_HALT
      { HALT }
| TK_FAIL
      { FAIL }
| TK_ASSUME expr0
      { ASSUME($2) }
| variable TK_AF expr
      { ASSIGN ([$1],[$3]) }
| TK_LPAR variables TK_RPAR TK_AF TK_LPAR list_expr TK_RPAR
      { ASSIGN ($2,$6) }
| TK_LPAR variables TK_RPAR TK_AF TK_ID TK_LPAR variables TK_RPAR
      { CALL ($2,$5,$7) }
| variable TK_AF TK_ID TK_LPAR variables TK_RPAR
      { CALL ([$1],$3,$5) }
| TK_ID TK_LPAR variables TK_RPAR
      { CALL ([],$1,$3) }
| TK_GOTO TK_ID
      { GOTO(Lab($2,[||],true)) }
| TK_IF expr tk_then instr_seq TK_ENDIF
      { IF ($2, { bpoint=$3; instrs = $4 }, None) }
| TK_IF expr tk_then instr_seq tk_else instr_seq TK_ENDIF
      { IF($2,
	   { bpoint=$3; instrs = $4},
	   Some { bpoint=$5; instrs = $6})
      }
| TK_WHILE expr tk_do instr_seq TK_DONE
      { LOOP ($2, { bpoint=$3; instrs = $4}) }
| TK_VAR declarations TK_IN tk_begin instr_seq TK_END
      { LOCAL(true, $2, { bpoint=$4; instrs = $5 }) }
| TK_ATOMIC tk_begin instr_seq TK_END
      { ATOMIC({ bpoint=$2; instrs = $3 }) }

instr:
  instruction TK_SEMICOLON label
    { { instruction = $1; ipoint = Lab($3,[||],true) } }
| instruction TK_SEMICOLON
    { { instruction = $1; ipoint = Pos($2,[||]) } }

revinstr_seq:
  revinstr_seq instr
    { $2::$1 }
|
    { [] }

instr_seq: revinstr_seq { List.rev $1 }

initial:
  TK_INITIAL expr TK_SEMICOLON
    { $2 }
| { None }
fail:
  TK_FAIL expr TK_SEMICOLON
    { $2 }
| { None }

variables: revvariables { List.rev $1 }
revvariables:
  revvariables TK_COMMA variable { $3::$1 }
| variable { [$1] }
| { [] }

variable: TK_ID { $1 }

procedure:
  TK_PROC TK_ID TK_LPAR declarations0 TK_RPAR TK_RETURNS TK_LPAR declarations0 TK_RPAR vardeclarations tk_begin2 instr_seq tk_end
    {
      let (bbpoint,bpoint) = $11 in
      let code = { bpoint = bpoint; instrs = $12 } in
      let block =
	if $10=[] then
	  code
	else
	  let maininstr = {
	    instruction = LOCAL(false, $10, code);
	    ipoint = $13;
	  }
	  in
	  { bpoint = bbpoint; instrs = [maininstr] }
      in
      {
	pname = $2;
	pinput = $4;
	poutput = $8;
	pcode = block;
      }
    }

revprocedures:
  revprocedures procedure { ($2.pname, $2)::$1 }
| procedure { [($1.pname, $1)] }

revprocedures0:
  revprocedures { $1 }
|  { [] }


typ:
  TK_BOOL { `Bool }
| TK_INT { (`Int) }
| TK_REAL { (`Real) }
| bint { (`Bint $1) }
| TK_ID { (`Benum $1) }
bint:
  TK_UINT TK_LBRACKET num TK_RBRACKET
{
  if $3 < 0 then
    raise (Error (Print.sprintf "Error: in type uint[x], x should be positive"))
  ;
  (false,$3)
}
| TK_SINT TK_LBRACKET num TK_RBRACKET
{
  if $3 < 0 then
    raise (Error (Print.sprintf "Error: in type sint[x], x should be positive"))
  ;
  (true,$3)
}

declaration:
  revvariables TK_COLON typ
  {
    let lvar = $1 and typ = $3 in
    List.map (fun var -> (var,typ)) lvar
  }

revdeclarations:
  revdeclarations TK_COMMA declaration { $3 @ $1 }
| declaration { $1 }

declarations:
  revdeclarations { List.rev $1 }
declarations0:
  declarations { $1 }
| { [] }

vardeclarations:
  TK_VAR declarations TK_SEMICOLON
    { $2 }
| { [] }

typedef:
  TK_ID TK_AF TK_ENUM TK_LBRACE variables TK_RBRACE TK_SEMICOLON
    { ($1,Array.of_list $5) }

ltypedef:
  ltypedef typedef {$2::$1}
| typedef {[$1]}

typedefs:
  TK_TYPEDEF ltypedef { $2 }
| {[]}

thread:
  TK_THREAD TK_ID TK_COLON vardeclarations tk_begin2 instr_seq tk_end
  {
    let (bbpoint,bpoint) = $5 in
    let proc =
      let code = { bpoint = bpoint; instrs = $6 } in
      let block =
	if $4=[] then
	  code
	else
	  let maininstr = {
	    instruction = LOCAL(false, $4, code);
	    ipoint = $7;
	  }
	  in
	  { bpoint = bbpoint; instrs = [maininstr] }
      in
      {
	pname = $2;
	pinput = [];
	poutput = [];
	pcode = block;
      }
    in
    (proc.pname,proc)
  }

revthreads:
  revthreads thread { $2::$1 }
| thread thread { [$2;$1] }

thread0:
  vardeclarations tk_begin2 instr_seq tk_end
  {
    let (bbpoint,bpoint) = $2 in
    let proc =
      let code = { bpoint = bpoint; instrs = $3 } in
      let block =
	if $1=[] then
	  code
	else
	  let maininstr = {
	    instruction = LOCAL(false, $1, code);
	    ipoint = $4;
	  }
	  in
	  { bpoint = bbpoint; instrs = [maininstr] }
      in
      {
	pname = "";
	pinput = [];
	poutput = [];
	pcode = block;
      }
    in
    (proc.pname,proc)
  }

program:
/*  typedefs thread0 TK_EOF
  {
    {
      typenumdef = $1; global = []; initial = None; procedures = [$2]; threads = [fst $2];
    }
  }
*/
  typedefs vardeclarations initial revprocedures0 thread0 fail TK_EOF
  {
    {
      typenumdef = $1;
      global = $2;
      initial = $3;
      final = $6;
      procedures = List.rev ($5 :: $4);
      threads = [fst $5];
    }
  }
| typedefs vardeclarations initial revprocedures0 revthreads fail TK_EOF
    {
      {
	typenumdef = $1;
	global = $2;
	initial = $3;
	final = $6;
	procedures = List.rev ($5 @ $4);
	threads = List.fold_left
	(begin fun res (name,proc) -> name::res end)
	[] $5
      }
    }
