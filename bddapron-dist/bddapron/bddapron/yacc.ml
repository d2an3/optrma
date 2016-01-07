type token =
  | TK_LBRACKET
  | TK_RBRACKET
  | TK_SEMICOLON
  | TK_COLON
  | TK_LPAR
  | TK_RPAR
  | TK_LBRACE
  | TK_RBRACE
  | TK_BOOL
  | TK_UINT
  | TK_SINT
  | TK_INT
  | TK_REAL
  | TK_IN
  | TK_COMMA
  | TK_TYPEDEF
  | TK_ENUM
  | TK_IF
  | TK_THEN
  | TK_ELSE
  | TK_VERTEX
  | TK_RAY
  | TK_LINE
  | TK_MOD
  | TK_RAYMOD
  | TK_LINEMOD
  | TK_MUL of ((Apron.Texpr1.typ * Apron.Texpr1.round))
  | TK_ADD of ((Apron.Texpr1.typ * Apron.Texpr1.round))
  | TK_SUB of ((Apron.Texpr1.typ * Apron.Texpr1.round))
  | TK_DIV of ((Apron.Texpr1.typ * Apron.Texpr1.round))
  | TK_MODULO of ((Apron.Texpr1.typ * Apron.Texpr1.round))
  | TK_CAST of ((Apron.Texpr1.typ * Apron.Texpr1.round))
  | TK_SQRT of ((Apron.Texpr1.typ * Apron.Texpr1.round))
  | TK_MPQF of (Mpqf.t)
  | TK_FLOAT of (float)
  | TK_LEQ
  | TK_GEQ
  | TK_LT
  | TK_GT
  | TK_EQ
  | TK_NEQ
  | TK_AND
  | TK_OR
  | TK_NOT
  | TK_ID of (string)
  | TK_TRUE
  | TK_FALSE
  | TK_EOF

open Parsing;;
let _ = parse_error;;
# 7 "bddapron/yacc.mly"
open Syntax
# 56 "bddapron/yacc.ml"
let yytransl_const = [|
  257 (* TK_LBRACKET *);
  258 (* TK_RBRACKET *);
  259 (* TK_SEMICOLON *);
  260 (* TK_COLON *);
  261 (* TK_LPAR *);
  262 (* TK_RPAR *);
  263 (* TK_LBRACE *);
  264 (* TK_RBRACE *);
  265 (* TK_BOOL *);
  266 (* TK_UINT *);
  267 (* TK_SINT *);
  268 (* TK_INT *);
  269 (* TK_REAL *);
  270 (* TK_IN *);
  271 (* TK_COMMA *);
  272 (* TK_TYPEDEF *);
  273 (* TK_ENUM *);
  274 (* TK_IF *);
  275 (* TK_THEN *);
  276 (* TK_ELSE *);
  277 (* TK_VERTEX *);
  278 (* TK_RAY *);
  279 (* TK_LINE *);
  280 (* TK_MOD *);
  281 (* TK_RAYMOD *);
  282 (* TK_LINEMOD *);
  292 (* TK_LEQ *);
  293 (* TK_GEQ *);
  294 (* TK_LT *);
  295 (* TK_GT *);
  296 (* TK_EQ *);
  297 (* TK_NEQ *);
  298 (* TK_AND *);
  299 (* TK_OR *);
  300 (* TK_NOT *);
  302 (* TK_TRUE *);
  303 (* TK_FALSE *);
  304 (* TK_EOF *);
    0|]

let yytransl_block = [|
  283 (* TK_MUL *);
  284 (* TK_ADD *);
  285 (* TK_SUB *);
  286 (* TK_DIV *);
  287 (* TK_MODULO *);
  288 (* TK_CAST *);
  289 (* TK_SQRT *);
  290 (* TK_MPQF *);
  291 (* TK_FLOAT *);
  301 (* TK_ID *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\003\000\003\000\004\000\005\000\005\000\005\000\
\005\000\005\000\007\000\001\000\009\000\009\000\008\000\008\000\
\010\000\010\000\011\000\011\000\012\000\012\000\012\000\012\000\
\013\000\013\000\015\000\015\000\015\000\015\000\015\000\014\000\
\014\000\014\000\016\000\016\000\016\000\016\000\017\000\017\000\
\017\000\017\000\017\000\017\000\018\000\018\000\018\000\018\000\
\018\000\006\000\006\000\000\000"

let yylen = "\002\000\
\001\000\001\000\001\000\001\000\005\000\001\000\001\000\002\000\
\003\000\001\000\001\000\002\000\001\000\003\000\001\000\006\000\
\001\000\003\000\001\000\003\000\001\000\003\000\003\000\005\000\
\001\000\002\000\001\000\003\000\003\000\003\000\003\000\001\000\
\003\000\003\000\001\000\003\000\003\000\003\000\002\000\002\000\
\002\000\003\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\004\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\003\000\004\000\000\000\044\000\006\000\007\000\
\052\000\010\000\001\000\002\000\043\000\000\000\000\000\000\000\
\000\000\000\000\021\000\000\000\025\000\000\000\035\000\000\000\
\000\000\000\000\000\000\000\000\041\000\039\000\040\000\026\000\
\000\000\000\000\011\000\008\000\012\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\042\000\000\000\000\000\000\000\
\009\000\000\000\000\000\023\000\022\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\036\000\037\000\038\000\000\000\
\050\000\051\000\000\000\000\000\000\000\005\000\000\000\000\000\
\024\000\016\000\014\000"

let yydgoto = "\002\000\
\017\000\018\000\019\000\020\000\021\000\022\000\044\000\084\000\
\085\000\024\000\025\000\026\000\027\000\028\000\029\000\030\000\
\031\000\000\000"

let yysindex = "\001\000\
\003\255\000\000\244\254\003\255\004\255\008\255\003\255\068\255\
\068\255\068\255\000\000\000\000\061\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\247\254\224\254\240\254\
\253\254\249\254\000\000\017\255\000\000\037\255\000\000\015\255\
\035\255\010\255\010\255\046\255\000\000\000\000\000\000\000\000\
\091\255\010\255\000\000\000\000\000\000\061\255\061\255\061\255\
\061\255\063\255\068\255\068\255\068\255\068\255\068\255\068\255\
\068\255\068\255\068\255\244\254\000\000\072\255\075\255\003\255\
\000\000\253\254\249\254\000\000\000\000\003\255\037\255\037\255\
\056\255\056\255\056\255\056\255\000\000\000\000\000\000\087\255\
\000\000\000\000\071\255\077\255\090\255\000\000\003\255\003\255\
\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\118\255\
\069\000\009\255\000\000\194\255\000\000\103\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\194\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\072\000\031\000\000\000\000\000\000\000\141\255\179\255\
\224\255\239\255\013\000\028\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\096\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\254\255\000\000\000\000\000\000\240\255\255\255\
\011\000\000\000\064\000\065\000\250\255\027\000\000\000\036\000\
\002\000\000\000"

let yytablesize = 376
let yytable = "\023\000\
\032\000\001\000\033\000\003\000\034\000\036\000\040\000\004\000\
\035\000\037\000\038\000\039\000\005\000\006\000\019\000\045\000\
\019\000\062\000\063\000\042\000\007\000\011\000\012\000\019\000\
\043\000\065\000\046\000\019\000\019\000\060\000\050\000\008\000\
\048\000\049\000\009\000\010\000\011\000\012\000\047\000\041\000\
\061\000\068\000\069\000\043\000\051\000\052\000\013\000\014\000\
\015\000\016\000\019\000\019\000\053\000\054\000\055\000\056\000\
\019\000\080\000\077\000\078\000\079\000\003\000\083\000\057\000\
\064\000\004\000\058\000\059\000\003\000\070\000\005\000\006\000\
\004\000\081\000\041\000\041\000\082\000\005\000\006\000\073\000\
\074\000\075\000\076\000\051\000\052\000\090\000\071\000\072\000\
\086\000\008\000\087\000\088\000\009\000\010\000\011\000\012\000\
\008\000\089\000\091\000\009\000\010\000\011\000\012\000\013\000\
\013\000\014\000\015\000\016\000\032\000\066\000\032\000\067\000\
\014\000\015\000\016\000\000\000\032\000\032\000\051\000\052\000\
\000\000\032\000\032\000\015\000\000\000\015\000\053\000\054\000\
\055\000\056\000\032\000\032\000\015\000\000\000\000\000\000\000\
\015\000\015\000\032\000\032\000\032\000\032\000\032\000\032\000\
\032\000\032\000\033\000\000\000\033\000\000\000\032\000\000\000\
\000\000\000\000\033\000\033\000\000\000\000\000\000\000\033\000\
\033\000\000\000\000\000\000\000\000\000\015\000\000\000\000\000\
\033\000\033\000\000\000\000\000\000\000\000\000\000\000\000\000\
\033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
\034\000\000\000\034\000\000\000\033\000\000\000\000\000\000\000\
\034\000\034\000\000\000\000\000\000\000\034\000\034\000\027\000\
\000\000\027\000\000\000\000\000\000\000\000\000\034\000\034\000\
\027\000\000\000\000\000\000\000\027\000\027\000\034\000\034\000\
\034\000\034\000\034\000\034\000\034\000\034\000\000\000\000\000\
\000\000\000\000\034\000\000\000\000\000\030\000\000\000\030\000\
\000\000\027\000\027\000\027\000\027\000\000\000\030\000\000\000\
\000\000\027\000\030\000\030\000\028\000\000\000\028\000\000\000\
\000\000\000\000\000\000\000\000\000\000\028\000\000\000\000\000\
\000\000\028\000\028\000\000\000\000\000\000\000\000\000\030\000\
\030\000\030\000\030\000\000\000\000\000\000\000\000\000\030\000\
\000\000\000\000\031\000\000\000\031\000\000\000\028\000\028\000\
\028\000\028\000\000\000\031\000\000\000\000\000\028\000\031\000\
\031\000\029\000\000\000\029\000\020\000\000\000\020\000\000\000\
\000\000\000\000\029\000\000\000\000\000\020\000\029\000\029\000\
\000\000\020\000\020\000\000\000\031\000\031\000\031\000\031\000\
\000\000\000\000\000\000\000\000\031\000\000\000\000\000\000\000\
\000\000\000\000\000\000\029\000\029\000\029\000\029\000\000\000\
\020\000\020\000\017\000\029\000\017\000\018\000\020\000\018\000\
\000\000\000\000\000\000\017\000\000\000\000\000\018\000\017\000\
\017\000\000\000\018\000\018\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\017\000\
\000\000\000\000\018\000\000\000\017\000\000\000\000\000\018\000"

let yycheck = "\001\000\
\003\000\001\000\004\000\001\001\001\001\007\000\013\000\005\001\
\001\001\008\000\009\000\010\000\010\001\011\001\006\001\048\001\
\008\001\034\000\035\000\029\001\018\001\034\001\035\001\015\001\
\034\001\042\000\043\001\019\001\020\001\015\001\014\001\029\001\
\040\001\041\001\032\001\033\001\034\001\035\001\042\001\013\000\
\006\001\048\000\049\000\034\001\028\001\029\001\044\001\045\001\
\046\001\047\001\042\001\043\001\036\001\037\001\038\001\039\001\
\048\001\060\000\057\000\058\000\059\000\001\001\064\000\027\001\
\019\001\005\001\030\001\031\001\001\001\007\001\010\001\011\001\
\005\001\002\001\048\000\049\000\002\001\010\001\011\001\053\000\
\054\000\055\000\056\000\028\001\029\001\087\000\051\000\052\000\
\002\001\029\001\020\001\015\001\032\001\033\001\034\001\035\001\
\029\001\008\001\088\000\032\001\033\001\034\001\035\001\008\001\
\044\001\045\001\046\001\047\001\006\001\046\000\008\001\047\000\
\045\001\046\001\047\001\255\255\014\001\015\001\028\001\029\001\
\255\255\019\001\020\001\006\001\255\255\008\001\036\001\037\001\
\038\001\039\001\028\001\029\001\015\001\255\255\255\255\255\255\
\019\001\020\001\036\001\037\001\038\001\039\001\040\001\041\001\
\042\001\043\001\006\001\255\255\008\001\255\255\048\001\255\255\
\255\255\255\255\014\001\015\001\255\255\255\255\255\255\019\001\
\020\001\255\255\255\255\255\255\255\255\048\001\255\255\255\255\
\028\001\029\001\255\255\255\255\255\255\255\255\255\255\255\255\
\036\001\037\001\038\001\039\001\040\001\041\001\042\001\043\001\
\006\001\255\255\008\001\255\255\048\001\255\255\255\255\255\255\
\014\001\015\001\255\255\255\255\255\255\019\001\020\001\006\001\
\255\255\008\001\255\255\255\255\255\255\255\255\028\001\029\001\
\015\001\255\255\255\255\255\255\019\001\020\001\036\001\037\001\
\038\001\039\001\040\001\041\001\042\001\043\001\255\255\255\255\
\255\255\255\255\048\001\255\255\255\255\006\001\255\255\008\001\
\255\255\040\001\041\001\042\001\043\001\255\255\015\001\255\255\
\255\255\048\001\019\001\020\001\006\001\255\255\008\001\255\255\
\255\255\255\255\255\255\255\255\255\255\015\001\255\255\255\255\
\255\255\019\001\020\001\255\255\255\255\255\255\255\255\040\001\
\041\001\042\001\043\001\255\255\255\255\255\255\255\255\048\001\
\255\255\255\255\006\001\255\255\008\001\255\255\040\001\041\001\
\042\001\043\001\255\255\015\001\255\255\255\255\048\001\019\001\
\020\001\006\001\255\255\008\001\006\001\255\255\008\001\255\255\
\255\255\255\255\015\001\255\255\255\255\015\001\019\001\020\001\
\255\255\019\001\020\001\255\255\040\001\041\001\042\001\043\001\
\255\255\255\255\255\255\255\255\048\001\255\255\255\255\255\255\
\255\255\255\255\255\255\040\001\041\001\042\001\043\001\255\255\
\042\001\043\001\006\001\048\001\008\001\006\001\048\001\008\001\
\255\255\255\255\255\255\015\001\255\255\255\255\015\001\019\001\
\020\001\255\255\019\001\020\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\043\001\
\255\255\255\255\043\001\255\255\048\001\255\255\255\255\048\001"

let yynames_const = "\
  TK_LBRACKET\000\
  TK_RBRACKET\000\
  TK_SEMICOLON\000\
  TK_COLON\000\
  TK_LPAR\000\
  TK_RPAR\000\
  TK_LBRACE\000\
  TK_RBRACE\000\
  TK_BOOL\000\
  TK_UINT\000\
  TK_SINT\000\
  TK_INT\000\
  TK_REAL\000\
  TK_IN\000\
  TK_COMMA\000\
  TK_TYPEDEF\000\
  TK_ENUM\000\
  TK_IF\000\
  TK_THEN\000\
  TK_ELSE\000\
  TK_VERTEX\000\
  TK_RAY\000\
  TK_LINE\000\
  TK_MOD\000\
  TK_RAYMOD\000\
  TK_LINEMOD\000\
  TK_LEQ\000\
  TK_GEQ\000\
  TK_LT\000\
  TK_GT\000\
  TK_EQ\000\
  TK_NEQ\000\
  TK_AND\000\
  TK_OR\000\
  TK_NOT\000\
  TK_TRUE\000\
  TK_FALSE\000\
  TK_EOF\000\
  "

let yynames_block = "\
  TK_MUL\000\
  TK_ADD\000\
  TK_SUB\000\
  TK_DIV\000\
  TK_MODULO\000\
  TK_CAST\000\
  TK_SQRT\000\
  TK_MPQF\000\
  TK_FLOAT\000\
  TK_ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'scalar) in
    Obj.repr(
# 50 "bddapron/yacc.mly"
         ( Apron.Coeff.Scalar _1 )
# 341 "bddapron/yacc.ml"
               : 'coeff))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'interval) in
    Obj.repr(
# 51 "bddapron/yacc.mly"
           ( Apron.Coeff.Interval _1 )
# 348 "bddapron/yacc.ml"
               : 'coeff))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Mpqf.t) in
    Obj.repr(
# 53 "bddapron/yacc.mly"
          ( Apron.Scalar.Mpqf(_1) )
# 355 "bddapron/yacc.ml"
               : 'scalar))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 54 "bddapron/yacc.mly"
           ( Apron.Scalar.Float(_1) )
# 362 "bddapron/yacc.ml"
               : 'scalar))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'scalar) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'scalar) in
    Obj.repr(
# 57 "bddapron/yacc.mly"
   ( Apron.Interval.of_scalar _2 _4 )
# 370 "bddapron/yacc.ml"
               : 'interval))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "bddapron/yacc.mly"
           ( `Bool(true) )
# 376 "bddapron/yacc.ml"
               : 'cst))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "bddapron/yacc.mly"
           ( `Bool(false) )
# 382 "bddapron/yacc.ml"
               : 'cst))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'bint) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'num) in
    Obj.repr(
# 61 "bddapron/yacc.mly"
           (
    `Bint(_1,_2) )
# 391 "bddapron/yacc.ml"
               : 'cst))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bint) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : (Apron.Texpr1.typ * Apron.Texpr1.round)) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'num) in
    Obj.repr(
# 63 "bddapron/yacc.mly"
                  ( `Bint(_1,-_3) )
# 400 "bddapron/yacc.ml"
               : 'cst))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'coeff) in
    Obj.repr(
# 64 "bddapron/yacc.mly"
        ( `Apron _1 )
# 407 "bddapron/yacc.ml"
               : 'cst))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Mpqf.t) in
    Obj.repr(
# 66 "bddapron/yacc.mly"
(
  let mpqf = _1 in
  if Mpzf.cmp_int (Mpqf.get_den mpqf) 1 = 0 then
    let mpz = Mpz.init() in
    Mpq.get_num mpz mpqf;
    Mpz.get_int mpz
  else
    raise (Error (Print.sprintf "Error: expecting integer here"))
)
# 422 "bddapron/yacc.ml"
               : 'num))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr0) in
    Obj.repr(
# 75 "bddapron/yacc.mly"
                   ( _1 )
# 429 "bddapron/yacc.ml"
               : string Syntax.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr0) in
    Obj.repr(
# 78 "bddapron/yacc.mly"
        ( [_1] )
# 436 "bddapron/yacc.ml"
               : 'list_expr0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr0) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_expr0) in
    Obj.repr(
# 79 "bddapron/yacc.mly"
                            ( _1::_3 )
# 444 "bddapron/yacc.ml"
               : 'list_expr0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr1) in
    Obj.repr(
# 82 "bddapron/yacc.mly"
        ( _1 )
# 451 "bddapron/yacc.ml"
               : 'expr0))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr0) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr0) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr0) in
    Obj.repr(
# 83 "bddapron/yacc.mly"
                                          ( `If(_2,_4,_6) )
# 460 "bddapron/yacc.ml"
               : 'expr0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 85 "bddapron/yacc.mly"
        ( _1 )
# 467 "bddapron/yacc.ml"
               : 'expr1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr1) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 86 "bddapron/yacc.mly"
                      ( `Binop(`Bool `Or,_1,_3) )
# 475 "bddapron/yacc.ml"
               : 'expr1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr3) in
    Obj.repr(
# 88 "bddapron/yacc.mly"
        ( _1 )
# 482 "bddapron/yacc.ml"
               : 'expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr2) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr3) in
    Obj.repr(
# 89 "bddapron/yacc.mly"
                      ( `Binop(`Bool `And,_1,_3) )
# 490 "bddapron/yacc.ml"
               : 'expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 91 "bddapron/yacc.mly"
        ( _1 )
# 497 "bddapron/yacc.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 92 "bddapron/yacc.mly"
                      ( `Binop((`Bool `NEQ),_1,_3) )
# 505 "bddapron/yacc.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 93 "bddapron/yacc.mly"
                      ( `Binop((`Bool `EQ),_1,_3) )
# 513 "bddapron/yacc.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'expr6) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'list_expr0) in
    Obj.repr(
# 94 "bddapron/yacc.mly"
                                              ( `In(_1,_4) )
# 521 "bddapron/yacc.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr5) in
    Obj.repr(
# 96 "bddapron/yacc.mly"
        ( _1 )
# 528 "bddapron/yacc.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 97 "bddapron/yacc.mly"
               ( `Unop(`Not,_2) )
# 535 "bddapron/yacc.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr6) in
    Obj.repr(
# 99 "bddapron/yacc.mly"
        ( _1 )
# 542 "bddapron/yacc.ml"
               : 'expr5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr6) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr6) in
    Obj.repr(
# 100 "bddapron/yacc.mly"
                     ( `Binop(`Bool `GEQ,_1,_3) )
# 550 "bddapron/yacc.ml"
               : 'expr5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr6) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr6) in
    Obj.repr(
# 101 "bddapron/yacc.mly"
                    ( `Binop(`Bool `GT,_1,_3) )
# 558 "bddapron/yacc.ml"
               : 'expr5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr6) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr6) in
    Obj.repr(
# 102 "bddapron/yacc.mly"
                     ( `Binop(`Bool `LEQ,_1,_3) )
# 566 "bddapron/yacc.ml"
               : 'expr5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr6) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr6) in
    Obj.repr(
# 103 "bddapron/yacc.mly"
                    ( `Binop(`Bool `LT,_1,_3) )
# 574 "bddapron/yacc.ml"
               : 'expr5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr7) in
    Obj.repr(
# 105 "bddapron/yacc.mly"
        ( _1 )
# 581 "bddapron/yacc.ml"
               : 'expr6))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr6) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : (Apron.Texpr1.typ * Apron.Texpr1.round)) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr7) in
    Obj.repr(
# 107 "bddapron/yacc.mly"
    ( let (t,r) = _2 in `Binop(`Apron(Apron.Texpr1.Add,t,r), _1,_3) )
# 590 "bddapron/yacc.ml"
               : 'expr6))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr6) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : (Apron.Texpr1.typ * Apron.Texpr1.round)) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr7) in
    Obj.repr(
# 109 "bddapron/yacc.mly"
    ( let (t,r) = _2 in `Binop(`Apron(Apron.Texpr1.Sub,t,r), _1,_3) )
# 599 "bddapron/yacc.ml"
               : 'expr6))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr8) in
    Obj.repr(
# 111 "bddapron/yacc.mly"
        ( _1 )
# 606 "bddapron/yacc.ml"
               : 'expr7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr7) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : (Apron.Texpr1.typ * Apron.Texpr1.round)) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr8) in
    Obj.repr(
# 113 "bddapron/yacc.mly"
    ( let (t,r) = _2 in `Binop(`Apron(Apron.Texpr1.Mul,t,r), _1,_3) )
# 615 "bddapron/yacc.ml"
               : 'expr7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr7) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : (Apron.Texpr1.typ * Apron.Texpr1.round)) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr8) in
    Obj.repr(
# 115 "bddapron/yacc.mly"
    ( let (t,r) = _2 in `Binop(`Apron(Apron.Texpr1.Div,t,r), _1,_3) )
# 624 "bddapron/yacc.ml"
               : 'expr7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr7) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : (Apron.Texpr1.typ * Apron.Texpr1.round)) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr8) in
    Obj.repr(
# 117 "bddapron/yacc.mly"
    ( let (t,r) = _2 in `Binop(`Apron(Apron.Texpr1.Mod,t,r), _1,_3) )
# 633 "bddapron/yacc.ml"
               : 'expr7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : (Apron.Texpr1.typ * Apron.Texpr1.round)) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr8) in
    Obj.repr(
# 120 "bddapron/yacc.mly"
    ( let (t,r) = _1 in `Unop(`Apron(Apron.Texpr1.Cast,t,r),_2) )
# 641 "bddapron/yacc.ml"
               : 'expr8))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : (Apron.Texpr1.typ * Apron.Texpr1.round)) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr8) in
    Obj.repr(
# 122 "bddapron/yacc.mly"
    ( let (t,r) = _1 in `Unop(`Apron(Apron.Texpr1.Sqrt,t,r),_2) )
# 649 "bddapron/yacc.ml"
               : 'expr8))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : (Apron.Texpr1.typ * Apron.Texpr1.round)) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr8) in
    Obj.repr(
# 124 "bddapron/yacc.mly"
    ( let (t,r) = _1 in `Unop(`Apron(Apron.Texpr1.Neg,t,r),_2) )
# 657 "bddapron/yacc.ml"
               : 'expr8))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr0) in
    Obj.repr(
# 126 "bddapron/yacc.mly"
    ( _2 )
# 664 "bddapron/yacc.ml"
               : 'expr8))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cst) in
    Obj.repr(
# 128 "bddapron/yacc.mly"
   ( `Cst _1 )
# 671 "bddapron/yacc.ml"
               : 'expr8))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 130 "bddapron/yacc.mly"
    ( `Ref _1 )
# 678 "bddapron/yacc.ml"
               : 'expr8))
; (fun __caml_parser_env ->
    Obj.repr(
# 133 "bddapron/yacc.mly"
          ( `Bool )
# 684 "bddapron/yacc.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 134 "bddapron/yacc.mly"
         ( (`Int) )
# 690 "bddapron/yacc.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 135 "bddapron/yacc.mly"
          ( (`Real) )
# 696 "bddapron/yacc.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bint) in
    Obj.repr(
# 136 "bddapron/yacc.mly"
       ( (`Bint _1) )
# 703 "bddapron/yacc.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 137 "bddapron/yacc.mly"
        ( (`Benum _1) )
# 710 "bddapron/yacc.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'num) in
    Obj.repr(
# 140 "bddapron/yacc.mly"
(
  if _3 < 0 then
    raise (Error (Print.sprintf "Error: in type uint[x], x should be positive"))
  ;
  (false,_3)
)
# 722 "bddapron/yacc.ml"
               : 'bint))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'num) in
    Obj.repr(
# 147 "bddapron/yacc.mly"
(
  if _3 < 0 then
    raise (Error (Print.sprintf "Error: in type sint[x], x should be positive"))
  ;
  (true,_3)
)
# 734 "bddapron/yacc.ml"
               : 'bint))
(* Entry expr *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let expr (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : string Syntax.expr)
