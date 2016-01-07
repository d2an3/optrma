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

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string Syntax.expr
