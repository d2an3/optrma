type token =
  | TK_BOOL
  | TK_UINT
  | TK_SINT
  | TK_IN
  | TK_LPAR
  | TK_RPAR
  | TK_LBRACE
  | TK_RBRACE
  | TK_LBRACKET
  | TK_RBRACKET
  | TK_RANDOM
  | TK_ASSUME
  | TK_HALT
  | TK_FAIL
  | TK_VAR
  | TK_DONE
  | TK_ENDIF
  | TK_SKIP
  | TK_YIELD
  | TK_INITIAL
  | TK_INLINE
  | TK_ATOMIC
  | TK_TYPEDEF
  | TK_ENUM
  | TK_PROC
  | TK_THREAD
  | TK_WHILE
  | TK_IF
  | TK_GOTO
  | TK_MUL of ((Apron.Texpr1.typ * Apron.Texpr1.round))
  | TK_ADD of ((Apron.Texpr1.typ * Apron.Texpr1.round))
  | TK_SUB of ((Apron.Texpr1.typ * Apron.Texpr1.round))
  | TK_DIV of ((Apron.Texpr1.typ * Apron.Texpr1.round))
  | TK_MODULO of ((Apron.Texpr1.typ * Apron.Texpr1.round))
  | TK_CAST of ((Apron.Texpr1.typ * Apron.Texpr1.round))
  | TK_SQRT of ((Apron.Texpr1.typ * Apron.Texpr1.round))
  | TK_MPQF of (Mpqf.t)
  | TK_FLOAT of (float)
  | TK_PLUS
  | TK_MINUS
  | TK_TIMES
  | TK_LEQ
  | TK_GEQ
  | TK_LT
  | TK_GT
  | TK_EQ
  | TK_NEQ
  | TK_AF
  | TK_AND
  | TK_OR
  | TK_NOT
  | TK_COMMA
  | TK_RETURNS
  | TK_COLON
  | TK_INT
  | TK_REAL
  | TK_LABEL of (string)
  | TK_ID of (string)
  | TK_DO of (Syntax.pos)
  | TK_THEN of (Syntax.pos)
  | TK_ELSE of (Syntax.pos)
  | TK_BEGIN of (Syntax.pos)
  | TK_END of (Syntax.pos)
  | TK_SEMICOLON of (Syntax.pos)
  | TK_TRUE
  | TK_FALSE
  | TK_EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string Syntax.program
