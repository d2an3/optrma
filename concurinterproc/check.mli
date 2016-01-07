(** Spl language: parsing, building AST and checking it is well-formed *)

val controllabels : Syntax.var Syntax.program -> string Sette.t * string Sette.t list
val parse_and_check : Format.formatter -> Lexing.lexbuf -> Syntax.var Syntax.program
