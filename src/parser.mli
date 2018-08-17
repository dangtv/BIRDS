type token =
  | VAL of (int)
  | STRING of (string)
  | RELNAME of (string)
  | VARNAME of (string)
  | QMARK
  | UMARK
  | DOT
  | IMPLIEDBY
  | AND
  | NOT
  | EQ
  | NE
  | LE
  | GE
  | LT
  | GT
  | PLUS
  | MINUS
  | LPAREN
  | RPAREN
  | SEP
  | EOP
  | EOF
  | ANONVAR
  | ANON

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Expr.expr
