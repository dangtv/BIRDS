type token =
  | INT of (int)
  | FLOAT of (float)
  | STRING of (string)
  | RELNAME of (string)
  | VARNAME of (string)
  | QMARK
  | UMARK
  | DOT
  | IMPLIEDBY
  | AND
  | NOT
  | OR
  | BOT
  | TOP
  | NULL
  | EQ
  | NE
  | LE
  | GE
  | LT
  | GT
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | CONCAT
  | LPAREN
  | RPAREN
  | SEP
  | EOP
  | EOF
  | ANONVAR
  | ANON

open Parsing;;
let _ = parse_error;;
# 1 "src/parser.mly"
 (* OCaml preamble *)

  open Expr ;;
  open Utils ;;
  
   (* end preamble *)
 
# 47 "src/parser.ml"
let yytransl_const = [|
  262 (* QMARK *);
  263 (* UMARK *);
  264 (* DOT *);
  265 (* IMPLIEDBY *);
  266 (* AND *);
  267 (* NOT *);
  268 (* OR *);
  269 (* BOT *);
  270 (* TOP *);
  271 (* NULL *);
  272 (* EQ *);
  273 (* NE *);
  274 (* LE *);
  275 (* GE *);
  276 (* LT *);
  277 (* GT *);
  278 (* PLUS *);
  279 (* MINUS *);
  280 (* TIMES *);
  281 (* DIVIDE *);
  282 (* CONCAT *);
  283 (* LPAREN *);
  284 (* RPAREN *);
  285 (* SEP *);
  286 (* EOP *);
    0 (* EOF *);
  287 (* ANONVAR *);
  288 (* ANON *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* FLOAT *);
  259 (* STRING *);
  260 (* RELNAME *);
  261 (* VARNAME *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\002\000\002\000\003\000\003\000\003\000\
\004\000\004\000\004\000\004\000\005\000\005\000\005\000\008\000\
\008\000\009\000\009\000\006\000\006\000\006\000\007\000\007\000\
\007\000\011\000\011\000\011\000\011\000\011\000\012\000\012\000\
\012\000\012\000\012\000\010\000\010\000\010\000\010\000\013\000\
\013\000\013\000\013\000\013\000\013\000\015\000\015\000\015\000\
\016\000\016\000\016\000\016\000\016\000\016\000\014\000\014\000\
\014\000\014\000\018\000\018\000\018\000\018\000\018\000\017\000\
\017\000\000\000"

let yylen = "\002\000\
\001\000\002\000\001\000\001\000\001\000\001\000\002\000\001\000\
\001\000\001\000\001\000\001\000\004\000\004\000\001\000\001\000\
\001\000\001\000\001\000\003\000\003\000\001\000\003\000\003\000\
\001\000\000\000\001\000\003\000\003\000\001\000\001\000\002\000\
\001\000\002\000\001\000\004\000\005\000\005\000\001\000\003\000\
\003\000\003\000\003\000\003\000\003\000\001\000\001\000\001\000\
\001\000\002\000\001\000\002\000\001\000\001\000\000\000\001\000\
\003\000\001\000\001\000\001\000\001\000\001\000\001\000\004\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\066\000\000\000\000\000\006\000\009\000\010\000\011\000\
\000\000\016\000\000\000\039\000\000\000\000\000\000\000\000\000\
\002\000\000\000\007\000\000\000\054\000\049\000\051\000\053\000\
\000\000\000\000\060\000\000\000\061\000\062\000\000\000\020\000\
\021\000\023\000\024\000\000\000\000\000\000\000\000\000\000\000\
\000\000\031\000\000\000\027\000\033\000\000\000\047\000\000\000\
\050\000\052\000\036\000\000\000\000\000\000\000\000\000\032\000\
\034\000\013\000\014\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\057\000\037\000\038\000\000\000\
\028\000\029\000\054\000\040\000\041\000\044\000\045\000\042\000\
\043\000\064\000"

let yydgoto = "\002\000\
\010\000\011\000\012\000\013\000\014\000\015\000\016\000\017\000\
\049\000\050\000\051\000\052\000\053\000\036\000\054\000\037\000\
\038\000\039\000"

let yysindex = "\002\000\
\064\000\000\000\000\000\236\254\068\255\068\255\010\255\013\255\
\000\000\000\000\012\000\027\255\000\000\000\000\000\000\000\000\
\030\255\000\000\023\255\000\000\008\000\016\000\014\255\028\255\
\000\000\000\000\000\000\040\255\000\000\000\000\000\000\000\000\
\033\255\008\255\000\000\029\255\000\000\000\000\038\255\000\000\
\000\000\000\000\000\000\023\255\023\255\000\000\033\255\061\255\
\018\000\000\000\003\255\000\000\000\000\077\255\000\000\064\255\
\000\000\000\000\000\000\023\255\047\255\050\255\000\000\000\000\
\000\000\000\000\000\000\048\255\048\255\035\255\035\255\035\255\
\035\255\035\255\035\255\051\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\006\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\080\000\000\000\000\000\000\000\000\000\
\000\000\000\000\053\255\000\000\000\000\000\000\000\000\000\000\
\000\000\056\000\000\000\030\000\000\000\000\000\000\000\000\000\
\248\254\000\000\000\000\000\000\000\000\000\000\054\255\000\000\
\000\000\000\000\000\000\053\255\053\255\001\000\083\255\000\000\
\000\000\000\000\019\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\053\255\000\000\000\000\015\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\029\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\073\000\000\000\000\000\000\000\000\000\
\000\000\255\255\000\000\005\000\038\000\032\000\000\000\035\000\
\230\255\000\000"

let yytablesize = 343
let yytable = "\018\000\
\019\000\055\000\001\000\021\000\022\000\005\000\019\000\041\000\
\057\000\058\000\018\000\025\000\068\000\023\000\039\000\043\000\
\024\000\067\000\018\000\059\000\059\000\055\000\029\000\030\000\
\031\000\032\000\026\000\033\000\035\000\026\000\004\000\069\000\
\005\000\006\000\083\000\030\000\031\000\032\000\028\000\046\000\
\044\000\055\000\055\000\004\000\047\000\034\000\064\000\080\000\
\007\000\008\000\048\000\004\000\047\000\035\000\045\000\012\000\
\059\000\034\000\048\000\056\000\063\000\007\000\008\000\009\000\
\004\000\047\000\060\000\020\000\076\000\007\000\008\000\004\000\
\081\000\082\000\078\000\061\000\062\000\079\000\090\000\004\000\
\055\000\056\000\007\000\008\000\027\000\065\000\000\000\000\000\
\000\000\007\000\008\000\077\000\070\000\071\000\072\000\073\000\
\074\000\075\000\046\000\046\000\046\000\046\000\046\000\046\000\
\084\000\085\000\086\000\087\000\088\000\089\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\008\000\000\000\000\000\
\019\000\008\000\030\000\008\000\008\000\000\000\017\000\040\000\
\048\000\048\000\048\000\048\000\048\000\048\000\039\000\042\000\
\039\000\066\000\018\000\008\000\008\000\030\000\048\000\048\000\
\048\000\048\000\048\000\048\000\035\000\026\000\035\000\026\000\
\000\000\000\000\000\000\039\000\048\000\048\000\048\000\048\000\
\048\000\048\000\000\000\000\000\000\000\000\000\000\000\012\000\
\000\000\035\000\026\000\012\000\000\000\012\000\012\000\003\000\
\017\000\000\000\000\000\004\000\000\000\005\000\006\000\000\000\
\000\000\000\000\000\000\000\000\000\000\012\000\012\000\000\000\
\000\000\000\000\000\000\000\000\000\000\007\000\008\000"

let yycheck = "\001\000\
\000\000\028\000\001\000\005\000\006\000\000\000\027\001\000\000\
\001\001\002\001\012\000\000\000\010\001\004\001\000\000\000\000\
\004\001\000\000\000\000\028\001\029\001\048\000\000\001\001\001\
\002\001\003\001\000\001\005\001\000\000\000\000\004\001\029\001\
\006\001\007\001\000\001\001\001\002\001\003\001\009\001\000\001\
\027\001\068\000\069\000\004\001\005\001\023\001\048\000\000\001\
\022\001\023\001\011\001\004\001\005\001\031\001\027\001\000\000\
\028\001\023\001\011\001\027\001\000\001\022\001\023\001\000\000\
\004\001\005\001\029\001\000\001\005\001\022\001\023\001\004\001\
\068\000\069\000\028\001\044\000\045\000\028\001\028\001\000\000\
\028\001\028\001\022\001\023\001\012\000\048\000\255\255\255\255\
\255\255\022\001\023\001\060\000\016\001\017\001\018\001\019\001\
\020\001\021\001\016\001\017\001\018\001\019\001\020\001\021\001\
\070\000\071\000\072\000\073\000\074\000\075\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\000\001\255\255\255\255\
\008\001\004\001\010\001\006\001\007\001\255\255\009\001\008\001\
\016\001\017\001\018\001\019\001\020\001\021\001\008\001\008\001\
\010\001\008\001\008\001\022\001\023\001\029\001\016\001\017\001\
\018\001\019\001\020\001\021\001\008\001\008\001\010\001\010\001\
\255\255\255\255\255\255\029\001\016\001\017\001\018\001\019\001\
\020\001\021\001\255\255\255\255\255\255\255\255\255\255\000\001\
\255\255\029\001\029\001\004\001\255\255\006\001\007\001\000\001\
\009\001\255\255\255\255\004\001\255\255\006\001\007\001\255\255\
\255\255\255\255\255\255\255\255\255\255\022\001\023\001\255\255\
\255\255\255\255\255\255\255\255\255\255\022\001\023\001"

let yynames_const = "\
  QMARK\000\
  UMARK\000\
  DOT\000\
  IMPLIEDBY\000\
  AND\000\
  NOT\000\
  OR\000\
  BOT\000\
  TOP\000\
  NULL\000\
  EQ\000\
  NE\000\
  LE\000\
  GE\000\
  LT\000\
  GT\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  CONCAT\000\
  LPAREN\000\
  RPAREN\000\
  SEP\000\
  EOP\000\
  EOF\000\
  ANONVAR\000\
  ANON\000\
  "

let yynames_block = "\
  INT\000\
  FLOAT\000\
  STRING\000\
  RELNAME\000\
  VARNAME\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "src/parser.mly"
                    ( Prog [] )
# 297 "src/parser.ml"
               : Expr.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'program) in
    Obj.repr(
# 41 "src/parser.mly"
                                        ( Prog (List.rev _1)  )
# 304 "src/parser.ml"
               : Expr.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a main program" 1; )
# 310 "src/parser.ml"
               : Expr.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exprlist) in
    Obj.repr(
# 46 "src/parser.mly"
                  ( _1 )
# 317 "src/parser.ml"
               : 'program))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a program" 1; )
# 323 "src/parser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 51 "src/parser.mly"
                ( _1 :: []  )
# 330 "src/parser.ml"
               : 'exprlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exprlist) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 52 "src/parser.mly"
                        ( _2 :: _1 )
# 338 "src/parser.ml"
               : 'exprlist))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a list of rules" 1; )
# 344 "src/parser.ml"
               : 'exprlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rule) in
    Obj.repr(
# 57 "src/parser.mly"
                                     ( _1 )
# 351 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'query) in
    Obj.repr(
# 58 "src/parser.mly"
                                      ( _1 )
# 358 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'update) in
    Obj.repr(
# 59 "src/parser.mly"
                                       ( _1 )
# 365 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a rule or a declaration of query/base table" 1; )
# 371 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'head) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'body) in
    Obj.repr(
# 64 "src/parser.mly"
                             ( Rule (_1,_3) )
# 379 "src/parser.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'head) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'body) in
    Obj.repr(
# 65 "src/parser.mly"
                             ( spec_parse_error "miss a dot for a rule" 4; )
# 387 "src/parser.ml"
               : 'rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a rule" 1; )
# 393 "src/parser.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'predicate) in
    Obj.repr(
# 70 "src/parser.mly"
                 ( _1 )
# 400 "src/parser.ml"
               : 'head))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a head" 1; )
# 406 "src/parser.ml"
               : 'head))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'litlist) in
    Obj.repr(
# 75 "src/parser.mly"
               ( List.rev _1 )
# 413 "src/parser.ml"
               : 'body))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a body" 1; )
# 419 "src/parser.ml"
               : 'body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'predicate) in
    Obj.repr(
# 80 "src/parser.mly"
                            ( Query _2 )
# 426 "src/parser.ml"
               : 'query))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'predicate) in
    Obj.repr(
# 81 "src/parser.mly"
                            ( spec_parse_error "miss a dot for a query" 3; )
# 433 "src/parser.ml"
               : 'query))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a query" 1; )
# 439 "src/parser.ml"
               : 'query))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'predicate) in
    Obj.repr(
# 86 "src/parser.mly"
                            ( Base _2 )
# 446 "src/parser.ml"
               : 'update))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'predicate) in
    Obj.repr(
# 87 "src/parser.mly"
                                    ( spec_parse_error "miss a dot for a base relation" 3; )
# 453 "src/parser.ml"
               : 'update))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a base relation" 1; )
# 459 "src/parser.ml"
               : 'update))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "src/parser.mly"
                           ( [] )
# 465 "src/parser.ml"
               : 'litlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'literal) in
    Obj.repr(
# 92 "src/parser.mly"
                 ( _1 :: [] )
# 472 "src/parser.ml"
               : 'litlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'litlist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'literal) in
    Obj.repr(
# 93 "src/parser.mly"
                            ( _3 :: _1 )
# 480 "src/parser.ml"
               : 'litlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'litlist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'literal) in
    Obj.repr(
# 94 "src/parser.mly"
                                   ( _3 :: _1 )
# 488 "src/parser.ml"
               : 'litlist))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a conjunction of literals" 1; )
# 494 "src/parser.ml"
               : 'litlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'predicate) in
    Obj.repr(
# 99 "src/parser.mly"
                    ( Rel _1 )
# 501 "src/parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'predicate) in
    Obj.repr(
# 100 "src/parser.mly"
                        ( Not _2 )
# 508 "src/parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'equation) in
    Obj.repr(
# 101 "src/parser.mly"
                   ( _1 )
# 515 "src/parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'equation) in
    Obj.repr(
# 102 "src/parser.mly"
                             ( negate_eq _2 )
# 522 "src/parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a literal" 1; )
# 528 "src/parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'varlist) in
    Obj.repr(
# 107 "src/parser.mly"
                                   ( Pred (_1, _3) )
# 536 "src/parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'varlist) in
    Obj.repr(
# 108 "src/parser.mly"
                                        ( Deltainsert (_2, _4) )
# 544 "src/parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'varlist) in
    Obj.repr(
# 109 "src/parser.mly"
                                         ( Deltadelete (_2, _4) )
# 552 "src/parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a predicate" 1; )
# 558 "src/parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var_or_agg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 114 "src/parser.mly"
                           ( Equal (_1, _3) )
# 566 "src/parser.ml"
               : 'equation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var_or_agg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 115 "src/parser.mly"
                           ( Ineq ("<>", _1, _3) )
# 574 "src/parser.ml"
               : 'equation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var_or_agg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 116 "src/parser.mly"
                           ( Ineq ( "<", _1, _3) )
# 582 "src/parser.ml"
               : 'equation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var_or_agg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 117 "src/parser.mly"
                           ( Ineq ( ">", _1, _3) )
# 590 "src/parser.ml"
               : 'equation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var_or_agg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 118 "src/parser.mly"
                           ( Ineq ("<=", _1, _3) )
# 598 "src/parser.ml"
               : 'equation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var_or_agg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 119 "src/parser.mly"
                           ( Ineq (">=", _1, _3) )
# 606 "src/parser.ml"
               : 'equation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 123 "src/parser.mly"
                ( NamedVar _1 )
# 613 "src/parser.ml"
               : 'var_or_agg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aggregate) in
    Obj.repr(
# 124 "src/parser.mly"
                ( _1 )
# 620 "src/parser.ml"
               : 'var_or_agg))
; (fun __caml_parser_env ->
    Obj.repr(
# 125 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a var or a aggreation" 1; )
# 626 "src/parser.ml"
               : 'var_or_agg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 129 "src/parser.mly"
                      (Int _1)
# 633 "src/parser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 130 "src/parser.mly"
                            (Int (- _2))
# 640 "src/parser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 131 "src/parser.mly"
                        (Real _1)
# 647 "src/parser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 132 "src/parser.mly"
                              (Real (-. _2))
# 654 "src/parser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 133 "src/parser.mly"
                      (String _1)
# 661 "src/parser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    Obj.repr(
# 134 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a constant" 1; )
# 667 "src/parser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    Obj.repr(
# 137 "src/parser.mly"
                           ( [] )
# 673 "src/parser.ml"
               : 'varlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 138 "src/parser.mly"
                   ( _1 :: [] )
# 680 "src/parser.ml"
               : 'varlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'varlist) in
    Obj.repr(
# 139 "src/parser.mly"
                         ( _1 :: _3 )
# 688 "src/parser.ml"
               : 'varlist))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a list of variables" 1; )
# 694 "src/parser.ml"
               : 'varlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 144 "src/parser.mly"
                ( NamedVar _1 )
# 701 "src/parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    Obj.repr(
# 145 "src/parser.mly"
                ( AnonVar )
# 707 "src/parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 146 "src/parser.mly"
                ( ConstVar _1 )
# 714 "src/parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aggregate) in
    Obj.repr(
# 147 "src/parser.mly"
                 ( _1 )
# 721 "src/parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    Obj.repr(
# 148 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a variables" 1; )
# 727 "src/parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 152 "src/parser.mly"
                                        ( AggVar (_1,_3) )
# 735 "src/parser.ml"
               : 'aggregate))
; (fun __caml_parser_env ->
    Obj.repr(
# 153 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a aggregation" 1; )
# 741 "src/parser.ml"
               : 'aggregate))
(* Entry main *)
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
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Expr.expr)
