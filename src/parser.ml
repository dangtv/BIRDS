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

open Parsing;;
let _ = parse_error;;
# 1 "src/parser.mly"
 (* OCaml preamble *)

  open Expr ;;
  open Utils;;
  
   (* end preamble *)
 
# 40 "src/parser.ml"
let yytransl_const = [|
  262 (* QMARK *);
  263 (* UMARK *);
  264 (* DOT *);
  265 (* IMPLIEDBY *);
  266 (* AND *);
  267 (* NOT *);
  268 (* EQ *);
  269 (* NE *);
  270 (* LE *);
  271 (* GE *);
  272 (* LT *);
  273 (* GT *);
  274 (* PLUS *);
  275 (* MINUS *);
  276 (* LPAREN *);
  277 (* RPAREN *);
  278 (* SEP *);
  279 (* EOP *);
    0 (* EOF *);
  280 (* ANONVAR *);
  281 (* ANON *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* FLOAT *);
  259 (* STRING *);
  260 (* RELNAME *);
  261 (* VARNAME *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\003\000\003\000\004\000\004\000\004\000\
\005\000\005\000\008\000\008\000\009\000\009\000\006\000\006\000\
\007\000\007\000\011\000\011\000\011\000\011\000\011\000\012\000\
\012\000\012\000\012\000\012\000\010\000\010\000\010\000\010\000\
\013\000\013\000\013\000\013\000\013\000\013\000\015\000\015\000\
\016\000\016\000\016\000\016\000\014\000\014\000\014\000\014\000\
\018\000\018\000\018\000\018\000\017\000\000\000"

let yylen = "\002\000\
\001\000\002\000\001\000\001\000\002\000\001\000\001\000\001\000\
\004\000\001\000\001\000\001\000\001\000\001\000\003\000\001\000\
\003\000\001\000\000\000\001\000\003\000\003\000\001\000\001\000\
\002\000\001\000\002\000\001\000\004\000\005\000\005\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\001\000\001\000\
\001\000\001\000\001\000\001\000\000\000\001\000\003\000\001\000\
\001\000\001\000\001\000\001\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\054\000\000\000\000\000\004\000\006\000\007\000\008\000\
\000\000\011\000\000\000\032\000\000\000\000\000\000\000\000\000\
\002\000\005\000\000\000\044\000\041\000\042\000\043\000\000\000\
\050\000\000\000\051\000\052\000\000\000\015\000\017\000\000\000\
\000\000\000\000\000\000\000\000\000\000\024\000\000\000\020\000\
\026\000\000\000\040\000\000\000\029\000\000\000\000\000\000\000\
\025\000\027\000\009\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\047\000\030\000\031\000\028\000\
\021\000\022\000\044\000\033\000\034\000\037\000\038\000\035\000\
\036\000\053\000"

let yydgoto = "\002\000\
\010\000\011\000\012\000\013\000\014\000\015\000\016\000\017\000\
\045\000\046\000\047\000\048\000\049\000\034\000\050\000\035\000\
\036\000\037\000"

let yysindex = "\011\000\
\009\000\000\000\000\000\242\254\049\255\049\255\006\255\019\255\
\000\000\000\000\017\000\020\255\000\000\000\000\000\000\000\000\
\016\255\000\000\013\255\000\000\023\255\048\255\008\255\038\255\
\000\000\000\000\029\255\000\000\000\000\000\000\000\000\043\255\
\000\000\071\255\000\000\000\000\072\255\000\000\000\000\013\255\
\013\255\000\000\043\255\003\255\085\255\000\000\032\255\000\000\
\000\000\058\255\000\000\090\255\000\000\013\255\075\255\076\255\
\000\000\000\000\000\000\041\255\041\255\088\255\088\255\088\255\
\088\255\088\255\088\255\077\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\099\000\000\000\000\000\000\000\000\000\
\000\000\000\000\079\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\022\255\000\000\000\000\000\000\000\000\040\255\
\000\000\000\000\000\000\000\000\080\255\000\000\000\000\079\255\
\079\255\047\255\064\255\000\000\000\000\000\000\094\255\000\000\
\000\000\000\000\000\000\000\000\000\000\079\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\091\000\000\000\000\000\000\000\000\000\
\000\000\255\255\000\000\005\000\060\000\010\000\000\000\020\000\
\231\255\000\000"

let yytablesize = 284
let yytable = "\018\000\
\010\000\051\000\020\000\021\000\022\000\019\000\004\000\043\000\
\009\000\023\000\018\000\001\000\028\000\029\000\030\000\031\000\
\025\000\032\000\051\000\003\000\007\000\008\000\024\000\004\000\
\027\000\005\000\006\000\040\000\042\000\019\000\038\000\019\000\
\004\000\043\000\051\000\051\000\033\000\007\000\008\000\044\000\
\072\000\060\000\057\000\019\000\004\000\043\000\007\000\008\000\
\020\000\055\000\056\000\044\000\004\000\061\000\014\000\039\000\
\023\000\041\000\007\000\008\000\049\000\049\000\052\000\069\000\
\073\000\074\000\007\000\008\000\023\000\062\000\063\000\064\000\
\065\000\066\000\067\000\039\000\039\000\039\000\039\000\039\000\
\039\000\076\000\077\000\078\000\079\000\080\000\081\000\075\000\
\029\000\030\000\031\000\053\000\059\000\054\000\068\000\070\000\
\071\000\082\000\003\000\045\000\046\000\013\000\026\000\058\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\010\000\000\000\000\000\000\000\010\000\000\000\010\000\010\000\
\003\000\012\000\000\000\000\000\004\000\000\000\005\000\006\000\
\000\000\000\000\010\000\010\000\000\000\000\000\000\000\000\000\
\000\000\000\000\007\000\008\000"

let yycheck = "\001\000\
\000\000\027\000\000\001\005\000\006\000\020\001\004\001\005\001\
\000\000\004\001\012\000\001\000\000\001\001\001\002\001\003\001\
\000\000\005\001\044\000\000\001\018\001\019\001\004\001\004\001\
\009\001\006\001\007\001\020\001\000\001\008\001\008\001\010\001\
\004\001\005\001\060\000\061\000\024\001\018\001\019\001\011\001\
\000\001\010\001\044\000\022\001\004\001\005\001\018\001\019\001\
\000\001\040\000\041\000\011\001\004\001\022\001\008\001\008\001\
\010\001\020\001\018\001\019\001\021\001\022\001\020\001\054\000\
\060\000\061\000\018\001\019\001\022\001\012\001\013\001\014\001\
\015\001\016\001\017\001\012\001\013\001\014\001\015\001\016\001\
\017\001\062\000\063\000\064\000\065\000\066\000\067\000\000\001\
\001\001\002\001\003\001\021\001\008\001\022\001\005\001\021\001\
\021\001\021\001\000\000\021\001\021\001\008\001\012\000\044\000\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\001\255\255\255\255\255\255\004\001\255\255\006\001\007\001\
\000\001\009\001\255\255\255\255\004\001\255\255\006\001\007\001\
\255\255\255\255\018\001\019\001\255\255\255\255\255\255\255\255\
\255\255\255\255\018\001\019\001"

let yynames_const = "\
  QMARK\000\
  UMARK\000\
  DOT\000\
  IMPLIEDBY\000\
  AND\000\
  NOT\000\
  EQ\000\
  NE\000\
  LE\000\
  GE\000\
  LT\000\
  GT\000\
  PLUS\000\
  MINUS\000\
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
# 39 "src/parser.mly"
                    ( Prog [] )
# 255 "src/parser.ml"
               : Expr.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'program) in
    Obj.repr(
# 40 "src/parser.mly"
                                        ( Prog (List.rev _1)  )
# 262 "src/parser.ml"
               : Expr.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exprlist) in
    Obj.repr(
# 44 "src/parser.mly"
                  ( _1 )
# 269 "src/parser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 48 "src/parser.mly"
                ( _1 :: []  )
# 276 "src/parser.ml"
               : 'exprlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exprlist) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 49 "src/parser.mly"
                        ( _2 :: _1 )
# 284 "src/parser.ml"
               : 'exprlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rule) in
    Obj.repr(
# 53 "src/parser.mly"
                                     ( _1 )
# 291 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'query) in
    Obj.repr(
# 54 "src/parser.mly"
                                      ( _1 )
# 298 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'update) in
    Obj.repr(
# 55 "src/parser.mly"
                                       ( _1 )
# 305 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'head) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'body) in
    Obj.repr(
# 59 "src/parser.mly"
                             ( Rule (_1,_3) )
# 313 "src/parser.ml"
               : 'rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a rule" 1; )
# 319 "src/parser.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'predicate) in
    Obj.repr(
# 64 "src/parser.mly"
                 ( _1 )
# 326 "src/parser.ml"
               : 'head))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a head" 1; )
# 332 "src/parser.ml"
               : 'head))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'litlist) in
    Obj.repr(
# 69 "src/parser.mly"
               ( List.rev _1 )
# 339 "src/parser.ml"
               : 'body))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a body" 1; )
# 345 "src/parser.ml"
               : 'body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'predicate) in
    Obj.repr(
# 74 "src/parser.mly"
                            ( Query _2 )
# 352 "src/parser.ml"
               : 'query))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a query" 1; )
# 358 "src/parser.ml"
               : 'query))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'predicate) in
    Obj.repr(
# 79 "src/parser.mly"
                            ( Base _2 )
# 365 "src/parser.ml"
               : 'update))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a base relation" 1; )
# 371 "src/parser.ml"
               : 'update))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "src/parser.mly"
                           ( [] )
# 377 "src/parser.ml"
               : 'litlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'literal) in
    Obj.repr(
# 84 "src/parser.mly"
                 ( _1 :: [] )
# 384 "src/parser.ml"
               : 'litlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'litlist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'literal) in
    Obj.repr(
# 85 "src/parser.mly"
                            ( _3 :: _1 )
# 392 "src/parser.ml"
               : 'litlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'litlist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'literal) in
    Obj.repr(
# 86 "src/parser.mly"
                                   ( _3 :: _1 )
# 400 "src/parser.ml"
               : 'litlist))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a conjunction of literals" 1; )
# 406 "src/parser.ml"
               : 'litlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'predicate) in
    Obj.repr(
# 91 "src/parser.mly"
                    ( Rel _1 )
# 413 "src/parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'predicate) in
    Obj.repr(
# 92 "src/parser.mly"
                        ( Not _2 )
# 420 "src/parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'equation) in
    Obj.repr(
# 93 "src/parser.mly"
                   ( _1 )
# 427 "src/parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'equation) in
    Obj.repr(
# 94 "src/parser.mly"
                             ( negate_eq _2 )
# 434 "src/parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a literal" 1; )
# 440 "src/parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'varlist) in
    Obj.repr(
# 99 "src/parser.mly"
                                   ( Pred (_1, _3) )
# 448 "src/parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'varlist) in
    Obj.repr(
# 100 "src/parser.mly"
                                        ( Deltainsert (_2, _4) )
# 456 "src/parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'varlist) in
    Obj.repr(
# 101 "src/parser.mly"
                                         ( Deltadelete (_2, _4) )
# 464 "src/parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a predicate" 1; )
# 470 "src/parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var_or_agg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 106 "src/parser.mly"
                           ( Equal (_1, _3) )
# 478 "src/parser.ml"
               : 'equation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var_or_agg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 107 "src/parser.mly"
                           ( Ineq ("<>", _1, _3) )
# 486 "src/parser.ml"
               : 'equation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var_or_agg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 108 "src/parser.mly"
                           ( Ineq ( "<", _1, _3) )
# 494 "src/parser.ml"
               : 'equation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var_or_agg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 109 "src/parser.mly"
                           ( Ineq ( ">", _1, _3) )
# 502 "src/parser.ml"
               : 'equation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var_or_agg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 110 "src/parser.mly"
                           ( Ineq ("<=", _1, _3) )
# 510 "src/parser.ml"
               : 'equation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var_or_agg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 111 "src/parser.mly"
                           ( Ineq (">=", _1, _3) )
# 518 "src/parser.ml"
               : 'equation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 115 "src/parser.mly"
                ( NamedVar _1 )
# 525 "src/parser.ml"
               : 'var_or_agg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aggregate) in
    Obj.repr(
# 116 "src/parser.mly"
                ( _1 )
# 532 "src/parser.ml"
               : 'var_or_agg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 120 "src/parser.mly"
                      (Int _1)
# 539 "src/parser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 121 "src/parser.mly"
                        (Real _1)
# 546 "src/parser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 122 "src/parser.mly"
                      (String _1)
# 553 "src/parser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    Obj.repr(
# 123 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a constant" 1; )
# 559 "src/parser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "src/parser.mly"
                           ( [] )
# 565 "src/parser.ml"
               : 'varlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 127 "src/parser.mly"
                   ( _1 :: [] )
# 572 "src/parser.ml"
               : 'varlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'varlist) in
    Obj.repr(
# 128 "src/parser.mly"
                         ( _1 :: _3 )
# 580 "src/parser.ml"
               : 'varlist))
; (fun __caml_parser_env ->
    Obj.repr(
# 129 "src/parser.mly"
                      ( spec_parse_error "invalid syntax for a list of variables" 1; )
# 586 "src/parser.ml"
               : 'varlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 133 "src/parser.mly"
                ( NamedVar _1 )
# 593 "src/parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    Obj.repr(
# 134 "src/parser.mly"
                ( AnonVar )
# 599 "src/parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 135 "src/parser.mly"
                ( ConstVar _1 )
# 606 "src/parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aggregate) in
    Obj.repr(
# 136 "src/parser.mly"
                 ( _1 )
# 613 "src/parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 140 "src/parser.mly"
                                        ( AggVar (_1,_3) )
# 621 "src/parser.ml"
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
