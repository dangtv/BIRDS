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

open Parsing;;
let _ = parse_error;;
# 1 "src/parser.mly"
 (* OCaml preamble *)

  open Expr ;;
  
   (* end preamble *)
 
# 38 "src/parser.ml"
let yytransl_const = [|
  261 (* QMARK *);
  262 (* UMARK *);
  263 (* DOT *);
  264 (* IMPLIEDBY *);
  265 (* AND *);
  266 (* NOT *);
  267 (* EQ *);
  268 (* NE *);
  269 (* LE *);
  270 (* GE *);
  271 (* LT *);
  272 (* GT *);
  273 (* PLUS *);
  274 (* MINUS *);
  275 (* LPAREN *);
  276 (* RPAREN *);
  277 (* SEP *);
  278 (* EOP *);
    0 (* EOF *);
  279 (* ANONVAR *);
  280 (* ANON *);
    0|]

let yytransl_block = [|
  257 (* VAL *);
  258 (* STRING *);
  259 (* RELNAME *);
  260 (* VARNAME *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\003\000\003\000\004\000\004\000\004\000\
\004\000\008\000\005\000\010\000\011\000\006\000\007\000\013\000\
\013\000\013\000\013\000\009\000\009\000\009\000\009\000\012\000\
\012\000\012\000\014\000\014\000\014\000\014\000\014\000\014\000\
\016\000\016\000\017\000\017\000\015\000\015\000\015\000\019\000\
\019\000\019\000\019\000\018\000\000\000"

let yylen = "\002\000\
\001\000\002\000\001\000\001\000\002\000\001\000\001\000\001\000\
\001\000\001\000\004\000\001\000\001\000\003\000\003\000\000\000\
\001\000\003\000\003\000\001\000\002\000\001\000\002\000\004\000\
\005\000\005\000\003\000\003\000\003\000\003\000\003\000\003\000\
\001\000\001\000\001\000\001\000\000\000\001\000\003\000\001\000\
\001\000\001\000\001\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\045\000\000\000\000\000\004\000\006\000\007\000\
\008\000\009\000\010\000\000\000\000\000\022\000\000\000\034\000\
\000\000\000\000\000\000\000\000\021\000\023\000\000\000\000\000\
\002\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\035\000\036\000\000\000\041\000\000\000\042\000\043\000\
\000\000\000\000\014\000\015\000\000\000\000\000\017\000\000\000\
\020\000\000\000\027\000\028\000\031\000\032\000\029\000\030\000\
\024\000\000\000\044\000\000\000\000\000\011\000\000\000\000\000\
\039\000\025\000\026\000\018\000\019\000"

let yydgoto = "\002\000\
\011\000\012\000\013\000\014\000\015\000\016\000\017\000\018\000\
\019\000\020\000\056\000\057\000\058\000\022\000\046\000\023\000\
\047\000\024\000\049\000"

let yysindex = "\004\000\
\010\000\000\000\246\254\248\254\015\255\015\255\004\255\017\255\
\021\255\000\000\000\000\037\000\009\255\000\000\000\000\000\000\
\000\000\000\000\000\000\043\255\000\000\000\000\045\255\000\000\
\002\255\070\255\068\255\069\255\000\000\000\000\058\255\059\255\
\000\000\000\000\013\255\048\255\048\255\048\255\048\255\048\255\
\048\255\000\000\000\000\248\254\000\000\060\255\000\000\000\000\
\061\255\063\255\000\000\000\000\002\255\002\255\000\000\072\255\
\000\000\026\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\002\255\000\000\064\255\065\255\000\000\013\255\013\255\
\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\051\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\081\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\066\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\027\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\033\255\000\000\000\000\000\000\000\000\
\067\255\000\000\000\000\000\000\066\255\066\255\000\000\000\000\
\000\000\081\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\066\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\076\000\000\000\000\000\000\000\000\000\
\223\255\000\000\000\000\039\000\000\000\083\000\245\255\000\000\
\032\000\231\255\000\000"

let yytablesize = 284
let yytable = "\048\000\
\020\000\055\000\042\000\043\000\001\000\044\000\003\000\004\000\
\025\000\010\000\026\000\003\000\004\000\005\000\006\000\003\000\
\004\000\003\000\007\000\031\000\008\000\009\000\007\000\032\000\
\045\000\008\000\009\000\048\000\048\000\008\000\009\000\008\000\
\009\000\016\000\071\000\016\000\033\000\076\000\077\000\021\000\
\048\000\068\000\069\000\027\000\028\000\029\000\072\000\016\000\
\042\000\043\000\035\000\021\000\040\000\040\000\073\000\036\000\
\037\000\038\000\039\000\040\000\041\000\033\000\033\000\033\000\
\033\000\033\000\033\000\059\000\060\000\061\000\062\000\063\000\
\064\000\050\000\051\000\052\000\053\000\054\000\070\000\065\000\
\003\000\066\000\067\000\074\000\075\000\037\000\038\000\013\000\
\034\000\030\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\020\000\020\000\020\000\020\000\000\000\
\012\000\000\000\020\000\000\000\003\000\004\000\005\000\006\000\
\000\000\020\000\020\000\007\000\000\000\000\000\000\000\000\000\
\000\000\000\000\008\000\009\000"

let yycheck = "\025\000\
\000\000\035\000\001\001\002\001\001\000\004\001\003\001\004\001\
\019\001\000\000\019\001\003\001\004\001\005\001\006\001\003\001\
\004\001\003\001\010\001\003\001\017\001\018\001\010\001\003\001\
\023\001\017\001\018\001\053\000\054\000\017\001\018\001\017\001\
\018\001\007\001\009\001\009\001\000\000\071\000\072\000\001\000\
\066\000\053\000\054\000\005\000\006\000\007\000\021\001\021\001\
\001\001\002\001\008\001\013\000\020\001\021\001\066\000\011\001\
\012\001\013\001\014\001\015\001\016\001\011\001\012\001\013\001\
\014\001\015\001\016\001\036\000\037\000\038\000\039\000\040\000\
\041\000\004\001\007\001\007\001\019\001\019\001\007\001\020\001\
\000\000\021\001\020\001\020\001\020\001\020\001\020\001\007\001\
\013\000\007\000\255\255\255\255\255\255\255\255\255\255\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\004\001\005\001\006\001\255\255\
\008\001\255\255\010\001\255\255\003\001\004\001\005\001\006\001\
\255\255\017\001\018\001\010\001\255\255\255\255\255\255\255\255\
\255\255\255\255\017\001\018\001"

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
  VAL\000\
  STRING\000\
  RELNAME\000\
  VARNAME\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "src/parser.mly"
                    ( Prog [] )
# 246 "src/parser.ml"
               : Expr.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'program) in
    Obj.repr(
# 38 "src/parser.mly"
                                        ( Prog (List.rev _1)  )
# 253 "src/parser.ml"
               : Expr.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exprlist) in
    Obj.repr(
# 42 "src/parser.mly"
                  ( _1 )
# 260 "src/parser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 46 "src/parser.mly"
                ( _1 :: []  )
# 267 "src/parser.ml"
               : 'exprlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exprlist) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 47 "src/parser.mly"
                        ( _2 :: _1 )
# 275 "src/parser.ml"
               : 'exprlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rule) in
    Obj.repr(
# 51 "src/parser.mly"
                                     ( _1 )
# 282 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'query) in
    Obj.repr(
# 52 "src/parser.mly"
                                      ( _1 )
# 289 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'update) in
    Obj.repr(
# 53 "src/parser.mly"
                                       ( _1 )
# 296 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fact) in
    Obj.repr(
# 54 "src/parser.mly"
                ( failwith "fact: to be implemented" )
# 303 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'literal) in
    Obj.repr(
# 58 "src/parser.mly"
                                        ( _1 )
# 310 "src/parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'head) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'body) in
    Obj.repr(
# 62 "src/parser.mly"
                             ( Rule (_1,_3) )
# 318 "src/parser.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'predicate) in
    Obj.repr(
# 66 "src/parser.mly"
                 ( _1 )
# 325 "src/parser.ml"
               : 'head))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'litlist) in
    Obj.repr(
# 70 "src/parser.mly"
               ( List.rev _1 )
# 332 "src/parser.ml"
               : 'body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'predicate) in
    Obj.repr(
# 74 "src/parser.mly"
                            ( Query _2 )
# 339 "src/parser.ml"
               : 'query))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'predicate) in
    Obj.repr(
# 78 "src/parser.mly"
                            ( Base _2 )
# 346 "src/parser.ml"
               : 'update))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "src/parser.mly"
                           ( [] )
# 352 "src/parser.ml"
               : 'litlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'literal) in
    Obj.repr(
# 82 "src/parser.mly"
                 ( _1 :: [] )
# 359 "src/parser.ml"
               : 'litlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'litlist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'literal) in
    Obj.repr(
# 83 "src/parser.mly"
                            ( _3 :: _1 )
# 367 "src/parser.ml"
               : 'litlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'litlist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'literal) in
    Obj.repr(
# 84 "src/parser.mly"
                                   ( _3 :: _1 )
# 375 "src/parser.ml"
               : 'litlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'predicate) in
    Obj.repr(
# 88 "src/parser.mly"
                    ( Rel _1 )
# 382 "src/parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'predicate) in
    Obj.repr(
# 89 "src/parser.mly"
                        ( Not _2 )
# 389 "src/parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'equation) in
    Obj.repr(
# 90 "src/parser.mly"
                   ( _1 )
# 396 "src/parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'equation) in
    Obj.repr(
# 91 "src/parser.mly"
                             ( negate_eq _2 )
# 403 "src/parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'varlist) in
    Obj.repr(
# 95 "src/parser.mly"
                                   ( Pred (_1, _3) )
# 411 "src/parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'varlist) in
    Obj.repr(
# 96 "src/parser.mly"
                                        ( Deltainsert (_2, _4) )
# 419 "src/parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'varlist) in
    Obj.repr(
# 97 "src/parser.mly"
                                         ( Deltadelete (_2, _4) )
# 427 "src/parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var_or_agg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 101 "src/parser.mly"
                           ( Equal (_1, _3) )
# 435 "src/parser.ml"
               : 'equation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var_or_agg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 102 "src/parser.mly"
                           ( Ineq ("<>", _1, _3) )
# 443 "src/parser.ml"
               : 'equation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var_or_agg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 103 "src/parser.mly"
                           ( Ineq ( "<", _1, _3) )
# 451 "src/parser.ml"
               : 'equation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var_or_agg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 104 "src/parser.mly"
                           ( Ineq ( ">", _1, _3) )
# 459 "src/parser.ml"
               : 'equation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var_or_agg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 105 "src/parser.mly"
                           ( Ineq ("<=", _1, _3) )
# 467 "src/parser.ml"
               : 'equation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var_or_agg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 106 "src/parser.mly"
                           ( Ineq (">=", _1, _3) )
# 475 "src/parser.ml"
               : 'equation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 110 "src/parser.mly"
                ( NamedVar _1 )
# 482 "src/parser.ml"
               : 'var_or_agg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aggregate) in
    Obj.repr(
# 111 "src/parser.mly"
                ( _1 )
# 489 "src/parser.ml"
               : 'var_or_agg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 115 "src/parser.mly"
                      (Int _1)
# 496 "src/parser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 116 "src/parser.mly"
                      (String _1)
# 503 "src/parser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "src/parser.mly"
                           ( [] )
# 509 "src/parser.ml"
               : 'varlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 120 "src/parser.mly"
                   ( _1 :: [] )
# 516 "src/parser.ml"
               : 'varlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'varlist) in
    Obj.repr(
# 121 "src/parser.mly"
                         ( _1 :: _3 )
# 524 "src/parser.ml"
               : 'varlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 125 "src/parser.mly"
                ( NamedVar _1 )
# 531 "src/parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "src/parser.mly"
                ( AnonVar )
# 537 "src/parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 127 "src/parser.mly"
                ( ConstVar _1 )
# 544 "src/parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aggregate) in
    Obj.repr(
# 128 "src/parser.mly"
                 ( _1 )
# 551 "src/parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 132 "src/parser.mly"
                                        ( AggVar (_1,_3) )
# 559 "src/parser.ml"
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
