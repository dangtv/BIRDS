%{ (* OCaml preamble *)

  open Expr ;;
  
   (* end preamble *)
 %}
  

/* tokens declaration */

%token <int> VAL            /* token with int value    */
%token <string> STRING            /* token with string value    */
%token <string> RELNAME       /* token with string value */
%token <string> VARNAME         /* token with string value */

  
%token QMARK UMARK DOT IMPLIEDBY
%token AND NOT
%token EQ
%token NE LE GE LT GT
%token PLUS MINUS
%token LPAREN RPAREN SEP
%token EOP
%token EOF
%token ANONVAR /* anonymous variable */
%token ANON   /* fake token to stop the grammar in the fact rule */

/* associativity and precedence when needed */
%nonassoc IMPLIEDBY


%start main               /*  entry point    */
%type <Expr.expr> main
%%

/* Grammar */
  main:	EOF	        { Prog [] }
  | program  EOF                        { Prog (List.rev $1)  }
  ;
  
  program: 
  exprlist								{ $1 }
  ;

  exprlist:
  | expr								{ $1 :: []  }
  | exprlist expr 						{ $2 :: $1 }
  ;

  expr: 
  | rule	                            { $1 }
  | query	                            { $1 }
  | update	                            { $1 }
  | fact				    { failwith "fact: to be implemented" }
  ;
  
  fact:
  literal                               { $1 }
  ;

  rule:
  head IMPLIEDBY body DOT				{ Rule ($1,$3) }
  ; 

  head:
  predicate						{ $1 }
  ;

  body:
  litlist						{ List.rev $1 }
  ;

  query:
  | QMARK predicate DOT					{ Query $2 } 
  ;

  update:
  | UMARK predicate DOT					{ Base $2 } 
  ;

  litlist: /* empty */					{ [] }
  | literal						{ $1 :: [] }
  | litlist AND literal					{ $3 :: $1 }
  | litlist SEP literal				        { $3 :: $1 }
  ;

  literal:
  | predicate							{ Rel $1 }
  | NOT predicate 						{ Not $2 }
  | equation							{ $1 }
  | NOT equation					        { negate_eq $2 }
  ;

  predicate:
  | RELNAME LPAREN varlist RPAREN		{ Pred ($1, $3) }
  | PLUS RELNAME LPAREN varlist RPAREN		{ Deltainsert ($2, $4) }
  | MINUS RELNAME LPAREN varlist RPAREN		{ Deltadelete ($2, $4) }
  ;

  equation:	
  | var_or_agg EQ constant	{ Equal ($1, $3) }
  | var_or_agg NE constant	{ Ineq ("<>", $1, $3) }
  | var_or_agg LT constant	{ Ineq ( "<", $1, $3) }
  | var_or_agg GT constant	{ Ineq ( ">", $1, $3) }
  | var_or_agg LE constant	{ Ineq ("<=", $1, $3) }
  | var_or_agg GE constant	{ Ineq (">=", $1, $3) }
  ;

  var_or_agg:
  | VARNAME     { NamedVar $1 }
  | aggregate   { $1 }
  ;

  constant:
  | VAL               {Int $1}
  | STRING            {String $1}
  ;

  varlist: /* empty */					{ [] }
  | var				    				{ $1 :: [] }
  | var SEP varlist 					{ $1 :: $3 } /* \!/ rec. on the right */
  ;

  var:
  | VARNAME     { NamedVar $1 }
  | ANONVAR     { AnonVar }
  | constant    { ConstVar $1 }
  | aggregate    { $1 }
  ;

  aggregate:
  | VARNAME LPAREN VARNAME RPAREN       { AggVar ($1,$3) }
  ;
