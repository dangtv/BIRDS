%{ (* OCaml preamble *)

  open Expr ;;
  open Utils ;;
  
   (* end preamble *)
 %}
  

/* tokens declaration */

%token <int> INT            /* token with int value    */
%token <float> FLOAT            /* token with float value    */
%token <string> STRING            /* token with string value    */
%token <string> RELNAME       /* token with string value */
%token <string> VARNAME         /* token with string value */

  
%token QMARK UMARK DOT IMPLIEDBY
%token AND NOT OR BOT TOP
%token NULL
%token EQ
%token NE LE GE LT GT
%token PLUS MINUS TIMES DIVIDE CONCAT
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
  | error             { spec_parse_error "invalid syntax for a main program" 1; }
  ;
  
  program: 
  exprlist								{ $1 }
  | error             { spec_parse_error "invalid syntax for a program" 1; }
  ;

  exprlist:
  | expr								{ $1 :: []  }
  | exprlist expr 						{ $2 :: $1 }
  | error             { spec_parse_error "invalid syntax for a list of rules" 1; }
  ;

  expr: 
  | rule	                            { $1 }
  | query	                            { $1 }
  | update	                            { $1 }
  | error             { spec_parse_error "invalid syntax for a rule or a declaration of query/base table" 1; }
  ;

  rule:
  head IMPLIEDBY body DOT				{ Rule ($1,$3) }
  | head IMPLIEDBY body EOF  { spec_parse_error "miss a dot for a rule" 4; }
  | error             { spec_parse_error "invalid syntax for a rule" 1; }
  ; 

  head:
  predicate						{ $1 }
  | error             { spec_parse_error "invalid syntax for a head" 1; }
  ;

  body:
  litlist						{ List.rev $1 }
  | error             { spec_parse_error "invalid syntax for a body" 1; }
  ;

  query:
  | QMARK predicate DOT					{ Query $2 } 
  | QMARK predicate EOF					{ spec_parse_error "miss a dot for a query" 3; }
  | error             { spec_parse_error "invalid syntax for a query" 1; }
  ;

  update:
  | UMARK predicate DOT					{ Base $2 } 
  | UMARK predicate EOF             { spec_parse_error "miss a dot for a base relation" 3; }
  | error             { spec_parse_error "invalid syntax for a base relation" 1; }
  ;

  litlist: /* empty */					{ [] }
  | literal						{ $1 :: [] }
  | litlist AND literal					{ $3 :: $1 }
  | litlist SEP literal				        { $3 :: $1 }
  | error             { spec_parse_error "invalid syntax for a conjunction of literals" 1; }
  ;

  literal:
  | predicate							{ Rel $1 }
  | NOT predicate 						{ Not $2 }
  | equation							{ $1 }
  | NOT equation					        { negate_eq $2 }
  | error             { spec_parse_error "invalid syntax for a literal" 1; }
  ;

  predicate:
  | RELNAME LPAREN varlist RPAREN		{ Pred ($1, $3) }
  | PLUS RELNAME LPAREN varlist RPAREN		{ Deltainsert ($2, $4) }
  | MINUS RELNAME LPAREN varlist RPAREN		{ Deltadelete ($2, $4) }
  | error             { spec_parse_error "invalid syntax for a predicate" 1; }
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
  | error             { spec_parse_error "invalid syntax for a var or a aggreation" 1; }
  ;

  constant:
  | INT               {Int $1}
  | MINUS INT               {Int (- $2)}
  | FLOAT               {Real $1}
  | MINUS FLOAT               {Real (-. $2)}
  | STRING            {String $1}
  | error             { spec_parse_error "invalid syntax for a constant" 1; }
  ;

  varlist: /* empty */					{ [] }
  | var				    				{ $1 :: [] }
  | var SEP varlist 					{ $1 :: $3 } /* \!/ rec. on the right */
  | error             { spec_parse_error "invalid syntax for a list of variables" 1; }
  ;

  var:
  | VARNAME     { NamedVar $1 }
  | ANONVAR     { AnonVar }
  | constant    { ConstVar $1 }
  | aggregate    { $1 }
  | error             { spec_parse_error "invalid syntax for a variables" 1; }
  ;

  aggregate:
  | VARNAME LPAREN VARNAME RPAREN       { AggVar ($1,$3) }
  | error             { spec_parse_error "invalid syntax for a aggregation" 1; }
  ;
