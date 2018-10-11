 {
    open Parser;;        (* The type token is defined in parser.mli *)
    open Utils ;;
    let keyword_table = Hashtbl.create 100
    let _ =
        List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
        ["AND", AND;
        "And", AND;
        "and", AND;
        "OR", OR;
        "Or", OR;
        "or", OR;
        "NOT", NOT;
        "Not", NOT;
        "not", NOT;
        "FALSE", BOT;
        "False", BOT;
        "false", BOT;
        "TRUE", TOP;
        "True", TOP;
        "true", TOP;
        "null", NULL;
        "Null", NULL;
        "NULL", NULL;
        ]
(*		exception Eof
*)
 }

  rule token = parse
      [' ' '\t']     				{ token lexbuf }    (* skip blanks *)
    | ['\n' ]        				{ Lexing.new_line lexbuf; token lexbuf }    (* skip newline *)
    | '%''\n'        			{ Lexing.new_line lexbuf; token lexbuf }    (* skip comments *)
    | '%'[^'\n''v''s'][^'\n']*'\n'        			{ Lexing.new_line lexbuf; token lexbuf }    (* skip comments *)
    | '%'[^'\n''v''s'][^'\n']*eof        			{ Lexing.new_line lexbuf; token lexbuf }    (* skip comments *)
    | '%'['v''s'][^':'][^'\n']*'\n'        			{ Lexing.new_line lexbuf; token lexbuf }    (* skip comments *)
    | '%'['v''s'][^':'][^'\n']*eof        			{ Lexing.new_line lexbuf; token lexbuf }    (* skip comments *)
    | ['0'-'9']+ as lxm 			{ INT (int_of_string lxm) }
    | ['0'-'9']*'.'?['0'-'9']+(['e''E']['-''+']?['0'-'9']+)?  as lxm  { FLOAT (float_of_string (lxm)) }
    | '\''(('\'''\'')|[^'\n''\''])*'\'' as lxm  { STRING(lxm) }
    | '_'*['a'-'z']['a'-'z''0'-'9''_']* as lxm 	{ 
        try
            Hashtbl.find keyword_table lxm
        with Not_found -> RELNAME(lxm) }
    | ['A'-'Z']['A'-'Z''0'-'9''_']* as lxm 	{
        try
            Hashtbl.find keyword_table lxm
        with Not_found -> VARNAME(lxm)
						}
    | ":-"          				{ IMPLIEDBY }
    | "?-"            				{ QMARK }  (* query mark *)
    | "%v:"            				{ QMARK }  (* query mark *)
    | "%s:"                         {UMARK} (* updated base relation mark *)
    | '.'            				{ DOT }    (* end of rule or query *)
    | ','            				{ SEP }
    | '('            				{ LPAREN }
    | ')'            				{ RPAREN }
    | '='            				{ EQ }
    | "<>"            				{ NE }
    | "\="            				{ NE }
    | '_'                                       { ANONVAR }
    | "<="                                      { LE }
    | ">="                                      { GE }
    | '<'                                       { LT }
    | '>'                                       { GT }
    | '+'                                       { PLUS }
    | '-'                                       { MINUS }
	| eof            { EOF }
	| _                      { spec_lex_error lexbuf }

