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
        "FALSE", FF;
        "False", FF;
        "false", FF;
        "BOT", BOT;
        "Bot", BOT;
        "bot", BOT;
        "TRUE", TT;
        "True", TT;
        "true", TT;
        "TOP", TOP;
        "Top", TOP;
        "top", TOP;
        "null", NULL;
        "Null", NULL;
        "NULL", NULL;
        "int", SINT;
        "INT", SINT;
        "real", SREAL;
        "REAL", SREAL;
        "string", SSTRING;
        "STRING", SSTRING;
        "bool", SBOOL;
        "BOOL", SBOOL;
        "source", SMARK;
        "Source", SMARK;
        "SOURCE", SMARK;
        "view", VMARK;
        "View", VMARK;
        "VIEW", VMARK;
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
    | "_|_"                                       { BOT }
    | "%v:"            				{ VMARK }  (* view mark *)
    | "%s:"                         { SMARK } (* source relation mark *)
    | ":-"          				{ IMPLIEDBY }  
    | "<-"          				{ IMPLIEDBY }  
    | "?-"            				{ QMARK }  (* query mark *)
    | "<>"            				{ NE }
    | "\\="            				{ NE }
    | "<="                                      { LE }
    | ">="                                      { GE }
    | "^"                                       { CONCAT }
    | "←"           				{ IMPLIEDBY } 
    | "¬"           				{ NOT }  
    | '.'            				{ DOT }    (* end of rule or query *)
    | ','            				{ SEP }
    | '('            				{ LPAREN }
    | ')'            				{ RPAREN }
    | '='            				{ EQ }
    | '_'                                       { ANONVAR }
    | ':'          				{ TYPING }
    | "⊥"                                       { BOT }
    | "⊤"                                       { TOP } 
    | '<'                                       { LT }
    | '>'                                       { GT }
    | '+'                                       { PLUS }
    | '-'                                       { MINUS }
    | '*'                                       { TIMES }
    | '/'                                       { DIVIDE }   
	| eof            { EOF }
    | _                      { spec_lex_error lexbuf }
	
