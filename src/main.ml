(* 
@author: Vandang Tran
*)

open Lexer;;
exception Eof;;
open Printf;;
open Utils;;
open Arg;;

let debug = ref false;;
let inputf = ref "";;
let outputf = ref "";;
let dbschema = ref "public";;

let usage = "usage: " ^ Sys.argv.(0) ^ " [OPTIONS]"
let speclist = [
  ("-db", Arg.Unit (fun () -> debug := true),  " print debugging information");
  ("-f", Arg.String (fun s -> inputf := s),   "file read program from file, if not chosen, read from stdin");
  ("-o", Arg.String (fun s -> outputf := s),   "file write program out file, if not chosen, print to stdout");
  ("-s", Arg.String (fun s -> dbschema := s),   "schemaname database schema name to connect to (default: public)");
];;

let () =
  Arg.parse
    (Arg.align speclist)
    (fun x ->
       raise (Arg.Bad ("Bad argument : " ^ x))
    )
    usage;
;;

let main () =

  try 
    let chan = if !inputf = "" then stdin else open_in !inputf in
    let lexbuf = Lexing.from_channel chan in 
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname =  (if !inputf = "" then "stdin" else !inputf)};
    try
      let ast = Parser.main Lexer.token lexbuf in 
      let edb = extract_edb ast in 
      if !debug then (
        print_endline "--------------";
        print_endline "EDB: \n";
        print_symtable edb;
        print_endline "--------------";
        flush stdout;
      ) else ();
      let oc =if !outputf = "" then stdout else open_out !outputf  in   
      let get_ast = Derivation.derive (!debug) edb ast in
      fprintf oc "/*";
      fprintf oc "_____get datalog program_______\n";
      fprintf oc "%s\n" (Expr.string_of_prog  get_ast);
      fprintf oc "______________";
      fprintf oc "*/\n\n";

      (* sql query for the view *)
      let sql = Ast2sql.unfold_view_sql (!dbschema) (!debug) edb get_ast in
      fprintf oc "%s\n" sql;

      (* trigger construction sql for updating sources*)
      let trigger_sql = Ast2sql.unfold_delta_trigger_stt (!dbschema) (!debug) edb ast in

      fprintf oc "%s\n" trigger_sql;
      close_out oc; 
    with
      SemErr exp -> print_endline ("Semantic error: "^exp)
    | Parsing.Parse_error ->  print_endline "Syntax error"
    | LexErr msg -> print_endline (msg^":\nError: Illegal characters")
    | ParseErr msg -> print_endline (msg^":\nError: Syntax error")
  with Eof ->
    print_string "Lexer.Eof";
    exit 0
;; 

let _ =
  try main () with
  | e -> prerr_endline (Printexc.to_string e)
;;
