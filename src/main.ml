(* 
@author: Vandang Tran
*)

(**  main file, Execution from command line, connection info, help printouts for command line usage etc. 
*)
open Lib;;
open Formulas;;
open Fol;;
open Skolem;;
open Fol_ex;;
open Lexer;;
exception Eof;;
open Printf;;
open Postgresql;;
open Conn_ops;;
open Utils;;
open Derivation;;
open Arg;;
open Ast2theorem;;
open Ast2fol;;

(** check for options of command line*)
(*default values*)
let host = ref "192.168.56.101";;
let port = ref 5432;;
let user = ref "postgres";;
let dejima_user = ref "dejima";;
let password = ref "12345678";;
let dbname = ref "datalogdb";;
let debug = ref false;;
let connectdb = ref false;;
let importschema = ref false;;
let verification = ref false;;
let inc = ref false;;
let optimize = ref false;;
let inputf = ref "";;
let inputshell = ref "";;
let outputf = ref "";;
let outputlean = ref "";;
let dbschema = ref "public";;
let mode = ref 1;;

let usage = "usage: " ^ Sys.argv.(0) ^ " [OPTIONS]"
let speclist = [
  ("-db", Arg.Unit (fun () -> debug := true),  " print debugging information");
  ("-f", Arg.String (fun s -> inputf := s),   "file read program from file, if not chosen, read from stdin");
  ("-b", Arg.String (fun s -> inputshell := s),   "file shell script file specifying the action, which will be executed when there is any update on the view, if not chosen, execute nothing");
  ("-o", Arg.String (fun s -> outputf := s),   "file write program out file, if not chosen, print to stdout");
  ("-l", Arg.String (fun s -> outputlean := s),   "file write lean verification out file");
  ("-s", Arg.String (fun s -> dbschema := s),  "schema database schema name to connect to (default: public)");
  ("-h", Arg.String (fun s -> host := s),      "host database server host (default: \"localhost\")");
  ("-c", Arg.Unit (fun () -> connectdb := true),      " connect and run sql on database server");
  ("-import", Arg.Unit (fun () -> importschema := true),      " connect and import data schema from database server");
  ("-v", Arg.Unit (fun () -> verification := true),      " verify the well-behavedness");
  ("-i", Arg.Unit (fun () -> inc := true),      " optimize update propagation by incremental rewriting rules");
  ("-e", Arg.Unit (fun () -> optimize := true),      " optimize datalog rules");
  ("-p", Arg.Int    (fun d -> port := d),      "port database server port (default: \"5432\")");
  ("-U", Arg.String (fun s -> user := s),      "user database user (default: \"postgres\")");
  ("-g", Arg.String (fun s -> dejima_user := s),      "user special user for global dejima synchronization (default: \"dejima\")");
  ("-w", Arg.String (fun s -> password := s),  "password database user password (default: 12345678)");
  ("-d", Arg.String (fun s -> dbname := s),    "dbname database name to connect to (default: \"datalogdb\")");
  ("-m", Arg.Int (fun m -> mode := m),         "mode {1: For putback view update datalog program, 2: For view update datalog program containing view definition, update strategy and integrity constraints, 3: For only view definition datalog program} (default: 1)");
];;

let () =
  (* Read the arguments *)
  Arg.parse
    (Arg.align speclist)
    (fun x ->
       raise (Arg.Bad ("Bad argument : " ^ x))
    )
    usage;
;;

(** assign postgreSQL connection parameters to conninfo variable *)
let conninfo =
  sprintf "host=%s port=%d user=%s password=%s dbname=%s"
    !host !port !user !password !dbname;;

(* pretty print connection informations *)
let print_conn_info conn =
  print_endline "------Connected to:--------";
  printf "dbname    = %s\n" conn#db;
  printf "user      = %s\n" conn#user;
  printf "password  = %s\n" conn#pass;
  printf "host      = %s\n" conn#host;
  printf "port      = %s\n" conn#port;
  printf "tty       = %s\n" conn#tty;
  printf "options   = %s\n" conn#options;
  printf "pid       = %i\n" conn#backend_pid;
  print_endline "--------------\n";
;;

let main () =
  (* there are three tasks for the transformation program
     - derive put datalog program to get datalog program
     - translate get datalog program to sql language (need a query predicate (view))
     - translate put datalog program to PL/pgSQL procedure triggers (need update predicates (source tables))
  *)
  try 
    let chan = if !inputf = "" then stdin else open_in !inputf in
    let lexbuf = Lexing.from_channel chan in 
    (* add information of the file name to lexbuf *)
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname =  (if !inputf = "" then "stdin" else !inputf)};
    (* while true do *)
    try
      let original_ast = Parser.main Lexer.token lexbuf in 
      let shell_script = if !inputshell = "" then "#!/bin/sh\necho \"true\"" else (String.concat "\n" @@ read_file (!inputshell)) in
      let ast = 
      (* get source schema from database system *)
        if (!importschema) then 
            (let c = new connection ~conninfo () in
            if !debug then (
              print_conn_info c; 
              flush stdout
            ) else ();
            let view_name = Expr.get_rterm_predname (Expr.get_schema_rterm (get_view original_ast)) in
            let schema = import_dbschema c view_name (!dbschema) in 
            c#finish; 
            Expr.add_stts schema original_ast
            )
        else original_ast in

      let edb = extract_edb ast in 
      (* edb is set of rules whose head is tablename with column, and its body is none*)
      if !debug then (
        print_endline "------DB Schema:--------";
        print_endline @@ Expr.string_of_prog @@ Prog (Expr.get_schema_stts ast);
        (* print_symtable edb; *)
        print_endline "--------------\n";
        flush stdout;
      ) else ();
      (* let sql = Eval.sql_stt (!debug) edb ast in
         if !debug then (
          print_endline sql
         ) else ();
         print_sql_res c sql *)
      (* print_endline (Expr.to_string ast); *)
      let idb = extract_idb ast in 
      if !debug then (
        print_endline "------Rules:--------";
        print_symtable idb;
        print_endline "--------------\n";
      ) else ();
      
      (* let query_rt = Expr.get_query_rterm (get_query ast)  in
         Rule_preprocess.preprocess_rules idb;
         print_symtable idb;print_endline "______________";
         let strat = Stratification.stratify edb idb query_rt in
         print_endline "Stratification:\n";
         List.iter (fun x -> print_endline (string_of_symtkey x)) strat; *)

      match !mode with  
      | 1 | 2 -> (
        let ast2 = if (!mode) = 1 then
          (let get_ast = Derivation.derive (!debug) edb ast in
          let get_stts exp = fst (Rule_preprocess.seperate_rules exp) in 
          let bi_prog = Expr.add_stts (get_stts get_ast) ast in
          if !debug then (
            print_endline "_____get&put (bidirectional) datalog program_______"; 
            print_string (Expr.string_of_prog  bi_prog); 
            print_endline "______________\n";
                ) else ();
          bi_prog
          )
        else ast in
        let lean_code = validity_lean_code_of_datalog (!debug) (Expr.constraint2rule ast2) in 
        if not (!outputlean = "") then 
          (let ol =  open_out (!outputlean)  in  
          fprintf ol "%s\n" lean_code;
          close_out ol;)
        else ();
        let validity, message = 
          if (!verification) then 
            verify_fo_lean (!debug) lean_code 
          else 0, "" in 
        if not (validity=0) then 
          (print_endline @@ "Well-behavedness is not validated \nExit code: " ^ string_of_int validity
          ^ (if (!debug) then "\nError messange: "^ message else ""); exit 1)
        else
          (if (!verification) then print_endline @@ "Program is well-behaved";
          let oc =if !outputf = "" then stdout else open_out !outputf  in 
          let sql = Ast2sql.unfold_view_sql (!dbschema) (!debug) ast2 in
          fprintf oc "%s\n" sql;
          let trigger_sql = Ast2sql.unfold_delta_trigger_stt (!dbschema) (!debug) shell_script (!dejima_user) (!inc) (!optimize) (Expr.constraint2rule ast2) in
          fprintf oc "%s\n" trigger_sql;
          
          if (!connectdb) then 
            let c = new connection ~conninfo () in
            if !debug then (
              print_conn_info c; 
              flush stdout
            ) else ();
            c#set_notice_processor (fun s -> eprintf "postgresql error [%s]\n" s); 
          print_creating_sql c (sql^trigger_sql);
          close_out oc;
          c#finish; )
      )
      | 3 -> ( 
        let oc =if !outputf = "" then stdout else open_out !outputf  in 
        let ol =if !outputf = "" then stdout else open_out (!outputf^".lean")  in  
        let sql = Ast2sql.unfold_view_sql (!dbschema) (!debug) ast in
        fprintf oc "%s\n" sql;
        if (!connectdb) then 
          let c = new connection ~conninfo () in
          if !debug then (
            print_conn_info c; 
            flush stdout
          ) else ();
          c#set_notice_processor (fun s -> eprintf "postgresql error [%s]\n" s); 
        print_creating_sql c (sql);
        close_out oc;
        c#finish; 
      )
      | _ -> print_string "Error: mode has to be in {1,2,3}"
    with
      SemErr exp -> print_endline ("Semantic error: "^exp)
    | Parsing.Parse_error ->  print_endline "Syntax error"
    | LexErr msg -> print_endline (msg^":\nError: Illegal characters")
    | ParseErr msg -> print_endline (msg^":\nError: Syntax error")
  (* done *)
  with Eof ->
    print_string "Lexer.Eof"; exit 0
;; 

let test() = 
    let debug = true in
    let inputf = "tests/debug.dl" in
    let outputf = "tests/debug.out" in
    let outputlean = "tests/debug.lean" in
    let chan = if  inputf = "" then stdin else open_in  inputf in
    let lexbuf = Lexing.from_channel chan in 
    let dbschema = "public" in
    (* add information of the file name to lexbuf *)
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname =  (if  inputf = "" then "stdin" else  inputf)};
    (* while true do *)
    try
      let ast = Parser.main Lexer.token lexbuf in 
      (* let edb = import_dbschema c ( dbschema) in  *)
      let edb = extract_edb ast in 
      (* edb is set of rules whose head is tablename with column, and its body is none*)
      if  debug then (
        print_endline "--------------";
        print_endline "DB Schema:\n";
        print_symtable edb;
        print_endline "--------------";
        flush stdout;
      ) else ();

      let idb = extract_idb ast in 
      if  debug then (
                    print_symtable idb;print_endline "______________\n";
                ) else ();

      let oc =if  outputf = "" then stdout else open_out  outputf  in
      let ol =if  outputf = "" then stdout else open_out  outputlean  in
      (* fprintf oc "/*";
        fprintf oc "_____ datalog program _______\n";
        fprintf oc "%s\n" (Expr.string_of_prog  ast);
        fprintf oc "______________";
        fprintf oc "*/\n\n"; *)

      (* let lambda = Ast2lambda.disjoint_delta_theorem_of_stt ( debug) edb ast in
      fprintf oc "%s\n" lambda; *)
      (* fprintf oc "%s\n\n" (Ast2theorem.z3_assert_of_disjoint_delta ( debug) ast); *)
      (* fprintf ol "%s\n\n" (gen_lean_code_for_theorems [(Ast2theorem.lean_simp_theorem_of_disjoint_delta ( debug) ast);  *)
      (* (Ast2theorem.lean_simp_theorem_of_getput ( debug) ast);(Ast2theorem.lean_simp_theorem_of_putget ( debug) ast)]); *)
      (* let _,fm = fol_of_program_query ( debug) ast in
      fprintf oc "%s\n" (lean_string_of_fol_formula ( fm));
      fprintf oc "%s\n" (z3_string_of_fol_formula ( fm)); *)
      (* fprintf oc "%s\n" (lean_string_of_fol_formula (srnf fm)); *)
      (* fprintf oc "%s\n" (string_of_fol_formula (ranf fm)); *)
      (* let constructed_prog = ( Expr.add_stts (Expr.get_schema_stts ast) (fol2datalog (fv fm) fm)) in *)
      (* let constructed_prog = fol2datalog (get_query ast) (Expr.get_schema_stts ast) (fv fm) fm in *)
      (* let constructed_prog = optimize_query_datalog (debug) ast in *)
      (* fprintf oc "%s\n" (Expr.string_of_prog constructed_prog); *)
      (* fprintf oc "%s\n" (Ast2sql.unfold_program_query "public" false constructed_prog); *)
      (* fprintf oc "%s\n" (lean_string_of_fol_formula (srnf (fol_of_stt ( debug) edb (fol2datalog (fv fm) fm)))); *)
      (* if Fol_ex.is_safe_range fm then fprintf oc "OK === \n"; *)
    
      (* fprintf oc "%s\n" (Expr.string_of_prog (Expr.add_stts (Derivation.datalog_of_new_view debug ast) (Expr.add_stts (Derivation.datalog_of_delta_appliation ( debug) ast) ast))); *)
      (* let trigger_sql = Ast2sql.unfold_delta_trigger_stt "public" debug true true ast in
        fprintf oc "%s\n" trigger_sql; *)
      (* let constr_sql = Ast2sql.view_constraint_sql_of_stt "public" debug false ast in *)
         (* fprintf oc "%s\n" constr_sql; *)

      let fm = Ast2fol.sourcestability_sentence_of_stt ( debug) ast in
      let view_name = Expr.get_rterm_predname (Expr.get_schema_rterm (get_view ast)) in
      fprintf oc "%s\n" (lean_string_of_fol_formula fm);
      let phi, lst = ranf2lvnf view_name fm in 
      fprintf oc "phi: %s\n" (lean_string_of_fol_formula phi);

      List.iter (fun (vars, vfol, phi_i) -> 
      fprintf oc "conj: %s, " (lean_string_of_fol_formula ((vfol)));
      fprintf oc " %s \n" (lean_string_of_fol_formula ((phi_i)));
      ) lst;

      close_out oc;
      close_out ol;
    with
      SemErr exp -> print_endline ("Semantic error: "^exp)
    | Parsing.Parse_error ->  print_endline "Syntax error"
    | LexErr msg -> print_endline (msg^":\nError: Illegal characters")
    | ParseErr msg -> print_endline (msg^":\nError: Syntax error")

let small_test() = 
  let l = [1;2;3] in 
  let powerset = allnonemptysubsets l in 
  let sortlst = Lib.sort (fun a b -> List.length a < List.length b) powerset in
  List.iter (fun x -> List.iter (fun a -> print_int a) x; print_endline "; ") sortlst
;;

(** mainly a call to the above main function *)
let _ =
  try main () with
  (* try small_test () with *)
  (* try test () with *)
  | Error e -> prerr_endline (string_of_error e)
  | e -> prerr_endline (Printexc.to_string e)
;;
