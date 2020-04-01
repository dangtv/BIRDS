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
open Bx;;
open Expr;;

(** check for options of command line*)
(*default values*)
let host = ref "192.168.56.101";;
let port = ref 5432;;
let user = ref "postgres";;
let dejima_user = ref "dejima";;
let password = ref "12345678";;
let dbname = ref "datalogdb";;
let log = ref false;;
let debug = ref false;;
let explain = ref false;;
let print_version = ref false;;
let connectdb = ref false;;
let importschema = ref false;;
let dejima_ud = ref false;;
let verification = ref false;;
let cex_generation = ref false;;
let cex_max = ref 5;;
let inc = ref false;;
let optimize = ref false;;
let speedup = ref false;;
let inputf = ref "";;
let inputshell = ref "";;
let outputf = ref "";;
let outputlean = ref "";;
let dbschema = ref "public";;
let timeout = ref 180;;

let usage = "usage: " ^ Sys.argv.(0) ^ " [OPTIONS]"
let speclist = [
  ("--version", Arg.Unit (fun () -> print_version := true),  " Print version");
  ("--log", Arg.Unit (fun () -> log := true),  " Print running information");
  ("--debug", Arg.Unit (fun () -> debug := true),  " Enable debugging mode");
  ("--explain", Arg.Unit (fun () -> explain := true),  " Show only explanations in the debugging mode");
  ("-f", Arg.String (fun s -> inputf := s),   "<file> Input program file, if not chosen, read from stdin");
  ("-b", Arg.String (fun s -> inputshell := s),   "<file> Shell script file specifying the action, which will be executed when there is an update on the view, if not chosen, execute nothing");
  ("-o", Arg.String (fun s -> outputf := s),   "<file> Output SQL file, if not chosen, print to stdout");
  ("-l", Arg.String (fun s -> outputlean := s),   "<file> Output verification file (optional)");
  ("-s", Arg.String (fun s -> dbschema := s),  "<schema> Database schema name to connect to (default: public)");
  ("-h", Arg.String (fun s -> host := s),      "<host> Database server host (default: \"localhost\")");
  ("-c", Arg.Unit (fun () -> connectdb := true),      " Connect and run the generated SQL on the database server");
  ("--import", Arg.Unit (fun () -> importschema := true),      " Connect and import the data schema from database server");
  ("-v", Arg.Unit (fun () -> verification := true),      " Enable verifications");
  ("--verification", Arg.Unit (fun () -> verification := true),      " The same as -v");
  ("-x", Arg.Int (fun d -> cex_generation := true; cex_max := d),      "<size> Get a counterexample with the maximum size if the program is not well-behaved");
  ("--counterexample", Arg.Int (fun d -> cex_generation := true; cex_max := d),      "<size> The same as -x");
  ("-i", Arg.Unit (fun () -> inc := true),      " Optimize the update propagation by incremental rewriting rules");
  ("--incrementalization", Arg.Unit (fun () -> inc := true),      " The same as -i");
  ("-e", Arg.Unit (fun () -> optimize := true),      " Optimize datalog rules");
  ("--optimization", Arg.Unit (fun () -> optimize := true),      " The same as -e");
  ("-u", Arg.Unit (fun () -> speedup := true),      " Speed up the verifications");
  ("--speedup", Arg.Unit (fun () -> speedup := true),      " The same as -u");
  ("-p", Arg.Int    (fun d -> port := d),      "<port> Database server port (default: \"5432\")");
  ("-U", Arg.String (fun s -> user := s),      "<user> Database user (default: \"postgres\")");
  ("-g", Arg.String (fun s -> dejima_user := s),      "<user> The special user for global dejima synchronization (default: \"dejima\")");
  ("--dejima", Arg.Unit (fun () -> dejima_ud := true),      " Detect updates on dejima views to perform pre-defined actions in the shell script file");
  ("-w", Arg.String (fun s -> password := s),  "<password> Database user password (default: 12345678)");
  ("-d", Arg.String (fun s -> dbname := s),    "<dbname> Database name to connect to (default: \"datalogdb\")");
  ("-t", Arg.Int (fun d -> timeout := d),    "<timeout> Timeout (second) (default: 120s)");
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
  if (!print_version) then ((print_endline "BIRDS version 0.0.4"); exit 0);
  if (!importschema) then 
    (if !log then print_endline "importing schema from the database"; 
    let c = new connection ~conninfo () in
    if !log then (
      print_conn_info c; 
      flush stdout
    ) else ();
    let schema = import_dbschema c (!dbschema) in 
    c#finish; 
    if (!log) then (
      print_endline "------imported DB Schema:--------";
      print_endline @@ Expr.string_of_prog @@ Prog (schema);
      print_endline "--------------\n";
      flush stdout;
      );
    let oc =if !outputf = "" then stdout else open_out !outputf  in 
    fprintf oc "%s\n" (Expr.string_of_prog @@ Prog (schema));
    close_out oc;
    exit 0;
    );

  let chan = if !inputf = "" then stdin else open_in !inputf in
  let lexbuf = Lexing.from_channel chan in 
  (* add information of the file name to lexbuf *)
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname =  (if !inputf = "" then "stdin" else !inputf)};
  (* while true do *)
  let original_ast = Parser.main Lexer.token lexbuf in 
  let shell_script = if !inputshell = "" then "#!/bin/sh\necho \"true\"" else (String.concat "\n" @@ read_file (!inputshell)) in
  let ast =  original_ast in
  let edb = extract_edb ast in 
  (* edb is set of rules whose head is tablename with column, and its body is none*)
  if !log then (
    print_endline "------DB Schema:--------";
    print_endline @@ Expr.string_of_prog @@ Prog (Expr.get_schema_stts ast);
    (* print_symtable edb; *)
    print_endline "--------------\n";
    flush stdout;
  ) else ();
  let idb = extract_idb ast in 
  if !log then (
    print_endline "------Rules:--------";
    print_symtable idb;
    print_endline "--------------\n";
  ) else ();
  let has_get = is_defined_pred (get_rterm_predname (get_schema_rterm (get_view ast))) ast in

  if !debug then 
    (if !log then print_endline ">> Start debugging";
    if has_get then 
      if !explain then 
        (
        print_endline "==========Checking constraints=============" ; 
        if (Debugger.check_constraints !log ast) then print_endline "Constraints are satisfied"
        else print_endline "Constraints are not satisfied";
        print_endline "===========================================\n" ; 
        print_endline "==========Testing the delta disjointness property w.r.t the given tuples=============" ; 
        Debugger.explain_disdelta !log ast;
        print_endline "==============================================================\n" ; 
        print_endline "===============Testing the getput property w.r.t the given tuples================";  
        Debugger.explain_getput !log ast;
        print_endline "==========================================================\n" ; 
        print_endline "===============Testing the putget property w.r.t the given tuples================";  
        Debugger.explain_putget !log ast;
        print_endline "==========================================================\n" ; 
        )
      else (
        Debugger.debug_disdelta !log ast;
        Debugger.debug_getput !log ast;
        Debugger.debug_putget !log ast
      )
    )
  else 
    (let ast2, view_rules_string = 
      if has_get then (
        (* verification *)
        if (!verification || !cex_generation) then 
          (
          let constr_ast = constraint2rule ast in
          let disdelta_thm = lean_simp_theorem_of_disjoint_delta (!log) constr_ast in
          let getput_thm = lean_simp_theorem_of_getput (!log) constr_ast in
          let putget_thm = lean_simp_theorem_of_putget (!log) constr_ast in
          if(!speedup) then 
            (
            (* verify all properties *)
            if (!log) then print_endline "==> verifying all properties";
            let lean_code_all_thms = gen_lean_code_for_theorems [ disdelta_thm; getput_thm; putget_thm ] in
              let exitcode, message = verify_fo_lean (!log) (!timeout) lean_code_all_thms in 
              if not (exitcode=0) then 
                if (exitcode = 124) then (raise (ChkErr ("Stop verifying: Timeout"))) 
                else
                  if str_contains message "type mismatch at application" then 
                  (* type error *)
                  (raise (ChkErr ("Invalidity: Type mismatch: \n"^ cut_str_by_word message "error"
                    ^ (if (!log) then "\nError messange: "^ message else ""))))
                  else
                    (raise (ChkErr ("Invalidity: Well-behavedness is not validated \nExit code: " ^ string_of_int exitcode
                    ^ (if (!log) then "\nError messange: "^ message else ""))));
            )
          else (
            (* verify delta disjointness property *)
            let satisfying_getput = ref false in 
            let satisfying_putget = ref false in 
            let satisfying_deltadis = ref false in 
            let verification_mess = ref "" in
            let counterexample_mess = ref "" in
            if (!log) then print_endline "==> verifying delta disjointness property";
            let lean_code_disdelta = gen_lean_code_for_theorems [ disdelta_thm ] in
              let exitcode, message = verify_fo_lean (!log) (!timeout) lean_code_disdelta in 
              if not (exitcode=0) then 
                if (exitcode = 124) then (
                  if (!log) then print_endline "Stop verifying the delta disjointness property: Timeout";
                  verification_mess :=  (!verification_mess)^ "\n\n" ^"Stop verifying the delta disjointness property: Timeout")
                else
                  if str_contains message "type mismatch at application" then 
                  (* type error *)
                  (let m = "Invalidity of delta disjointness: Type mismatch: \n"^ cut_str_by_word message "error"
                    ^ (if (!log) then "\nError messange: "^ message else "") in 
                    if(!log) then print_endline m;
                    verification_mess :=  (!verification_mess)^ "\n\n" ^m)
                  else
                    if (exitcode=1 && !cex_generation) then 
                      (let error, counterexample = Debugger.gen_counterexample !log "disdelta" !cex_max !timeout constr_ast in
                      let m = if (error = "") then ("% Invalidity: The following counterexample shows that deltas in the datalog program are not disjoint:\n" ^ string_of_prog (Prog counterexample)) 
                              else "% Fail to generate a couterexample of delta disjointness: " ^ error in 
                      if (!log) then print_endline m;
                      counterexample_mess := (!counterexample_mess) ^ "\n\n" ^ m
                        (* exit 0 *))
                    else
                      (let m = ("Invalidity: Deltas in the datalog program are not disjoint \nExit code: " ^ string_of_int exitcode
                      ^ (if (!log) then "\nError messange: "^ message else "")) in 
                      if (!log) then print_endline m;
                      verification_mess := (!verification_mess) ^ "\n\n" ^ m)
              else satisfying_deltadis := true;
            (* verify getput property *)
            if (!log) then print_endline "==> verifying getput property";
            let lean_code_getput = gen_lean_code_for_theorems [ getput_thm ] in
              let exitcode, message = verify_fo_lean (!log) (!timeout) lean_code_getput in 
              if not (exitcode=0) then 
                if (exitcode = 124) then (
                  if (!log) then print_endline "Stop verifying the getput property: Timeout";
                  verification_mess :=  (!verification_mess)^ "\n\n" ^ "Stop verifying the getput property: Timeout")
                else
                  if str_contains message "type mismatch at application" then 
                  (* type error *)
                  (let m = ("Invalidity of getput: Type mismatch: \n"^ cut_str_by_word message "error"
                    ^ (if (!log) then "\nError messange: "^ message else "")) in 
                    if (!log) then print_endline m;
                    verification_mess :=  (!verification_mess)^ "\n\n" ^ m)
                  else
                    if (exitcode=1 && !cex_generation) then 
                      (let error, counterexample = Debugger.gen_counterexample !log "getput" !cex_max !timeout constr_ast in
                      let m = if (error = "") then ("% Invalidity: The following counterexample shows that getput is not satisfied:\n" ^ string_of_prog (Prog counterexample)) 
                              else "% Fail to generate a couterexample of getput: " ^error in 
                      if (!log) then print_endline m;
                        counterexample_mess := (!counterexample_mess) ^ "\n\n" ^ m
                      (* exit 0 *)
                      )
                    else
                      (let m = ("Invalidity: Property getput is not validated \nExit code: " ^ string_of_int exitcode
                      ^ (if (!log) then "\nError messange: "^ message else "")) in 
                      if (!log) then print_endline m;
                      verification_mess := (!verification_mess) ^ "\n\n" ^ m)
              else satisfying_getput := true;
            (* verify putget property *)
            if (!log) then print_endline "==> verifying putget property";
            let lean_code_putget = gen_lean_code_for_theorems [ putget_thm ] in
              let exitcode, message = verify_fo_lean (!log) (!timeout) lean_code_putget in 
              if not (exitcode=0) then 
                if (exitcode = 124) then (
                  if (!log) then print_endline "Stop verifying getput property: Timeout";
                  verification_mess := (!verification_mess) ^ "\n\n" ^ ("Stop verifying getput property: Timeout"))
                else
                  if str_contains message "type mismatch at application" then 
                  (* type error *)
                  (let m = ("Invalidity of putget: Type mismatch: \n"^ cut_str_by_word message "error"
                    ^ (if (!log) then "\nError messange: "^ message else "")) in
                  verification_mess := (!verification_mess) ^ "\n\n" ^ m)
                  else
                    if (exitcode=1 && !cex_generation) then 
                      (let error, counterexample = Debugger.gen_counterexample !log "putget" !cex_max !timeout constr_ast in
                      let m = if (error = "") then ("% Invalidity: The following counterexample shows that putget is not satisfied:\n" ^ string_of_prog (Prog counterexample)) 
                              else "% Fail to generate a couterexample of putget: " ^error in 
                      if (!log) then print_endline m;
                      counterexample_mess := (!counterexample_mess) ^ "\n\n" ^ m
                        (* exit 0 *))
                    else
                      (let m = ("Invalidity: Property putget is not validated \nExit code: " ^ string_of_int exitcode
                      ^ (if (!log) then "\nError messange: "^ message else "")) in 
                      if (!log) then print_endline m;
                      verification_mess := (!verification_mess) ^ "\n\n" ^ m)
              else satisfying_putget := true; 
              (* conclusion *)
              if(!satisfying_deltadis && !satisfying_getput && !satisfying_putget) then (
                if (!log) then print_endline "The program satisfies all delta disjointness, getput and putget"
              )
              else (
                if (!cex_generation) then (print_endline !counterexample_mess; exit 0)
                else (raise (ChkErr !verification_mess))
              )
            );
          );
        ast, "")
      else (
        let get_ast = Bx.derive_get_datalog (!log) (!speedup) (!timeout) ast in
        let get_rules exp = fst (Rule_preprocess.seperate_rules exp) in 
        let bi_prog = Expr.add_stts (get_rules get_ast) ast in
        if !log then (
          print_endline "_____get&put (bidirectional) datalog program_______"; 
          print_string (Expr.string_of_prog  bi_prog); 
          print_endline "______________\n";
              ) else ();
        bi_prog, (Expr.string_of_prog (Prog (get_rules get_ast)))) in
    let lean_code = 
    (
      if has_get then validity_lean_code_of_bidirectional_datalog (!log) (constraint2rule ast2) else
        gen_lean_code_for_theorems [ (lean_simp_theorem_of_putget (!log) (constraint2rule ast2)) ]
    ) in 
    if not (!outputlean = "") then 
      (let ol =  open_out (!outputlean)  in  
      fprintf ol "%s\n" lean_code;
      close_out ol;);
      
    if (!verification) then print_endline @@ "-- Program is validated --";
    let oc =if !outputf = "" then stdout else open_out !outputf  in 
    if (not has_get) then fprintf oc "\n/*view definition (get):\n%s*/\n\n" view_rules_string;
    let sql = Ast2sql.unfold_view_sql (!dbschema) (!log) ast2 in
    fprintf oc "%s\n" sql;
    let trigger_sql = Ast2sql.unfold_delta_trigger_stt (!dbschema) (!log) (!dejima_ud) shell_script (!dejima_user) (!inc) (!optimize) (constraint2rule ast2) in
    fprintf oc "%s\n" trigger_sql;
  
    if (!connectdb) then 
      let c = new connection ~conninfo () in
      if !log then (
        print_conn_info c; 
        flush stdout
      ) else ();
      c#set_notice_processor (fun s -> eprintf "postgresql error [%s]\n" s); 
    print_creating_sql c (sql^trigger_sql);
    close_out oc;
    c#finish);  
;; 

let test() = 
    let log = true in
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
      let ast1 = Parser.main Lexer.token lexbuf in 
      let ast = (constraint2rule ast1) in
      (* let edb = import_dbschema c ( dbschema) in  *)
      let edb = extract_edb ast in 
      (* edb is set of rules whose head is tablename with column, and its body is none*)
      if  log then (
        print_endline "--------------";
        print_endline "DB Schema:\n";
        print_symtable edb;
        print_endline "--------------";
        flush stdout;
      ) else ();

      (* let idb = extract_idb ast in 
      if  log then (
                    print_symtable idb;print_endline "______________\n";
                ) else (); *)
      if  log then (
        print_endline (Expr.string_of_prog ast);
                    print_endline "______________\n";
                ) else ();
      let oc =if  outputf = "" then stdout else open_out  outputf  in
      let ol =if  outputf = "" then stdout else open_out  outputlean  in
      (* fprintf oc "/*";
        fprintf oc "_____ datalog program _______\n";
        fprintf oc "%s\n" (Expr.string_of_prog  ast);
        fprintf oc "______________";
        fprintf oc "*/\n\n"; *)

      (* let lambda = Ast2lambda.disjoint_delta_theorem_of_stt ( log) edb ast in
      fprintf oc "%s\n" lambda; *)
      (* fprintf oc "%s\n\n" (Ast2theorem.z3_assert_of_disjoint_delta ( log) ast); *)
      (* fprintf ol "%s\n\n" (gen_lean_code_for_theorems [(Ast2theorem.lean_simp_theorem_of_disjoint_delta ( log) ast);  *)
      (* (Ast2theorem.lean_simp_theorem_of_getput ( log) ast);(Ast2theorem.lean_simp_theorem_of_putget ( log) ast)]); *)
      (* let _,fm = fol_of_program_query ( log) ast in
      fprintf oc "%s\n" (lean_string_of_fol_formula ( fm));
      fprintf oc "%s\n" (z3_string_of_fol_formula ( fm)); *)
      (* fprintf oc "%s\n" (lean_string_of_fol_formula (srnf fm)); *)
      (* fprintf oc "%s\n" (string_of_fol_formula (ranf fm)); *)
      (* let constructed_prog = ( Expr.add_stts (Expr.get_schema_stts ast) (fol2datalog (fv fm) fm)) in *)
      (* let constructed_prog = fol2datalog (get_query ast) (Expr.get_schema_stts ast) (fv fm) fm in *)
      (* let constructed_prog = optimize_query_datalog (log) ast in *)
      (* fprintf oc "%s\n" (Expr.string_of_prog constructed_prog); *)
      (* fprintf oc "%s\n" (Ast2sql.unfold_program_query "public" false constructed_prog); *)
      (* fprintf oc "%s\n" (lean_string_of_fol_formula (srnf (fol_of_stt ( log) edb (fol2datalog (fv fm) fm)))); *)
      (* if Fol_ex.is_safe_range fm then fprintf oc "OK === \n"; *)
    
      (* fprintf oc "%s\n" (Expr.string_of_prog (Expr.add_stts (Derivation.datalog_of_new_view log ast) (Expr.add_stts (Derivation.datalog_of_delta_appliation ( log) ast) ast))); *)
      (* let trigger_sql = Ast2sql.unfold_delta_trigger_stt "public" log true true ast in
        fprintf oc "%s\n" trigger_sql; *)
      (* let constr_sql = Ast2sql.view_constraint_sql_of_stt "public" log false ast in *)
         (* fprintf oc "%s\n" constr_sql; *)

      
      
      (* let get_ast = derive_get_datalog log false 500 ast in
      fprintf oc "view datalog: %s \n" (Expr.string_of_prog get_ast); *)

      let error, counterexample = Debugger.gen_counterexample log "getput" 5 120 ast in
      fprintf oc "\n===== getput counterexample ===== \n" ;
      let buggy_prog = Expr.add_stts counterexample ast in
      fprintf oc "%s \n" (string_of_prog buggy_prog);
      let resulted_facts, explanation = Evaluation.eval log (get_delta_rterms buggy_prog) buggy_prog in
      fprintf oc "\n===== results ===== \n%s \n" (string_of_prog (Prog resulted_facts));
      fprintf oc "%s" ( String.concat "\n" (List.map (Debugger.string_of_explanation) explanation));

      (* Debugger.debug_getput log ast; *)
      (* Debugger.debug_putget log ast; *)
      Debugger.debug_disdelta log ast;
      
      
      close_out oc;
      close_out ol;
    with
      SemErr exp -> print_endline ("Semantic error: "^exp)
    | ChkErr exp -> print_endline (exp)
    | Parsing.Parse_error ->  print_endline "Syntax error"
    | LexErr msg -> print_endline (msg^":\nError: Illegal characters")
    | ParseErr msg -> print_endline (msg^":\nError: Syntax error")

let small_test() = 
  let l = ["a4"; "a2"; "a3"] in 
  let sortlst = Lib.sort (fun a b -> a < b) l in
  List.iter (fun a -> print_endline a) sortlst
;;

(** mainly a call to the above main function *)
let _ =
  try main () with
  (* try small_test () with *)
  (* try test () with *)
  | Error e -> prerr_endline (string_of_error e)
  | SemErr exp -> fprintf stderr "%s\n" ("Semantic error: "^exp); exit 1;
  | Parsing.Parse_error ->  fprintf stderr "%s\n"  "Syntax error"; exit 1;
  | LexErr msg -> fprintf stderr "%s\n"  (msg^":\nError: Illegal characters"); exit 1;
  | ChkErr exp -> fprintf stderr "%s\n"  (exp); exit 1;
  | ParseErr msg -> fprintf stderr "%s\n"  (msg^":\nError: Syntax error"); exit 1;
  | Failure msg -> fprintf stderr "%s\n"  msg; exit 1;
  | Invalid_argument msg -> fprintf stderr "%s\n"  msg; exit 1;
  | Eof -> fprintf stderr "%s\n"  "Lexer.Eof"; exit 1;
  | e -> prerr_endline (Printexc.to_string e)
;;
