(*******************************************************)
(** Functions for debugging a bidirectional Datalog program
*)
(********************************************************)
(*
@author: Vandang Tran
*)

open Expr
open Utils

let init_vocabulary = ["'..aa..'"; "'..bb..'"; "'..cc..'"; "'..dd..'";"'..ee..'"; "'..ff..'"; "'..gg..'"; "'..hh..'"; "'..ii..'"; "'..jj..'"; "'..kk..'"; "'..ll..'"; "'..mm..'"; "'..nn..'"; "'..oo..'"; "'..pp..'"; "'..qq..'"; "'..rr..'"; "'..ss..'"; "'..tt..'"; "'..uu..'"; "'..vv..'"; "'..ww..'"; "'..xx..'"; "'..yy..'"; "'..zz..'"]


(** get the string of a explanation tree *)
let string_of_summary_explanation (fact, edb_facts, all_clauses, detail) =
      "  (Summary) explain "^(string_of_fact fact)^" by: \n    " ^
      String.concat " " (List.map string_of_fact edb_facts) ^
      "\n    ~~~~~~~~~~~~~~~~~~~~~\n"^
      String.concat "" (List.map (fun clause -> "         || "^ (string_of_rule clause)) all_clauses) ^
      "         \\/  \n"^
      "    ~~~~~~~~~~~~~~~~~~~~~\n"^
      "    "^ (string_of_fact fact)^ "\n"

let string_of_detail_explanation (fact, edb_facts, all_clauses, detail) =
      "\n  (Details) explain "^(string_of_fact fact)^" by: \n" ^
      String.concat "" (List.map (fun (fact, clause, premises) ->
        "    "^
        String.concat " " (List.map string_of_fact premises)^
        "\n    ------------------ "^ (string_of_rule clause)^
        "    "^ (string_of_fact fact)^
        "\n\n"
      ) detail)

let string_of_explanation (fact, edb_facts, all_clauses, detail) =
    string_of_summary_explanation (fact, edb_facts, all_clauses, detail) ^ string_of_detail_explanation (fact, edb_facts, all_clauses, detail)

(** interatively generate a counterexample for a given property *)
let gen_counterexample (log:bool) (property:string) (maxsize:int) (timeout:int) iprog =
    let error_mess = ref "" in
    (* let all_rules, non_rules = Rule_preprocess.seperate_rules iprog in *)
    let string_adom = init_vocabulary@(Rule_preprocess.extract_rules_string_adom iprog.rules) in
    let string_adom = Lib.sort (fun a b -> a < b) string_adom in
    let adom_tbl = Hashtbl.create (List.length string_adom) in
    List.iteri (fun i x -> Hashtbl.add adom_tbl x i) string_adom;
    let string_to_int str =
        if Hashtbl.mem adom_tbl str then
            Hashtbl.find adom_tbl str
        else invalid_arg "function string_to_int called with an unkown string" in
    let prog = {iprog with rules = Rule_preprocess.string2int_rules string_to_int iprog.rules} in
    if log then (
        print_endline "______string-mapped program______";
        print_endline @@"String active domain in the program is: " ^ (String.concat ", " string_adom);
        print_endline (string_of_prog prog);
        print_endline "__________________________________"
        );
    let vocsize = List.length string_adom in
    let rec gen_ctex i maxsize =
        let exitcode, mes = check_ros_prog log timeout (
            match property with
            "getput" -> if log then print_endline "==> generating a counterexample for getput"; Ast2ros.ros_check_getput_of_stt log i vocsize prog
            | "putget" -> if log then print_endline "==> generating a counterexample for putget"; Ast2ros.ros_check_putget_of_stt log i vocsize prog
            | "disdelta" -> if log then print_endline "==> generating a counterexample for delta disjointness"; Ast2ros.ros_check_disdelta_of_stt log i vocsize prog
            | _ -> invalid_arg "function gen_counterexample called with an unkown property"
            ) in
        if not (exitcode=0) then
            if (exitcode = 124) then (error_mess := "Stop generating a counterexample of "^property ^": Timeout"; i,"")
            else
                (error_mess := "Stop generating a counterexample of "^property ^": \nExit code: " ^ string_of_int exitcode
                    ^ (if (log) then "\nError messange: "^ mes else ""); i,"")
        else
            if (!error_mess = "" && mes = "(unsat)" && i < maxsize) then
                gen_ctex (i+1) maxsize
            else
                i,mes in
      let size, message = gen_ctex 1 maxsize in
      if (!error_mess = "") then
        (if log then (print_endline "________Model from rosette______"; print_endline message; print_endline "_____________________");
        if (message = "(unsat)") then error_mess := "there is no counter example of the specified size for "^property;
        let model_data = Ast2ros.parse_ros_models log message in
        let relation_lst = (match property with
                "getput" -> get_source_stts prog
                | "putget" -> get_schema_stts prog
                | "disdelta" -> get_schema_stts prog
                | _ -> invalid_arg "function gen_counterexample called with an unkown property"
                ) in
        let table_lst = List.map (Ast2ros.instantiate_relation size string_adom model_data) relation_lst in
        let facts = List.concat table_lst in
        if log then (print_endline ("\n________"^property^" counterexample________ ") ;
            print_endline (string_of_prog {get_empty_expr with facts = facts} );
            print_endline "________________");
        !error_mess, facts)
    else !error_mess, []


let is_missing_fact rt =
    let name = get_rterm_predname rt in
        (match name.[0] with
        '!' -> true
        | _ -> false
        )

let is_builtin_fact rt =
    let name = get_rterm_predname rt in
        (match name with
        | "lt"
        | "<"
        | "le"
        | "<="
        | "gt"
        | ">"
        | "ge"
        | ">="
        | "ineq"
        | "<>"
        | "="
        | "equal"   -> true
        | _ -> false
        )

(** get the predicate name of an rterm using +/- for delta predicates *)
let get_rterm_relname rterm = match rterm with
    | Pred (x, vl) -> x
    | Deltainsert (x, vl) -> "+"^ x
    | Deltadelete (x, vl) -> "-"^ x


let extract_facts facts relname =
    List.filter (fun rt -> (get_rterm_predname rt) = relname) facts


let is_builtin_rule rule =
    let head_name = get_rterm_predname (rule_head rule) in
    str_contains head_name "±"


let debug_getput (log:bool) prog =
    let view_rt = get_view_rterm prog in
    let buggy_prog = delete_fact_of_predname (get_rterm_predname view_rt) prog in
    if log then (print_endline "\n_______getput counterexample full program_____" ;
        print_endline (string_of_prog buggy_prog);
        print_endline "________________");
    let resulted_facts, explanation = Evaluation.eval log (get_delta_rterms buggy_prog) buggy_prog in
    if (List.length explanation = 0) then
        print_endline "The getput property is satisfied"
    else (
        print_endline ">>> Debugging the getput property";
        print_endline "______Unexpected output_______";
        print_endline "The following derived tuples are wrong:";
        print_endline (String.concat " " (List.map (fun x -> colored_string "red" (string_of_fact x)) (List.map (fun (fact, edb_facts, all_clauses, detail) -> fact) explanation)));
        print_endline "________________";
        print_endline "Please choose one to inspect";
        let (fact, edb_facts, all_clauses, detail) = List.hd explanation in
        print_string @@ "Inspecting wrong tuple " ^ string_of_fact fact;
        print_endline @@ string_of_summary_explanation (fact, edb_facts, (List.filter (Lib.non is_builtin_rule) all_clauses), detail);
        (* chi nhung fact nao ma prednam cua no nam trong source schema thi moi bo qua *)
        let original_source_names = List.map get_schema_name (get_source_stts prog) in
        let source_names = original_source_names @ (List.map (fun x -> "!"^x) original_source_names) in
        let buggy_facts = List.filter (fun rt -> (Lib.non is_builtin_fact rt) && (not (List.mem (get_rterm_predname rt) source_names)) ) edb_facts in
        (if List.length buggy_facts > 0 then
            print_endline ("One of the facts " ^
            String.concat ", " (List.map (fun x -> colored_string "red" (string_of_fact x)) buggy_facts)
            ^ " is wrong; or one of the above rules is wrong, select one to inspect")
        else
            print_endline "One of the above rules is wrong , select one to inspect");

        let using_fact = if List.length buggy_facts > 0 then
            (print_endline "Type 1 for debugging the facts, type 2 for debugging the rules, type others for exit";
            match (Scanf.scanf " %d" (fun d -> d))  with
                    | option -> if (option=1) then true else
                                if (option=2) then false else exit 0
                    | exception Scanf.Scan_failure m -> print_endline m; exit 0
                    | exception End_of_file -> exit 0
            ) else false in
        if using_fact then
            print_endline ("Type a number from 1 to "^string_of_int (List.length buggy_facts)  ^" to inspect one fact in " ^
                String.concat ", " (List.map (fun x -> colored_string "red" (string_of_fact x)) buggy_facts)
                ^ " ( type others to exit)")
        else
            print_endline ("Type a number from 1 to "^string_of_int (List.length all_clauses )  ^ "to choose one rule in: \n" ^
                String.concat "" (List.mapi (fun num clause -> colored_string "red" (string_of_int (num+1) ^ ": "^string_of_rule clause)) all_clauses) ^
                " (type others to exit)");

            let option = match (Scanf.scanf " %d" (fun d -> d))  with
                        | d -> if (d > (List.length all_clauses) || d < 1) then exit 0 else d
                        | exception Scanf.Scan_failure m -> print_endline m; exit 0
                        | exception End_of_file -> print_endline "End of file"; exit 0 in
            let inspect_rule i =
                let (fact, clause, premises) = List.nth detail i in
                print_endline ("Inspecting "^ string_of_rule clause) ;
                print_endline ("    "^
                    String.concat " " (List.map string_of_fact premises)^
                    "\n    ------------------ "^ colored_string "red" (string_of_rule clause)^
                    "    "^ (string_of_fact fact)^" "^ (colored_string "red"  "is wrong")^
                    "\n");
                print_endline "Please change the rule such that";
                let head = rule_head clause in
                let upper_program = {get_empty_expr with view = prog.view ; sources = prog.sources; rules = (Stratification.get_preceding_rules prog head)} in
                let upper_facts,_ = Evaluation.eval false [] {upper_program with facts = (buggy_prog.facts)@upper_program.facts} in
                let upper_facts = List.filter (fun x -> (Lib.non is_missing_fact x) && (Lib.non is_builtin_fact x)  ) upper_facts in
                let derived_facts = extract_facts resulted_facts (get_rterm_relname head) in
                print_endline ("    "^
                String.concat " " (List.map string_of_fact upper_facts)^
                "\n    ~~~~~~~~~~~~~~~~~~~~~"^
                colored_string "blue" ("\n         || "^" << "^string_of_rterm head^" :- ... >>\n" ^
                "         \\/  \n")^
                "    ~~~~~~~~~~~~~~~~~~~~~\n"^
                "    " ^ String.concat " " (List.map string_of_fact derived_facts)^ (colored_string "red" (" --> excluding "^(string_of_fact fact)))^
                "\n\n"); in
            inspect_rule (option-1));
            ()

let check_constraints (log:bool) prog =
    let view_rt = get_view_rterm prog in
    let buggy_prog = delete_rule_of_predname (get_rterm_predname view_rt) (Expr.view_schema_to_source_schema prog) in
    if log then (print_endline "\n_______full program with counterexample_____" ;
        print_endline (string_of_prog buggy_prog);
        print_endline "________________");
    let resulted_facts, explanation = Evaluation.eval log [Expr.get_empty_pred] (Utils.constraint2rule buggy_prog) in
    if log then (print_endline "\n_______ derived facts _____" ;
        print_endline (string_of_prog {get_empty_expr with facts = resulted_facts} );
        print_endline "________________");
    ((List.length explanation) = 0)

let explain_getput (log:bool) prog =
    let view_rt = get_view_rterm prog in
    let buggy_prog = delete_fact_of_predname (get_rterm_predname view_rt) prog in
    if log then (print_endline "\n_______getput counterexample full program_____" ;
        print_endline (string_of_prog buggy_prog);
        print_endline "________________");
    let resulted_facts, explanation = Evaluation.eval log (get_delta_rterms buggy_prog) buggy_prog in
    (* filter the buggy delta tuple (insertion of new tuples, deletiong of existing tuple) *)
    let explanation = List.filter (fun (fact, edb_facts, all_clauses, detail) ->
                let init_facts = buggy_prog.facts in
                if ((string_of_fact fact).[0] = '-') then (
                    (* deletion: should delete the existing tuples *)
                    let facts = List.map (fun x -> "-" ^ string_of_fact x) init_facts in
                    List.mem (string_of_fact fact) facts
                )
                else if ((string_of_fact fact).[0] = '+') then (
                    (* insertion: should insert new tuples *)
                    let facts = List.map (fun x -> "+" ^ string_of_fact x) init_facts in
                    not (List.mem (string_of_fact fact) facts)
                )
                else false
        ) explanation in
    if (List.length explanation = 0) then
        print_endline "The getput property is satisfied"
    else (
        print_endline ">>> The getput property is not satisfied:";
        print_endline "+--------- Source --------------------------+";
        let source_facts_lst = List.map  (fun x -> extract_facts resulted_facts (get_rterm_relname x)) (get_source_rterms buggy_prog) in
        let source_facts = (List.concat source_facts_lst) in
        if (List.length source_facts > 0) then (
            List.iter (fun x -> print_string "| "; print_string (string_of_fact x)) source_facts
        )
        else
            print_endline "| empty";
        print_endline "+-------------------------------------------+";
        print_endline @@ colored_string "" ("     || \n     || get the view\n" ^
                "     \\/  \n")^
                "+--------- View ----------------------------+";
        let getted_view = extract_facts resulted_facts (get_rterm_predname view_rt) in
        if (List.length getted_view > 0) then (
            List.iter (fun x -> print_string "| "; print_string (string_of_fact x)) getted_view
        )
        else
            print_endline "| empty";
        print_endline "+-------------------------------------------+";
        print_endline @@ colored_string "" ("     || \n     || put to the source\n" ^
                "     \\/  \n")^
                "+--------- Deltas ------------------------------------+";
        let delta_facts_lst = List.map  (fun x -> extract_facts resulted_facts (get_rterm_relname x)) (get_delta_rterms buggy_prog) in
        let delta_facts = List.concat delta_facts_lst in
        if (List.length delta_facts > 0) then (
            List.iter (fun x -> print_string "| "; print_string (string_of_fact x)) delta_facts
        )
        else
            print_endline "| empty";
        print_string "|      ";
        print_endline (colored_string "" ">>> Unexpected tuples:");
        if (List.length explanation > 0) then
        ( print_endline "|      The following derived tuples are wrong:";
        List.iter (fun x -> print_string "|         ";
            print_string (colored_string "" (string_of_fact x)))
        (List.map (fun (fact, edb_facts, all_clauses, detail) -> fact) explanation));
        print_endline "+----------------------------------------------------+";
    )

let debug_disdelta (log:bool) prog =
    let delta_rt_lst = get_delta_rterms prog in
    (* get each pair of delta relations from the delta relation lst delta_rt_lst *)
    let delta_pair_lst =
        let pair_of_delta_insert lst ins_rel =
            let del_rels = List.filter (is_delta_pair ins_rel) delta_rt_lst in
            if (List.length del_rels = 0) then lst else (ins_rel, (List.hd del_rels))::lst in
        List.fold_left pair_of_delta_insert [] delta_rt_lst in
    let emptiness = Pred ("__emptiness",[]) in
    let disdelta_rules = List.map (fun (r1,r2) -> (rename_rterm "±" (get_source_rel_pred r1),[ Rel r1; Rel r2])) delta_pair_lst in
    let buggy_prog = Expr.add_rules (disdelta_rules) (Expr.delete_rule_of_predname (get_rterm_predname (get_view_rterm prog)) prog) in
    let buggy_prog = Expr.view_schema_to_source_schema buggy_prog in
    if log then (print_endline "\n_______disdelta counterexample full program_____" ;
        print_endline (string_of_prog buggy_prog);
        print_endline "________________");
    let resulted_facts, explanation = Evaluation.eval log (List.map rule_head disdelta_rules) buggy_prog in
    if (List.length explanation = 0) then
        print_endline ">>> The delta disjointness property is satisfied"
    else (
        (* interaction to choose the fact to be inspected *)
        print_endline ">>> Debugging the delta disjointness";
        print_endline "______Unexpected output_______";
        print_endline "The following derived tuples are wrong:";
        print_endline (String.concat " " (List.map (fun x -> colored_string "red" (string_of_fact x)) (List.map (fun (fact, edb_facts, all_clauses, detail) -> fact) explanation)));
        print_endline "________________";
        print_endline "Please choose one to inspect";
        let (fact, edb_facts, all_clauses, detail) = List.hd explanation in
        print_endline @@ "Inspecting wrong tuple " ^ string_of_fact fact;
        print_endline @@ string_of_summary_explanation (fact, edb_facts, (List.filter (Lib.non is_builtin_rule) all_clauses), detail);
        (* chi bo qua nhung fact nao ma prednam cua no nam trong source/view schema *)
        let original_source_view_names = List.map get_schema_name (get_schema_stts prog) in
        let source_view_names = original_source_view_names @ (List.map (fun x -> "!"^x) original_source_view_names) in
        let buggy_facts = List.filter (fun rt -> (Lib.non is_builtin_fact rt) && (not (List.mem (get_rterm_predname rt) source_view_names)) ) edb_facts in
        (if List.length buggy_facts > 0 then
            print_endline ("One of the facts " ^
            String.concat ", " (List.map (fun x -> colored_string "red" (string_of_fact x)) buggy_facts)
            ^ " is wrong; or one of the above rules is wrong, select one to inspect")
        else
            print_endline "One of the above rules is wrong , select one to inspect");

        let using_fact = if List.length buggy_facts > 0 then
            (print_endline "Type 1 for debugging the facts, type 2 for debugging the rules, type others for exit";
            match (Scanf.scanf " %d" (fun d -> d))  with
                    | option -> if (option=1) then true else
                                if (option=2) then false else exit 0
                    | exception Scanf.Scan_failure m -> print_endline m; exit 0
                    | exception End_of_file -> exit 0
            ) else false in
        if using_fact then
            print_endline ("Type a number from 1 to "^string_of_int (List.length buggy_facts)  ^" to inspect one fact in " ^
                String.concat ", " (List.map (fun x -> colored_string "red" (string_of_fact x)) buggy_facts)
                ^ " (type others to exit)")
        else
            print_endline ("Type a number from 1 to "^string_of_int (List.length all_clauses )  ^" to choose one rule in: \n" ^
                String.concat "" (List.mapi (fun num clause -> colored_string "red" (string_of_int (num+1) ^ ": "^string_of_rule clause)) (List.filter (Lib.non is_builtin_rule) all_clauses)) ^
                " (type others to exit)");

            let option = match (Scanf.scanf " %d" (fun d -> d))  with
                        | d -> if (d > (List.length all_clauses) || d < 1) then exit 0 else d
                        | exception Scanf.Scan_failure m -> print_endline m; exit 0
                        | exception End_of_file -> print_endline "End of file"; exit 0 in
            let inspect_rule i =
                let (fact, clause, premises) = List.nth detail i in
                print_endline ("Inspecting "^ string_of_rule clause) ;
                print_endline ("    "^
                    String.concat " " (List.map string_of_fact premises)^
                    "\n    ------------------ "^ colored_string "red" (string_of_rule clause)^
                    "    "^ (string_of_fact fact)^" "^ (colored_string "red"  "is wrong")^
                    "\n");
                print_endline "Please change the rule such that";
                let head = rule_head clause in
                let upper_program = {get_empty_expr with rules = (Stratification.get_preceding_rules buggy_prog head )} in
                let upper_facts,_ = Evaluation.eval false [] {upper_program with facts = buggy_prog.facts @ upper_program.facts} in
                let upper_facts = List.filter (fun x -> (Lib.non is_missing_fact x) && (Lib.non is_builtin_fact x)  ) upper_facts in
                let derived_facts = extract_facts resulted_facts (get_rterm_relname head) in
                print_endline ("    "^
                String.concat " " (List.map string_of_fact upper_facts)^
                "\n    ~~~~~~~~~~~~~~~~~~~~~"^
                colored_string "blue" ("\n         || "^" << "^string_of_rterm head^" :- ... >>\n" ^
                "         \\/  \n")^
                "    ~~~~~~~~~~~~~~~~~~~~~\n"^
                "    " ^ String.concat " " (List.map string_of_fact derived_facts)^ (colored_string "red" (" --> excluding "^(string_of_fact fact)))^
                "\n\n"); in
            inspect_rule (option-1));
            ()

let explain_disdelta (log:bool) prog =
    let view_rt = get_view_rterm prog in
    let delta_rt_lst = get_delta_rterms prog in
    (* get each pair of delta relations from the delta relation lst delta_rt_lst *)
    let delta_pair_lst =
        let pair_of_delta_insert lst ins_rel =
            let del_rels = List.filter (is_delta_pair ins_rel) delta_rt_lst in
            if (List.length del_rels = 0) then lst else (ins_rel, (List.hd del_rels))::lst in
        List.fold_left pair_of_delta_insert [] delta_rt_lst in
    let emptiness = Pred ("__emptiness",[]) in
    let disdelta_rules = List.map (fun (r1,r2) -> (rename_rterm "±" (get_source_rel_pred r1),[ Rel r1; Rel r2])) delta_pair_lst in
    let buggy_prog = Expr.add_rules (disdelta_rules) (Expr.delete_rule_of_predname (get_rterm_predname (get_view_rterm prog)) prog) in
    let buggy_prog = Expr.view_schema_to_source_schema buggy_prog in
    if log then (print_endline "\n_______disdelta counterexample full program_____" ;
        print_endline (string_of_prog buggy_prog);
        print_endline "________________");
    let resulted_facts, explanation = Evaluation.eval log (List.map rule_head disdelta_rules) buggy_prog in
    if (List.length explanation = 0) then
        print_endline "The delta disjointness property is satisfied"
    else (
        print_endline ">>> The delta disjointness property is not satisfied: ";
        print_endline "+--------- Updated View -----------------+";
        let init_view = extract_facts buggy_prog.facts (get_rterm_predname view_rt) in
        if (List.length init_view > 0) then (
            List.iter (fun x -> print_string "| "; print_string (string_of_fact x)) init_view
        )
        else
            print_endline "| empty";
        print_endline "+----------------------------------------+";
        print_endline @@ colored_string "" ("     || \n     || put to the source\n" ^
                "     \\/  \n")^
                "+--------- Deltas ------------------------------------+";
        let delta_facts_lst = List.map  (fun x -> extract_facts resulted_facts (get_rterm_relname x)) (get_delta_rterms buggy_prog) in
        let delta_facts = List.concat delta_facts_lst in
        if (List.length delta_facts > 0) then (
            List.iter (fun x -> print_string "| "; print_string (string_of_fact x)) delta_facts
        )
        else
            print_endline "| empty";
        print_string "|      ";
        print_endline (colored_string "" ">>> Unexpected tuples:");
        if (List.length explanation > 0) then
        ( print_endline "|      The following tuples are both inserted and deleted on a relation:";
          List.iter (fun x -> print_string "|         ";
            print_endline (colored_string "" (string_of_fact x))) (List.map (fun (fact, edb_facts, all_clauses, detail) -> fact) explanation));
        print_endline "+------------------------------------------------------+";
    )


let debug_putget (log:bool) prog =
    let buggy_prog = prog in
    let view_rt = get_view_rterm prog in
    let buggy_putget_prog = delete_rule_of_predname (get_rterm_predname view_rt) (Expr.view_schema_to_source_schema (Derivation.datalog_of_putget log false buggy_prog)) in
    if log then (print_endline "\n_______putget counterexample full program_____" ;
        print_endline (string_of_prog buggy_putget_prog);
        print_endline "________________");
    let resulted_facts, explanation = Evaluation.eval log [rename_rterm "__dummy_new_" view_rt] buggy_putget_prog in
    let updated_view = List.map (rename_fact "__dummy_new_") (extract_facts buggy_putget_prog.facts (get_rterm_predname view_rt)) in
    (* find wrong tuples in new view which is not in updated_view *)
    let wrong_tuples_explanation = List.filter (fun (fact, edb_facts, all_clauses, detail) -> List.length (List.filter (fun x -> string_of_fact x = string_of_fact fact) updated_view) = 0) explanation in
    let new_view = (List.map (fun (fact, edb_facts, all_clauses, detail) -> fact) explanation) in
    let new_view_missing_facts = List.filter (fun fact -> List.length (List.filter (fun x -> string_of_fact x = string_of_fact fact) new_view) = 0)  updated_view in
    if ((List.length wrong_tuples_explanation + List.length new_view_missing_facts) = 0) then
        print_endline "The putget property is satisfied"
    else (
        print_endline ">>> Debugging the putget property";

        print_endline "______Unexpected output_______";
        if (List.length wrong_tuples_explanation > 0) then
        ( print_endline "The following derived tuples are wrong:";
            print_endline (String.concat " " (List.map (fun x -> colored_string "red" (string_of_fact x)) (List.map (fun (fact, edb_facts, all_clauses, detail) -> fact) wrong_tuples_explanation))));
        if (List.length new_view_missing_facts > 0) then
            (print_endline "The following tuples are missing:";
            print_endline (String.concat " " (List.map (fun x -> colored_string "purple" (string_of_fact x)) new_view_missing_facts)));
        print_endline "________________";
        print_endline "Please choose one to inspect";

        let (fact, edb_facts, all_clauses, detail) = List.hd wrong_tuples_explanation in
        print_endline @@ "Inspecting wrong tuple " ^ string_of_fact fact;
        print_endline @@ string_of_summary_explanation (fact, edb_facts, all_clauses, detail);
        (* chi bo qua nhung fact nao ma prednam cua no nam trong source/view schema *)
        let original_source_view_names = List.map get_schema_name (get_schema_stts prog) in
        let source_view_names = original_source_view_names @ (List.map (fun x -> "!"^x) original_source_view_names) in
        let buggy_facts = List.filter (fun rt -> (Lib.non is_builtin_fact rt) && (not (List.mem (get_rterm_predname rt) source_view_names))) edb_facts in
        (if List.length buggy_facts > 0 then
            print_endline ("One of the facts " ^
            String.concat ", " (List.map (fun x -> colored_string "red" (string_of_fact x)) buggy_facts)
            ^ " is wrong; or one of the above rules is wrong, select one to inspect")
        else
            print_endline "One of the above rules is wrong , select one to inspect");

        let using_fact = if List.length buggy_facts > 0 then
            (print_endline "Type 1 for debugging the facts, type 2 for debugging the rules, type others for exit";
            match (Scanf.scanf " %d" (fun d -> d))  with
                    | option -> if (option=1) then true else
                                if (option=2) then false else exit 0
                    | exception Scanf.Scan_failure m -> print_endline m; exit 0
                    | exception End_of_file -> exit 0
            ) else false in
        if using_fact then
            print_endline ("Type a number from 1 to "^string_of_int (List.length buggy_facts)  ^" to inspect one fact in " ^
                String.concat ", " (List.map (fun x -> colored_string "red" (string_of_fact x)) buggy_facts)
                ^ " (type others to exit)")
        else
            print_endline ("Type a number from 1 to "^string_of_int (List.length all_clauses )  ^" to choose one rule in: \n" ^
                String.concat "" (List.mapi (fun num clause -> colored_string "red" (string_of_int (num+1) ^ ": "^string_of_rule clause)) (List.filter (Lib.non is_builtin_rule) all_clauses)) ^
                " (type others to exit)");

            let option = match (Scanf.scanf " %d" (fun d -> d))  with
                        | d -> if (d > (List.length all_clauses) || d < 1) then exit 0 else d
                        | exception Scanf.Scan_failure m -> print_endline m; exit 0
                        | exception End_of_file -> print_endline "End of file"; exit 0 in
            let inspect_rule i =
                let (fact, clause, premises) = List.nth detail i in
                print_endline ("Inspecting "^ string_of_rule clause) ;
                print_endline ("    "^
                    String.concat " " (List.map string_of_fact premises)^
                    "\n    ------------------ "^ colored_string "red" (string_of_rule clause)^
                    "    "^ (string_of_fact fact)^" "^ (colored_string "red"  "is wrong")^
                    "\n");
                print_endline "Please change the rule such that";
                let head = rule_head clause in
                let upper_program = {get_empty_expr with view = buggy_putget_prog.view; sources = buggy_putget_prog.sources; rules = (Stratification.get_preceding_rules buggy_putget_prog head)} in
                let upper_facts,_ = Evaluation.eval false [] {upper_program with facts = buggy_putget_prog.facts @ upper_program.facts} in
                let upper_facts = List.filter (fun x -> (Lib.non is_missing_fact x) && (Lib.non is_builtin_fact x)  ) upper_facts in
                let derived_facts = extract_facts resulted_facts (get_rterm_relname head) in
                print_endline ("    "^
                String.concat " " (List.map string_of_fact upper_facts)^
                "\n    ~~~~~~~~~~~~~~~~~~~~~"^
                colored_string "blue" ("\n         || "^" << "^string_of_rterm head^" :- ... >>\n" ^
                "         \\/  \n")^
                "    ~~~~~~~~~~~~~~~~~~~~~\n"^
                "    " ^ String.concat " " (List.map string_of_fact derived_facts)^ (colored_string "red" (" --> excluding "^(string_of_fact fact)))^
                "\n\n"); in
            inspect_rule (option-1));
            ()

let explain_putget (log:bool) prog =
    let buggy_prog = prog in
    let view_rt = get_view_rterm prog in
    let buggy_putget_prog = delete_rule_of_predname (get_rterm_predname view_rt) (Expr.view_schema_to_source_schema (Derivation.datalog_of_putget log false buggy_prog)) in
    if log then (print_endline "\n_______putget counterexample full program_____" ;
        print_endline (string_of_prog buggy_putget_prog);
        print_endline "________________");
    let resulted_facts, explanation = Evaluation.eval log [rename_rterm "__dummy_new_" view_rt] buggy_putget_prog in
    let init_view = extract_facts buggy_putget_prog.facts (get_rterm_predname view_rt) in
    let updated_view = List.map (rename_fact "__dummy_new_") init_view in
    (* find wrong tuples in new view which is not in updated_view *)
    let wrong_tuples_explanation = List.filter (fun (fact, edb_facts, all_clauses, detail) -> List.length (List.filter (fun x -> string_of_fact x = string_of_fact fact) updated_view) = 0) explanation in
    let new_view = (List.map (fun (fact, edb_facts, all_clauses, detail) -> fact) explanation) in
    let new_view_missing_facts = List.filter (fun fact -> List.length (List.filter (fun x -> string_of_fact x = string_of_fact fact) new_view) = 0)  updated_view in
    if ((List.length wrong_tuples_explanation + List.length new_view_missing_facts) = 0) then
        print_endline "The putget property is satisfied"
    else (
        print_endline ">>> The putget property is not satisfied:";
        print_endline "+--------- Updated View -----------------+";
        if (List.length init_view > 0) then (
            List.iter (fun x -> print_string "| "; print_string (string_of_fact x)) init_view
        )
        else
            print_endline "| empty";
        print_endline "+----------------------------------------+";
        print_endline @@ colored_string "" ("     || \n     || put to the source\n" ^
                "     \\/  \n")^
                "+--------- Deltas ------------------------------------+";
        let delta_facts_lst = List.map  (fun x -> extract_facts resulted_facts (get_rterm_relname x)) (get_delta_rterms buggy_putget_prog) in
        let delta_facts = List.concat delta_facts_lst in
        if (List.length delta_facts > 0) then (
            List.iter (fun x -> print_string "| "; print_string (string_of_fact x)) delta_facts
        )
        else
            print_endline "| empty";
        print_endline "+------------------------------------------------------+";
        print_endline @@ colored_string "" ("     || \n     || apply to the source\n" ^
                "     \\/  \n")^
                "+--------- New source ------------------------+";
        let new_source_facts_lst = List.map  (fun x -> extract_facts resulted_facts (get_rterm_relname x)) (List.map (rename_rterm "__dummy_new_") (get_source_rterms buggy_prog)) in
        let new_source_facts = (List.concat new_source_facts_lst) in
        if (List.length new_source_facts > 0) then (
            List.iter (fun x -> print_string "| "; print_string (String.sub (string_of_fact x) 12 (String.length (string_of_fact x) -12) )) new_source_facts
        )
        else
            print_endline "| empty";
        print_endline "+---------------------------------------------+";
        print_endline @@ colored_string "" ("     || \n     || get the view again\n" ^
                "     \\/  \n")^
                "+--------- Recomputed View ------------------------+";
        if (List.length new_view > 0) then (
            List.iter (fun x -> print_string "| "; print_string (String.sub (string_of_fact x) 12 (String.length (string_of_fact x) -12))) new_view
        )
        else
            print_endline "| empty";
        print_string "|      ";
        print_endline (colored_string "" ">>> Unexpected tuples:");
        if (List.length wrong_tuples_explanation > 0) then
        ( print_endline "|      The following derived tuples are wrong:";
            List.iter (fun x -> print_string "|         ";
            print_string (colored_string "" (String.sub (string_of_fact x) 12 (String.length (string_of_fact x) -12))))
            (List.map (fun (fact, edb_facts, all_clauses, detail) -> fact) wrong_tuples_explanation));
        if (List.length new_view_missing_facts > 0) then
            (print_endline "|      The following tuples are missing:";
            List.iter (fun x -> print_string "|         ";
            print_string (colored_string "" (String.sub (string_of_fact x) 12 (String.length (string_of_fact x) -12)))) new_view_missing_facts);
        print_endline "+--------------------------------------------------+";
    )
