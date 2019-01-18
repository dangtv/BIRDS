(*******************************************************)
(**  
Theorem generation for verification
 *)
(********************************************************)
(* 
@author: Vandang Tran
*)

open Expr;;
open Utils;;
open Rule_preprocess;;
open Stratification;;

let rec lambda_of_symtkey (idb:symtable) (cnt:colnamtab) (goal:symtkey)  =
    let rule_lst = Hashtbl.find idb goal in
    let lambda_of_rule_lst (idb:symtable) (cnt:colnamtab) rules =
        let lambda_of_rule (idb:symtable) (cnt:colnamtab) rule =
            let lambda_of_rterm r = 
                let lst = get_rterm_varlist r in
                let ind = 0 in 
                let anony_names, var_lst = List.fold_right (fun v (anony_names,vars) -> match v with AnonVar -> let alias = "anon_"^ string_of_int (List.length lst -1 - List.length vars) in (alias :: anony_names , (NamedVar alias) :: vars) | _ -> (anony_names,v::vars) )  lst ([],[]) in
                (if (List.length anony_names >0) then "∃ " ^ String.concat " " anony_names ^ ", "
                else "")^
                if Hashtbl.mem idb (symtkey_of_rterm r) then 
                "(" ^ lambda_of_symtkey idb cnt (symtkey_of_rterm r) ^") " ^ String.concat "  " (List.map string_of_var var_lst) 
                else  
                get_rterm_predname r ^ " " ^ String.concat "  " (List.map string_of_var var_lst) in
            let head = rule_head rule in
            let body = rule_body rule in
            let (p_rt,n_rt,all_eqs,all_ineqs) = split_terms body in
            let exvars = VarSet.filter (fun x -> not (is_anon x)) (VarSet.diff (get_termlst_varset body) (VarSet.of_list (get_rterm_varlist head))) in
            "λ " ^ String.concat " " (List.map string_of_var (get_rterm_varlist head))
            ^ ", " ^ 
            (* for existential variables *)
            (if (VarSet.is_empty exvars) then "" else "∃ " ^ String.concat " " (List.map string_of_var (VarSet.elements exvars)) ^", ")
            (* positive predicate *)
            ^ String.concat " ∧ " (List.map (fun x -> "(" ^ lambda_of_rterm x^")" ) p_rt)
            (* negative predicate *)
            ^ (if (List.length n_rt) > 0 then " ∧ " else "" ) ^ String.concat " ∧ " (List.map (fun x -> "¬ (" ^ lambda_of_rterm x^")" ) n_rt) 
            (* conjunction of all_eqs and all_ineqs *)
            ^ (if (List.length (all_eqs@all_ineqs)) > 0 then " ∧ " else "" ) ^ String.concat " ∧ " (List.map (fun x -> "(" ^ string_of_term x^")" ) (all_eqs@all_ineqs))
            in
        let lambda_list = List.map (lambda_of_rule idb cnt) rules in
        let cols = gen_cols 0 (snd goal) in
        "λ " ^ String.concat " " cols ^ ", " ^
        String.concat " ∨ "  (List.map (fun pred -> "(" ^ pred^ ") " ^ String.concat " " cols) lambda_list) in
    let lambda_expr = lambda_of_rule_lst idb cnt rule_lst in
    lambda_expr

(** take a query term and rules of idb relations stored in a symtable, generate lambda expression for it *)
let lambda_of_query (idb:symtable) (cnt:colnamtab) (query:rterm) =
    let qrule = rule_of_query query idb in
        let local_idb = Hashtbl.copy idb in 
        symt_insert local_idb qrule;
        lambda_of_symtkey local_idb cnt (symtkey_of_rterm (rule_head qrule))

let lambda_of_stt (debug:bool) (edb:symtable) prog =
    (* todo: need to check if prog is non-recursive *)
    let query_rt = get_query_rterm (get_query prog) in
    (*Extract and pre-process the IDB from the program*)
    let idb = extract_idb prog in
    preprocess_rules idb; 
    (*Build the colnamtab for referencing the table's columns*)
    let cnt = build_colnamtab edb idb in
    (*Return the desired lambda expression*)
    let lambda = lambda_of_query idb cnt query_rt  in
    lambda

let edb_to_func_types edb =
    (* currently just set all the types are int (ℤ) *)
    let rel_to_function rel = get_rterm_predname rel ^ ": " ^ 
        String.concat " → " ( List.map (fun x -> "ℤ") (get_rterm_varlist rel)) ^ " → Prop" in 
    let p_el funcs s = (rel_to_function (rule_head s))::funcs in
    let p_lst _ lst funcs = (List.fold_left p_el [] lst)@funcs in
    Hashtbl.fold p_lst edb []

(* take a view update datalog program and generate the theorem of checking whether all delta relations are disjoint *)
let lean_theorem_of_disjoint_delta (debug:bool) (edb:symtable) prog = 
    (* need to change the view (in query predicate) to a edb relation *)
    let query_rt = get_query_rterm (get_query prog) in
    (* need to convert the query to be an edb relation *)
    let local_edb = Hashtbl.copy edb in
    symt_insert local_edb (Rule(query_rt,[]));
    let idb = extract_idb prog in
    preprocess_rules idb;
    let cnt = build_colnamtab local_edb idb in
    let delta_rt_lst = get_delta_rterms prog in
    (* get each pair of delta relations from the delta relation lst delta_rt_lst *)
    let delta_pair_lst = 
        let pair_of_delta_insert lst ins_rel = 
            let del_rels = List.filter (is_delta_pair ins_rel) delta_rt_lst in 
            if (List.length del_rels = 0) then lst else (ins_rel, (List.hd del_rels))::lst in 
        List.fold_left pair_of_delta_insert [] delta_rt_lst in 
    
    (* get the emptiness FO sentence of a relation *)
    let disjoint_fo_sentence ins_rel del_rel = 
        let cols = gen_cols 0 (get_arity ins_rel) in
        "∃ " ^ String.concat " " cols ^ ", (" ^  (lambda_of_query idb cnt ins_rel) ^ ") "  ^ String.concat " " cols ^ " ∧ " ^ "(" ^  (lambda_of_query idb cnt del_rel) ^ ") "  ^ String.concat " " cols in
    let djsjoint_sen_lst = List.map (fun (r1,r2) -> disjoint_fo_sentence r1 r2) delta_pair_lst in 
    "theorem disjoint_deltas " ^ String.concat " " (List.map (fun x -> "{"^x^"}") (edb_to_func_types local_edb)) ^ ": " ^ (String.concat " ∨ " (List.map (fun pred -> "(" ^ pred^ ")") djsjoint_sen_lst)) ^ " → false"

(* take a view update datalog program and generate the theorem of checking whether all delta relations are disjoint *)
let lean_simp_theorem_of_disjoint_delta (debug:bool) (edb:symtable) prog = 
    let query_rt = get_query_rterm (get_query prog) in
    let local_edb = Hashtbl.copy edb in
    symt_insert local_edb (Rule(query_rt,[]));
    "theorem disjoint_deltas " ^ String.concat " " (List.map (fun x -> "{"^x^"}") (edb_to_func_types local_edb)) ^ ": " ^ (Fol_ex.lean_string_of_fol_formula (Imp(Ast2fol.disjoint_delta_sentence_of_stt debug edb prog, False)))