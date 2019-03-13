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

(** for non-recursive datalog, we do not need stratification, we just need recursively translate each idb predicate (identified symkey) to a lambda expression, 
this function take a symtkey of a rterm and generate its lambda expreesion recursively (this function is recursive because of unfolding all the idb predicate)*)
let rec lambda_of_symtkey (idb:symtable) (cnt:colnamtab) (goal:symtkey)  =
    let rule_lst = 
        try Hashtbl.find idb goal 
        with Not_found -> print_endline ("Not_found in idb the goal "^string_of_symtkey goal); exit 0;
        in
    (* disjunction of all rules then we have lambda expression for a idb predicate*)
    let lambda_of_rule_lst (idb:symtable) (cnt:colnamtab) rules =
        let lambda_of_rule (idb:symtable) (cnt:colnamtab) rule =
            let lambda_of_rterm r = 
                let lst = get_rterm_varlist r in
                (* convert anonymous variables to named variable with alias,
                they will be existential varialbes *)
                let ind = 0 in 
                let anony_names, var_lst = List.fold_right (fun v (anony_names,vars) -> match v with AnonVar -> let alias = "anon_"^ string_of_int (List.length lst -1 - List.length vars) in (alias :: anony_names , (NamedVar alias) :: vars) | _ -> (anony_names,v::vars) )  lst ([],[]) in
                (if (List.length anony_names >0) then "∃ " ^ String.concat " " anony_names ^ ", "
                else "")^
                if Hashtbl.mem idb (symtkey_of_rterm r) then 
                (* in the case that the predicate is of idb relation, need to recursive construct lambda expression for it *)
                "(" ^ lambda_of_symtkey idb cnt (symtkey_of_rterm r) ^") " ^ String.concat "  " (List.map string_of_var var_lst) 
                else  
                (* if this predicate is of an edb relation, just need to call by its name *)
                get_rterm_predname r ^ " " ^ String.concat "  " (List.map string_of_var var_lst) in
            let head = rule_head rule in
            let body = rule_body rule in
            let (p_rt,n_rt,all_eqs,all_ineqs) = split_terms body in
            (* lambda argument is vars in head *)
            (* existential vars of the body is vars in body but not in the head *)
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
    lambda_expr;;

(** take a query term and rules of idb relations stored in a symtable, generate lambda expression for it *)
let lambda_of_query (idb:symtable) (cnt:colnamtab) (query:rterm) =
    (* query is just a rterm which is a predicate therefore need to create a new temporary rule for this query term 
    for example if query is q(X,Y,_,5) we create a rule for it: _dummy_(X,Y) :- q(X,Y,_,Z), Z=5. (_dummy_ is a fixed name in the function rule_of_query)
    *)
    let qrule = rule_of_query query idb in
    (* qrule is in the form of _dummy_(x,y) :- query_predicate(x,y), x=1 *)
        let local_idb = Hashtbl.copy idb in 
        (* because insert a temporary dummy qrule, we should work with a local variable of idb *)
        symt_insert local_idb qrule;
        lambda_of_symtkey local_idb cnt (symtkey_of_rterm (rule_head qrule));;

(** generate lambda expression from the ast, the goal is the query predicate of datalog program, receives a symtable of the database's edb description.
The boolean variable debug indicates whether debugging information should be printed
*)
let lambda_of_stt (debug:bool) prog =
    let edb = extract_edb prog in
    (* todo: need to check if prog is non-recursive *)
    let view_rt = get_schema_rterm (get_view prog) in
    (*Extract and pre-process the IDB from the program*)
    let idb = extract_idb prog in
    preprocess_rules idb; 
    (* print_symtable idb; *)
    (*Build the colnamtab for referencing the table's columns*)
    let cnt = build_colnamtab edb idb in
    (*Return the desired lambda expression*)
    let lambda = lambda_of_query idb cnt view_rt  in
    lambda;;

(* transform edb relations to a list of functions from product of n (the arity) types to Prop *)
let edb_to_func_types edb =
    (* currently just set all the types are int (ℤ) *)
    let rel_to_function rel = get_rterm_predname rel ^ ": " ^ 
        String.concat " → " ( List.map (fun x -> "ℤ") (get_rterm_varlist rel)) ^ " → Prop" in 
    let p_el funcs s = (rel_to_function (rule_head s))::funcs in
    let p_lst _ lst funcs = (List.fold_left p_el [] lst)@funcs in
    Hashtbl.fold p_lst edb [];;

let stype_to_lean_type st = match st with 
    (* | Sint -> "ℤ" *)
    | Sint -> "int"
    (* | Sreal -> "ℝ" *)
    | Sreal -> "real"
    | Sbool -> "Prop"
    | Sstring -> "string"

let stype_to_z3_type st = match st with 
    (* | Sint -> "ℤ" *)
    | Sint -> "Int"
    (* | Sreal -> "ℝ" *)
    | Sreal -> "Real"
    | Sbool -> "Bool"
    | Sstring -> "String"

(* transform source relations in program to a list of functions from product of n (the arity) types to Prop *)
let source_to_lean_func_types prog = match prog with 
    Prog sttlst ->
    (* currently just set all the types are int (ℤ) *)
    let p_el funcs s = match s with 
        Rule _ -> funcs
        | Query _ -> funcs
        | Constraint _ -> funcs 
        | View _ -> funcs 
        | Source (name, lst) -> ( name ^ ": " ^ String.concat " → " ( List.map (fun (col,typ) -> stype_to_lean_type typ) lst) ^ " → Prop" )::funcs in
    List.fold_left p_el [] sttlst;;

(* transform source relations in program to a list of functions from product of n (the arity) types to Prop *)
let source_to_z3_func_types prog = match prog with 
    Prog sttlst ->
    (* currently just set all the types are int (ℤ) *)
    let p_el funcs s = match s with 
        Rule _ -> funcs
        | Query _ -> funcs
        | Constraint _ -> funcs 
        | View _ -> funcs 
        | Source (name, lst) -> ( "(declare-fun " ^name ^ " (" ^ 
        String.concat " " ( List.map (fun (col,typ) -> stype_to_z3_type typ) lst) ^ ") Bool)" )::funcs in
    List.fold_left p_el [] sttlst;;

(* transform source and view relations in program to a list of functions from product of n (the arity) types to Prop *)
let source_view_to_lean_func_types prog =match prog with 
    Prog sttlst ->
    (* currently just set all the types are int (ℤ) *)
    let p_el funcs s = match s with 
        Rule _ -> funcs
        | Query _ -> funcs
        | Constraint _ -> funcs 
        | Source (name, lst)  
        | View (name, lst) -> ( name ^ ": " ^ String.concat " → " ( List.map (fun (col,typ) -> stype_to_lean_type typ) lst) ^ " → Prop" )::funcs in
    List.fold_left p_el [] sttlst;;

(* transform source and view relations in program to a list of functions from product of n (the arity) types to Prop *)
let source_view_to_z3_func_types prog =match prog with 
    Prog sttlst ->
    (* currently just set all the types are int (ℤ) *)
    let p_el funcs s = match s with 
        Rule _ -> funcs
        | Query _ -> funcs
        | Constraint _ -> funcs 
        | Source (name, lst)  
        | View (name, lst) -> ( "(declare-fun " ^name ^ " (" ^ 
        String.concat " " ( List.map (fun (col,typ) -> stype_to_z3_type typ) lst) ^ ") Bool)" )::funcs in
    List.fold_left p_el [] sttlst;;

(* take a view update datalog program and generate the theorem of checking whether all delta relations are disjoint *)
let lean_theorem_of_disjoint_delta (debug:bool) prog = 
    (* need to change the view (in query predicate) to a edb relation *)
    let edb = extract_edb prog in 
    let view_rt = get_schema_rterm (get_view prog) in
    (* need to convert the query to be an edb relation *)
    symt_insert edb (Rule(view_rt,[]));
    let idb = extract_idb prog in
    preprocess_rules idb;
    let cnt = build_colnamtab edb idb in
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
    "theorem disjoint_deltas " ^ String.concat " " (List.map (fun x -> "{"^x^"}") (source_view_to_lean_func_types prog)) ^ ": " ^ (String.concat " ∨ " (List.map (fun pred -> "(" ^ pred^ ")") djsjoint_sen_lst)) ^ " → false";;

(* take a view update datalog program and generate the theorem of checking whether all delta relations are disjoint *)
let lean_simp_theorem_of_disjoint_delta (debug:bool) prog = 
    if debug then (print_endline "==> generating theorem for disjoint deltas";) else ();
    "theorem disjoint_deltas " ^ String.concat " " (List.map (fun x -> "{"^x^"}") (source_view_to_lean_func_types prog)) ^ ": " ^ (Fol_ex.lean_string_of_fol_formula (Imp (Ast2fol.constraint_sentence_of_stt debug prog, (Imp(Ast2fol.disjoint_delta_sentence_of_stt debug prog, False)))));;

let lean_simp_theorem_of_getput (debug:bool) prog = 
    if debug then (print_endline "==> generating theorem of getput property";) else ();
    "theorem getput " ^ String.concat " " (List.map (fun x -> "{"^x^"}") (source_to_lean_func_types prog)) ^
     ": " ^ (Fol_ex.lean_string_of_fol_formula (Imp (Ast2fol.non_view_constraint_sentence_of_stt debug prog, 
     (Imp(Ast2fol.getput_sentence_of_stt debug prog, False)))));;

let lean_simp_theorem_of_putget (debug:bool) prog = 
    if debug then (print_endline "==> generating theorem of putget property";) else ();
    "theorem putget " ^ String.concat " " (List.map (fun x -> "{"^x^"}") (source_view_to_lean_func_types prog)) ^ ": " ^ (Fol_ex.lean_string_of_fol_formula (Imp (Ast2fol.constraint_sentence_of_stt debug prog, Ast2fol.putget_sentence_of_stt debug prog)));;

(* take a view update datalog program and generate the theorem of checking whether all delta relations are disjoint *)
let z3_assert_of_disjoint_delta (debug:bool) prog = 
    if debug then (print_endline "==> generating z3 assert for disjoint deltas";) else ();
    String.concat "\n"  (source_view_to_z3_func_types prog) ^ 
    "\n (assert " ^ (Fol_ex.z3_string_of_fol_formula (Not (Imp (Ast2fol.constraint_sentence_of_stt debug prog, 
    (Imp(Ast2fol.disjoint_delta_sentence_of_stt debug prog, False)))))) ^ ") \n (check-sat)";;

let z3_assert_of_getput (debug:bool) prog = 
    if debug then (print_endline "==> generating z3 assert of getput property";) else ();
    String.concat "\n"  (source_to_z3_func_types prog) ^
     "\n(assert " ^ (Fol_ex.z3_string_of_fol_formula (Not (Imp (Ast2fol.non_view_constraint_sentence_of_stt debug prog, 
     (Imp(Ast2fol.getput_sentence_of_stt debug prog, False)))))) ^") \n (check-sat)";;

let z3_assert_of_putget (debug:bool) prog = 
    if debug then (print_endline "==> generating z3 assert of putget property";) else ();
    String.concat " " (source_view_to_z3_func_types prog) ^ 
    "\n (assert " ^ (Fol_ex.lean_string_of_fol_formula 
    (Not (Imp (Ast2fol.constraint_sentence_of_stt debug prog, Ast2fol.putget_sentence_of_stt debug prog)))) ^ 
    ")\n (check-sat)";;

(* (unnecessary now see sourcestability_sentence_of_stt in ast2fol.ml) take a view update datalog program and generate SourceStability constraint (put s v = s) for its view update strategy *)
let sourcestability_of_stt (debug:bool) prog =
    let edb = extract_edb prog in
    (* need to change the view (in query predicate) to a edb relation *)
    let view_rt = get_schema_rterm (get_view prog) in
    (* need to convert the view to be an edb relation *)
    symt_insert edb (Rule(view_rt,[]));
    let idb = extract_idb prog in
    symt_remove idb (symtkey_of_rterm view_rt);
    preprocess_rules idb;
    let cnt = build_colnamtab edb idb in
    let delta_rt_lst = get_delta_rterms prog in
    (* get the emptiness FO sentence of a relation *)
    let emptiness_fo_sentence rel = 
        let cols = gen_cols 0 (get_arity rel) in
        "∃ " ^ String.concat " " cols ^ ", (" ^  (lambda_of_query idb cnt rel) ^ ") "  ^ String.concat " " cols  in
    let delta_lambda_exp_lst = List.map emptiness_fo_sentence delta_rt_lst in 
    "theorem sourcestability " ^ String.concat " " (List.map (fun x -> "{"^x^"}") (source_view_to_lean_func_types prog)) ^ ": " ^ (String.concat " ∨ " (List.map (fun pred -> "(" ^ pred^ ")") delta_lambda_exp_lst)) ^ " → false"
;;

let lean_simp_sourcestability_theorem_of_stt (debug:bool) prog = 
    "theorem sourcestability " ^ String.concat " " (List.map (fun x -> "{"^x^"}") (source_to_lean_func_types prog)) ^ ": " ^ (Fol_ex.lean_string_of_fol_formula (Imp(Ast2fol.sourcestability_sentence_of_stt debug prog, False)));;

let gen_lean_code_for_theorems thms = 
    "import logic.basic
import tactic.basic
import tactic.finish
import tactic.interactive
import bx
import super
import smt2
universe u
open int
open auto

local attribute [instance] classical.prop_decidable -- make all prop decidable

" ^ String.concat "\n\n" (List.map (fun x ->  x ^ ":= 
    begin
    intro h,
    try{rw[imp_false] at *},
    try{simp at *},
    revert h,
    z3_smt,
    end") thms)

let validity_lean_code_of_datalog (debug:bool) prog = 
    gen_lean_code_for_theorems [
        (lean_simp_theorem_of_disjoint_delta ( debug) prog); 
        (lean_simp_theorem_of_getput ( debug) prog);
        (lean_simp_theorem_of_putget ( debug) prog)
        ]