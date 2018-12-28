(* 
@author: Vandang Tran
*)

open Expr;; 
open Utils;;
open Rule_preprocess;;

let rec lambda_of_symtkey (idb:symtable) (cnt:colnamtab) (goal:symtkey)  =
    let rule_lst = Hashtbl.find idb goal in
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

let lambda_of_stt (debug:bool) (edb:symtable) prog =
    (* todo: need to check if prog is non-recursive *)
    let query_rt = get_query_rterm (get_query prog) in
    (*Extract and pre-process the IDB from the program*)
    let idb = extract_idb prog in
    preprocess_rules idb; 
    (* print_symtable idb; *)
    (*Build the colnamtab for referencing the table's columns*)
    let cnt = build_colnamtab edb idb in
    let lambda = lambda_of_query idb cnt query_rt  in
    lambda;;