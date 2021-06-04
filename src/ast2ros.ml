(*******************************************************)
(**  
AST-to-Racket functions
 *)
(********************************************************)
(* 
@author: Vandang Tran
*)

open Expr2 
open Utils
open Rule_preprocess
open Stratification
open Derivation

(** Given an aggregate function name, check if it is supported and returns it. *)
let check_agg_function fn =
    let allowed = ["MAX";"MIN";"SUM";"AVG";"COUNT"] in
    if List.mem fn allowed then fn
    else raise (SemErr (
        "Aggregate function '"^fn^"' is not supported, "^
        "allowed functions are: "^(String.concat ", " allowed)
    ))

(** Given an arithmetic expression, return it in racket, this function is similar to string_of_vterm. *)
let racket_of_vterm (vt:vartab) (eqt:eqtab) (expr:vterm)  = 
        let open_paren prec op_prec = 
            if prec > op_prec then  "(" else "" in 
        let close_paren prec op_prec = 
            if prec > op_prec then  ")" else "" in
        let rec racket_of prec a_expr = match a_expr with 
            | Const c -> string_of_const c
            | Var variable -> 
                (*If the variable appears in a positive rterm, the value
                * is the name of the respective rterm's table column*)
                if Hashtbl.mem vt (string_of_var variable)
                    then List.hd (Hashtbl.find vt (string_of_var variable))
                (*If the variable does not appear in a positive rterm, but
                * it does in an equality value, then the value is the eq's evaluation*)
                else if Hashtbl.mem eqt (Var variable)
                    then 
                    let ve = (Hashtbl.find eqt (Var variable)) in racket_of prec ve
                (*Else, the query is unsafe or inclomplete*)
                else raise (SemErr (
                        "Can not evaluate variable "^(string_of_var variable)^" because it is not in a positive "^
                        "goal or strict equality relation."
                    )
                )
            | BinaryOp("+",f,g) -> (open_paren prec 0)^ (racket_of 0 f) ^ "+" ^ (racket_of 0 g) ^ (close_paren prec 0)
            | BinaryOp("-",f,g) -> (open_paren prec 0) ^ (racket_of 0 f) ^  "-" ^ (racket_of 1 g) ^ (close_paren prec 0)
            | BinaryOp("*",f,g) -> (open_paren prec 2) ^ (racket_of 2 f) ^  "*" ^ (racket_of 2 g) ^ (close_paren prec 2)
            | BinaryOp("/",f,g) -> (open_paren prec 2)^ (racket_of 2 f) ^ "/" ^ (racket_of 3 g) ^ (close_paren prec 2)
            | UnaryOp ("-", e) ->  (open_paren prec 4)^ "-" ^ (racket_of 5 e)^(close_paren prec 4)
            | BinaryOp("^",f,g) -> (open_paren prec 0)^ (racket_of 0 f) ^ "||" ^ (racket_of 0 g) ^ (close_paren prec 0)
            | BinaryOp(op,_,_) | UnaryOp (op, _) -> failwith "Function sql_of_vterm is called with an unknown operator" ^ op
            (* | BoolAnd (f,g) -> (open_paren prec 2) ^ (racket_of 2 f) ^  "*" ^ (racket_of 2 g) ^ (close_paren prec 2)
            | BoolOr (f,g) -> (open_paren prec 0)^ (racket_of 0 f) ^ "+" ^ (racket_of 0 g) ^ (close_paren prec 0)
            | BoolNot e ->  (open_paren prec 4)^ "-" ^ (racket_of 5 e)^(close_paren prec 4) *)
        in racket_of 0 expr

(** Given a variable, returns the name of an EDB/IDB column that defines it, or if it is equal to a constant, the value of the constant. *)
let var_to_col (vt:vartab) (eqt:eqtab) key (variable:var) =
    (*If the variable appears in a positive rterm, the value
     * is the name of the respective rterm's table column*)
    if Hashtbl.mem vt (string_of_var variable)
        then List.hd (Hashtbl.find vt (string_of_var variable))
    (*If the variable does not appear in a positive rterm, but
     * it does in an equality value, then the value is the eq's
     * constant, the var has to be removed from the eqtab*)
    else if Hashtbl.mem eqt (Var variable)
        then racket_of_vterm vt eqt (eqt_extract eqt (Var variable))
    (*Else, the query is unsafe or inclomplete*)
    else raise (SemErr (
            "Predicate "^(string_of_symtkey key)^
            " is unsafe, variable "^(string_of_var variable)^" is not in a positive "^
            "goal or strict equality relation."
        )
    )

(** Given the head of the rule, the vartab, and te eqtab, returns the code that must be in the select clause. All columns are aliased as col0, col1, ... *)
let get_select_clause (vt:vartab) (eqt:eqtab) rterm =
    let vlst = get_rterm_varlist rterm in 
    let key = symtkey_of_rterm rterm in
    if vlst = [] then
        (* select no colum, two choices: raise error or continue with select no column clause*)
        (* raise (SemErr
            ("Predicate "^(get_rterm_predname rterm)^
            " has arity 0, which is not allowed")) *)
        "map (lambda (tuplelst) '())" 
    else
        (*Transform variables to column names. Treat namedVars and
        * aggregates differently*)
        let var_value v = match v with
            NamedVar _ | NumberedVar _ ->
                var_to_col vt eqt key v
            | AggVar (fn,vn) ->
                (check_agg_function fn)^"("^(var_to_col vt eqt key (NamedVar vn))^")"
            | _ -> invalid_arg ("not-expected vartype in head of predicate"^
                (string_of_symtkey key))
        in
        let cols = List.map var_value vlst in
        (*Create aliases*)
        let rec alias = function
            | [] -> ""
            | [col] -> col
            | col::col2::tl ->
                col^" "^(alias (col2::tl))
        in
        "map (lambda (tuplelst) (list "^(alias cols)^"))"

let get_aggregation_racket (vt:vartab) (cnt:colnamtab) head agg_eqs agg_ineqs =
    let vars = get_rterm_varlist head in
    let key = symtkey_of_rterm head in
    (*Merge the equalities and inequalities in a simple list*)
    let eq_t = List.map extract_eq_tuple agg_eqs in
    let aug_eq_t = List.map (fun (x,y) -> ("=",x,y)) eq_t in
    let ieq_t = List.map extract_ineq_tuple agg_ineqs in
    let comparisons = aug_eq_t@ieq_t in
    (*Check if the rule has aggregation*)
    let is_agg = List.exists is_aggvar vars in
    if not is_agg then
        if comparisons = [] then ""
        else raise (SemErr (
            "Predicate "^(string_of_symtkey key)^
            " contains comparisons of aggregates but defines no "^
            "aggregations in its head"))
    else
    let cols = Hashtbl.find cnt key in
    (*Calculate the GROUP BY clause*)
    let group_var acc col = function
        | NamedVar _ -> col::acc
        | _ -> acc in
    let grp_cols = List.fold_left2 group_var [] cols vars in
    let group_by_racket =
        if grp_cols = [] then ""
        else ("GROUP BY "^(String.concat ", " grp_cols)) in
    (*Calculate the HAVING clause*)
    (*Extract the available aggregations in the head, and place them
     * in a list, which values will be the function applied to a column-name*)
    let av_aggs = Hashtbl.create 100 in
    let fake_eqt:eqtab = Hashtbl.create 100 in
    let insert_agg = function
        | AggVar (fn,vn) ->
            let col = var_to_col vt fake_eqt key (NamedVar vn) in
            Hashtbl.add av_aggs (fn,vn) (fn^"("^col^")")
        | _ -> () in
    List.iter insert_agg vars;
    (*Build the contraints and check for unavailable aggregates*)
    let agg_var_col agv =
        let tuple = extract_aggvar_tuple agv in
        if Hashtbl.mem av_aggs tuple then Hashtbl.find av_aggs tuple
        else raise (SemErr (
            "Predicate "^(string_of_symtkey key)^" contains comparisons of "^
            "aggregates that are not defined in its head"
        )) in
    let comp_const (op,e1,e2) = match (op,e1,e2) with 
        (* currently only support constraint for aggreation in the form  agg_fun(X) comparason_op const*)
        | (_, Var (AggVar (fn,vlst)), Const c) ->
        (agg_var_col (AggVar (fn,vlst)))^" "^op^" "^(string_of_const c)
        | _ -> raise (SemErr (
            "Predicate "^(string_of_symtkey key)^" contains comparisons of "^
            "aggregates that are not in the form of agg_fun(X) op const"
        )) in 
    let comp_racket = List.map comp_const comparisons in
    let having_racket = if comp_racket = [] then "" else
        "HAVING "^(String.concat " AND " comp_racket) in
    group_by_racket^" "^having_racket

let rec non_rec_unfold_racket_of_symtkey (idb:symtable) (cnt:colnamtab) (goal:symtkey)  =
    (* get all the rule having this query in head *)
    (* print_endline ("Reach " ^ (string_of_symtkey goal)); *)
    if not (Hashtbl.mem idb goal) then raise (SemErr ("No rule for the idb predicate "^string_of_symtkey goal))
    else
    let rule_lst = Hashtbl.find idb  goal in
    (* union all rules by appending the corresponding lists *)
    let unfold_racket_of_rule_lst (idb:symtable) (cnt:colnamtab) rules =
        let unfold_racket_of_rule (idb:symtable) (cnt:colnamtab) rule =
            let head = rule_head rule in
            let body = rule_body rule in
            (*Split terms in the rule's body. Separate equalities
            * and inequalities in variable and aggregates relations.*)
            let (p_rt,n_rt,all_eqs,all_ineqs) = split_terms body in
            let (agg_eqs,eqs) = List.partition is_agg_equality all_eqs in
            let (agg_ineqs,ineqs) = List.partition is_agg_inequality all_ineqs in
            (*Build vartab, and eqtab for select and where clauses, build vartabl by p_rt which is list of positive predicates*)
            let vt = build_num_vartab cnt p_rt in
            let eqtb = build_eqtab eqs in
            let select_racket = get_select_clause vt eqtb head in
            let unfold_get_from_clause (idb:symtable) rterms =
                if rterms == [] then "'(1)" else
                let idb_alias pname arity n =
                    (* generate racket query for idb predicate *)
                    let idb_racket = non_rec_unfold_racket_of_symtkey idb cnt (pname,arity)  in
                    "("^idb_racket^")"
                in
                let edb_alias pname arity n = pname
                in
                let set_alias rterm (a_lst,n) =
                    let pname = get_rterm_predname rterm in
                    let arity = get_arity rterm in
                    let key = symtkey_of_rterm rterm in
                    let alias_f = if Hashtbl.mem idb key then idb_alias else edb_alias in
                    let alias = alias_f pname arity n in
                    (alias::a_lst,n-1)
                in
                let len = List.length rterms in
                let (aliases,_) = List.fold_right set_alias rterms ([],len-1) in
                "(cartesian-map list "^(String.concat " " aliases)^")" in
            let from_racket = unfold_get_from_clause idb p_rt in
            
            let unfold_get_where_clause (idb:symtable) (vt:vartab) (cnt:colnamtab) (eqt:eqtab) ineq neg_rt = 
                (*Transform a list of column names in eq relations [a,b,c] -> ['a=b';'a=c']*)
                let var_const _ cols acc = match cols with
                    | [] -> acc
                    | hd::tl ->
                        let eq_rels el = "(equal? "^ hd^" "^el^")" in
                        (List.map eq_rels tl)::acc
                in
                let fvt = List.flatten (Hashtbl.fold var_const vt []) in
                (*Transform the equalities in the eqtab to strings of the form
                * "CName = value" *)
                let eq_comp e1 e2 acc = if (is_free_var vt e1) then acc else ( "(equal? "^ (racket_of_vterm vt eqt e1)^" "^(racket_of_vterm vt eqt e2) ^")")::acc in
                let feqt = Hashtbl.fold eq_comp eqt [] in
                (*Transform the inequalities in the list for strings of the form
                * "CName op value" *)
                let ineq_tuples = List.map extract_ineq_tuple ineq in
                let ineq_comp (op,e1,e2) acc =
                    if (op="<>") then
                        ("(not (equal? "^ (racket_of_vterm vt eqt e1)^" "^(racket_of_vterm vt eqt e2)^"))")::acc
                    else
                    ("("^op^" "^ (racket_of_vterm vt eqt e1)^" "^(racket_of_vterm vt eqt e2)^")")::acc in
                let fineq = List.fold_right ineq_comp ineq_tuples [] in
                (*Transform the negated rterms into racket*)
                let unfold_racket_of_negated_rterms (idb:symtable) (vt:vartab) (cnt:colnamtab) (eqt:eqtab) neg_rt =
                    let gen_neg_racket rt =
                        (*get basic info of the rterm*)
                        let key = symtkey_of_rterm rt in
                        let pname = get_rterm_predname rt in
                        let arity = get_arity rt in 
                        let alias = pname^"_a"^(string_of_int arity) in
                        let vlst = get_rterm_varlist rt in
                        if not (Hashtbl.mem cnt key) then raise (SemErr ("not found edb or idb predicate "^string_of_symtkey key)) else
                        (*Get the from racket of the rterm*)
                        let from_racket =
                            if Hashtbl.mem idb key then 
                                    "("^ non_rec_unfold_racket_of_symtkey idb cnt (pname,arity) ^")"
                            else
                                pname
                        in
                        (* print_endline "___neg racket___"; print_string from_racket; print_endline "___neg racket___"; *)
                        (*Get the where racket of the rterm*)
                        let build_const acc col var =
                            let eq_to = "(equal? (list-ref negtuple "^col^") " in
                            match var with
                            | NamedVar vn -> 
                                if Hashtbl.mem vt vn then
                                    (eq_to^(List.hd (Hashtbl.find vt vn))^")")::acc
                                else if Hashtbl.mem eqt (Var var) then
                                    (eq_to^(racket_of_vterm vt eqt (Hashtbl.find eqt (Var var)))^")")::acc
                                else raise (SemErr (
                                    "Program is unsafe, variable "^vn^
                                    " in negated call to predicate "^
                                    (string_of_symtkey key)^" does not appear in a positive "^
                                    "goal or strict equation. Try anonimous variables."
                                ))  
                            | NumberedVar _ -> let vn = string_of_var var in
                            if Hashtbl.mem vt vn then
                                (eq_to^(List.hd (Hashtbl.find vt vn))^")")::acc
                            else if Hashtbl.mem eqt (Var var) then
                                (eq_to^(racket_of_vterm vt eqt (Hashtbl.find eqt (Var var)))^")")::acc
                            else raise (SemErr (
                                "Program is unsafe, variable "^vn^
                                " in negated call to predicate "^
                                (string_of_symtkey key)^" does not appear in a positive "^
                                "goal or strict equation. Try anonimous variables."
                            ))
                            | ConstVar c -> (eq_to^(string_of_const c)^")")::acc
                            | AnonVar -> acc
                            | _ -> invalid_arg "There is a non-expected type of var in a negated rterm"
                        in
                        let rec gen_nums ind n =
                            if ind<n then ((string_of_int ind))::(gen_nums (ind+1) n) 
                            else [] in
                        let cols = gen_nums 0 arity in
                        let const_lst = List.fold_left2 build_const [] cols vlst in
                        let where_racket =
                            match const_lst with
                            | [] -> ""
                            | _ -> "filter (lambda (negtuple) "^(
                                if (List.length const_lst = 1) then String.concat "" const_lst
                                else 
                                List.fold_right (fun a b -> "(and " ^ a ^" "^b^")") const_lst "true"
                                )^")" in
                        (**Return the final string*)
                        "(= 0 (length "^
                        (if (where_racket = "") then from_racket
                            else "("^where_racket^" "^from_racket^")")
                        ^"))"
                    in
                    List.map gen_neg_racket neg_rt in
                let fnrt = unfold_racket_of_negated_rterms idb vt cnt eqt neg_rt in
                (*merge all constraints*)
                let constraints = fvt@feqt@fineq@fnrt in
                match constraints with
                    | [] -> ""
                    | _ -> "filter (lambda (tuplelst) "^(
                        if (List.length constraints = 1) then String.concat "" constraints
                        else 
                        List.fold_right (fun a b -> "(and " ^ a ^" "^b^")") constraints "true"
                        )^")" in
            let where_racket = unfold_get_where_clause idb vt cnt eqtb ineqs n_rt in
            let agg_racket = get_aggregation_racket vt cnt head agg_eqs agg_ineqs in
            if (where_racket = "") then select_racket ^ " "^from_racket
                else select_racket ^ " ("^where_racket^" "^from_racket^")" in
            (* String.concat " " [select_racket;from_racket;where_racket;agg_racket] in *)
        let racket_list = List.map (unfold_racket_of_rule idb cnt) rules in
        if (List.length racket_list > 1) then
        "append " ^ (String.concat " " (List.map (fun x -> "("^x^")") racket_list))
        else (String.concat "" racket_list) in
    let racket = unfold_racket_of_rule_lst idb cnt rule_lst in
    racket

(** Take a query term and generate unfolded racket source code for it. *)
let non_rec_unfold_racket_of_query (idb:symtable) (cnt:colnamtab) (query:rterm) =
    let qrule = rule_of_query query idb in
    (* qrule is in the form of _dummy_(x,y) :- query_predicate(x,y), x=1 *)
    let local_idb = Hashtbl.copy idb in 
    (* because insert a temporary dummy qrule, we should work with a local variable of idb *)
    symt_insert local_idb qrule;
    (* get column names (cols_by_var) for the view by using the dummy predicate which is head of qrule *)
    let rec gen_nums ind n =
        if ind<n then ((string_of_int ind))::(gen_nums (ind+1) n) 
        else [] in
    let cols = gen_nums 0 (get_arity (rule_head qrule)) in
    if not (Hashtbl.mem cnt (symtkey_of_rterm query)) then raise (SemErr "The query does not match any idb relation") 
    else
    let sel_lst = List.map (fun x -> "(list-ref tuplelst "^x^")") cols in 
    "map (lambda (tuplelst) (list "^(String.concat " " sel_lst)^")) " ^
    (* by inserting the dummy rule to idb, we now find racket for this dummy predicate *)
    "("^non_rec_unfold_racket_of_symtkey local_idb cnt (symtkey_of_rterm (rule_head qrule))^")"

(** Generate racket functions from the ast, the goal is the query predicate, the query is a query over source relations. *)
let unfold_query_racket_stt (debug:bool) (edb:symtable) prog =
    let query_rt = get_query prog in
    (*Extract and pre-process the IDB from the program*)
    let idb = extract_idb prog in
    preprocess_rules idb; 
    (* print_symtable idb; *)
    (*Build the colnamtab for referencing the table's columns*)
    let cnt = build_colnamtab edb idb in
    (*Return the desired racket*)
    let racket = non_rec_unfold_racket_of_query idb cnt query_rt  in
    racket


let unfold_program_query (debug:bool) prog =
    if (debug) then print_endline ("==> generating racket function for a query of a datalog program"^ string_of_query (get_query prog)) else ();
    let edb = extract_edb prog in
    unfold_query_racket_stt debug edb prog


(** Take a view update datalog program (containing both get and put directions) and generate racket query of constraints involving view. *)
let view_constraint_racket_of_stt (dbschema:string) (debug:bool) (inc:bool) (optimize:bool) prog =
    let clean_prog = keep_only_constraint_of_view debug prog in
    if inc then     
        let inc_prog = incrementalize_by_view debug clean_prog in
        let view_sch = get_view inc_prog in
        let view_rt = get_schema_rterm view_sch in
        let new_view_rt = rename_rterm "new_" view_rt in
        let subst_prog = subst_pred (get_rterm_predname view_rt) (get_rterm_predname new_view_rt) inc_prog in
        let prog2 = {subst_prog with sources = [
            get_rterm_predname (view_rt), get_schema_col_typs view_sch;
            get_rterm_predname (get_temp_delta_deletion_rterm view_rt), get_schema_col_typs view_sch;
            get_rterm_predname (get_temp_delta_insertion_rterm view_rt), get_schema_col_typs view_sch
        ]@subst_prog.sources; rules = [
            (get_inc_original view_rt,[Rel (view_rt)]);
            (get_inc_ins view_rt,[Rel (get_temp_delta_insertion_rterm view_rt)]);
            (get_inc_del view_rt,[Rel (get_temp_delta_deletion_rterm view_rt)])
        ]@subst_prog.rules} in
        (* let edb = extract_edb prog2 in *)
        let idb = extract_idb prog2 in
        if Hashtbl.mem idb (symtkey_of_rterm get_empty_pred) then
            (
            (* keep_only_constraint_of_view debug view_rt edb idb ; *)
            preprocess_rules idb;
            (* let cnt = build_colnamtab edb idb in *)
            if Hashtbl.mem idb (symtkey_of_rterm get_empty_pred) then
                let remain_rules = rules_of_symt idb in
                let prog3 = {get_empty_expr with view = prog2.view; sources = prog2.sources; rules = remain_rules}  in
                (* non_rec_unfold_racket_of_query dbschema idb cnt get_empty_pred *)
                let prog4 = if (optimize) then (Ast2fol.optimize_query_datalog debug {prog3 with query = Some get_empty_pred} ) else {prog3 with query = Some get_empty_pred} in
                (* the optimization may drop the empty_predicate of prog4 when the empty_predicate is trival (always empty) *)
                if (has_query prog4) then
                    (unfold_program_query debug prog4)
                else "SELECT WHERE false"
            else "SELECT WHERE false")
        else "SELECT WHERE false"
    else
        let view_sch = get_view clean_prog in
        let view_rt = get_schema_rterm view_sch in
        let new_view_rt = rename_rterm "new_" view_rt in
        let subst_prog = subst_pred (get_rterm_predname view_rt) (get_rterm_predname new_view_rt) (delete_rule_of_predname (get_rterm_predname view_rt) clean_prog) in
        let prog2 = {subst_prog with sources = [
            get_rterm_predname (view_rt), get_schema_col_typs view_sch;
            get_rterm_predname (get_temp_delta_deletion_rterm view_rt), get_schema_col_typs view_sch;
            get_rterm_predname (get_temp_delta_insertion_rterm view_rt), get_schema_col_typs view_sch
        ]@subst_prog.sources; rules = [
            (new_view_rt,[Rel (view_rt); Not (get_temp_delta_deletion_rterm view_rt)]);
            (new_view_rt,[Rel (get_temp_delta_insertion_rterm view_rt)])
        ]@subst_prog.rules} in
        (* let edb = extract_edb prog2 in *)
        let idb = extract_idb prog2 in
        if Hashtbl.mem idb (symtkey_of_rterm get_empty_pred) then
            (
            (* keep_only_constraint_of_view debug view_rt edb idb ; *)
            preprocess_rules idb;
            (* let cnt = build_colnamtab edb idb in *)
            if Hashtbl.mem idb (symtkey_of_rterm get_empty_pred) then
                let remain_rules = Hashtbl.fold (fun k rules lst -> rules@lst) idb [] in
                let prog3 = {get_empty_expr with view = prog2.view; sources = prog2.sources; rules = remain_rules} in
                (* non_rec_unfold_racket_of_query dbschema idb cnt get_empty_pred *)
                let prog4 = if (optimize) then (Ast2fol.optimize_query_datalog debug {prog3 with query = Some get_empty_pred} ) else {prog3 with query = Some get_empty_pred} in
                (* the optimization may drop the empty_predicate of prog4 when the empty_predicate is trival (always empty) *)
                if (has_query prog4) then
                    (unfold_program_query debug prog4)
                else "SELECT WHERE false"
            else "SELECT WHERE false")
        else "SELECT WHERE false"


(** Take a view update datalog program (containing both get and put directions) and generate a racket query of constraints not involving view. *)
let non_view_constraint_racket_of_stt (dbschema:string) (debug:bool) (inc:bool) (optimize:bool) prog =
    let clean_prog = remove_constraint_of_view debug prog in
    if inc then
        let inc_prog = incrementalize_by_view debug clean_prog in
        let view_sch = get_view inc_prog in
        let view_rt = get_schema_rterm view_sch in
        let new_view_rt = rename_rterm "new_" view_rt in
        let subst_prog = subst_pred (get_rterm_predname view_rt) (get_rterm_predname new_view_rt) inc_prog in
        let prog2 = {subst_prog with sources = [
            get_rterm_predname (view_rt), get_schema_col_typs view_sch;
            get_rterm_predname (get_temp_delta_deletion_rterm view_rt), get_schema_col_typs view_sch;
            get_rterm_predname (get_temp_delta_insertion_rterm view_rt), get_schema_col_typs view_sch
        ]@subst_prog.sources; rules = [
            (get_inc_original view_rt,[Rel (view_rt)]);
            (get_inc_ins view_rt,[Rel (get_temp_delta_insertion_rterm view_rt)]);
            (get_inc_del view_rt,[Rel (get_temp_delta_deletion_rterm view_rt)])
        ]@subst_prog.rules} in
        (* let edb = extract_edb prog2 in *)
        let idb = extract_idb prog2 in
        if Hashtbl.mem idb (symtkey_of_rterm get_empty_pred) then
            (
            (* keep_only_constraint_of_view debug view_rt edb idb ; *)
            preprocess_rules idb;
            (* let cnt = build_colnamtab edb idb in *)
            if Hashtbl.mem idb (symtkey_of_rterm get_empty_pred) then
                let remain_rules = rules_of_symt idb in
                let prog3 = {get_empty_expr with view = prog2.view; sources = prog2.sources; rules = remain_rules} in
                (* non_rec_unfold_racket_of_query dbschema idb cnt get_empty_pred *)
                let prog4 = if (optimize) then (Ast2fol.optimize_query_datalog debug {prog3 with query = Some get_empty_pred} ) else {prog3 with query = Some get_empty_pred} in
                (* the optimization may drop the empty_predicate of prog4 when the empty_predicate is trival (always empty) *)
                if (has_query prog4) then
                    (unfold_program_query debug prog4)
                else "SELECT WHERE false"
            else "SELECT WHERE false")
        else "SELECT WHERE false"
    else
        let view_sch = get_view clean_prog in
        let view_rt = get_schema_rterm view_sch in
        let new_view_rt = rename_rterm "new_" view_rt in
        let subst_prog = subst_pred (get_rterm_predname view_rt) (get_rterm_predname new_view_rt) (delete_rule_of_predname (get_rterm_predname view_rt) clean_prog) in
        let prog2 = {subst_prog with sources = [
            get_rterm_predname (view_rt), get_schema_col_typs view_sch;
            get_rterm_predname (get_temp_delta_deletion_rterm view_rt), get_schema_col_typs view_sch;
            get_rterm_predname (get_temp_delta_insertion_rterm view_rt), get_schema_col_typs view_sch
        ]@subst_prog.sources; rules = [
            (new_view_rt,[Rel (view_rt); Not (get_temp_delta_deletion_rterm view_rt)]);
            (new_view_rt,[Rel (get_temp_delta_insertion_rterm view_rt)])
        ]@subst_prog.rules} in
        (* let edb = extract_edb prog2 in *)
        let idb = extract_idb prog2 in
        if Hashtbl.mem idb (symtkey_of_rterm get_empty_pred) then
            (
            (* keep_only_constraint_of_view debug view_rt edb idb ; *)
            preprocess_rules idb;
            (* let cnt = build_colnamtab edb idb in *)
            if Hashtbl.mem idb (symtkey_of_rterm get_empty_pred) then
                let remain_rules = Hashtbl.fold (fun k rules lst -> rules@lst) idb [] in
                let prog3 = {get_empty_expr with view = prog2.view; sources = prog2.sources; rules = remain_rules}  in
                (* non_rec_unfold_racket_of_query dbschema idb cnt get_empty_pred *)
                let prog4 = if (optimize) then (Ast2fol.optimize_query_datalog debug {prog3 with query = Some get_empty_pred} ) else {prog3 with query = Some get_empty_pred} in
                (* the optimization may drop the empty_predicate of prog4 when the empty_predicate is trival (always empty) *)
                if (has_query prog4) then
                    (unfold_program_query debug prog4)
                else "SELECT WHERE false"
            else "SELECT WHERE false")
        else "SELECT WHERE false"


let stype_to_racket_type st = match st with 
    | Sint -> "integer?"
    | Sreal -> "real?"
    | Sbool -> "boolean?"
    | Sstring -> "integer?"

(* transform source relations in program into symbolic tables in rosette *)
let gen_symbolic_source size vocsize prog = 
    (* currently just set all the types are int (ℤ) *)
    let p_el sym_table_lst (name, lst) = 
        let tup num = 
        (( String.concat "\n" ( List.map (fun (col,typ) -> "(define-symbolic "^name^"$"^col^"$"^ string_of_int num ^" " ^ stype_to_racket_type typ^")" ^(if (typ = Sstring) then "\n (assert (and (< -1  "^name^"$"^col^"$"^ string_of_int num ^") (< " ^name^"$"^col^"$"^ string_of_int num ^ " "^ string_of_int vocsize^")))" else "") ) lst) )
        ^ "\n\n(define "^name^"_tuple_"^ string_of_int num ^ " (list "^ String.concat " " ( List.map (fun (col,typ) -> name^"$"^col^"$"^ string_of_int num) lst)^")" ^")") in 
        let rec sym_tuples sz = if sz <= 0 then "" else sym_tuples (sz-1) ^ "\n\n" ^ (tup sz) in
        let rec sym_table sz = if sz <= 0 then "" else sym_table (sz-1) ^ " " ^name^"_tuple_" ^string_of_int sz in
        ((sym_tuples size) ^ "\n\n(define "^name^ " (list " ^ sym_table size ^ "))") ::sym_table_lst in
    List.fold_left p_el [] prog.sources

(* transform source relations in program into symbolic tables in rosette *)
let gen_symbolic_source_view size vocsize prog = 
    (* currently just set all the types are int (ℤ) *)
    let p_el sym_table_lst (name, lst) =
        let tup num = 
        (( String.concat "\n" ( List.map (fun (col,typ) -> "(define-symbolic "^name^"$"^col^"$"^ string_of_int num ^" " ^ stype_to_racket_type typ^")" ^(if (typ = Sstring) then "\n (assert (and (< -1  "^name^"$"^col^"$"^ string_of_int num ^") (< " ^name^"$"^col^"$"^ string_of_int num ^ " "^ string_of_int vocsize^")))" else "") ) lst) )
        ^ "\n\n(define "^name^"_tuple_"^ string_of_int num ^ " (list "^ String.concat " " ( List.map (fun (col,typ) -> name^"$"^col^"$"^ string_of_int num) lst)^")" ^")") in 
        let rec sym_tuples sz = if sz <= 0 then "" else sym_tuples (sz-1) ^ "\n\n" ^ (tup sz) in
        let rec sym_table sz = if sz <= 0 then "" else sym_table (sz-1) ^ " " ^name^"_tuple_" ^string_of_int sz in
        ((sym_tuples size) ^ "\n\n(define "^name^ " (list " ^ sym_table size ^ "))") ::sym_table_lst in
    List.fold_left p_el [] (get_schema_stts prog)

(* take a view update datalog program (containing both get and put directions) and generate a rosette constraint of getput property for its view update strategy *)
let ros_getput_of_stt (debug:bool) prog =
    
    let delta_rt_lst = get_delta_rterms prog in
    (* remove overlap of delta and the source *)
    let dummy_delta_rules = List.map (fun delta -> let dummy = rename_rterm "__dummy__" delta in
                match delta with 
                    Deltadelete (p,vars) -> (dummy, [Rel (Deltadelete (p,vars)); Rel(Pred(p,vars))]) 
                    | Deltainsert (p,vars) -> (dummy, [Rel (Deltainsert (p,vars)); Not(Pred(p,vars))])  
                    | _ -> invalid_arg "Function ros_getput_of_stt called with not a delta predicate")  delta_rt_lst in
    let dummy_delta_rt_lst =  List.map (rename_rterm "__dummy__")  delta_rt_lst in
    (* get the emptiness FO sentence of a relation *)
    let getput_prog = Expr2.add_rules dummy_delta_rules prog in
    let edb = extract_edb getput_prog in
    let idb = extract_idb getput_prog in
    preprocess_rules idb;
    if debug then (
        print_endline "_____preprocessed datalog rules_______"; 
        print_symtable idb; 
        print_endline "______________\n";
    ) else ();
    let cnt = build_colnamtab edb idb in
    let delta_definition rel = 
         let delta_ros_code = non_rec_unfold_racket_of_query idb cnt rel in
         "(define "^ get_rterm_predname rel ^ " (" ^delta_ros_code^ "))" in
    let delta_definition_lst = List.map delta_definition dummy_delta_rt_lst in 
    let emptiness = "(define emptiness (append "^String.concat " " (List.map get_rterm_predname dummy_delta_rt_lst) ^"))" in
    let ros_constraint = "(solve (assert (< 0 (length emptiness))))" in 
     (String.concat "\n\n" delta_definition_lst) ^ "\n\n"^ emptiness ^ "\n\n" ^ ros_constraint


(* take a view update datalog program (containing both get and put directions) and generate a rosette constraint of delta disjointness property for its view update strategy *)
let ros_disdelta_of_stt (debug:bool) prog =
    let edb = extract_edb prog in
    (* need to change the view (in query predicate) to a edb relation *)
    let view_rt = get_schema_rterm (get_view prog) in
    (* need to convert the view to be an edb relation *)
    symt_insert edb (view_rt,[]);
    let idb = extract_idb prog in
    symt_remove idb (symtkey_of_rterm view_rt);
    let emptiness = Pred ("__emptiness",[]) in
    let delta_rt_lst = get_delta_rterms prog in
    (* get each pair of delta relations from the delta relation lst delta_rt_lst *)
    let delta_pair_lst = 
        let pair_of_delta_insert lst ins_rel = 
            let del_rels = List.filter (is_delta_pair ins_rel) delta_rt_lst in 
            if (List.length del_rels = 0) then lst else (ins_rel, (List.hd del_rels))::lst in 
        List.fold_left pair_of_delta_insert [] delta_rt_lst in 
    List.iter (fun (r1,r2) -> symt_insert idb (emptiness,[ Rel r1; Rel r2])) delta_pair_lst;
    preprocess_rules idb;
    if debug then (
        print_endline "_____preprocessed datalog rules_______"; 
        print_symtable idb; 
        print_endline "______________\n";
    ) else ();
    let cnt = build_colnamtab edb idb in
    let disdelta_ros = non_rec_unfold_racket_of_query idb cnt emptiness in
    let disdelta_emptiness = "(define disdelta_emptiness ("^disdelta_ros^"))" in
    let ros_constraint = "(solve (assert (< 0 (length disdelta_emptiness))))" in 
    disdelta_emptiness ^ "\n\n" ^ ros_constraint


(* take a view update datalog program (containing both get and put directions) and generate a rosette constraint of putget property for its view update strategy *)
let ros_putget_of_stt (debug:bool) prog =
    let putget_prog =  datalog_of_putget debug false prog in
    let edb = extract_edb putget_prog in
    (* need to change the view (in query predicate) to a edb relation *)
    let view_rt = get_schema_rterm (get_view putget_prog) in
    (* need to convert the view to be an edb relation *)
    symt_insert edb (view_rt,[]);
    let idb = extract_idb putget_prog in
    (* symt_remove idb (symtkey_of_rterm view_rt); *)
    let new_view_rt = rename_rterm "__dummy_new_" view_rt in
    let emptiness = rename_rterm "__emptiness" view_rt in
    symt_insert idb (emptiness,[ Rel new_view_rt; Not view_rt]);
    symt_insert idb (emptiness,[ Rel view_rt; Not new_view_rt]);
    preprocess_rules idb;
    if debug then (
        print_endline "_____preprocessed datalog rules_______"; 
        print_symtable idb; 
        print_endline "______________\n";
    ) else ();
    let cnt = build_colnamtab edb idb in
    let putget_ros = non_rec_unfold_racket_of_query idb cnt emptiness in
    let putget_emptiness = "(define putget_emptiness ("^putget_ros^"))" in
    let ros_constraint = "(solve (assert (< 0 (length putget_emptiness))))" in 
    putget_emptiness ^ "\n\n" ^ ros_constraint


(* take a view update datalog program (may contain both get and put directions) and generate FO sentence of all contraints *)
let ros_constraint_sentence_of_stt (debug:bool) prog =
    if debug then (print_endline "==> generating racket code for all constraints";) else ();
    let edb = extract_edb prog in
    let idb = extract_idb prog in 
    if Hashtbl.mem idb (symtkey_of_rterm get_empty_pred) then
        (* need to change the view (in query predicate) to a edb relation *)
        let view_rt = get_schema_rterm (get_view prog) in
        (* need to convert the view to be an edb relation *)
        symt_insert edb (view_rt,[]);
        (* if having get (view rules) then remove it *)
        if Hashtbl.mem idb (symtkey_of_rterm view_rt) then
            symt_remove idb (symtkey_of_rterm view_rt);
        preprocess_rules idb;
        if debug then (
        print_endline "_____preprocessed datalog rules_______"; 
        print_symtable idb; 
        print_endline "______________\n";
    ) else ();
        let cnt = build_colnamtab edb idb in
        let constr_ros = non_rec_unfold_racket_of_query idb cnt get_empty_pred in 
        let constr_emptiness = "(define constr_emptiness ("^constr_ros^"))" in
        let ros_constraint = "(assert (= 0 (length constr_emptiness)))" in 
        constr_emptiness ^ "\n\n" ^ ros_constraint
    else ""


(* take a view update datalog program (may contain both get and put directions) and generate FO sentence of contraints not involving view *)
let ros_non_view_constraint_sentence_of_stt (debug:bool) prog =
    if debug then (print_endline "==> generating constraint not involving view";) else ();
    let clean_prog = remove_constraint_of_view debug prog in
    let edb = extract_edb clean_prog in
    let idb = extract_idb clean_prog in
    if Hashtbl.mem idb (symtkey_of_rterm get_empty_pred) then
        (* need to change the view (in query predicate) to a edb relation *)
        let view_rt = get_schema_rterm (get_view clean_prog) in
        (* need to convert the view to be an edb relation *)
        (* remove_constraint_of_view debug view_rt edb idb ; *)
        symt_insert edb (view_rt,[]);
        symt_remove idb (symtkey_of_rterm view_rt);
        preprocess_rules idb;
        if debug then (
        print_endline "_____preprocessed datalog rules_______"; 
        print_symtable idb; 
        print_endline "______________\n";
    ) else ();
        let cnt = build_colnamtab edb idb in
        let constr_ros = non_rec_unfold_racket_of_query idb cnt get_empty_pred in 
        let constr_emptiness = "(define constr_emptiness ("^constr_ros^"))" in
        let ros_constraint = "(assert (= 0 (length constr_emptiness)))" in 
        constr_emptiness ^ "\n\n" ^ ros_constraint
    else ""


(* take a view update datalog program (may contain both get and put directions) and generate FO sentence of only contraints involving view *)
let ros_view_constraint_sentence_of_stt (debug:bool) prog =
    if debug then (print_endline "==> generating a sentence of only constraints involving view";) else ();
    let clean_prog = keep_only_constraint_of_view debug prog in
    let edb = extract_edb clean_prog in
    let idb = extract_idb clean_prog in
    if Hashtbl.mem idb (symtkey_of_rterm get_empty_pred) then
        (* need to change the view (in query predicate) to a edb relation *)
        let view_rt = get_schema_rterm (get_view clean_prog) in
        (* need to convert the view to be an edb relation *)
        (* remove_constraint_of_view debug view_rt edb idb ; *)
        symt_insert edb (view_rt,[]);
        symt_remove idb (symtkey_of_rterm view_rt);
        preprocess_rules idb;
        if debug then (
        print_endline "_____preprocessed datalog rules_______"; 
        print_symtable idb; 
        print_endline "______________\n";
    ) else ();
        let cnt = build_colnamtab edb idb in
        let constr_ros = non_rec_unfold_racket_of_query idb cnt get_empty_pred in 
        let constr_emptiness = "(define constr_emptiness ("^constr_ros^"))" in
        let ros_constraint = "(assert (= 0 (length constr_emptiness)))" in 
        constr_emptiness ^ "\n\n" ^ ros_constraint
    else ""


let predefined_ros = 
"#lang rosette/safe

(define curry
    (lambda (f . c) (lambda x (apply f (append c x)))))

(define stitch
    (lambda (tuples element)
        (map (curry cons element) tuples)))

(define flatten
    (curry apply append))

(define cartesian
    (lambda (l1 l2)
        (flatten (map (curry stitch l2) l1))))

(define cartesian-lists
    (lambda (lists)
        (foldr cartesian '(()) lists)))

(define cartesian-map
    (lambda (f . lists)
        (map (curry apply f) (cartesian-lists lists))))

"

let ros_check_disdelta_of_stt (debug:bool) srcsize vocsize prog =
    predefined_ros^ (String.concat "\n" (gen_symbolic_source_view srcsize vocsize prog)) ^ "\n\n"^ ros_constraint_sentence_of_stt debug prog ^ "\n\n"^ ros_disdelta_of_stt debug prog


let ros_check_getput_of_stt (debug:bool) srcsize vocsize prog =
    predefined_ros^ (String.concat "\n" (gen_symbolic_source srcsize vocsize prog)) ^ "\n\n"^ ros_non_view_constraint_sentence_of_stt debug prog ^ "\n\n"^ ros_getput_of_stt debug prog


let ros_check_putget_of_stt (debug:bool) srcsize vocsize prog =
    predefined_ros^ (String.concat "\n" (gen_symbolic_source_view srcsize vocsize prog)) ^ "\n\n"^ ros_constraint_sentence_of_stt debug prog ^ "\n\n"^ ros_putget_of_stt debug prog


type cellkey = (string*string*int) (* table name, column name, rowid *)
type celltable = (cellkey, string) Hashtbl.t (* value of cells*)

let parse_ros_models log str =
    if log then print_endline ">>> parsing model from rosette";
    let stream = (Scanf.Scanning.from_string (String.sub str 6 ((String.length str) - 6))) in
    let rec do_parse acc =  
        match (Scanf.bscanf stream " [%s@$%s@$%d %s@] " (fun rel col id value -> (rel,col,id,value) :: acc)) 
        with 
            | xs -> do_parse xs 
            | exception Scanf.Scan_failure _ -> acc 
            | exception End_of_file -> acc in
        let raw_data = List.rev (do_parse []) in 
        let data:celltable = Hashtbl.create (List.length raw_data) in
        List.iter (fun (rel,col,id,value) -> if log then print_endline ("("^rel^", "^col^", "^string_of_int id^") = "^ value); Hashtbl.add data (rel,col,id) value) raw_data;
        data

let get_default_val_of_type t = match t with 
        (* | Sint -> "ℤ" *)
    | Sint -> "0"
    (* | Sreal -> "ℝ" *)
    (* | Sreal -> "real" *)
    | Sreal -> "0.0"
    | Sbool -> "true"
    | Sstring -> "'none'"


let const_of_string str = 
    try  (Int (int_of_string str)) with
    (* try test () with *)
    | Failure e -> 
        try  (Real (float_of_string str)) with
        (* try test () with *)
        | Failure e -> 
            try  (Bool (bool_of_string str)) with
            (* try test () with *)
            | Failure e | Invalid_argument e -> match str with 
                "null" -> Null 
                | "#t" -> (Bool true)
                | "#f" -> (Bool false)
                | _ -> String str


let instantiate_relation size string_adom (data:celltable) rel_chema = 
    let col_types = get_schema_col_typs rel_chema in
    let rel_name = get_schema_name rel_chema in
    let rec tuple_lst id = if (id<=0) then []
        else try
        ( (Pred (rel_name, List.map (fun (col,typ) -> ConstVar (const_of_string (
            if Hashtbl.mem data (rel_name, col, id) then
                let value = Hashtbl.find data (rel_name, col, id) in 
                if typ = Sstring then (
                    let int_to_string i = List.nth string_adom i in
                    try  (int_to_string (int_of_string value)) with
                        (* try test () with *)
                        | Failure e -> invalid_arg "Function instantiate_relation called with invalid data, tuple "^string_of_int id ^" of relation " ^ rel_name^" not valid"
                )
                else value
            else (get_default_val_of_type typ) (* given a default value for it *)
            ))) col_types)) )::(tuple_lst (id-1))
        with Not_found -> 
            invalid_arg ("Function instantiate_relation called with invalid data, tuple "^string_of_int id ^" of relation " ^ rel_name^" not found") in 
    (* from bag to set *)
    Lib.setify (List.rev (tuple_lst size))
