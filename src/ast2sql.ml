(*******************************************************)
(**  
AST-to-SQL functions
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

(** Given an aggregate function name, checks if it is supported and returns it. *)
let check_agg_function fn =
    let allowed = ["MAX";"MIN";"SUM";"AVG";"COUNT"] in
    if List.mem fn allowed then fn
    else raise (SemErr (
        "Aggregate function '"^fn^"' is not supported, "^
        "allowed functions are: "^(String.concat ", " allowed)
    ))

(** Get sql code for comparison operators.  *)
let sql_of_operator op = match op with 
    (* == accept null, return true if both are null. This is because in postgres the operator = return null if one of its operand is null. "==" is useful in the case of negation or delete from. For example, if a tuple (a,null) in both table 1 and table 2 and there is a rule that table1(X,Y), not table2(X,Y) then (a,null) does not sastify *)
    (* | "==" -> " IS NOT DISTINCT FROM "  
    | "<>" -> " IS DISTINCT FROM " *)
    | "==" -> " = "  
    | "<>" -> " <> "
    | _ -> " "^op^" "

(** Given an arithmetic expression, return in SQL, this function is similar to string_of_vterm. *)
let sql_of_vterm (vt:vartab) (eqt:eqtab) (expr:vterm)  = 
        let open_paren prec op_prec = 
            if prec > op_prec then  "(" else "" in 
        let close_paren prec op_prec = 
            if prec > op_prec then  ")" else "" in
        let rec sql_of prec a_expr = match a_expr with 
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
                    let ve = (Hashtbl.find eqt (Var variable)) in sql_of prec ve
                (*Else, the query is unsafe or inclomplete*)
                else raise (SemErr (
                        "Can not evaluate variable "^(string_of_var variable)^" because it is not in a positive "^
                        "goal or strict equality relation."
                    )
                )
            | BinaryOp("+",f,g) -> (open_paren prec 0)^ (sql_of 0 f) ^ "+" ^ (sql_of 0 g) ^ (close_paren prec 0)
            | BinaryOp("-",f,g) -> (open_paren prec 0) ^ (sql_of 0 f) ^  "-" ^ (sql_of 1 g) ^ (close_paren prec 0)
            | BinaryOp("*",f,g) -> (open_paren prec 2) ^ (sql_of 2 f) ^  "*" ^ (sql_of 2 g) ^ (close_paren prec 2)
            | BinaryOp("/",f,g) -> (open_paren prec 2)^ (sql_of 2 f) ^ "/" ^ (sql_of 3 g) ^ (close_paren prec 2)
            | UnaryOp ("-", e) ->  (open_paren prec 4)^ "-" ^ (sql_of 5 e)^(close_paren prec 4)
            | BinaryOp("^",f,g) -> (open_paren prec 0)^ (sql_of 0 f) ^ "||" ^ (sql_of 0 g) ^ (close_paren prec 0)
            | BinaryOp(op,_,_) | UnaryOp (op, _) -> failwith "Function sql_of_vterm is called with an unknown operator" ^ op
            (* | BoolAnd (f,g) -> (open_paren prec 2) ^ (sql_of 2 f) ^  "*" ^ (sql_of 2 g) ^ (close_paren prec 2)
            | BoolOr (f,g) -> (open_paren prec 0)^ (sql_of 0 f) ^ "+" ^ (sql_of 0 g) ^ (close_paren prec 0)
            | BoolNot e ->  (open_paren prec 4)^ "-" ^ (sql_of 5 e)^(close_paren prec 4) *)
        in sql_of 0 expr

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
        then sql_of_vterm vt eqt (eqt_extract eqt (Var variable))
    (*Else, the query is unsafe or inclomplete*)
    else raise (SemErr (
            "Predicate "^(string_of_symtkey key)^
            " is unsafe, variable "^(string_of_var variable)^" is not in a positive "^
            "goal or strict equality relation."
        )
    )

(** Given the head of the rule, vartab, and eqtab, return the code that
  must be in the select clause. All columns are aliased as col0, col1, ... *)
let get_select_clause (vt:vartab) (eqt:eqtab) rterm =
    let vlst = get_rterm_varlist rterm in 
    let key = symtkey_of_rterm rterm in
    if vlst = [] then
        (* select no colum, two choices: raise error or continue with select no column clause*)
        (* raise (SemErr
            ("Predicate "^(get_rterm_predname rterm)^
            " has arity 0, which is not allowed")) *)
        "SELECT " 
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
    let rec alias ind = function
        | [] -> ""
        | [col] -> col^" AS COL"^(string_of_int ind)
        | col::col2::tl ->
            (col^" AS COL"^(string_of_int ind))^", "^(alias (ind+1) (col2::tl))
    in
    "SELECT "^(alias 0 cols)

let get_aggregation_sql (vt:vartab) (cnt:colnamtab) head agg_eqs agg_ineqs =
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
    let group_by_sql =
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
        (agg_var_col (AggVar (fn,vlst)))^" "^(sql_of_operator op)^" "^(string_of_const c)
        | _ -> raise (SemErr (
            "Predicate "^(string_of_symtkey key)^" contains comparisons of "^
            "aggregates that are not in the form of agg_fun(X) op const"
        )) in 
    let comp_sql = List.map comp_const comparisons in
    let having_sql = if comp_sql = [] then "" else
        "HAVING "^(String.concat " AND " comp_sql) in
    group_by_sql^" "^having_sql

let rec non_rec_unfold_sql_of_symtkey (dbschema:string) (idb:symtable) (cnt:colnamtab) (goal:symtkey)  =
    (* get all the rule having this query in head *)
    (* print_endline ("Reach " ^ (string_of_symtkey goal)); *)
    if not (Hashtbl.mem idb goal) then raise (SemErr ("No rule for the idb predicate "^string_of_symtkey goal))
    else
    let rule_lst = Hashtbl.find idb  goal in
    (* union the SQL of all rules then we have sql of union*)
    let unfold_sql_of_rule_lst (idb:symtable) (cnt:colnamtab) rules =
        let unfold_sql_of_rule (idb:symtable) (cnt:colnamtab) rule =
            let head = rule_head rule in
            let body = rule_body rule in
            (*Split terms in the rule's body. Separate equalities
            * and inequalities in variable and aggregates relations.*)
            let (p_rt,n_rt,all_eqs,all_ineqs) = split_terms body in
            let (agg_eqs,eqs) = List.partition is_agg_equality all_eqs in
            let (agg_ineqs,ineqs) = List.partition is_agg_inequality all_ineqs in
            (*Build vartab, and eqtab for select and where clauses, build vartabl by p_rt which is list of positive predicates*)
            let vt = build_vartab cnt p_rt in
            let eqtb = build_eqtab eqs in
            let select_sql = get_select_clause vt eqtb head in
            let unfold_get_from_clause (idb:symtable) rterms =
                if rterms == [] then "" else
                let idb_alias pname arity n =
                    (* generate sql query for idb predicate *)
                    let idb_sql = non_rec_unfold_sql_of_symtkey dbschema idb cnt (pname,arity)  in
                    let pn_a = pname^"_a"^(string_of_int arity) in
                    "("^idb_sql^")"^" AS "^pn_a^"_"^(string_of_int n)
                in
                let edb_alias pname arity n =
                    if str_contains pname "__tmp_" then pname^" AS "^pname^"_a"^(string_of_int arity)^"_"^(string_of_int n)
                    else dbschema^"."^pname^" AS "^pname^"_a"^(string_of_int arity)^"_"^(string_of_int n) 
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
                "\nFROM "^(String.concat ", " aliases) in
            let from_sql = unfold_get_from_clause idb p_rt in
            
            let unfold_get_where_clause (idb:symtable) (vt:vartab) (cnt:colnamtab) (eqt:eqtab) ineq neg_rt = 
                (*Transform a list of column names in eq relations [a,b,c] -> ['a=b';'a=c']*)
                let var_const _ cols acc = match cols with
                    | [] -> acc
                    | hd::tl ->
                        let eq_rels el = hd^(sql_of_operator "=")^el in
                        (List.map eq_rels tl)::acc
                in
                let fvt = List.flatten (Hashtbl.fold var_const vt []) in
                (*Transform the equalities in the eqtab to strings of the form
                * "CName = value" *)
                let eq_comp e1 e2 acc = if (is_free_var vt e1) then acc else ((sql_of_vterm vt eqt e1)^(sql_of_operator "=")^(sql_of_vterm vt eqt e2))::acc in
                let feqt = Hashtbl.fold eq_comp eqt [] in
                (*Transform the inequalities in the list for strings of the form
                * "CName op value" *)
                let ineq_tuples = List.map extract_ineq_tuple ineq in
                let ineq_comp (op,e1,e2) acc =
                    ((sql_of_vterm vt eqt e1)^" "^(sql_of_operator op)^" "^(sql_of_vterm vt eqt e2))::acc in
                let fineq = List.fold_right ineq_comp ineq_tuples [] in
                (*Transform the negated rterms into SQL*)
                let unfold_sql_of_negated_rterms (idb:symtable) (vt:vartab) (cnt:colnamtab) (eqt:eqtab) neg_rt =
                    let gen_neg_sql rt =
                        (*get basic info of the rterm*)
                        let key = symtkey_of_rterm rt in
                        let pname = get_rterm_predname rt in
                        let arity = get_arity rt in 
                        let alias = pname^"_a"^(string_of_int arity) in
                        let vlst = get_rterm_varlist rt in
                        if not (Hashtbl.mem cnt key) then raise (SemErr ("not found edb or idb predicate "^string_of_symtkey key)) else
                        let cnames = Hashtbl.find cnt key in
                        (*Get the from sql of the rterm*)
                        let from_sql =
                            if Hashtbl.mem idb key then
                                "\nFROM "^ "("^non_rec_unfold_sql_of_symtkey dbschema idb cnt (pname,arity) ^")"^" AS " ^ alias
                            else
                                if str_contains pname "__tmp_" then "\nFROM "^pname^" AS "^alias
                                else "\nFROM "^dbschema^"."^pname^" AS "^alias
                        in
                        (* print_endline "___neg sql___"; print_string from_sql; print_endline "___neg sql___"; *)
                        (*Get the where sql of the rterm*)
                        let build_const acc col var =
                            let eq_to = alias^"."^col^(sql_of_operator "==") in
                            match var with
                            | NamedVar vn -> 
                                if Hashtbl.mem vt vn then
                                    (eq_to^(List.hd (Hashtbl.find vt vn)))::acc
                                else if Hashtbl.mem eqt (Var var) then
                                    (eq_to^(sql_of_vterm vt eqt (Hashtbl.find eqt (Var var))))::acc
                                else raise (SemErr (
                                    "Program is unsafe, variable "^vn^
                                    " in negated call to predicate "^
                                    (string_of_symtkey key)^" does not appear in a positive "^
                                    "goal or strict equation. Try anonimous variables."
                                ))  
                            | NumberedVar _ -> let vn = string_of_var var in
                            if Hashtbl.mem vt vn then
                                (eq_to^(List.hd (Hashtbl.find vt vn)))::acc
                            else if Hashtbl.mem eqt (Var var) then
                                (eq_to^(sql_of_vterm vt eqt (Hashtbl.find eqt (Var var))))::acc
                            else raise (SemErr (
                                "Program is unsafe, variable "^vn^
                                " in negated call to predicate "^
                                (string_of_symtkey key)^" does not appear in a positive "^
                                "goal or strict equation. Try anonimous variables."
                            ))
                            | ConstVar c -> (eq_to^(string_of_const c))::acc
                            | AnonVar -> acc
                            | _ -> invalid_arg "There is a non-expected type of var in a negated rterm"
                        in
                        let const_lst = List.fold_left2 build_const [] cnames vlst in
                        let where_sql =
                            if const_lst = [] then ""
                            else "\nWHERE "^(String.concat " AND " const_lst)
                        in
                        (**Return the final string*)
                        "NOT EXISTS ( SELECT * "^from_sql^" "^where_sql^" )"
                    in
                    List.map gen_neg_sql neg_rt in
                let fnrt = unfold_sql_of_negated_rterms idb vt cnt eqt neg_rt in
                (*merge all constraints*)
                let constraints = fvt@feqt@fineq@fnrt in
                match constraints with
                    | [] -> ""
                    | _ -> "\nWHERE "^(String.concat " AND " constraints) in
            let where_sql = unfold_get_where_clause idb vt cnt eqtb ineqs n_rt in
            let agg_sql = get_aggregation_sql vt cnt head agg_eqs agg_ineqs in
            String.concat " " [select_sql;from_sql;where_sql;agg_sql] in
        let sql_list = List.map (unfold_sql_of_rule idb cnt) rules in
        String.concat (if (get_symtkey_arity goal) = 0 then " UNION ALL " else " UNION ") sql_list in
    let sql = unfold_sql_of_rule_lst idb cnt rule_lst in
    sql

(** Take a query term and generate unfolded SQL for it. *)
let non_rec_unfold_sql_of_query (dbschema:string) (idb:symtable) (cnt:colnamtab) (query:rterm) =
    let qrule = rule_of_query query idb in
    (* qrule is in the form of _dummy_(x,y) :- query_predicate(x,y), x=1 *)
        let local_idb = Hashtbl.copy idb in 
        (* because insert a temporary dummy qrule, we should work with a local variable of idb *)
        symt_insert local_idb qrule;
        (* get column names (cols_by_var) for the view by using the dummy predicate which is head of qrule *)
        let cols_by_var = List.map string_of_var (get_rterm_varlist (rule_head qrule)) in
        let qrule_alias = get_rule_predname qrule in
        if not (Hashtbl.mem cnt (symtkey_of_rterm query)) then raise (SemErr "The query does not match any idb relation") 
        else
        let cols = Hashtbl.find cnt (symtkey_of_rterm query) in
        let sel_lst = List.map (fun (a,b) -> qrule_alias^"."^a^" AS "^b)
                            (List.combine cols cols_by_var) in 
        "SELECT "^(String.concat "," sel_lst) ^ " \nFROM (" ^
        (* by insert the dummy rule to idb, we now find sql for this dummy predicate *)
        non_rec_unfold_sql_of_symtkey dbschema local_idb cnt (symtkey_of_rterm (rule_head qrule)) ^") AS "^qrule_alias

(** Generate unfolded SQL statement from ast, the goal is the query predicate of datalog program, the query is a query over source relations.
The result of this function is SQL query, whose returned table has column names of col0, col1,.... *)
let unfold_query_sql_stt (dbschema:string) (log:bool) (edb:symtable) prog =
    let query_rt = get_query prog in
    (*Extract and pre-process the IDB from the program*)
    let idb = extract_idb prog in
    preprocess_rules idb; 
    (* print_symtable idb; *)
    (*Build the colnamtab for referencing the table's columns*)
    let cnt = build_colnamtab edb idb in
    (*Return the desired SQL*)
    let sql = non_rec_unfold_sql_of_query dbschema idb cnt query_rt  in
    sql


let unfold_view_sql (dbschema:string) (log:bool) prog =
    let edb = extract_edb prog in
    let view_rt = get_schema_rterm (get_view prog) in 
    if (get_arity view_rt = 0) then raise (SemErr
            ("The view "^(string_of_rterm view_rt)^
            " has arity 0, which is not allowed to create a view"))
    else
    "CREATE OR REPLACE VIEW "^ dbschema ^"."^(get_rterm_predname view_rt) ^ " AS \n" ^(unfold_query_sql_stt dbschema log edb {prog with query = Some view_rt}) ^";"


let unfold_program_query (dbschema:string) (log:bool) prog =
    if (log) then print_endline ("==> generating SQL query of datalog program of query "^ string_of_query (get_query prog)) else ();
    let edb = extract_edb prog in
    unfold_query_sql_stt dbschema log edb prog


(** Take a view update datalog program (containing both get and put directions) and generate SQL queries of constraints involving view. *)
let view_constraint_sql_of_stt (dbschema:string) (log:bool) (inc:bool) (optimize:bool) prog =
    let clean_prog = keep_only_constraint_of_view log prog in
    if inc then     
        let inc_prog = incrementalize_by_view log clean_prog in
        let view_sch = get_view inc_prog in
        let view_rt = get_schema_rterm view_sch in
        let new_view_rt = rename_rterm "new_" view_rt in
        let subst_prog = subst_pred (get_rterm_predname view_rt) (get_rterm_predname new_view_rt) inc_prog in
        let prog2 = {subst_prog with 
            sources = [(get_rterm_predname (view_rt), get_schema_col_typs view_sch ); 
                (get_rterm_predname (get_temp_delta_deletion_rterm view_rt), get_schema_col_typs view_sch ); 
                (get_rterm_predname (get_temp_delta_insertion_rterm view_rt), get_schema_col_typs view_sch )]@subst_prog.sources; 
            rules = [(get_inc_original view_rt,[Rel (view_rt)]); 
                (get_inc_ins view_rt,[Rel (get_temp_delta_insertion_rterm view_rt)]); 
                (get_inc_del view_rt,[Rel (get_temp_delta_deletion_rterm view_rt)])] @ subst_prog.rules} in
        (* let edb = extract_edb prog2 in *)
        let idb = extract_idb prog2 in
        if Hashtbl.mem idb (symtkey_of_rterm get_empty_pred) then
            (
            (* keep_only_constraint_of_view log view_rt edb idb ; *)
            preprocess_rules idb;
            (* let cnt = build_colnamtab edb idb in *)
            if Hashtbl.mem idb (symtkey_of_rterm get_empty_pred) then
                let remain_rules = rules_of_symt idb in
                let prog3 = {get_empty_expr with view = prog2.view; sources = prog2.sources; rules = remain_rules} in
                (* non_rec_unfold_sql_of_query dbschema idb cnt get_empty_pred *)
                let prog4 = if (optimize) then (Ast2fol.optimize_query_datalog log {prog3 with query = Some (get_empty_pred)}) else {prog3 with query = Some (get_empty_pred)} in
                (* the optimization may drop the empty_predicate of prog4 when the empty_predicate is trival (always empty) *)
                if (has_query prog4) then
                    (unfold_program_query dbschema log prog4)
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
            (* keep_only_constraint_of_view log view_rt edb idb ; *)
            preprocess_rules idb;
            (* let cnt = build_colnamtab edb idb in *)
            if Hashtbl.mem idb (symtkey_of_rterm get_empty_pred) then
                let remain_rules = Hashtbl.fold (fun k rules lst -> rules@lst) idb [] in
                let prog3 = {get_empty_expr with view = prog2.view; sources = prog2.sources; rules = remain_rules} in
                (* non_rec_unfold_sql_of_query dbschema idb cnt get_empty_pred *)
                let prog4 = if (optimize) then (Ast2fol.optimize_query_datalog log {prog3 with query = Some get_empty_pred} ) else {prog3 with query = Some get_empty_pred} in
                (* the optimization may drop the empty_predicate of prog4 when the empty_predicate is trival (always empty) *)
                if (has_query prog4) then
                    (unfold_program_query dbschema log prog4)
                else "SELECT WHERE false"
            else "SELECT WHERE false")
        else "SELECT WHERE false"


(** take a view update datalog program (containing both get and put directions) and generate SQL query of contraints not involving view *)
let non_view_constraint_sql_of_stt (dbschema:string) (log:bool) (inc:bool) (optimize:bool) prog =
    let clean_prog = remove_constraint_of_view log prog in
    if inc then
        let inc_prog = incrementalize_by_view log clean_prog in
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
            (* keep_only_constraint_of_view log view_rt edb idb ; *)
            preprocess_rules idb;
            (* let cnt = build_colnamtab edb idb in *)
            if Hashtbl.mem idb (symtkey_of_rterm get_empty_pred) then
                let remain_rules = rules_of_symt idb in
                let prog3 = {get_empty_expr with view = prog2.view; sources = prog2.sources; rules = remain_rules} in
                (* non_rec_unfold_sql_of_query dbschema idb cnt get_empty_pred *)
                let prog4 = if (optimize) then (Ast2fol.optimize_query_datalog log {prog3 with query = Some get_empty_pred}) else {prog3 with query=Some get_empty_pred} in
                (* the optimization may drop the empty_predicate of prog4 when the empty_predicate is trival (always empty) *)
                if (has_query prog4) then
                    (unfold_program_query dbschema log prog4)
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
            (* keep_only_constraint_of_view log view_rt edb idb ; *)
            preprocess_rules idb;
            (* let cnt = build_colnamtab edb idb in *)
            if Hashtbl.mem idb (symtkey_of_rterm get_empty_pred) then
                let remain_rules = Hashtbl.fold (fun k rules lst -> rules@lst) idb [] in
                let prog3 = {get_empty_expr with view = prog2.view; sources = prog2.sources; rules = remain_rules} in
                (* non_rec_unfold_sql_of_query dbschema idb cnt get_empty_pred *)
                let prog4 = if (optimize) then (Ast2fol.optimize_query_datalog log {prog3 with query = Some get_empty_pred} ) else {prog3 with query = Some get_empty_pred} in
                (* the optimization may drop the empty_predicate of prog4 when the empty_predicate is trival (always empty) *)
                if (has_query prog4) then
                    (unfold_program_query dbschema log prog4)
                else "SELECT WHERE false"
            else "SELECT WHERE false")
        else "SELECT WHERE false"


(** Get SQL code for a delta term, the update SQL code contains two strings: SQL queries for the delta, and SQL updates for the delta. *)
let non_rec_unfold_sql_of_update (dbschema:string) (log:bool) (optimize:bool) prog (delta:rterm)  =
    (* 
    *)
    let view_sch = get_view prog in
    let view_rt = get_schema_rterm view_sch in
    let prog2 = 
        if (optimize) then (
            let opt_prog1 = Ast2fol.optimize_query_datalog log {prog with query = Some delta} in
            Ast2fol.optimize_query_datalog log {opt_prog1 with query = Some delta}
        ) else 
        prog in
        (* print_endline "___ optimized __";
        print_endline (string_of_prog prog2); *)
    (* the optimization may drop the empty_predicate of prog4 when the empty_predicate is trival (always empty) *)
    if (optimize && (not (has_query prog2))) then ("","","")
    else 
    (*Build the colnamtab for referencing the table's columns*)
    let edb = extract_edb prog2 in
    (* print_endline "___local_edb____"; print_symtable local_edb; *)
    (*Extract and pre-process the IDB from the program*)
    let idb = extract_idb prog2 in
    preprocess_rules idb;
    let cnt = build_colnamtab edb idb in
    let qrule = rule_of_query delta idb in
        let local_idb = Hashtbl.copy idb in 
        symt_insert local_idb qrule;
        match delta with
        Deltainsert (pname, varlst) -> if Hashtbl.mem edb (pname, List.length varlst) 
            then  (
            (* variable with rowtype of the source relation *)
            "temprec_"^ (get_rterm_predname delta) ^" " ^dbschema^"."^ pname ^"%ROWTYPE;
            array_"^ (get_rterm_predname delta)^" " ^dbschema^"."^ pname ^"[];", 
            (* calculate the delta relation by creating a temporary table *)
            "
            WITH "^(get_rterm_predname (rename2_rterm "_ar" (get_temp_delta_deletion_rterm view_rt)))^" AS (SELECT * FROM unnest(array_delta_del) as array_delta_del_alias limit delta_del_size),
            "^(get_rterm_predname (rename2_rterm "_ar" (get_temp_delta_insertion_rterm view_rt)))^" as (SELECT * FROM unnest(array_delta_ins) as array_delta_ins_alias limit delta_ins_size)
            SELECT array_agg(tbl) INTO array_"^ (get_rterm_predname delta)^" FROM ("^ 
            "SELECT "^"(ROW("^(String.concat "," (Hashtbl.find cnt (symtkey_of_rterm delta))) ^") :: "^dbschema^"."^ pname ^").* 
            FROM ("^
            (non_rec_unfold_sql_of_symtkey dbschema local_idb cnt (symtkey_of_rterm (rule_head qrule))) ^") AS "^(get_rterm_predname delta)^"_extra_alias) AS tbl"
            (* ^" 
            EXCEPT 
            SELECT * FROM  "^dbschema^"."^ pname  *)
            ^";", 
            (* insert tuples using batch insertion *)
            "
            IF array_"^ (get_rterm_predname delta)^" IS DISTINCT FROM NULL THEN 
                INSERT INTO " ^dbschema^"."^ pname ^" (SELECT * FROM unnest(array_"^ (get_rterm_predname delta)^") as array_"^ (get_rterm_predname delta)^"_alias) ; 
            END IF;"
            (* insert  each tuple by using a LOOP*)
            (* "FOR temprec"^ (get_rterm_predname delta) ^" IN ( SELECT * FROM " ^ (get_rterm_predname delta) ^ ") LOOP 
            " ^
            "INSERT INTO " ^dbschema^"."^ pname ^" SELECT (temprec"^ (get_rterm_predname delta) ^").*; 
            END LOOP; " ^ *)

            )
            else raise (SemErr "delta predicate is not of any base predicate")
        
        | Deltadelete (pname, varlst) -> if Hashtbl.mem edb (pname, List.length varlst) 
            then 
            (* get all the columns of base predicate *)
            let cols = Hashtbl.find cnt (pname, List.length varlst) in
            (* convert these cols to string of tuple of these cols *)
            (
            (* variable with rowtype of the source relation *)
            "temprec_"^ (get_rterm_predname delta) ^" " ^dbschema^"."^ pname ^"%ROWTYPE;
            array_"^ (get_rterm_predname delta)^" " ^dbschema^"."^ pname ^"[];", 
            (* calculate the delta relation by creating a temporary table *)
            "
            WITH "^(get_rterm_predname (rename2_rterm "_ar" (get_temp_delta_deletion_rterm view_rt)))^" AS (SELECT * FROM unnest(array_delta_del) as array_delta_del_alias limit delta_del_size),
            "^(get_rterm_predname (rename2_rterm "_ar" (get_temp_delta_insertion_rterm view_rt)))^" as (SELECT * FROM unnest(array_delta_ins) as array_delta_ins_alias limit delta_ins_size)
            SELECT array_agg(tbl) INTO array_"^ (get_rterm_predname delta)^" FROM (" ^ 
            "SELECT "^"(ROW("^(String.concat "," (Hashtbl.find cnt (symtkey_of_rterm delta))) ^") :: "^dbschema^"."^ pname ^").* 
            FROM ("^
            (non_rec_unfold_sql_of_symtkey dbschema local_idb cnt (symtkey_of_rterm (rule_head qrule)))^") AS "^(get_rterm_predname delta)^"_extra_alias) AS tbl;", 
            (* delete each tuple *)
            "
            IF array_"^ (get_rterm_predname delta)^" IS DISTINCT FROM NULL THEN 
                FOREACH temprec_"^ (get_rterm_predname delta) ^" IN array array_"^ (get_rterm_predname delta)^"  LOOP 
            " ^
            "       DELETE FROM " ^dbschema^"."^ pname ^" WHERE "^(String.concat " AND " (List.map (fun x -> x^ (sql_of_operator "==") ^" temprec_"^ (get_rterm_predname delta)^"."^x) cols)) ^ ";
                END LOOP;
            END IF;")
            
            else raise (SemErr "delta predicate is not of any base predicate")
        | _ -> raise (SemErr "the non_rec_unfold_sql_of_update is called without and delta predicate")

let unfold_delta_sql_stt (dbschema:string) (log:bool) (inc:bool) (optimize:bool) prog =
    if inc then 
        let inc_prog = incrementalize_by_view log prog in
        let view_sch = get_view inc_prog in
        let view_rt = get_schema_rterm view_sch in
        let get_ast = Ast2fol.optimize_query_datalog log {prog with query = Some view_rt} in
        let new_view_rt = rename_rterm "new_" view_rt in
        let subst_prog = subst_pred (get_rterm_predname view_rt) (get_rterm_predname new_view_rt) inc_prog in
        (* let subst_prog = inc_prog in *)
        let prog2 = {subst_prog with sources = [
            (* get_rterm_predname (view_rt), get_schema_col_typs view_sch; *)
            get_rterm_predname (rename2_rterm "_ar" (get_temp_delta_deletion_rterm view_rt)), get_schema_col_typs view_sch;
            get_rterm_predname (rename2_rterm "_ar" (get_temp_delta_insertion_rterm view_rt)), get_schema_col_typs view_sch
        ]@subst_prog.sources; rules = [
            (get_inc_original view_rt,[Rel (view_rt)]);
            (get_inc_ins view_rt,[Rel (rename2_rterm "_ar" (get_temp_delta_insertion_rterm view_rt))]);
            (get_inc_del view_rt,[Rel (rename2_rterm "_ar" (get_temp_delta_deletion_rterm view_rt))])
        ]@subst_prog.rules@get_ast.rules} in
        let delta_rt_lst = get_delta_rterms inc_prog in
        (*Return the desired SQL*)
        let update_sql_lst = List.map (non_rec_unfold_sql_of_update dbschema log optimize prog2 ) delta_rt_lst in 
        let concat_update_sql (v, d, u) (vardec,delquery, updateaction) = (v::vardec,d::delquery, u::updateaction) in 
        let (vars, deltas, actions) = List.fold_right concat_update_sql update_sql_lst ([],[],[]) in 
        ((String.concat "\n" vars)^"",
        ((String.concat "\n\n" deltas)^"") ^ " \n\n" ^ ((String.concat "\n\n" actions)^""))
    else
        let view_sch = get_view prog in
        let view_rt = get_schema_rterm view_sch in
        let new_view_rt = rename_rterm "new_" view_rt in
        let subst_prog = subst_pred (get_rterm_predname view_rt) (get_rterm_predname new_view_rt) (delete_rule_of_predname (get_rterm_predname view_rt) prog) in
        let prog2 = {subst_prog with sources = [
            get_rterm_predname view_rt , get_schema_col_typs view_sch;
            get_rterm_predname (rename2_rterm "_ar" (get_temp_delta_deletion_rterm view_rt)), get_schema_col_typs view_sch;
            get_rterm_predname (rename2_rterm "_ar" (get_temp_delta_insertion_rterm view_rt)), get_schema_col_typs view_sch
        ]@subst_prog.sources; rules = [
            (new_view_rt,[Rel (view_rt); Not (rename2_rterm "_ar" (get_temp_delta_deletion_rterm view_rt))]);
            (new_view_rt,[Rel (rename2_rterm "_ar" (get_temp_delta_insertion_rterm view_rt))])
        ]@subst_prog.rules} in

        (* need to insert a temporary predicate of query (view) to edb because for update sql we need to assume a tempoarary view is created. Suppose that query rterm contains only variables *)

        let delta_rt_lst = get_delta_rterms prog in
        (*Return the desired SQL*)
        let update_sql_lst = List.map (non_rec_unfold_sql_of_update dbschema log optimize prog2 ) delta_rt_lst in 
        let concat_update_sql (v, d, u) (vardec,delquery, updateaction) = (v::vardec,d::delquery, u::updateaction) in 
        let (vars, deltas, actions) = List.fold_right concat_update_sql update_sql_lst ([],[],[]) in 
        ((String.concat "\n" vars)^"",
        ((String.concat "\n\n" deltas)^"") ^ " \n\n" ^ ((String.concat "\n\n" actions)^""))


(** SQL code for triggers of detecting updates on the source relations, which call the action of executing shell script on the view. *)
let source_update_detection_trigger_stt (dbschema:string) (log:bool) (dejima_user:string) prog =
    let view_rt = get_schema_rterm (get_view prog) in
    let view_name = get_rterm_predname view_rt in
    let all_source = get_source_stts prog in 
    let effect_sources, sql_lst = List.fold_left (fun (source_lst, code_lst) x  -> 
        let source_rt = get_schema_rterm x in
        let cols_tuple_str = "("^ (String.concat "," (List.map  string_of_var (get_rterm_varlist (source_rt)) )) ^")" in
        let source_name = get_rterm_predname source_rt in 
        let inc_view_definition_raw = incrementalize_view_definition log source_rt prog in
        (* let new_source_rt = rename_rterm "new_" source_rt in *)
        (* let subst_prog = subst_pred (get_rterm_predname source_rt) (get_rterm_predname new_source_rt) inc_view_definition_raw in *)
        let subst_prog = delete_rule_of_predname (get_rterm_predname source_rt) inc_view_definition_raw in
        let inc_view_definition = {subst_prog with sources = [
            get_rterm_predname (get_temp_delta_deletion_rterm source_rt)^"_for_"^view_name^"_ar", get_schema_col_typs x;
            get_rterm_predname (get_temp_delta_insertion_rterm source_rt)^"_for_"^view_name^"_ar", get_schema_col_typs x
        ]@subst_prog.sources; rules = [
            (get_inc_original source_rt,[Rel (source_rt); Not(get_inc_ins source_rt)]);
            (get_inc_original source_rt,[Rel (get_inc_del source_rt)]);
            (get_inc_ins source_rt,[Rel (rename2_rterm ("_for_"^view_name^"_ar") (get_temp_delta_insertion_rterm source_rt))]);
            (get_inc_del source_rt,[Rel (rename2_rterm ("_for_"^view_name^"_ar") (get_temp_delta_deletion_rterm source_rt))])
        ]@subst_prog.rules} in
        if not (is_defined_pred (get_rterm_predname (get_inc_ins view_rt)) inc_view_definition) || not (is_defined_pred (get_rterm_predname (get_inc_del view_rt)) inc_view_definition) then (source_lst, code_lst)
        else
            let ins_view_optimized_datalog = (Ast2fol.optimize_query_datalog log {inc_view_definition with query = Some (get_inc_ins view_rt)}) in 
            let del_view_optimized_datalog = (Ast2fol.optimize_query_datalog log {inc_view_definition with query = Some (get_inc_del view_rt)}) in
            (* print_endline (non_rec_unfold_sql_of_symtkey dbschema idb cnt (symtkey_of_rterm (get_inc_del view_rt))); *)
            (x::source_lst,
("
CREATE OR REPLACE FUNCTION "^dbschema^"."^source_name^"_materialization_for_"^view_name^"()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
DECLARE
text_var1 text;
text_var2 text;
text_var3 text;
BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = '"^(get_rterm_predname (get_temp_delta_insertion_rterm source_rt))^ "_for_"^view_name^ "' OR table_name = '"^(get_rterm_predname (get_temp_delta_deletion_rterm source_rt))^"_for_"^view_name^"')
    THEN
        -- RAISE LOG 'execute procedure "^source_name^"_materialization_for_"^view_name^"';
        CREATE TEMPORARY TABLE "^(get_rterm_predname (get_temp_delta_insertion_rterm source_rt))^"_for_"^view_name^" ( LIKE " ^dbschema^"."^(get_rterm_predname (source_rt)) ^" ) WITH OIDS ON COMMIT DROP;
        CREATE TEMPORARY TABLE "^(get_rterm_predname (get_temp_delta_deletion_rterm source_rt))^"_for_"^view_name^" ( LIKE " ^dbschema^"."^(get_rterm_predname (source_rt)) ^" ) WITH OIDS ON COMMIT DROP;
        
    END IF;
    RETURN NULL;
EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to "^dbschema^"."^source_name^"';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of "^dbschema^"."^source_name^" ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
END;
$$;

DROP TRIGGER IF EXISTS "^source_name^"_trigger_materialization_for_"^view_name^" ON "^dbschema^"."^source_name^";
CREATE TRIGGER "^source_name^"_trigger_materialization_for_"^view_name^"
    BEFORE INSERT OR UPDATE OR DELETE ON
    "^dbschema^"."^source_name^" FOR EACH STATEMENT EXECUTE PROCEDURE "^dbschema^"."^source_name^"_materialization_for_"^view_name^"();

CREATE OR REPLACE FUNCTION "^dbschema^"."^source_name^"_update_for_"^view_name^"()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
DECLARE
text_var1 text;
text_var2 text;
text_var3 text;
BEGIN
    -- RAISE LOG 'execute procedure "^source_name^"_update_for_"^view_name^"';
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = '"^view_name^"_delta_action_flag') THEN
        IF TG_OP = 'INSERT' THEN
        -- RAISE LOG 'NEW: %', NEW;
        IF (SELECT count(*) FILTER (WHERE j.value = jsonb 'null') FROM  jsonb_each(to_jsonb(NEW)) j) > 0 THEN 
            RAISE check_violation USING MESSAGE = 'Invalid update: null value is not accepted';
        END IF;
        DELETE FROM "^(get_rterm_predname (get_temp_delta_deletion_rterm source_rt)^"_for_"^view_name)^" WHERE ROW"^cols_tuple_str^(sql_of_operator "==")^"NEW;
        INSERT INTO "^(get_rterm_predname (get_temp_delta_insertion_rterm source_rt)^"_for_"^view_name)^" SELECT (NEW).*; 
        ELSIF TG_OP = 'UPDATE' THEN
        IF (SELECT count(*) FILTER (WHERE j.value = jsonb 'null') FROM  jsonb_each(to_jsonb(NEW)) j) > 0 THEN 
            RAISE check_violation USING MESSAGE = 'Invalid update: null value is not accepted';
        END IF;
        DELETE FROM "^(get_rterm_predname (get_temp_delta_insertion_rterm source_rt)^"_for_"^view_name)^" WHERE ROW"^cols_tuple_str^(sql_of_operator "==")^"OLD;
        INSERT INTO "^(get_rterm_predname (get_temp_delta_deletion_rterm source_rt)^"_for_"^view_name)^" SELECT (OLD).*;
        DELETE FROM "^(get_rterm_predname (get_temp_delta_deletion_rterm source_rt)^"_for_"^view_name)^" WHERE ROW"^cols_tuple_str^(sql_of_operator "==")^"NEW;
        INSERT INTO "^(get_rterm_predname (get_temp_delta_insertion_rterm source_rt)^"_for_"^view_name)^" SELECT (NEW).*; 
        ELSIF TG_OP = 'DELETE' THEN
        -- RAISE LOG 'OLD: %', OLD;
        DELETE FROM "^(get_rterm_predname (get_temp_delta_insertion_rterm source_rt)^"_for_"^view_name)^" WHERE ROW"^cols_tuple_str^(sql_of_operator "==")^"OLD;
        INSERT INTO "^(get_rterm_predname (get_temp_delta_deletion_rterm source_rt)^"_for_"^view_name)^" SELECT (OLD).*;
        END IF;
    END IF;
    RETURN NULL;
EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to "^dbschema^"."^source_name^"';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of "^dbschema^"."^source_name^" ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
END;
$$;

DROP TRIGGER IF EXISTS "^source_name^"_trigger_update_for_"^view_name^" ON "^dbschema^"."^source_name^";
CREATE TRIGGER "^source_name^"_trigger_update_for_"^view_name^"
    AFTER INSERT OR UPDATE OR DELETE ON
    "^dbschema^"."^source_name^" FOR EACH ROW EXECUTE PROCEDURE "^dbschema^"."^source_name^"_update_for_"^view_name^"();

CREATE OR REPLACE FUNCTION "^dbschema^"."^source_name^"_detect_update_on_"^view_name^"()
RETURNS trigger
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
DECLARE
text_var1 text;
text_var2 text;
text_var3 text;
func text;
tv text;
deletion_data text;
insertion_data text;
json_data text;
result text;
user_name text;
xid int;
array_delta_del "^dbschema^"."^source_name^"[];
array_delta_ins "^dbschema^"."^source_name^"[];
detected_deletions "^dbschema^"."^view_name^"[];
detected_insertions "^dbschema^"."^view_name^"[];
delta_ins_size int;
delta_del_size int;
BEGIN
IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = '"^source_name^"_detect_update_on_"^view_name^"_flag') THEN
    CREATE TEMPORARY TABLE "^source_name^"_detect_update_on_"^view_name^"_flag ON COMMIT DROP AS (SELECT true as finish);
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = '"^view_name^"_delta_action_flag') THEN
        SELECT array_agg(tbl) INTO array_delta_ins FROM "^(get_rterm_predname (get_temp_delta_insertion_rterm source_rt)^"_for_"^view_name)^" AS tbl;
        select count(*) INTO delta_ins_size FROM "^(get_rterm_predname (get_temp_delta_insertion_rterm source_rt)^"_for_"^view_name)^";
        SELECT array_agg(tbl) INTO array_delta_del FROM "^(get_rterm_predname (get_temp_delta_deletion_rterm source_rt)^"_for_"^view_name)^" tbl;
        select count(*) INTO delta_del_size FROM "^(get_rterm_predname (get_temp_delta_deletion_rterm source_rt)^"_for_"^view_name)^";

        WITH " ^get_rterm_predname (get_temp_delta_insertion_rterm source_rt)^"_for_"^view_name^"_ar as (SELECT * FROM unnest(array_delta_ins) as array_delta_ins_alias limit delta_ins_size), 
        " ^get_rterm_predname (get_temp_delta_deletion_rterm source_rt)^"_for_"^view_name^"_ar as (SELECT * FROM unnest(array_delta_del) as array_delta_del_alias limit delta_del_size)
        SELECT array_agg(tbl) INTO detected_insertions FROM ("^(
            unfold_program_query dbschema log ins_view_optimized_datalog
            ) ^") AS tbl;

        insertion_data := (SELECT (array_to_json(detected_insertions))::text);
        IF insertion_data IS NOT DISTINCT FROM NULL THEN 
            insertion_data := '[]';
        END IF; 

        WITH " ^get_rterm_predname (get_temp_delta_insertion_rterm source_rt)^"_for_"^view_name^"_ar as (SELECT * FROM unnest(array_delta_ins) as array_delta_ins_alias limit delta_ins_size), 
        " ^get_rterm_predname (get_temp_delta_deletion_rterm source_rt)^"_for_"^view_name^"_ar as (SELECT * FROM unnest(array_delta_del) as array_delta_del_alias limit delta_del_size)
        SELECT array_agg(tbl) INTO detected_deletions FROM ("^(
            unfold_program_query dbschema log del_view_optimized_datalog
            ) ^") AS tbl;

        deletion_data := (  
        SELECT (array_to_json(detected_deletions))::text);
        IF deletion_data IS NOT DISTINCT FROM NULL THEN 
            deletion_data := '[]';
        END IF; 
        IF (insertion_data IS DISTINCT FROM '[]') OR (deletion_data IS DISTINCT FROM '[]') THEN 
            user_name := (SELECT session_user);
            IF NOT (user_name = '"^dejima_user^"') THEN 
                xid := (SELECT txid_current());
                json_data := concat('{\"xid\": \"', xid, '\" , \"view\": ' , '\""^dbschema^"."^view_name^"\"', ', ' , '\"insertions\": ' , insertion_data , ', ' , '\"deletions\": ' , deletion_data , '}');
                result := "^dbschema^"."^view_name^"_run_shell(json_data);
                IF result = 'true' THEN 
                    DROP TABLE "^(get_rterm_predname (get_temp_delta_insertion_rterm source_rt)^"_for_"^view_name)^";
                    DROP TABLE "^(get_rterm_predname (get_temp_delta_deletion_rterm source_rt)^"_for_"^view_name)^";
                ELSE
                    CREATE TEMPORARY TABLE IF NOT EXISTS dejima_abort_flag ON COMMIT DROP AS (SELECT true as finish);
                    RAISE LOG 'update on view is rejected by the external tool, result from running the sh script: %', result;
                    -- RAISE check_violation USING MESSAGE = 'update on view is rejected by the external tool, result from running the sh script: ' 
                    -- || result;
                END IF;
            ELSE 
                RAISE LOG 'function of detecting dejima update is called by % , no request sent to dejima proxy', user_name;
                xid := (SELECT txid_current());

                -- update the table that stores the insertions and deletions we calculated
                -- DELETE FROM "^dbschema^".__dummy__"^view_name^"_detected_deletions;
                INSERT INTO "^dbschema^".__dummy__"^view_name^"_detected_deletions
                    ( SELECT xid, * FROM unnest(detected_deletions) as detected_deletions_alias );

                -- DELETE FROM "^dbschema^".__dummy__"^view_name^"_detected_insertions;
                INSERT INTO "^dbschema^".__dummy__"^view_name^"_detected_insertions
                    ( SELECT xid, * FROM unnest(detected_insertions) as detected_insertions_alias );
            END IF;
        END IF;
    END IF;
END IF;
RETURN NULL;
EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to "^dbschema^"."^source_name^"';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the function "^dbschema^"."^source_name^"_detect_update_on_"^view_name^"() ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
END;
$$;

DROP TRIGGER IF EXISTS "^source_name^"_detect_update_on_"^view_name^" ON "^dbschema^"."^source_name^";
CREATE CONSTRAINT TRIGGER "^source_name^"_detect_update_on_"^view_name^"
    AFTER INSERT OR UPDATE OR DELETE ON
    "^dbschema^"."^source_name^" DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE "^dbschema^"."^source_name^"_detect_update_on_"^view_name^"();

CREATE OR REPLACE FUNCTION "^dbschema^"."^source_name^"_propagate_updates_to_"^view_name^" ()
RETURNS boolean
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  BEGIN
    SET CONSTRAINTS "^dbschema^"."^source_name^"_detect_update_on_"^view_name^" IMMEDIATE;
    SET CONSTRAINTS "^dbschema^"."^source_name^"_detect_update_on_"^view_name^" DEFERRED;
    DROP TABLE IF EXISTS "^source_name^"_detect_update_on_"^view_name^"_flag;
    RETURN true;
  END;
$$;

") :: code_lst) )
    ([], [])    all_source in 
    (effect_sources, String.concat "\n \n" sql_lst)


(** Generate trigger for delta predicates on the view. *)
let unfold_delta_trigger_stt (dbschema:string) (log:bool) (dejima_update_detect) (sh_script:string) (dejima_user:string) (inc:bool) (optimize:bool) prog =
    let view_rt = get_schema_rterm (get_view prog) in
    let view_name = get_rterm_predname view_rt in
    (* let temporary_view_name = get_rterm_predname (get_temp_rterm view_rt) in *)
    (* get all the columns of base predicate *)
    (* convert these cols to string of tuple of these cols *)
    let cols_tuple_str = "("^ (String.concat "," (List.map  string_of_var (get_rterm_varlist (get_temp_rterm view_rt)) )) ^")" in
    let (vardec, delta_sql_stt) = unfold_delta_sql_stt dbschema log inc optimize prog in
    let effect_sources, update_detection_trigger_sql = if dejima_update_detect then source_update_detection_trigger_stt dbschema log dejima_user prog else ([], "") in
    let trigger_pgsql = 
(* "
DROP MATERIALIZED VIEW IF EXISTS "^dbschema^"."^(get_rterm_predname (get_materializied_rterm view_rt))^";

CREATE  MATERIALIZED VIEW "^dbschema^"."^(get_rterm_predname (get_materializied_rterm view_rt))^" AS 
SELECT * FROM "^dbschema^"."^view_name^";

" ^ *)

"
CREATE EXTENSION IF NOT EXISTS plsh;

CREATE TABLE IF NOT EXISTS "^dbschema^".__dummy__"^view_name^"_detected_deletions (txid int, LIKE "^dbschema^"."^view_name^" );
CREATE INDEX IF NOT EXISTS idx__dummy__"^view_name^"_detected_deletions ON "^dbschema^".__dummy__"^view_name^"_detected_deletions (txid);
CREATE TABLE IF NOT EXISTS "^dbschema^".__dummy__"^view_name^"_detected_insertions (txid int, LIKE "^dbschema^"."^view_name^" );
CREATE INDEX IF NOT EXISTS idx__dummy__"^view_name^"_detected_insertions ON "^dbschema^".__dummy__"^view_name^"_detected_insertions (txid);

CREATE OR REPLACE FUNCTION "^dbschema^"."^view_name^"_get_detected_update_data(txid int)
RETURNS text
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  deletion_data text;
  insertion_data text;
  json_data text;
  BEGIN
    insertion_data := (SELECT (array_to_json(array_agg(t)))::text FROM "^dbschema^".__dummy__"^view_name^"_detected_insertions as t where t.txid = $1);
    IF insertion_data IS NOT DISTINCT FROM NULL THEN 
        insertion_data := '[]';
    END IF; 
    deletion_data := (SELECT (array_to_json(array_agg(t)))::text FROM "^dbschema^".__dummy__"^view_name^"_detected_deletions as t where t.txid = $1);
    IF deletion_data IS NOT DISTINCT FROM NULL THEN 
        deletion_data := '[]';
    END IF; 
    IF (insertion_data IS DISTINCT FROM '[]') OR (deletion_data IS DISTINCT FROM '[]') THEN 
        -- calcuate the update data
        json_data := concat('{\"view\": ' , '\""^dbschema^"."^view_name^"\"', ', ' , '\"insertions\": ' , insertion_data , ', ' , '\"deletions\": ' , deletion_data , '}');
        -- clear the update data
        --DELETE FROM "^dbschema^".__dummy__"^view_name^"_detected_deletions;
        --DELETE FROM "^dbschema^".__dummy__"^view_name^"_detected_insertions;
    END IF;
    RETURN json_data;
  END;
$$;

CREATE OR REPLACE FUNCTION "^dbschema^"."^view_name^"_run_shell(text) RETURNS text AS $$
"^sh_script^"
$$ LANGUAGE plsh;
"^
update_detection_trigger_sql
^"

CREATE OR REPLACE FUNCTION "^dbschema^"."^view_name^"_delta_action()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  deletion_data text;
  insertion_data text;
  json_data text;
  result text;
  user_name text;
  xid int;
  delta_ins_size int;
  delta_del_size int;
  array_delta_del "^dbschema^"."^view_name^"[];
  array_delta_ins "^dbschema^"."^view_name^"[];
  "^vardec^"
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = '"^view_name^"_delta_action_flag') THEN
        -- RAISE LOG 'execute procedure "^view_name^"_delta_action';
        CREATE TEMPORARY TABLE "^view_name^"_delta_action_flag ON COMMIT DROP AS (SELECT true as finish);
        IF EXISTS (" ^ view_constraint_sql_of_stt dbschema log inc optimize prog^" )
        THEN 
          RAISE check_violation USING MESSAGE = 'Invalid view update: constraints on the view are violated';
        END IF;
        IF EXISTS (" ^ non_view_constraint_sql_of_stt dbschema log false optimize prog^" )
        THEN 
          RAISE check_violation USING MESSAGE = 'Invalid view update: constraints on the source relations are violated';
        END IF;
        SELECT array_agg(tbl) INTO array_delta_ins FROM "^(get_rterm_predname (get_temp_delta_insertion_rterm view_rt))^" AS tbl;
        SELECT array_agg(tbl) INTO array_delta_del FROM "^(get_rterm_predname (get_temp_delta_deletion_rterm view_rt))^" as tbl;
        select count(*) INTO delta_ins_size FROM "^(get_rterm_predname (get_temp_delta_insertion_rterm view_rt))^";
        select count(*) INTO delta_del_size FROM "^(get_rterm_predname (get_temp_delta_deletion_rterm view_rt))^";
        "^delta_sql_stt^
        (if dejima_update_detect then "
        
        insertion_data := (SELECT (array_to_json(array_delta_ins))::text);
        IF insertion_data IS NOT DISTINCT FROM NULL THEN 
            insertion_data := '[]';
        END IF; 
        deletion_data := (SELECT (array_to_json(array_delta_del))::text);
        IF deletion_data IS NOT DISTINCT FROM NULL THEN 
            deletion_data := '[]';
        END IF; 
        IF (insertion_data IS DISTINCT FROM '[]') OR (deletion_data IS DISTINCT FROM '[]') THEN 
            user_name := (SELECT session_user);
            IF NOT (user_name = '"^dejima_user^"') THEN 
                xid := (SELECT txid_current());
                json_data := concat('{\"xid\": \"', xid, '\" , \"view\": ' , '\""^dbschema^"."^view_name^"\"', ', ' , '\"insertions\": ' , insertion_data , ', ' , '\"deletions\": ' , deletion_data , '}');
                result := "^dbschema^"."^view_name^"_run_shell(json_data);
                IF NOT (result = 'true') THEN
                    CREATE TEMPORARY TABLE IF NOT EXISTS dejima_abort_flag ON COMMIT DROP AS (SELECT true as finish);
                    RAISE LOG 'update on view is rejected by the external tool, result from running the sh script: %', result;
                    -- RAISE check_violation USING MESSAGE = 'update on view is rejected by the external tool, result from running the sh script: ' 
                    -- || result;
                END IF;
            ELSE 
                RAISE LOG 'function of detecting dejima update is called by % , no request sent to dejima proxy', user_name;
                xid := (SELECT txid_current());

                -- update the table that stores the insertions and deletions we calculated
                --DELETE FROM "^dbschema^".__dummy__"^view_name^"_detected_deletions;
                INSERT INTO "^dbschema^".__dummy__"^view_name^"_detected_deletions
                    SELECT xid, * FROM "^(get_rterm_predname (get_temp_delta_deletion_rterm view_rt))^";

                --DELETE FROM "^dbschema^".__dummy__"^view_name^"_detected_insertions;
                INSERT INTO "^dbschema^".__dummy__"^view_name^"_detected_insertions
                    SELECT xid, * FROM "^(get_rterm_predname (get_temp_delta_insertion_rterm view_rt))^";
            END IF;
        END IF;" else "") ^"
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of "^dbschema^"."^view_name^"';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of "^dbschema^"."^view_name^" ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

CREATE OR REPLACE FUNCTION "^dbschema^"."^view_name^"_materialization()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = '"^(get_rterm_predname (get_temp_delta_insertion_rterm view_rt))^"' OR table_name = '"^(get_rterm_predname (get_temp_delta_deletion_rterm view_rt))^"')
    THEN
        -- RAISE LOG 'execute procedure "^view_name^"_materialization';
        CREATE TEMPORARY TABLE "^(get_rterm_predname (get_temp_delta_insertion_rterm view_rt))^" ( LIKE " ^dbschema^"."^(get_rterm_predname (view_rt)) ^" ) WITH OIDS ON COMMIT DROP;
        CREATE CONSTRAINT TRIGGER __tmp_"^view_name^"_trigger_delta_action_ins
        AFTER INSERT OR UPDATE OR DELETE ON 
            "^(get_rterm_predname (get_temp_delta_insertion_rterm view_rt))^" DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE "^dbschema^"."^view_name^"_delta_action();

        CREATE TEMPORARY TABLE "^(get_rterm_predname (get_temp_delta_deletion_rterm view_rt))^" ( LIKE " ^dbschema^"."^(get_rterm_predname (view_rt)) ^" ) WITH OIDS ON COMMIT DROP;
        CREATE CONSTRAINT TRIGGER __tmp_"^view_name^"_trigger_delta_action_del
        AFTER INSERT OR UPDATE OR DELETE ON 
            "^(get_rterm_predname (get_temp_delta_deletion_rterm view_rt))^" DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE "^dbschema^"."^view_name^"_delta_action();
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of "^dbschema^"."^view_name^"';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of "^dbschema^"."^view_name^" ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

DROP TRIGGER IF EXISTS "^view_name^"_trigger_materialization ON "^dbschema^"."^view_name^";
CREATE TRIGGER "^view_name^"_trigger_materialization
    BEFORE INSERT OR UPDATE OR DELETE ON
      "^dbschema^"."^view_name^" FOR EACH STATEMENT EXECUTE PROCEDURE "^dbschema^"."^view_name^"_materialization();

CREATE OR REPLACE FUNCTION "^dbschema^"."^view_name^"_update()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  BEGIN
    -- RAISE LOG 'execute procedure "^view_name^"_update';
    IF TG_OP = 'INSERT' THEN
      -- RAISE LOG 'NEW: %', NEW;
      IF (SELECT count(*) FILTER (WHERE j.value = jsonb 'null') FROM  jsonb_each(to_jsonb(NEW)) j) > 0 THEN 
        RAISE check_violation USING MESSAGE = 'Invalid update on view: view does not accept null value';
      END IF;
      DELETE FROM "^(get_rterm_predname (get_temp_delta_deletion_rterm view_rt))^" WHERE ROW"^cols_tuple_str^(sql_of_operator "==")^"NEW;
      INSERT INTO "^(get_rterm_predname (get_temp_delta_insertion_rterm view_rt))^" SELECT (NEW).*; 
    ELSIF TG_OP = 'UPDATE' THEN
      IF (SELECT count(*) FILTER (WHERE j.value = jsonb 'null') FROM  jsonb_each(to_jsonb(NEW)) j) > 0 THEN 
        RAISE check_violation USING MESSAGE = 'Invalid update on view: view does not accept null value';
      END IF;
      DELETE FROM "^(get_rterm_predname (get_temp_delta_insertion_rterm view_rt))^" WHERE ROW"^cols_tuple_str^(sql_of_operator "==")^"OLD;
      INSERT INTO "^(get_rterm_predname (get_temp_delta_deletion_rterm view_rt))^" SELECT (OLD).*;
      DELETE FROM "^(get_rterm_predname (get_temp_delta_deletion_rterm view_rt))^" WHERE ROW"^cols_tuple_str^(sql_of_operator "==")^"NEW;
      INSERT INTO "^(get_rterm_predname (get_temp_delta_insertion_rterm view_rt))^" SELECT (NEW).*; 
    ELSIF TG_OP = 'DELETE' THEN
      -- RAISE LOG 'OLD: %', OLD;
      DELETE FROM "^(get_rterm_predname (get_temp_delta_insertion_rterm view_rt))^" WHERE ROW"^cols_tuple_str^(sql_of_operator "==")^"OLD;
      INSERT INTO "^(get_rterm_predname (get_temp_delta_deletion_rterm view_rt))^" SELECT (OLD).*;
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of "^dbschema^"."^view_name^"';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of "^dbschema^"."^view_name^" ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

DROP TRIGGER IF EXISTS "^view_name^"_trigger_update ON "^dbschema^"."^view_name^";
CREATE TRIGGER "^view_name^"_trigger_update
    INSTEAD OF INSERT OR UPDATE OR DELETE ON
      "^dbschema^"."^view_name^" FOR EACH ROW EXECUTE PROCEDURE "^dbschema^"."^view_name^"_update();

CREATE OR REPLACE FUNCTION "^dbschema^"."^view_name^"_propagate_updates ()
RETURNS boolean
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  BEGIN
    SET CONSTRAINTS __tmp_"^view_name^"_trigger_delta_action_ins, __tmp_"^view_name^"_trigger_delta_action_del"
    (* ^
    (
    let trigger_names = List.map (fun x  -> 
        let source_rt = get_schema_rterm x in
        let source_name = get_rterm_predname source_rt in 
        ", "^dbschema^"."^source_name^"_detect_update_on_"^view_name) effect_sources in 
        String.concat "" trigger_names) *)
    ^ 
    " IMMEDIATE;
    SET CONSTRAINTS __tmp_"^view_name^"_trigger_delta_action_ins, __tmp_"^view_name^"_trigger_delta_action_del"
    (* ^
    (
    let trigger_names = List.map (fun x  -> 
        let source_rt = get_schema_rterm x in
        let source_name = get_rterm_predname source_rt in 
        ", "^dbschema^"."^source_name^"_detect_update_on_"^view_name) effect_sources in 
        String.concat "" trigger_names) *)
    ^ 
    " DEFERRED;
    DROP TABLE IF EXISTS "^view_name^"_delta_action_flag;
    DROP TABLE IF EXISTS "^(get_rterm_predname (get_temp_delta_deletion_rterm view_rt))^";
    DROP TABLE IF EXISTS "^(get_rterm_predname (get_temp_delta_insertion_rterm view_rt))^";
    RETURN true;
  END;
$$;
"
    in trigger_pgsql

