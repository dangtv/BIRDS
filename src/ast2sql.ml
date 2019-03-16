(*******************************************************)
(**  
AST-to-SQL functions
 *)
(********************************************************)
(* 
@author: Vandang Tran
*)

open Expr ;;
open Utils;;
open Rule_preprocess;;
open Stratification;;
open Derivation;;

(** Given an aggregate function name, checks if it is supported and
  returns it*)
let check_agg_function fn =
    let allowed = ["MAX";"MIN";"SUM";"AVG";"COUNT"] in
    if List.mem fn allowed then fn
    else raise (SemErr (
        "Aggregate function '"^fn^"' is not supported, "^
        "allowed functions are: "^(String.concat ", " allowed)
    ))

(** get sql code for comparison operators  *)
let sql_of_operator op = match op with 
    (* == accept null, return true if both are null. This is because in postgres the operator = return null if one of its operand is null. "==" is useful in the case of negation or delete from. For example, if a tuple (a,null) in both table 1 and table 2 and there is a rule that table1(X,Y), not table2(X,Y) then (a,null) does not sastify *)
    | "==" -> " IS NOT DISTINCT FROM "  
    | "<>" -> " IS DISTINCT FROM "
    | _ -> " "^op^" "

(** given an arithmetric expression, return in sql string , this function is similar to string_of_vterm*)
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
            | Sum(f,g) -> (open_paren prec 0)^ (sql_of 0 f) ^ "+" ^ (sql_of 0 g) ^ (close_paren prec 0)
            | Diff(f,g) -> (open_paren prec 0) ^ (sql_of 0 f) ^  "-" ^ (sql_of 1 g) ^ (close_paren prec 0)
            | Times(f,g) -> (open_paren prec 2) ^ (sql_of 2 f) ^  "*" ^ (sql_of 2 g) ^ (close_paren prec 2)
            | Div (f,g) -> (open_paren prec 2)^ (sql_of 2 f) ^ "/" ^ (sql_of 3 g) ^ (close_paren prec 2)
            | Neg e ->  (open_paren prec 4)^ "-" ^ (sql_of 5 e)^(close_paren prec 4)
            | Concat(f,g) -> (open_paren prec 0)^ (sql_of 0 f) ^ "||" ^ (sql_of 0 g) ^ (close_paren prec 0)
            | BoolAnd (f,g) -> (open_paren prec 2) ^ (sql_of 2 f) ^  "*" ^ (sql_of 2 g) ^ (close_paren prec 2)
            | BoolOr (f,g) -> (open_paren prec 0)^ (sql_of 0 f) ^ "+" ^ (sql_of 0 g) ^ (close_paren prec 0)
            | BoolNot e ->  (open_paren prec 4)^ "-" ^ (sql_of 5 e)^(close_paren prec 4)
        in sql_of 0 expr;;

(** Given a variable, returns the name of a EDB/IDB column that defines it, or if it is equal to a constant, the value of the constant.*)
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

(** Given the head of the rule, the vartab, and te eqtab, returns the code that
  must be in the select clause. All columns are aliased as col0, col1, ...*)
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
    "SELECT DISTINCT "^(alias 0 cols)

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
                    if str_contains pname "__temp__" then pname^" AS "^pname^"_a"^(string_of_int arity)^"_"^(string_of_int n)
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
                                if str_contains pname "__temp__" then "\nFROM "^pname^" AS "^alias
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

(** take a query term and generate unfolded sql for it *)
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

(** generate unfolded SQL statement from the ast, the goal is the query predicate of datalog program, the query is a query over source relations, receives a symtable with the database's edb description.
The result of this function is sql query, whose returned table has colum names of col0, col1,....
The boolean variable debug indicates whether debugging information should be printed
*)
let unfold_query_sql_stt (dbschema:string) (debug:bool) (edb:symtable) prog =
    let query_rt = get_query_rterm (get_query prog) in
    (*Extract and pre-process the IDB from the program*)
    let idb = extract_idb prog in
    preprocess_rules idb; 
    (* print_symtable idb; *)
    (*Build the colnamtab for referencing the table's columns*)
    let cnt = build_colnamtab edb idb in
    (*Return the desired SQL*)
    let sql = non_rec_unfold_sql_of_query dbschema idb cnt query_rt  in
    sql
;;

let unfold_view_sql (dbschema:string) (debug:bool) prog = match prog with 
    | Prog sttlst ->
    let edb = extract_edb prog in
    let view_rt = get_schema_rterm (get_view prog) in 
    if (get_arity view_rt = 0) then raise (SemErr
            ("The view "^(string_of_rterm view_rt)^
            " has arity 0, which is not allowed to create a view"))
    else
    "CREATE OR REPLACE VIEW "^ dbschema ^"."^(get_rterm_predname view_rt) ^ " AS \n" ^unfold_query_sql_stt dbschema debug edb (Prog((Query view_rt)::sttlst)) ^";"
;;

let unfold_program_query (dbschema:string) (debug:bool) prog =
    if (debug) then print_endline ("==> generating SQL query of datalog program of query "^ string_of_stt (get_query prog)) else ();
    let edb = extract_edb prog in
    unfold_query_sql_stt dbschema debug edb prog
;;

(** take a view update datalog program (containing both get and put directions) and generate SQL query of contraints not involving view *)
let view_constraint_sql_of_stt (dbschema:string) (debug:bool) (inc:bool) (optimize:bool) prog =
    let clean_prog = keep_only_constraint_of_view debug prog in
    if inc then     
        let inc_prog = incrementalize_by_view debug clean_prog in
        let view_sch = get_view inc_prog in
        let view_rt = get_schema_rterm view_sch in
        let prog2 = add_stts [
        (Source(get_rterm_predname (get_materialzied_rterm view_rt), get_schema_col_typs view_sch ));
        (Source(get_rterm_predname (get_temp_delta_deletion_rterm view_rt), get_schema_col_typs view_sch ));
        (Source(get_rterm_predname (get_temp_delta_insertion_rterm view_rt), get_schema_col_typs view_sch ));
        (Rule(get_inc_original view_rt,[Rel (get_materialzied_rterm view_rt)]));
        (Rule(get_inc_ins view_rt,[Rel (get_temp_delta_insertion_rterm view_rt)]));
        (Rule(get_inc_del view_rt,[Rel (get_temp_delta_deletion_rterm view_rt)]))
        ] inc_prog in
        (* let edb = extract_edb prog2 in *)
        let idb = extract_idb prog2 in
        if Hashtbl.mem idb (symtkey_of_rterm get_empty_pred) then
            (
            (* keep_only_constraint_of_view debug view_rt edb idb ; *)
            preprocess_rules idb;
            (* let cnt = build_colnamtab edb idb in *)
            if Hashtbl.mem idb (symtkey_of_rterm get_empty_pred) then
                let remain_rules = rules_of_symt idb in
                let prog3 = Prog((get_schema_stts prog2)@remain_rules) in
                (* non_rec_unfold_sql_of_query dbschema idb cnt get_empty_pred *)
                let prog4 = if (optimize) then (Ast2fol.optimize_query_datalog debug (insert_stt (Query get_empty_pred) prog3)) else (insert_stt (Query get_empty_pred) prog3) in
                (* the optimization may drop the empty_predicate of prog4 when the empty_predicate is trival (always empty) *)
                if (has_query prog4) then
                    (unfold_program_query dbschema debug prog4)
                else "SELECT WHERE false"
            else "SELECT WHERE false")
        else "SELECT WHERE false"
    else
        let view_sch = get_view clean_prog in
        let view_rt = get_schema_rterm view_sch in
        let prog2 = add_stts [
        (Source(get_rterm_predname (get_materialzied_rterm view_rt), get_schema_col_typs view_sch ));
        (Source(get_rterm_predname (get_temp_delta_deletion_rterm view_rt), get_schema_col_typs view_sch ));
        (Source(get_rterm_predname (get_temp_delta_insertion_rterm view_rt), get_schema_col_typs view_sch ));
        (Rule(view_rt,[Rel (get_materialzied_rterm view_rt); Not (get_temp_delta_deletion_rterm view_rt)]));
        (Rule(view_rt,[Rel (get_temp_delta_insertion_rterm view_rt)]))
        ] (delete_rule_of_predname (get_rterm_predname view_rt) clean_prog) in
        (* let edb = extract_edb prog2 in *)
        let idb = extract_idb prog2 in
        if Hashtbl.mem idb (symtkey_of_rterm get_empty_pred) then
            (
            (* keep_only_constraint_of_view debug view_rt edb idb ; *)
            preprocess_rules idb;
            (* let cnt = build_colnamtab edb idb in *)
            if Hashtbl.mem idb (symtkey_of_rterm get_empty_pred) then
                let remain_rules = Hashtbl.fold (fun k rules lst -> rules@lst) idb [] in
                let prog3 = Prog((get_schema_stts prog2)@remain_rules) in
                (* non_rec_unfold_sql_of_query dbschema idb cnt get_empty_pred *)
                let prog4 = if (optimize) then (Ast2fol.optimize_query_datalog debug (insert_stt (Query get_empty_pred) prog3)) else (insert_stt (Query get_empty_pred) prog3) in
                (* the optimization may drop the empty_predicate of prog4 when the empty_predicate is trival (always empty) *)
                if (has_query prog4) then
                    (unfold_program_query dbschema debug prog4)
                else "SELECT WHERE false"
            else "SELECT WHERE false")
        else "SELECT WHERE false"
;;

(** get update sql for a delta term, the update sql contains two string: sql query for the delta, and sql update for the delta *)
let non_rec_unfold_sql_of_update (dbschema:string) (debug:bool) (optimize:bool) prog (delta:rterm)  =
    (* 
    *)
    let prog2 = 
        if (optimize) then (Ast2fol.optimize_query_datalog debug (insert_stt (Query delta) prog)) else 
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
            "temprec"^ (get_rterm_predname delta) ^" " ^dbschema^"."^ pname ^"%ROWTYPE;", 
            (* calculate the delta relation by creating a temporary table *)
            "CREATE TEMPORARY TABLE "^ (get_rterm_predname delta) ^" WITH OIDS ON COMMIT DROP AS " ^ 
            "SELECT "^"(ROW("^(String.concat "," (Hashtbl.find cnt (symtkey_of_rterm delta))) ^") :: "^dbschema^"."^ pname ^").* 
            FROM ("^
            (non_rec_unfold_sql_of_symtkey dbschema local_idb cnt (symtkey_of_rterm (rule_head qrule))) ^") AS "^(get_rterm_predname delta)^"_extra_alias;", 
            (* insert tuples using batch insertion *)
            "INSERT INTO " ^dbschema^"."^ pname ^" SELECT * FROM  "^ (get_rterm_predname delta) ^ "; " ^
            (* insert  each tuple by using a LOOP*)
            (* "FOR temprec"^ (get_rterm_predname delta) ^" IN ( SELECT * FROM " ^ (get_rterm_predname delta) ^ ") LOOP 
            " ^
            "INSERT INTO " ^dbschema^"."^ pname ^" SELECT (temprec"^ (get_rterm_predname delta) ^").*; 
            END LOOP; " ^ *)

            "\nDROP TABLE "^ (get_rterm_predname delta)^";")
            else raise (SemErr "delta predicate is not of any base predicate")
        
        | Deltadelete (pname, varlst) -> if Hashtbl.mem edb (pname, List.length varlst) 
            then 
            (* get all the columns of base predicate *)
            let cols = Hashtbl.find cnt (pname, List.length varlst) in
            (* convert these cols to string of tuple of these cols *)
            let cols_tuple_str = "("^(String.concat "," cols) ^")" in
            (
            (* variable with rowtype of the source relation *)
            "temprec"^ (get_rterm_predname delta) ^" " ^dbschema^"."^ pname ^"%ROWTYPE;", 
            (* calculate the delta relation by creating a temporary table *)
            "CREATE TEMPORARY TABLE "^ (get_rterm_predname delta) ^" WITH OIDS ON COMMIT DROP AS " ^ 
            "SELECT "^"(ROW("^(String.concat "," (Hashtbl.find cnt (symtkey_of_rterm delta))) ^") :: "^dbschema^"."^ pname ^").* 
            FROM ("^
            (non_rec_unfold_sql_of_symtkey dbschema local_idb cnt (symtkey_of_rterm (rule_head qrule)))^") AS "^(get_rterm_predname delta)^"_extra_alias;", 
            (* delete each tuple *)
            "FOR temprec"^ (get_rterm_predname delta) ^" IN ( SELECT * FROM " ^ (get_rterm_predname delta) ^ ") LOOP 
            " ^
            "DELETE FROM " ^dbschema^"."^ pname ^" WHERE ROW" ^cols_tuple_str^ (sql_of_operator "==") ^
            " temprec"^ (get_rterm_predname delta) ^";" ^ "
            END LOOP;\nDROP TABLE " ^ (get_rterm_predname delta)^";")
            
            else raise (SemErr "delta predicate is not of any base predicate")
        | _ -> raise (SemErr "the non_rec_unfold_sql_of_update is called without and delta predicate")

let unfold_delta_sql_stt (dbschema:string) (debug:bool) (inc:bool) (optimize:bool) prog =
    if inc then 
        let inc_prog = incrementalize_by_view debug prog in
        let view_sch = get_view inc_prog in
        let view_rt = get_schema_rterm view_sch in
        let prog2 = add_stts [
        (Source(get_rterm_predname (get_materialzied_rterm view_rt), get_schema_col_typs view_sch ));
        (Source(get_rterm_predname (get_temp_delta_deletion_rterm view_rt), get_schema_col_typs view_sch ));
        (Source(get_rterm_predname (get_temp_delta_insertion_rterm view_rt), get_schema_col_typs view_sch ));
        (Rule(get_inc_original view_rt,[Rel (get_materialzied_rterm view_rt)]));
        (Rule(get_inc_ins view_rt,[Rel (get_temp_delta_insertion_rterm view_rt)]));
        (Rule(get_inc_del view_rt,[Rel (get_temp_delta_deletion_rterm view_rt)]))
        ] inc_prog in
        
        let delta_rt_lst = get_delta_rterms inc_prog in
        (*Return the desired SQL*)
        let update_sql_lst = List.map (non_rec_unfold_sql_of_update dbschema debug optimize prog2 ) delta_rt_lst in 
        let concat_update_sql (v, d, u) (vardec,delquery, updateaction) = (v::vardec,d::delquery, u::updateaction) in 
        let (vars, deltas, actions) = List.fold_right concat_update_sql update_sql_lst ([],[],[]) in 
        ((String.concat "\n" vars)^"",
        ((String.concat "\n\n" deltas)^"") ^ " \n\n" ^ ((String.concat "\n\n" actions)^""))
    else
        let view_sch = get_view prog in
        let view_rt = get_schema_rterm view_sch in
        let prog2 = add_stts [
        (Source(get_rterm_predname (get_materialzied_rterm view_rt), get_schema_col_typs view_sch ));
        (Source(get_rterm_predname (get_temp_delta_deletion_rterm view_rt), get_schema_col_typs view_sch ));
        (Source(get_rterm_predname (get_temp_delta_insertion_rterm view_rt), get_schema_col_typs view_sch ));
        (Rule(view_rt,[Rel (get_materialzied_rterm view_rt); Not (get_temp_delta_deletion_rterm view_rt)]));
        (Rule(view_rt,[Rel (get_temp_delta_insertion_rterm view_rt)]))
        ] (delete_rule_of_predname (get_rterm_predname view_rt) prog) in
        (* need to insert a temporary predicate of query (view) to edb because for update sql we need to assume a tempoarary view is created. Suppose that query rterm contains only variables *)


        let delta_rt_lst = get_delta_rterms prog in
        (*Return the desired SQL*)
        let update_sql_lst = List.map (non_rec_unfold_sql_of_update dbschema debug optimize prog2 ) delta_rt_lst in 
        let concat_update_sql (v, d, u) (vardec,delquery, updateaction) = (v::vardec,d::delquery, u::updateaction) in 
        let (vars, deltas, actions) = List.fold_right concat_update_sql update_sql_lst ([],[],[]) in 
        ((String.concat "\n" vars)^"",
        ((String.concat "\n\n" deltas)^"") ^ " \n\n" ^ ((String.concat "\n\n" actions)^""))
;;

(** SQL for triggers of detecting update on the source relations, which call the action of executing shell script on the view*)
let source_update_detection_trigger_stt (dbschema:string) (debug:bool) prog =
    let view_rt = get_schema_rterm (get_view prog) in
    let view_name = get_rterm_predname view_rt in
    let all_source = (List.map (fun x -> get_schema_rterm x) (get_source_stts prog)) in 
    let sql_lst = List.map (fun x  -> 
        let  source_name = get_rterm_predname x in 
        "DROP TRIGGER IF EXISTS "^source_name^"_detect_update_"^view_name^" ON "^dbschema^"."^source_name^";
        CREATE TRIGGER "^source_name^"_detect_update_"^view_name^"
            AFTER INSERT OR UPDATE OR DELETE ON
            "^dbschema^"."^source_name^" FOR EACH STATEMENT EXECUTE PROCEDURE "^dbschema^"."^view_name^"_detect_update();")
        all_source in 
    String.concat "\n \n" sql_lst
;;

(** generate trigger for delta predicates on the view *)
let unfold_delta_trigger_stt (dbschema:string) (debug:bool) (sh_script:string) (dejima_user:string) (inc:bool) (optimize:bool) prog =
    let view_rt = get_schema_rterm (get_view prog) in
    let view_name = get_rterm_predname view_rt in
    (* let temporary_view_name = get_rterm_predname (get_temp_rterm view_rt) in *)
    (* get all the columns of base predicate *)
    (* convert these cols to string of tuple of these cols *)
    let cols_tuple_str = "("^ (String.concat "," (List.map  string_of_var (get_rterm_varlist (get_temp_rterm view_rt)) )) ^")" in
    let (vardec, delta_sql_stt) = unfold_delta_sql_stt dbschema debug inc optimize prog in
    let source_trigger_sql = source_update_detection_trigger_stt dbschema debug prog in
    let trigger_pgsql = 
"
DROP MATERIALIZED VIEW IF EXISTS "^dbschema^"."^(get_rterm_predname (get_materialzied_rterm view_rt))^";

CREATE  MATERIALIZED VIEW "^dbschema^"."^(get_rterm_predname (get_materialzied_rterm view_rt))^" AS 
SELECT * FROM "^dbschema^"."^view_name^";

CREATE EXTENSION IF NOT EXISTS plsh;

CREATE OR REPLACE FUNCTION "^dbschema^"."^view_name^"_run_shell(text) RETURNS text AS $$
"^sh_script^"
$$ LANGUAGE plsh;
"^(
    let func_body = 
"LANGUAGE plpgsql
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
  BEGIN
  IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = '"^view_name^"_delta_action_flag') THEN
    insertion_data := (SELECT (array_to_json(array_agg(t)))::text FROM (SELECT * FROM "^dbschema^"."^view_name^" EXCEPT SELECT * FROM "^dbschema^"."^(get_rterm_predname (get_materialzied_rterm view_rt))^") as t);
    IF insertion_data IS NOT DISTINCT FROM NULL THEN 
        insertion_data := '[]';
    END IF; 
    deletion_data := (SELECT (array_to_json(array_agg(t)))::text FROM (SELECT * FROM "^dbschema^"."^(get_rterm_predname (get_materialzied_rterm view_rt))^" EXCEPT SELECT * FROM "^dbschema^"."^view_name^") as t);
    IF deletion_data IS NOT DISTINCT FROM NULL THEN 
        deletion_data := '[]';
    END IF; 
    IF (insertion_data IS DISTINCT FROM '[]') OR (insertion_data IS DISTINCT FROM '[]') THEN 
        user_name := (SELECT session_user);
        IF NOT (user_name = '"^dejima_user^"') THEN 
            json_data := concat('{\"view\": ' , '\""^dbschema^"."^view_name^"\"', ', ' , '\"insertions\": ' , insertion_data , ', ' , '\"deletions\": ' , deletion_data , '}');
            result := "^dbschema^"."^view_name^"_run_shell(json_data);
            IF result = 'true' THEN 
                REFRESH MATERIALIZED VIEW "^dbschema^"."^(get_rterm_predname (get_materialzied_rterm view_rt))^";
                FOR func IN (select distinct trigger_schema||'.non_trigger_'||substring(action_statement, 19) as function 
                from information_schema.triggers where trigger_schema = '"^dbschema^"' and event_object_table='"^view_name^"'
                and action_timing='AFTER' and (event_manipulation='INSERT' or event_manipulation='DELETE' or event_manipulation='UPDATE')
                and action_statement like 'EXECUTE PROCEDURE %') 
                LOOP
                    EXECUTE 'SELECT ' || func into tv;
                END LOOP;
            ELSE
                -- RAISE LOG 'result from running the sh script: %', result;
                RAISE check_violation USING MESSAGE = 'update on view is rejected by the external tool, result from running the sh script: ' 
                || result;
            END IF;
        ELSE 
            RAISE LOG 'function of detecting dejima update is called by % , no request sent to dejima proxy', user_name;
        END IF;
    END IF;
  END IF;
  RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of "^dbschema^"."^view_name^"';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the function "^dbschema^"."^view_name^"_detect_update() ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;" in
"CREATE OR REPLACE FUNCTION "^dbschema^"."^view_name^"_detect_update()
RETURNS trigger
"^
func_body^"

CREATE OR REPLACE FUNCTION "^dbschema^".non_trigger_"^view_name^"_detect_update()
RETURNS text 
"^
func_body
)^"

"^source_trigger_sql^"

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
  "^vardec^"
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = '"^view_name^"_delta_action_flag') THEN
        -- RAISE LOG 'execute procedure "^view_name^"_delta_action';
        CREATE TEMPORARY TABLE "^view_name^"_delta_action_flag ON COMMIT DROP AS (SELECT true as finish);
        IF EXISTS (" ^ view_constraint_sql_of_stt dbschema debug inc optimize prog^" )
        THEN 
          RAISE check_violation USING MESSAGE = 'Invalid update on view';
        END IF;
        "^delta_sql_stt^"

        insertion_data := (SELECT (array_to_json(array_agg(t)))::text FROM (SELECT * FROM "^(get_rterm_predname (get_temp_delta_insertion_rterm view_rt))^" EXCEPT SELECT * FROM "^dbschema^"."^(get_rterm_predname (get_materialzied_rterm view_rt))^") as t);
        IF insertion_data IS NOT DISTINCT FROM NULL THEN 
            insertion_data := '[]';
        END IF; 
        deletion_data := (SELECT (array_to_json(array_agg(t)))::text FROM (SELECT * FROM "^(get_rterm_predname (get_temp_delta_deletion_rterm view_rt))^" INTERSECT SELECT * FROM "^dbschema^"."^(get_rterm_predname (get_materialzied_rterm view_rt))^") as t);
        IF deletion_data IS NOT DISTINCT FROM NULL THEN 
            deletion_data := '[]';
        END IF; 
        IF (insertion_data IS DISTINCT FROM '[]') OR (insertion_data IS DISTINCT FROM '[]') THEN 
            user_name := (SELECT session_user);
            IF NOT (user_name = '"^dejima_user^"') THEN 
                json_data := concat('{\"view\": ' , '\""^dbschema^"."^view_name^"\"', ', ' , '\"insertions\": ' , insertion_data , ', ' , '\"deletions\": ' , deletion_data , '}');
                result := "^dbschema^"."^view_name^"_run_shell(json_data);
                IF result = 'true' THEN 
                    REFRESH MATERIALIZED VIEW "^dbschema^"."^(get_rterm_predname (get_materialzied_rterm view_rt))^";
                ELSE
                    -- RAISE LOG 'result from running the sh script: %', result;
                    RAISE check_violation USING MESSAGE = 'update on view is rejected by the external tool, result from running the sh script: ' 
                    || result;
                END IF;
            ELSE 
                RAISE LOG 'function of detecting dejima update is called by % , no request sent to dejima proxy', user_name;
            END IF;
        END IF;
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
        REFRESH MATERIALIZED VIEW "^dbschema^"."^(get_rterm_predname (get_materialzied_rterm view_rt))^";
        CREATE TEMPORARY TABLE "^(get_rterm_predname (get_temp_delta_insertion_rterm view_rt))^" ( LIKE " ^dbschema^"."^(get_rterm_predname (get_materialzied_rterm view_rt)) ^" INCLUDING ALL ) WITH OIDS ON COMMIT DROP;
        CREATE CONSTRAINT TRIGGER __temp__"^view_name^"_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            "^(get_rterm_predname (get_temp_delta_insertion_rterm view_rt))^" DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE "^dbschema^"."^view_name^"_delta_action();

        CREATE TEMPORARY TABLE "^(get_rterm_predname (get_temp_delta_deletion_rterm view_rt))^" ( LIKE " ^dbschema^"."^(get_rterm_predname (get_materialzied_rterm view_rt)) ^" INCLUDING ALL ) WITH OIDS ON COMMIT DROP;
        CREATE CONSTRAINT TRIGGER __temp__"^view_name^"_trigger_delta_action
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
      DELETE FROM "^(get_rterm_predname (get_temp_delta_deletion_rterm view_rt))^" WHERE ROW"^cols_tuple_str^(sql_of_operator "==")^"NEW;
      INSERT INTO "^(get_rterm_predname (get_temp_delta_insertion_rterm view_rt))^" SELECT (NEW).*; 
    ELSIF TG_OP = 'UPDATE' THEN
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
"
    in trigger_pgsql
;;
