(* 
@author: Vandang Tran
*)

open Expr ;;
open Utils;;

let split_terms terms =
    let rec split t (pos,neg,eq,inq) = match t with
        | Rel rt -> (rt::pos,neg,eq,inq)
        | Not rt -> (pos,rt::neg,eq,inq)
        | Equal _ -> (pos,neg,t::eq,inq) 
        | Ineq _ -> (pos,neg,eq,t::inq) in
    List.fold_right split terms ([],[],[],[])

let vname_to_col (vt:vartab) (eqt:eqtab) key vname =
    if Hashtbl.mem vt vname
        then List.hd (Hashtbl.find vt vname)
    else if Hashtbl.mem eqt vname
        then string_of_const (eqt_extract eqt vname)
    (*Else, the query is unsafe or inclomplete*)
    else raise (Compile_error (
            "Predicate "^(string_of_symtkey key)^
            " is unsafe, variable "^vname^" is not in a positive "^
            "goal or strict equality relation."
        )
    )

let check_agg_function fn =
    let allowed = ["MAX";"MIN";"SUM";"AVG";"COUNT"] in
    if List.mem fn allowed then fn
    else raise (Compile_error (
        "Aggregate function '"^fn^"' is not supported, "^
        "allowed functions are: "^(String.concat ", " allowed)
    ))

let sql_of_operator op = match op with 
    | "==" -> " IS NOT DISTINCT FROM "  
    | "<>" -> " IS DISTINCT FROM "
    | _ -> " "^op^" "

let get_select_clause (vt:vartab) (eqt:eqtab) rterm =
    let vlst = get_rterm_varlist rterm in 
    let key = symtkey_of_rterm rterm in
    if vlst = [] then
        raise (Compile_error
            ("Predicate "^(get_rterm_predname rterm)^
            " has arity 0, which is not allowed"))
    else

    let var_value v = match v with
        NamedVar _ | NumberedVar _ ->
            vname_to_col vt eqt key (string_of_var v)
        | AggVar (fn,vn) ->
            (check_agg_function fn)^"("^(vname_to_col vt eqt key vn)^")"
        | _ -> invalid_arg ("not-expected vartype in head of predicate"^
            (string_of_symtkey key))
    in
    let cols = List.map var_value vlst in
    (*Create aliases*)
    let rec alias ind = function
        | [] -> ""
        | [col] -> col^" AS col"^(string_of_int ind)
        | col::col2::tl ->
            (col^" AS col"^(string_of_int ind))^", "^(alias (ind+1) (col2::tl))
    in
    "SELECT "^(alias 0 cols)

let rule_of_query query (idb:symtable) =
    let (q2,eqs) = extract_rterm_constants query in
    let dummy = Pred ("__dummy__", get_rterm_varlist q2) in
    Rule (dummy, (Rel q2)::eqs)

let get_aggregation_sql (vt:vartab) (cnt:colnamtab) head agg_eqs agg_ineqs =
    let vars = get_rterm_varlist head in
    let key = symtkey_of_rterm head in
    let eq_t = List.map extract_eq_tuple agg_eqs in
    let aug_eq_t = List.map (fun (x,y) -> ("=",x,y)) eq_t in
    let ieq_t = List.map extract_ineq_tuple agg_ineqs in
    let comparisons = aug_eq_t@ieq_t in
    let is_agg = List.exists is_aggvar vars in
    if not is_agg then
        if comparisons = [] then ""
        else raise (Compile_error (
            "Predicate "^(string_of_symtkey key)^
            " contains comparisons of aggregates but defines no "^
            "aggregations in its head"))
    else
    let cols = Hashtbl.find cnt key in
    let group_var acc col = function
        | NamedVar _ -> col::acc
        | _ -> acc in
    let grp_cols = List.fold_left2 group_var [] cols vars in
    let group_by_sql =
        if grp_cols = [] then ""
        else ("GROUP BY "^(String.concat ", " grp_cols)) in
    let av_aggs = Hashtbl.create 100 in
    let fake_eqt:eqtab = Hashtbl.create 100 in
    let insert_agg = function
        | AggVar (fn,vn) ->
            let col = vname_to_col vt fake_eqt key vn in
            Hashtbl.add av_aggs (fn,vn) (fn^"("^col^")")
        | _ -> () in
    List.iter insert_agg vars;
    let agg_var_col agv =
        let tuple = extract_aggvar_tuple agv in
        if Hashtbl.mem av_aggs tuple then Hashtbl.find av_aggs tuple
        else raise (Compile_error (
            "Predicate "^(string_of_symtkey key)^" contains comparisons of "^
            "aggregates that are not defined in its head"
        )) in
    let comp_const (op,var,const) =
        (agg_var_col var)^" "^(sql_of_operator op)^" "^(string_of_const const) in 
    let comp_sql = List.map comp_const comparisons in
    let having_sql = if comp_sql = [] then "" else
        "HAVING "^(String.concat " AND " comp_sql) in
    group_by_sql^" "^having_sql

let rec non_rec_unfold_sql_of_symtkey (dbschema:string) (idb:symtable) (cnt:colnamtab) (goal:symtkey)  =
    let rule_lst = Hashtbl.find idb  goal in
    let unfold_sql_of_rule_lst (idb:symtable) (cnt:colnamtab) rules =
        let unfold_sql_of_rule (idb:symtable) (cnt:colnamtab) rule =
            let head = rule_head rule in
            let body = rule_body rule in
            let (p_rt,n_rt,all_eqs,all_ineqs) = split_terms body in
            let (agg_eqs,eqs) = List.partition is_agg_equality all_eqs in
            let (agg_ineqs,ineqs) = List.partition is_agg_inequality all_ineqs in
            (*Build vartab, and eqtab for select and where clauses, build vartabl by p_rt*)
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
                "FROM "^(String.concat ", " aliases) in
            let from_sql = unfold_get_from_clause idb p_rt in
            
            let unfold_get_where_clause (idb:symtable) (vt:vartab) (cnt:colnamtab) (eqt:eqtab) ineq neg_rt = 
                let var_const _ cols acc = match cols with
                    | [] -> acc
                    | hd::tl ->
                        let eq_rels el = hd^(sql_of_operator "=")^el in
                        (List.map eq_rels tl)::acc
                in
                let fvt = List.flatten (Hashtbl.fold var_const vt []) in
               
                let eq_const vname value acc =
                    if Hashtbl.mem vt vname then
                        let cname = List.hd (Hashtbl.find vt vname) in
                        (cname^(sql_of_operator "=")^(string_of_const value))::acc
                    else acc
                    in
                let feqt = Hashtbl.fold eq_const eqt [] in
                
                let ineq_tuples = List.map extract_ineq_tuple ineq in
                let ineq_const (op,var,value) acc =
                    let vname = string_of_var var in
                    let cname = List.hd (Hashtbl.find vt vname) in
                    (cname^" "^(sql_of_operator op)^" "^(string_of_const value))::acc in
                let fineq = List.fold_right ineq_const ineq_tuples [] in
                (*Transform the negated rterms into SQL*)
                let unfold_sql_of_negated_rterms (idb:symtable) (vt:vartab) (cnt:colnamtab) (eqt:eqtab) neg_rt =
                    let gen_neg_sql rt =
                        (*get basic info of the rterm*)
                        let key = symtkey_of_rterm rt in
                        let pname = get_rterm_predname rt in
                        let arity = get_arity rt in 
                        let alias = pname^"_a"^(string_of_int arity) in
                        let vlst = get_rterm_varlist rt in
                        let cnames = Hashtbl.find cnt key in
                        (*Get the from sql of the rterm*)
                        let from_sql =
                            if Hashtbl.mem idb key then
                                "FROM "^ "("^non_rec_unfold_sql_of_symtkey dbschema idb cnt (pname,arity) ^")"^" AS " ^ alias
                            else
                                "FROM "^dbschema^"."^pname^" AS "^alias
                        in
                        (*Get the where sql of the rterm*)
                        let build_const acc col var =
                            let eq_to = alias^"."^col^(sql_of_operator "==") in
                            match var with
                            | NamedVar vn -> 
                                if Hashtbl.mem vt vn then
                                    (eq_to^(List.hd (Hashtbl.find vt vn)))::acc
                                else if Hashtbl.mem eqt vn then
                                    (eq_to^(string_of_const (Hashtbl.find eqt vn)))::acc
                                else raise (Compile_error (
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
                            else "WHERE "^(String.concat " AND " const_lst)
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
                    | _ -> "WHERE "^(String.concat " AND " constraints) in
            let where_sql = unfold_get_where_clause idb vt cnt eqtb ineqs n_rt in
            let agg_sql = get_aggregation_sql vt cnt head agg_eqs agg_ineqs in
            String.concat " " [select_sql;from_sql;where_sql;agg_sql] in
        let sql_list = List.map (unfold_sql_of_rule idb cnt) rules in
        String.concat " UNION ALL " sql_list in
    let sql = unfold_sql_of_rule_lst idb cnt rule_lst in
    sql

let non_rec_unfold_sql_of_query (idb:symtable) (cnt:colnamtab) (query:rterm) (dbschema:string)=
    let qrule = rule_of_query query idb in
        symt_insert idb qrule;
        let cols_by_var = List.map string_of_var (get_rterm_varlist (rule_head qrule)) in
        let qrule_alias = get_rule_predname qrule in
        let cols = Hashtbl.find cnt (symtkey_of_rterm query) in
        let sel_lst = List.map (fun (a,b) -> qrule_alias^"."^a^" AS "^b)
                            (List.combine cols cols_by_var) in 
        "SELECT "^(String.concat "," sel_lst) ^ " FROM (" ^
        non_rec_unfold_sql_of_symtkey dbschema idb cnt (symtkey_of_rterm (rule_head qrule)) ^") AS "^qrule_alias

let unfold_query_sql_stt (dbschema:string) (debug:bool) (edb:symtable) prog =
    let query_rt = get_query_rterm (get_query prog) in
    (*Extract and pre-process the IDB from the program*)
    let idb = extract_idb prog in
    preprocess_rules idb;
    (* print_symtable idb; *)
    let cnt = build_colnamtab edb idb in
    (*Return the desired SQL*)
    let sql = non_rec_unfold_sql_of_query idb cnt query_rt dbschema in
    sql^";"
;;

let unfold_view_sql (dbschema:string) (debug:bool) (edb:symtable) prog =
    "CREATE OR REPLACE VIEW "^ dbschema ^"."^(get_rterm_predname (get_query_rterm (get_query prog))) ^ " AS " ^unfold_query_sql_stt dbschema debug edb prog
;;

let non_rec_unfold_sql_of_update (dbschema:string) (idb:symtable) (edb:symtable) (delta:rterm)  =
    let cnt = build_colnamtab edb idb in
    let qrule = rule_of_query delta idb in
        let local_idb = Hashtbl.copy idb in 
        symt_insert local_idb qrule;
        match delta with
        Deltainsert (pname, varlst) -> if Hashtbl.mem edb (pname, List.length varlst) 
            then  ( "CREATE TEMPORARY TABLE "^ (get_rterm_predname delta) ^" WITH OIDS ON COMMIT DROP AS " ^ non_rec_unfold_sql_of_symtkey dbschema local_idb cnt (symtkey_of_rterm (rule_head qrule)), "INSERT INTO " ^dbschema^"."^ pname ^" SELECT * FROM " ^ (get_rterm_predname delta))
            else raise (Compile_error "delta predicate is not of any base predicate")
                
        | Deltadelete (pname, varlst) -> if Hashtbl.mem edb (pname, List.length varlst) then 
        (* get all the columns of base predicate *)
        let cols = Hashtbl.find cnt (pname, List.length varlst) in
        (* convert these cols to string of tuple of these cols *)
        let cols_tuple_str = "("^(String.concat "," cols) ^")" in
        ("CREATE TEMPORARY TABLE "^ (get_rterm_predname delta) ^" WITH OIDS ON COMMIT DROP AS " ^ (non_rec_unfold_sql_of_symtkey dbschema local_idb cnt (symtkey_of_rterm (rule_head qrule))), 
        " FOR temprec IN ( SELECT * FROM " ^ (get_rterm_predname delta) ^ ") LOOP 
        " ^
        "DELETE FROM " ^dbschema^"."^ pname ^" WHERE " ^cols_tuple_str^ (sql_of_operator "==") ^
        " ("^(String.concat "," (List.map (fun c -> "temprec."^c) (gen_cols 0 (List.length cols)) )) ^");" ^ "
        END LOOP")
        
        else raise (Compile_error "delta predicate is not of any base predicate")
        | _ -> raise (Compile_error "the non_rec_unfold_sql_of_update is called without and delta predicate")


let unfold_delta_sql_stt (dbschema:string) (debug:bool) (edb:symtable) prog =
    let query_rt = get_query_rterm (get_query prog) in
    let local_edb = Hashtbl.copy edb in
    symt_insert local_edb (Rule(get_temp_rterm query_rt,[]));
    (*Extract and pre-process the IDB from the program*)
    let idb = extract_idb prog in
    symt_insert idb (Rule(query_rt,[Rel (get_temp_rterm query_rt)]));
    preprocess_rules idb;
    let delta_rt_lst = get_delta_rterms prog in
    (*Return the desired SQL*)
    let update_sql_lst = List.map (non_rec_unfold_sql_of_update dbschema idb local_edb ) delta_rt_lst in 
    let concat_update_sql (d, u) (delquery, updateaction) = (d::delquery, u::updateaction) in 
    let (deltas, actions) = List.fold_right concat_update_sql update_sql_lst ([],[]) in 
    ((String.concat ";\n" deltas)^";") ^ " \n" ^ ((String.concat ";\n" actions)^";")
;;

let unfold_delta_trigger_stt (dbschema:string) (debug:bool) (edb:symtable) prog =
    let query_rt = get_query_rterm (get_query prog) in
    let view_name = get_rterm_predname query_rt in
    let temporary_view_name = get_rterm_predname (get_temp_rterm query_rt) in
    let cols_tuple_str = "("^ (String.concat "," (List.map  string_of_var (get_rterm_varlist (get_temp_rterm query_rt)) )) ^")" in
    let delta_sql_stt = unfold_delta_sql_stt dbschema debug edb prog in
    let trigger_pgsql = 
"
CREATE OR REPLACE FUNCTION "^dbschema^"."^view_name^"_procedure()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  temprec record;
  BEGIN
    CREATE TEMPORARY TABLE "^temporary_view_name^" WITH OIDS ON COMMIT DROP AS SELECT * FROM "^dbschema^"."^view_name^";
    IF TG_OP = 'INSERT' THEN
      INSERT INTO "^temporary_view_name^" SELECT (NEW).*; 
    ELSIF TG_OP = 'UPDATE' THEN
      DELETE FROM "^temporary_view_name^" WHERE "^cols_tuple_str^" = OLD;
      INSERT INTO "^temporary_view_name^" SELECT (NEW).*; 
    ELSIF TG_OP = 'DELETE' THEN
      DELETE FROM "^temporary_view_name^" WHERE "^cols_tuple_str^" = OLD;
    END IF;
    "^delta_sql_stt^"
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or update or delete from "^dbschema^"."^view_name^"';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of "^dbschema^"."^view_name^" ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
  
$$;
DROP TRIGGER IF EXISTS "^view_name^"_trigger ON "^dbschema^"."^view_name^";
CREATE TRIGGER "^view_name^"_trigger
    INSTEAD OF INSERT OR UPDATE OR DELETE ON
      "^dbschema^"."^view_name^" FOR EACH ROW EXECUTE PROCEDURE "^dbschema^"."^view_name^"_procedure();
"
    in trigger_pgsql
;;