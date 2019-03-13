(*******************************************************)
(** Functions for transforming from a put datalog program to get datalog program
*)
(********************************************************)
(* 
@author: Vandang Tran
*)

open Lib;;
open Formulas;;
open Fol;;
open Skolem;;
open Fol_ex;;
open Expr;;
open Utils;;
open Rule_preprocess;;
open Stratification;;
open Lib;;

(** Datalog normalization: convert Deltainsert and Deltadelete predicates to normal Pred with name having prefix prefix ^ make"_dummy_delta_insert_" *)
let deltapred_to_pred prefix = function
  | Prog stt_lst -> 
    let rterm_to_pred rt = match rt with
      | Pred (x, vl) -> rt
      | Deltainsert (x, vl) -> Pred (prefix ^ get_rterm_predname rt, vl)
      | Deltadelete (x, vl) -> Pred (prefix ^ get_rterm_predname rt, vl) in
    let term_map_to_pred tt = match tt with
      | Rel rt -> Rel (rterm_to_pred rt)
      | Equal _ -> tt
      | Ineq _ -> tt
      | Not rt -> Not (rterm_to_pred rt) in
    let in_stt t = match t with
      | Rule(head,body) -> Rule(rterm_to_pred head, List.map term_map_to_pred body)
      | _ -> t in            
    Prog (List.map in_stt stt_lst)

let check_neg_view (view:rterm) rule_body = List.mem view rule_body;;

(** mapping from other relations to the view, for example 

   delta_insert_tracks2_a4_col2: \[tracks3_prime_a4_col2\]

   delta_insert_tracks2_a4_col0: \[tracks3_prime_a4_col0\]

   delta_insert_tracks2_a4_col3: \[tracks3_prime_a4_col3\]

   delta_insert_tracks2_a4_col1: \[tracks3_prime_a4_col1\] 
   
   the list of mapped attribute names (e.g, \[tracks3_prime_a4_col2\]) may be greater than 1 but if so, that list must contain identical elements
   *)
let build_schema_mapping (mapping:vartab) (col_names:colnamtab) (view:rterm) (head:rterm) =
  let vt:vartab = Hashtbl.create 100 in
  let pname = get_rterm_predname head in
  let vlst = get_rterm_varlist head in
  let arity = get_arity head in
  let key = symtkey_of_rterm head in
  let cols = Hashtbl.find col_names key in
  let in_v cn v =
    let comp_cn =
      String.uppercase_ascii (pname^"_a"^(string_of_int arity)^
                        "_"^cn)
    in
    match v with
      NamedVar _ | NumberedVar _ -> (* print_endline "  insert vt: "; print_string ((string_of_var v) ^"  " ^ comp_cn); *)
      vt_insert vt (string_of_var v) comp_cn
    | AggVar _ -> raise (SemErr (
        "Goal "^(string_of_symtkey key)^
        " contains an aggregate function as a variable, "^
        "which is only allowed in rule heads"
      ))
    | _ -> ()
  in
  List.iter2 in_v cols vlst;
  let pname = get_rterm_predname view in
  let vlst = get_rterm_varlist view in
  let arity = get_arity view in
  let key = symtkey_of_rterm view in
  let cols = Hashtbl.find col_names key in
  let in_v cn v =
    let comp_cn =
      String.uppercase_ascii (pname^"_a"^(string_of_int arity)^
                        "_"^cn)
    in
    match v with
      NamedVar _ | NumberedVar _ -> (* print_endline "  insert vt: "; print_string ((string_of_var v) ^"  " ^ comp_cn); *)
      vt_insert vt (string_of_var v) comp_cn
    | AggVar _ -> raise (SemErr (
        "Goal "^(string_of_symtkey key)^
        " contains an aggregate function as a variable, "^
        "which is only allowed in rule heads"
      ))
    | _ -> ()
  in
  List.iter2 in_v cols vlst; 
  let rep_rule var_name lst =
    match lst with
      view_col ::head_col::_ -> (*print_string "view_col: "; print_endline view_col; print_string "head_col: "; print_endline head_col; *) vt_insert mapping head_col view_col 
    | _ -> () in
  Hashtbl.iter rep_rule vt ;; 

(* create vartab for the view

*)
let schema_mapping_of_rule (col_names:colnamtab) (mapping:vartab) (view:rterm) (rule:stt)  =
  match rule with 
    Rule(head, body) ->
    let (p_rt,_,_,_) = split_terms body in
    let in_rt rterm =
      if((key_comp (symtkey_of_rterm rterm )  (symtkey_of_rterm view) ==0 )|| (key_comp (symtkey_of_rterm rterm )  (symtkey_of_rterm head) ==0 )) then  
        build_schema_mapping mapping col_names rterm head
      else ()
    in
    List.iter in_rt p_rt;
  | _ -> ();;

let rename_view (view:rterm) = match view with 
    Pred(pname,varlst) -> Pred(pname^"_med",varlst)
    | _ -> raise (SemErr "view rterm is not a normal predicate") 

let rename_if_isview (view:rterm) (t:term) =
  match t with 
    Rel (rt) -> if (key_comp (symtkey_of_rterm view ) (symtkey_of_rterm rt) ==0) then Rel (Pred((get_rterm_predname rt)^"_med",(get_rterm_varlist rt)) ) else t
  | Not (rt) -> if (key_comp (symtkey_of_rterm view ) (symtkey_of_rterm rt) ==0) then Not (Pred((get_rterm_predname rt)^"_med",(get_rterm_varlist rt)) ) else t
  | _ -> t;;

(* let get_view_in_rterms (view:rterm) lst =  *)




let varlist_of_symtkey (key:symtkey) (col_names:colnamtab) =
  stringlist_to_varlist (List.map (fun str -> String.uppercase_ascii ((get_symtkey_predname key^"_a"^(string_of_int (get_symtkey_arity key))^"_")^str)) (Hashtbl.find col_names key))

(** return a list of head of rules which have positive view in its body*)
let pos_view_head_list (view:rterm) (idb:symtable) (col_names:colnamtab)=
  let extract_literal key rules lst= 
    if(symtkey_of_rterm view == key) then lst
    else 
      let valid = let has_positive_view yes (rule: stt)  = yes || (let pos_views = (List.filter (fun t -> match t with Rel v -> (key_comp (symtkey_of_rterm v) (symtkey_of_rterm view) ==0) | _ -> false) (rule_body rule)) in  
         if(List.length pos_views >0) then true else false) in
        List.fold_left has_positive_view false rules in 
      if valid then Pred(get_symtkey_predname key,  varlist_of_symtkey key col_names) :: lst 
      else lst in 
  Hashtbl.fold extract_literal idb [];;

(* give a literal, create new literal with variable corresponding to a variable in literal of view definition *)
let mapping_rterm (col_names:colnamtab) (mapping:vartab) (rt:rterm) =
  let varlist = varlist_of_symtkey (symtkey_of_rterm rt) col_names in 
  let mapped_var var = match var with
      NamedVar str -> 
      if(Hashtbl.mem mapping str) then 
        let mapped_strs =  Hashtbl.find mapping str in if(List.length mapped_strs >= 1) then NamedVar (List.hd mapped_strs) else AnonVar
      else AnonVar
    | _ -> AnonVar in 
  match rt with 
  Pred (pname, vl) -> Pred(pname, List.map mapped_var varlist)
  | Deltainsert (pname, vl) -> Deltainsert(pname, List.map mapped_var varlist)
  | Deltadelete (pname, vl) -> Deltadelete(pname, List.map mapped_var varlist)

let get_neg_term (rt:rterm) = Not rt;;



let create_view_definition (view:rterm) (col_names:colnamtab) literals mapping = 
  let head = Pred(get_rterm_predname view,  varlist_of_symtkey (symtkey_of_rterm view) col_names) in 
  Rule(head, Rel (Pred(get_rterm_predname (rename_view view),  varlist_of_symtkey (symtkey_of_rterm view) col_names)) :: List.map (fun x -> rename_term "_derived_" (get_neg_term x)) (List.map (mapping_rterm col_names mapping) literals))

(* todo: there is a gap here when the negated literal of view has anonimous variables
    for this case, need to find the view in rule body has anonimous variables and raise a warning
    there is anathoer gap when there is a rule whose head is view and whose view also has positive predicates of the view, then the swaped rule is recursive without base case
*)
let transform_rule (view:rterm) (cnt:colnamtab) (rule:stt) lst= 
  match rule with
  | Query _ -> lst
  | Constraint _ -> lst
  | Source _ -> rule::lst
  | View _ -> rule::lst
  | Rule(h, body) ->
    let negated_views = (List.filter (fun t -> match t with Not v -> (key_comp (symtkey_of_rterm v) (symtkey_of_rterm view) ==0) | _ -> false) body) in 
    if (List.length negated_views > 0) then
      let negated_view = List.hd negated_views in 
      let get_view_lit v = match v with 
          Not view_lit -> view_lit
        | _ -> invalid_arg "negated view is invalid" in
      (* need to change variable which does not appear in the view to anonymous variable _ *)
      let (p_rt,n_rt,all_eqs,all_ineqs) = split_terms body in
      let vt = build_vartab cnt (p_rt@n_rt) in
      let is_anonimous_variable s = match s with 
          NamedVar s ->  if Hashtbl.mem vt s then List.length (Hashtbl.find vt s)< 1 else true
        | _ -> false in
      let to_anonimous = function var -> if(is_anonimous_variable var) then AnonVar else var in
      let literal_without_view = (List.filter (fun t -> match t with Not v ->  (key_comp (symtkey_of_rterm v) (symtkey_of_rterm view) !=0) | _ -> true) body) in
      (* convert a variable to anonimous one if do not find more than two reference for it *)
      let term_to_anonimous term = match term with 
          Rel rt -> Rel (Pred ((get_rterm_predname rt), List.map to_anonimous (get_rterm_varlist rt)))
        | Not rt -> Not (Pred ((get_rterm_predname rt),List.map to_anonimous (get_rterm_varlist rt)))
        | _ -> term in
      let r = Rule( Pred (get_rterm_predname (rename_view (get_view_lit negated_view)), get_rterm_varlist (get_view_lit negated_view)), List.map term_to_anonimous literal_without_view ) in 
      Rule(rule_head r, List.map (rename_if_isview view) (rule_body r)) :: lst
    else
      let pos_views = (List.filter (fun t -> match t with Rel v -> (key_comp (symtkey_of_rterm v) (symtkey_of_rterm view) ==0) | _ -> false) body) in 
      if(List.length pos_views >0) then Rule(rule_head rule, List.map (rename_if_isview view) (rule_body rule)) :: lst 
      else lst (* if there is no view predicate in the rule body (it can not appear in the head) then remove this rule in the derived get*)

let derive (debug:bool) (edb:symtable) prog = 
  (* get the view user want to define *)
  match prog with
    Prog stt_lst ->
    let view_rt = get_schema_rterm (get_view prog) in
    let local_edb = Hashtbl.copy edb in
    symt_insert local_edb (Rule(view_rt,[]));
    let idb = extract_idb prog in 
    let view_rt = get_schema_rterm (get_view prog) in
    let cnt = build_colnamtab local_edb idb in
    let mapping:vartab = Hashtbl.create 100 in 
    List.iter (schema_mapping_of_rule cnt mapping view_rt) stt_lst;
    if debug then (
                   print_endline "========mapping===="; vt_print mapping;
                ) else ();
    let transformed_lst = List.fold_right (transform_rule view_rt cnt) stt_lst [] in 
    let new_idb = extract_idb (Prog transformed_lst) in 
    let new_cnt = build_colnamtab local_edb new_idb in
    let view_def = create_view_definition view_rt new_cnt (pos_view_head_list (rename_view view_rt) new_idb new_cnt) mapping in 
    (* print_endline "------"; print_endline (string_of_rterm (List.hd (pos_view_head_list view_rt new_idb cnt)) ); *)
    deltapred_to_pred "_derived_" (Prog (transformed_lst@[view_def]));;

(* take a view update datalog program and generate datalog rule for apply delta relation to the source *)
let datalog_of_delta_appliation (debug:bool) prog = 
    (* get all pair of delta relations *)
    let delta_rt_lst = get_delta_rterms prog in
    (* get each pair of delta relations from the delta relation lst delta_rt_lst *)
    let application_rules lst delta = match delta with 
      | Pred _ -> invalid_arg "function application_rules called with a non-delta predicate"
      | Deltainsert _  -> 
        let del_rels = List.filter (is_delta_pair delta) delta_rt_lst in 
        if (List.length del_rels = 0) then 
        (* there is only one insertion relation *)
          Rule(get_new_source_rel_pred delta, [Rel (get_source_rel_pred delta)]):: Rule(get_new_source_rel_pred delta, [Rel delta]) ::lst 
        else 
          let del_rel = List.hd del_rels in 
          Rule(get_new_source_rel_pred del_rel, [Rel (get_source_rel_pred del_rel); Not del_rel]):: Rule(get_new_source_rel_pred del_rel, [Rel delta]) ::lst
      | Deltadelete _ -> 
        let ins_rels = List.filter ( fun x -> is_delta_pair x delta) delta_rt_lst in 
        if (List.length ins_rels = 0) then 
        (* there is only on deletion relation *)
            Rule(get_new_source_rel_pred delta, [Rel (get_source_rel_pred delta); Not delta]) ::lst 
        else lst in 
    let lst1 = List.fold_left application_rules [] delta_rt_lst in 
    let source_lst = get_source_rterms prog in 
    let has_delta srt = List.fold_left (fun ok delta -> ok || (symtkey_of_rterm (get_source_rel_pred delta) = (symtkey_of_rterm srt))) false delta_rt_lst in 
    let non_delta_source_lst = List.filter (non has_delta) source_lst in 
    let lst2 = List.fold_left (fun lst src ->  Rule(get_new_source_rel_pred src, [Rel src]) :: lst ) [] non_delta_source_lst in 
    lst2@lst1

(** take a view update datalog program and generate datalog rule for computing new view from new source *)
let datalog_of_new_view (debug:bool) prog =
  let edb = extract_edb prog in
  (* need to change the view (in query predicate) to a edb relation *)
  let idb = extract_idb prog in
  preprocess_rules idb;
  let view_rt = get_schema_rterm (get_view prog) in
  let strat = stratify edb idb view_rt in
  let rule_lst_lst = List.map (Hashtbl.find idb) strat in
  let new_rule_lst_lst = List.map (rename_rules "__dummy_new_" debug) rule_lst_lst in
  List.fold_right (fun x lst -> x@lst) new_rule_lst_lst []
;;

(** take a view update datalog program and remove all the constraint involving views in the proramg  *)
let remove_constraint_of_view (debug:bool) prog = 
  let view_sch = get_view prog in
  let view_rt = get_schema_rterm view_sch in
  let edb = extract_edb prog in
  let idb = extract_idb prog in
  if Hashtbl.mem idb (symtkey_of_rterm get_empty_pred) then
    let constr_rules = Hashtbl.find idb (symtkey_of_rterm get_empty_pred) in 
    symt_remove idb (symtkey_of_rterm get_empty_pred);
    let is_constr_of_view constr = 
        let local_idb = Hashtbl.copy idb in 
        symt_insert local_idb constr;
        let strat = stratify edb local_idb get_empty_pred in
        List.mem (symtkey_of_rterm view_rt) strat in
    let constr_of_non_view_lst = List.filter (non is_constr_of_view) constr_rules in 
    List.iter (symt_insert idb) constr_of_non_view_lst;
    Prog((get_schema_stts prog)@ (rules_of_symt idb))
  else prog
;;

(** take a view update datalog program and remove all the constraint not involving views in the proramg  *)
let keep_only_constraint_of_view (debug:bool) prog = 
  let view_sch = get_view prog in
  let view_rt = get_schema_rterm view_sch in
  let edb = extract_edb prog in
  let idb = extract_idb prog in
  if Hashtbl.mem idb (symtkey_of_rterm get_empty_pred) then
    let constr_rules = Hashtbl.find idb (symtkey_of_rterm get_empty_pred) in 
    symt_remove idb (symtkey_of_rterm get_empty_pred);
    let is_constr_of_view constr = 
      let local_idb = Hashtbl.copy idb in 
      symt_insert local_idb constr;
      let strat = stratify edb local_idb get_empty_pred in
      List.mem (symtkey_of_rterm view_rt) strat in
    let constr_of_view_lst = List.filter (is_constr_of_view) constr_rules in 
    List.iter (symt_insert idb) constr_of_view_lst;
    Prog((get_schema_stts prog)@ (rules_of_symt idb))
  else prog
;;

(** given a view update datalog program return datalog program of putget property *)
let datalog_of_putget (debug:bool) prog = 
  let putget_datalog = add_stts (datalog_of_new_view debug prog) (Expr.add_stts (datalog_of_delta_appliation ( debug) prog) prog) in 
  if debug then (
    print_endline "_____putget datalog program_______"; 
    print_string (Expr.string_of_prog  putget_datalog); 
    print_endline "______________\n";
  ) else ();
  putget_datalog
;;

(** given a rule, extract projection opetor of the rule and do the projections in other rules  *)
let extract_projection stt ind= match stt with
    | Query _    -> invalid_arg "function extract_projection called with a query"
    | Constraint _    -> invalid_arg "function extract_projection called with a query"
    | Source _    -> invalid_arg "function extract_projection called with a source schema"
    | View _ -> invalid_arg "function extract_projection called with a view schema"
    | Rule(h, b) -> 
        let extract_projection_of_neg t (termlst, rulelst, loc_ind) = match t with 
            | Rel rt -> (t::termlst, rulelst, loc_ind)
            | Not rt -> let varlst = (get_rterm_varlist rt) in 
                if (List.mem AnonVar varlst) then 
                let new_rt = Pred("π_proj_"^string_of_int loc_ind, (List.filter (non is_anon) varlst) ) in
                let new_rule = Rule( new_rt , [Rel rt]) in 
                ((Not new_rt):: termlst, (anonvar2namedvar new_rule) ::rulelst, (loc_ind+1))
                else (t:: termlst, rulelst, loc_ind)
            (* AnonVar in a negated predicate can not be converted to NamedVar because it make program unsafe *)
            | Equal _ -> (t::termlst, rulelst, loc_ind)
            | Ineq _ -> (t::termlst, rulelst, loc_ind) in
        let new_b, new_rules, new_ind = List.fold_right extract_projection_of_neg b ([], [], ind) in 
        let body_varset = setify (get_termlst_vars new_b) in
        let h_varset = setify (get_rterm_varlist h) in 
        if (set_eq h_varset body_varset) || ((List.length new_b) = 1) then 
            (* no more projection *)
            (new_rules@ [Rule(h, new_b)] , new_ind)
        else 
            (* there is a projection *)
            let new_h = Pred("π_proj_"^string_of_int new_ind,  body_varset) in
            (new_rules@ [Rule(new_h, new_b); Rule(h, [Rel new_h])], (new_ind+1))
;;

(** take a view update datalog program and extract proejctions to other rules *)
let extract_projection_rules (st:symtable) =
  let extract_rules key rules (lst,ind) =
      let namedvar_rules = List.map anonvar2namedvar rules in
      let extract_rule (l,i) rule =
          let extracted_lst, new_i = extract_projection rule i in
          (l@extracted_lst, new_i) in
      List.fold_left extract_rule (lst,ind) namedvar_rules in
  let rules, _ = Hashtbl.fold extract_rules st ([],0) in 
  rules

(** take a view update datalog program and extract projections to other rules *)
let extract_projection_expr (debug:bool) prog = 
  let _, non_rules = seperate_rules prog in
  (* let edb = extract_edb prog in *)
  (* need to change the view (in query predicate) to a edb relation *)
  let view_rt = get_schema_rterm (get_view prog) in
  (* need to convert the view to be an edb relation *)
  (* symt_insert edb (Rule(view_rt,[])); *)
  let idb = extract_idb prog in
  let view_rules = Hashtbl.find idb (symtkey_of_rterm view_rt) in
  symt_remove idb (symtkey_of_rterm view_rt);
  preprocess_rules idb;
  if debug then (
                    print_endline "_____projection-preprocessed datalog rules_______"; 
                    print_endline (Expr.string_of_prog (Prog(non_rules @ view_rules @ (extract_projection_rules idb))));
                    print_endline "______________\n";
                ) else ();   
  Prog(non_rules @ view_rules @ (extract_projection_rules idb))
;;

(** given a rule, extract projection opetor of the rule and do the projections in other rules  *)
let binarize stt ind= match stt with
    | Query _    -> invalid_arg "function extract_projection called with a query"
    | Constraint _    -> invalid_arg "function extract_projection called with a query"
    | Source _    -> invalid_arg "function extract_projection called with a source schema"
    | View _ -> invalid_arg "function extract_projection called with a view schema"
    | Rule(h, b) -> 
      let (p_rt,n_rt,all_eqs,all_ineqs) = split_terms b in
      let is_eq_of_constant t = match t with 
        Equal(Var _, Const _) -> true 
        | Equal(Const _, Var _) -> true 
        | Equal(Const _, Const _) -> true 
        | _ -> false in 
      let cons_eqs, other_eqs = List.partition is_eq_of_constant all_eqs in 
      let p_t = (List.map (fun x -> Rel x) p_rt) @ (List.map (fun x -> Not x) n_rt) in 
      (* let buitin_t = all_eqs@all_ineqs in *)
      (match p_t with 
        fst::tail ->
          let init_lst, init_pre_head, init_ind = 
            if (List.length cons_eqs > 0) then
              let pre_vars = get_term_varlist fst in 
              let t_vars = get_termlst_vars cons_eqs in 
              let new_rt = 
              (* print_string ("==> gen bin "^string_of_int loc_ind);  *)
              Pred("π_bin_"^string_of_int ind, setify ((List.filter (non is_anon) (pre_vars@t_vars))) ) in
              let new_rule = Rule( new_rt, fst::cons_eqs) in 
              ([new_rule], (Rel new_rt), (ind+1)) 
            else ([], fst, ind) in
          let pair_terms (rulelst, pre_head, loc_ind) t= 
            let pre_vars = get_term_varlist pre_head in 
            let t_vars = get_term_varlist t in 
            let new_rt = 
            (* print_string ("==> gen bin "^string_of_int loc_ind);  *)
            Pred("π_bin_"^string_of_int loc_ind, setify ((List.filter (non is_anon) (pre_vars@t_vars))) ) in
            let new_rule = Rule( new_rt , [pre_head; t]) in 
            (rulelst@[new_rule], (Rel new_rt), (loc_ind+1)) in
          let rulelst, prehead, new_ind = List.fold_left pair_terms (init_lst, init_pre_head, init_ind) tail in 
          (rulelst, Rule(h, prehead::other_eqs@all_ineqs), new_ind)
        | _ -> ([],stt, ind)
      )
;;

(** take a list of rules and extract proejctions and transfrom to a equivalent set of rules where a predicate is defined by al most two other predicate, in other words, the precedence graph is a binary tree *)
let binarize_rules (st:symtable) =
  let extract_rules key rules (lst,ind) =
      let namedvar_rules = List.map anonvar2namedvar rules in
      let extract_rule (l,head_rules,i) rule =
          let extracted_lst, hr, new_i = binarize rule i in
          (l@extracted_lst,head_rules@[hr], new_i) in
      let e_lst, hrules, new_ind = List.fold_left extract_rule (lst, [],ind) namedvar_rules in
      (* for the case of union of many rules and each rule is not in the form of A(x,y) :- B(x,y) *)
      let trivial_rule rule = 
        let h = rule_head rule in 
        let b = rule_body rule in 
        (List.length b = 1) && ( (get_rterm_varlist h) = (get_term_varlist (List.hd b))) in
      let contain_nontrivial_rules rules = List.fold_left (fun ok x -> ok || (non trivial_rule x)) false rules in 
      if (List.length hrules > 2) || ((List.length hrules = 2) && (contain_nontrivial_rules hrules)) then 
        let orginal_rulehead = rule_head (List.hd hrules) in
        (* rename the head of each rule *)
        let new_rulehead (l,i) r = match r with 
          Rule(h,b) -> 
          (* print_string ("==> gen bin "^string_of_int i);  *)
          (l@[Rule(Pred("π_bin_"^string_of_int i, get_rterm_varlist h),b)], (i+1))
          | _ -> invalid_arg "function new_rulehead called with not a rule" in
        let new_hrules, new_new_ind = List.fold_left new_rulehead ([], new_ind) hrules in 
        let head_lst = List.map (fun x -> rule_head x) new_hrules in 
        (match head_lst with 
          fst::tail ->
          let cols = gen_cols 0 (get_arity fst) in
          let vars = List.map (fun x -> NamedVar x) cols in 
          let pair_heads (rulelst, pre_head, loc_ind) t = 
            let new_rt = 
            (* print_string ("==> gen bin "^string_of_int loc_ind);  *)
            Pred("π_bin_"^string_of_int loc_ind, vars ) in
            let new_rule1 = Rule( new_rt , [Rel (Pred(get_rterm_predname pre_head, vars))]) in 
            let new_rule2 = Rule( new_rt , [Rel (Pred(get_rterm_predname t, vars))]) in 
            (rulelst@[new_rule1; new_rule2], new_rt, (loc_ind+1)) in
          let rulelst, prehead, new3_ind = List.fold_left pair_heads ([], fst, new_new_ind) tail in 
          (e_lst@new_hrules@rulelst@[Rule( change_vars orginal_rulehead vars, [Rel prehead])], new3_ind)
          | _ -> (e_lst@hrules, new_ind)
        )
      else
        (e_lst@hrules, new_ind) in
  let rules, _ = Hashtbl.fold extract_rules st ([],0) in 
  rules

(** take a view update datalog program and extract projections to other rules *)
let binarize_expr (debug:bool) prog = 
  let proj_extracted_prog = extract_projection_expr debug prog in
  let _, non_rules = seperate_rules proj_extracted_prog in
  (* let edb = extract_edb prog in *)
  (* need to change the view (in query predicate) to a edb relation *)
  let view_rt = get_schema_rterm (get_view proj_extracted_prog) in
  (* need to convert the view to be an edb relation *)
  (* symt_insert edb (Rule(view_rt,[])); *)
  let idb = extract_idb proj_extracted_prog in
  let view_rules = Hashtbl.find idb (symtkey_of_rterm view_rt) in
  symt_remove idb (symtkey_of_rterm view_rt);
  preprocess_rules idb;
  if debug then (
    print_endline "_____binarization-preprocessed datalog rules_______"; 
    print_endline (Expr.string_of_prog (Prog(non_rules @ view_rules @ (binarize_rules idb))));
    print_endline "______________\n";
  ) else ();   
  Prog(non_rules @ view_rules @ (binarize_rules idb))
;;

let get_inc_original rt = Pred("∂_old_" ^ get_rterm_predname rt, get_rterm_varlist rt);;

let get_inc_del rt = Pred("∂_del_" ^ get_rterm_predname rt, get_rterm_varlist rt);;

let get_inc_ins rt = Pred("∂_ins_" ^ get_rterm_predname rt, get_rterm_varlist rt);;

(** take a valid (satisfy putget and getput) view update datalog program and incrementalize it by the update (delta relations) on view *)
let incrementalize_by_view (debug:bool) prog = 
  let bin_prog = binarize_expr debug prog in
  let _, non_rules = seperate_rules bin_prog in
  let putget_prog = datalog_of_putget false bin_prog in
  let edb = extract_edb putget_prog in
  (* need to change the view (in query predicate) to a edb relation *)
  let view_rt = get_schema_rterm (get_view putget_prog) in
  (* need to convert the view to be an edb relation *)
  symt_insert edb (Rule(view_rt,[]));
  let strat =
    let temp_idb = extract_idb putget_prog in
    symt_remove temp_idb (symtkey_of_rterm view_rt);
    preprocess_rules temp_idb;
    (* let cnt = build_colnamtab edb idb in *)
    let new_view_rt = rename_rterm "__dummy_new_" view_rt in
    let total_goal = rename_rterm "__total_goal" view_rt in 
    if Hashtbl.mem temp_idb (symtkey_of_rterm get_empty_pred) then
      symt_insert temp_idb (Rule(total_goal,[Rel(new_view_rt); Rel(get_empty_pred)]))
    else symt_insert temp_idb (Rule(total_goal,[Rel(new_view_rt)]));
    stratify edb temp_idb total_goal in
  let idb = extract_idb bin_prog in
  symt_remove idb (symtkey_of_rterm view_rt);
  preprocess_rules idb;
  let rule_lst_lst = List.map (fun x -> if (Hashtbl.mem idb x) then Hashtbl.find idb x else []) strat in
  let is_inc inc_symtkey rt = List.mem (symtkey_of_rterm rt) inc_symtkey in
  let inc_key = [symtkey_of_rterm view_rt] in 
  (* incrementalize view first *)
  let init_rules = [(Rule(view_rt,[Rel (get_inc_original view_rt); Not (get_inc_del view_rt)]));
  (Rule(view_rt,[Rel (get_inc_ins view_rt)]))] in
  let incrementalize (inc_key, lst) rules = 
    match rules with 
    [] -> (inc_key, lst) 
    | [r] ->
      let head = rule_head r in 
      let body = rule_body r in 
      (match body with 
        [Rel p_rt] -> 
          (* projection *)
          if is_inc inc_key p_rt then
            let drop_vars = subtract (get_rterm_varlist p_rt) (get_rterm_varlist head) in
             (*for special case, drop_vars is empty or vars of the head is empty  *)
            if ((List.length drop_vars = 0) || (List.length (get_rterm_varlist head) = 0)) then 
              ((symtkey_of_rterm head)::inc_key, 
              lst@[
                Rule(get_inc_original head, [Rel (get_inc_original p_rt)]); 
                Rule(get_inc_del head, [Rel (get_inc_del p_rt)]);
                Rule(get_inc_ins head, [Rel (get_inc_ins p_rt)])]@
                if is_delta_or_empty head then
                  [Rule(head,[Rel (get_inc_ins head)]) ]
                else
                (* [Rule(head, [Rel (get_inc_original head); Not (get_inc_del head)]);
                Rule(head,[Rel (get_inc_ins head)]) ] *)
                rules
              )
            else ((symtkey_of_rterm head)::inc_key, 
              lst@[
                Rule(get_inc_original head, [Rel (get_inc_original p_rt)]); 
                Rule(get_inc_ins head, [Rel (get_inc_ins p_rt); Not (get_inc_original head)]);

                (* the first way using count*)
                (* Rule(Pred(get_rterm_predname head ^ "__dummy__t1", (get_rterm_varlist head)@[AggVar("COUNT", string_of_var (List.hd drop_vars)) ]), [Rel (get_inc_del p_rt)]);
                Rule(Pred(get_rterm_predname head ^ "__dummy__t2", (get_rterm_varlist head)@[AggVar("COUNT", string_of_var (List.hd drop_vars)) ]), [Rel (get_inc_original p_rt); Rel( Pred(get_rterm_predname head ^ "__dummy__t1", (get_rterm_varlist head)@[AnonVar])) ]);
                Rule(get_inc_del head, [Rel (Pred(get_rterm_predname head ^ "__dummy__t1", (get_rterm_varlist head)@[NamedVar "DUMMY__C1"])); Rel (Pred(get_rterm_predname head ^ "__dummy__t2", (get_rterm_varlist head)@[NamedVar "DUMMY__C2"])); Equal(Var (NamedVar "DUMMY__C1"), Var (NamedVar "DUMMY__C2")) ]) *)

                (* the sencond way not using count*)
                Rule(Pred(get_rterm_predname head ^ "π_1", (get_rterm_varlist head)), [Rel (p_rt)]);
                Rule(get_inc_del head, [Rel (get_inc_del p_rt); Not(Pred(get_rterm_predname head ^ "π_1", (get_rterm_varlist head)))])
                ]@
                if is_delta_or_empty head then
                  [Rule(head,[Rel (get_inc_ins head)]) ]
                else
                (* [Rule(head, [Rel (get_inc_original head); Not (get_inc_del head)]);
                Rule(head,[Rel (get_inc_ins head)]) ] *)
                rules
              )
          else (inc_key, lst@rules)
        | [Rel aa; Rel bb] -> 
          (* join *)
          ( match (List.mem (symtkey_of_rterm aa) inc_key, List.mem (symtkey_of_rterm bb) inc_key) with 
          (false, false) -> (inc_key, lst@rules)
          | (true, false) -> 
            (* only one relation *)
            ((symtkey_of_rterm head)::inc_key, 
              lst@[
                Rule(get_inc_original head, [Rel (get_inc_original aa); Rel (bb)]); 
                Rule(get_inc_del head, [Rel (get_inc_del aa); Rel(bb)]);
                Rule(get_inc_ins head, [Rel (get_inc_ins aa); Rel( bb)])]@
                if is_delta_or_empty head then
                  [Rule(head,[Rel (get_inc_ins head)]) ]
                else
                (* [Rule(head, [Rel (get_inc_original head); Not (get_inc_del head)]);
                Rule(head,[Rel (get_inc_ins head)]) ] *)
                rules
              )

          | (false, true) -> 
            ((symtkey_of_rterm head)::inc_key, 
              lst@[
                Rule(get_inc_original head, [Rel (aa); Rel (get_inc_original bb)]); 
                Rule(get_inc_del head, [Rel( aa); Rel(get_inc_del bb)]);
                Rule(get_inc_ins head, [Rel (aa); Rel(get_inc_ins bb)])]@
                if is_delta_or_empty head then
                  [Rule(head,[Rel (get_inc_ins head)]) ]
                else
                (* [Rule(head, [Rel (get_inc_original head); Not (get_inc_del head)]);
                Rule(head,[Rel (get_inc_ins head)]) ] *)
                rules
              )
          | (true, true) -> 
            ((symtkey_of_rterm head)::inc_key, 
              lst@[
                Rule(get_inc_original head, [Rel (get_inc_original aa); Rel (get_inc_original bb)]); 
                Rule(get_inc_del head, [Rel (get_inc_del aa); Rel(get_inc_original bb); Not(get_inc_del bb)]);
                Rule(get_inc_del head, [Rel(get_inc_original aa); Rel(get_inc_del bb)]);
                Rule(get_inc_ins head, [Rel (get_inc_ins aa); Rel(get_inc_original bb); Not(get_inc_del bb)]); 
                Rule(get_inc_ins head, [Rel (aa); Rel(get_inc_ins bb)])]@
                if is_delta_or_empty head then
                  [Rule(head,[Rel (get_inc_ins head)]) ]
                else
                (* [Rule(head, [Rel (get_inc_original head); Not (get_inc_del head)]);
                Rule(head,[Rel (get_inc_ins head)]) ] *)
                rules
              )
          )
        | [Rel aa; Not bb] -> 
          (* negation *)
          ( match (List.mem (symtkey_of_rterm aa) inc_key, List.mem (symtkey_of_rterm bb) inc_key) with 
          (false, false) -> (inc_key, lst@rules)
          | (true, false) -> 
            (* only one relation *)
            ((symtkey_of_rterm head)::inc_key, 
              lst@[
                Rule(get_inc_original head, [Rel (get_inc_original aa); Not (bb)]); 
                Rule(get_inc_del head, [Rel (get_inc_del aa)]);
                Rule(get_inc_ins head, [Rel (get_inc_ins aa); Not(bb)])]@
                if is_delta_or_empty head then
                  [Rule(head,[Rel (get_inc_ins head)]) ]
                else
                (* [Rule(head, [Rel (get_inc_original head); Not (get_inc_del head)]);
                Rule(head,[Rel (get_inc_ins head)]) ] *)
                rules
              )

          | (false, true) -> 
            ((symtkey_of_rterm head)::inc_key, 
              lst@[
                Rule(get_inc_original head, [Rel (aa); Not (get_inc_original bb)]); 
                Rule(get_inc_del head, [Rel(aa); Rel(get_inc_ins bb)]);
                Rule(get_inc_ins head, [Rel (aa); Rel(get_inc_del bb)])]@
                if is_delta_or_empty head then
                  [Rule(head,[Rel (get_inc_ins head)]) ]
                else
                (* [Rule(head, [Rel (get_inc_original head); Not (get_inc_del head)]);
                Rule(head,[Rel (get_inc_ins head)]) ] *)
                rules
              )
          | (true, true) -> 
            ((symtkey_of_rterm head)::inc_key, 
              lst@[
                Rule(get_inc_original head, [Rel (get_inc_original aa); Not (get_inc_original bb)]); 
                Rule(get_inc_del head, [Rel (get_inc_del aa)]);
                Rule(get_inc_del head, [Rel(get_inc_original aa); Rel(get_inc_ins bb)]);
                Rule(get_inc_ins head, [Rel (get_inc_ins aa); Not(get_inc_original bb); Not(get_inc_ins bb)]); 
                Rule(get_inc_ins head, [Rel (aa); Rel(get_inc_del bb)])]@
                if is_delta_or_empty head then
                  [Rule(head,[Rel (get_inc_ins head)]) ]
                else
                (* [Rule(head, [Rel (get_inc_original head); Not (get_inc_del head)]);
                Rule(head,[Rel (get_inc_ins head)]) ] *)
                rules
              )
          )
        | (Rel p_rt) :: buitin_t -> 
          (* join a relation with equility*)
          if(List.mem (symtkey_of_rterm p_rt) inc_key) then
            ((symtkey_of_rterm head)::inc_key, 
              lst@[
                Rule(get_inc_original head, Rel (get_inc_original p_rt)::buitin_t); 
                Rule(get_inc_del head, Rel (get_inc_del p_rt)::buitin_t);
                Rule(get_inc_ins head, Rel (get_inc_ins p_rt)::buitin_t )]@
                if is_delta_or_empty head then
                  [Rule(head,[Rel (get_inc_ins head)]) ]
                else
                (* [Rule(head, [Rel (get_inc_original head); Not (get_inc_del head)]);
                Rule(head,[Rel (get_inc_ins head)]) ] *)
                rules
              )
          else (inc_key, lst@rules)
        | _ -> invalid_arg "function incrementalize called with not a valid list" 
      )
    | [Rule(head,[Rel aa]); Rule(_,[Rel temp_bb])] -> 
      (* this is for the case of union *)
      let bb = change_vars temp_bb (get_rterm_varlist head) in
      ( match (List.mem (symtkey_of_rterm aa) inc_key, List.mem (symtkey_of_rterm bb) inc_key) with 
          (false, false) -> (inc_key, lst@rules)
          | (true, false) -> 
            (* only one relation *)
            ((symtkey_of_rterm head)::inc_key, 
              lst@[
                Rule(get_inc_original head, [Rel (get_inc_original aa)]); 
                Rule(get_inc_original head, [Rel (bb)]);
                Rule(get_inc_del head, [Rel (get_inc_del aa); Not(bb)]);
                Rule(get_inc_ins head, [Rel (get_inc_ins aa)])]@
                if is_delta_or_empty head then
                  [Rule(head,[Rel (get_inc_ins head)]) ]
                else
                (* [Rule(head, [Rel (get_inc_original head); Not (get_inc_del head)]);
                Rule(head,[Rel (get_inc_ins head)]) ] *)
                rules
              )

          | (false, true) -> 
            ((symtkey_of_rterm head)::inc_key, 
              lst@[
                Rule(get_inc_original head, [Rel (aa)]); 
                Rule(get_inc_original head, [Rel (get_inc_original bb)]);
                Rule(get_inc_del head, [Rel(get_inc_del bb); Not(aa)]);
                Rule(get_inc_ins head, [Rel (get_inc_ins bb)])]@
                if is_delta_or_empty head then
                  [Rule(head,[Rel (get_inc_ins head)]) ]
                else
                (* [Rule(head, [Rel (get_inc_original head); Not (get_inc_del head)]);
                Rule(head,[Rel (get_inc_ins head)]) ] *)
                rules
              )
          | (true, true) -> 
            ((symtkey_of_rterm head)::inc_key, 
              lst@[
                Rule(get_inc_original head, [Rel (get_inc_original aa)]); 
                Rule(get_inc_original head, [Rel (get_inc_original bb)]);
                Rule(get_inc_del head, [Rel (get_inc_del aa); Not(get_inc_original bb); Not(get_inc_ins bb)]);
                (* Rule(get_inc_del head, [Rel(get_inc_del bb); Not(get_inc_original aa); Not(get_inc_ins aa)]);
                Rule(get_inc_del head, [Rel(get_inc_del aa); Rel(get_inc_del bb)]); *)
                Rule(get_inc_del head, [Rel(get_inc_del bb); Not(aa)]);
                Rule(get_inc_ins head, [Rel (get_inc_ins aa)]); 
                Rule(get_inc_ins head, [Rel (get_inc_ins bb)])]@
                if is_delta_or_empty head then
                  [Rule(head,[Rel (get_inc_ins head)]) ]
                else
                (* [Rule(head, [Rel (get_inc_original head); Not (get_inc_del head)]);
                Rule(head,[Rel (get_inc_ins head)]) ] *)
                rules
              )
          )
    | _ -> invalid_arg ("function incrementalize called with a list of "^string_of_int (List.length rules)^" elements: \n"^ Expr.string_of_prog (Prog rules) ) in
  let _, inc_rules = List.fold_left incrementalize (inc_key, []) rule_lst_lst in 
  if debug then (
    print_endline "_____incrementalize_by_view datalog rules_______"; 
    print_endline (Expr.string_of_prog (Prog (non_rules@init_rules@inc_rules)));
    print_endline "______________\n";
  ) else (); 
  Prog (non_rules@init_rules@inc_rules)

    (* let edb = extract_edb bin_prog in
    (* need to change the view (in query predicate) to a edb relation *)
    let view_rt = get_schema_rterm (get_view bin_prog) in
    (* need to convert the view to be an edb relation *)
    symt_insert edb (Rule(view_rt,[]));
    let idb = extract_idb bin_prog in
    symt_remove idb (symtkey_of_rterm view_rt);
    preprocess_rules idb;
    let cnt = build_colnamtab edb idb in
    1 *)
;;