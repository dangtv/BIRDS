(* 
@author: Vandang Tran
*)

open Utils;;
open Expr;;

let get_view e = get_query_rterm (get_query e)
;;

let check_neg_view (view:rterm) rule_body = List.mem view rule_body;;

let split_terms terms =
  let rec split t (pos,neg,eq,inq) = match t with
    | Rel rt -> (rt::pos,neg,eq,inq)
    | Not rt -> (pos,rt::neg,eq,inq)
    | Equal _ -> (pos,neg,t::eq,inq) 
    | Ineq _ -> (pos,neg,eq,t::inq) in
  List.fold_right split terms ([],[],[],[])

let build_schema_mapping (mapping:vartab) (col_names:colnamtab) (view:rterm) (head:rterm) =
  let vt:vartab = Hashtbl.create 100 in
  let pname = get_rterm_predname head in
  let vlst = get_rterm_varlist head in
  let arity = get_arity head in
  let key = symtkey_of_rterm head in
  let cols = Hashtbl.find col_names key in
  let in_v cn v =
    let comp_cn =
      String.uppercase (pname^"_a"^(string_of_int arity)^
                        "_"^cn)
    in
    match v with
      NamedVar _ | NumberedVar _ ->
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
      String.uppercase (pname^"_a"^(string_of_int arity)^
                        "_"^cn)
    in
    match v with
      NamedVar _ | NumberedVar _ ->
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
      view_col ::head_col::_ -> vt_insert mapping head_col view_col 
    | _ -> () in
  Hashtbl.iter rep_rule vt;;

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

let varlist_of_symtkey (key:symtkey) (col_names:colnamtab) =
  stringlist_to_varlist (List.map (fun str -> String.uppercase ((get_symtkey_predname key^"_a"^(string_of_int (get_symtkey_arity key))^"_")^str)) (Hashtbl.find col_names key))

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
  Rule(head, Rel (Pred(get_rterm_predname (rename_view view),  varlist_of_symtkey (symtkey_of_rterm view) col_names)) :: List.map get_neg_term (List.map (mapping_rterm col_names mapping) literals))

let transform_rule (view:rterm) (cnt:colnamtab) (rule:stt) lst= 
  match rule with
  | Query _ -> lst
  | Base _ -> lst
  | Rule(h, body) -> 
    let negated_views = (List.filter (fun t -> match t with Not v -> (key_comp (symtkey_of_rterm v) (symtkey_of_rterm view) ==0) | _ -> false) body) in 
    if (List.length negated_views > 0) then
      let negated_view = List.hd negated_views in 
      let get_view_lit v = match v with 
          Not view_lit -> view_lit
        | _ -> invalid_arg "negated view is invalid" in
      let (p_rt,n_rt,all_eqs,all_ineqs) = split_terms body in
      let vt = build_vartab cnt (p_rt@n_rt) in
      let is_anonimous_variable s = match s with 
          NamedVar s ->  if Hashtbl.mem vt s then List.length (Hashtbl.find vt s)< 1 else true
        | _ -> false in
      let to_anonimous = function var -> if(is_anonimous_variable var) then AnonVar else var in
      let literal_without_view = (List.filter (fun t -> match t with Not v ->  (key_comp (symtkey_of_rterm v) (symtkey_of_rterm view) !=0) | _ -> true) body) in
      let term_to_anonimous term = match term with 
          Rel rt -> Rel (Pred ((get_rterm_predname rt), List.map to_anonimous (get_rterm_varlist rt)))
        | Not rt -> Not (Pred ((get_rterm_predname rt),List.map to_anonimous (get_rterm_varlist rt)))
        | _ -> term in
      let r = Rule( Pred (get_rterm_predname (rename_view (get_view_lit negated_view)), get_rterm_varlist (get_view_lit negated_view)), List.map term_to_anonimous literal_without_view ) in 
      Rule(rule_head r, List.map (rename_if_isview view) (rule_body r)) :: lst
    else
      let pos_views = (List.filter (fun t -> match t with Rel v -> (key_comp (symtkey_of_rterm v) (symtkey_of_rterm view) ==0) | _ -> false) body) in 
      if(List.length pos_views >0) then Rule(rule_head rule, List.map (rename_if_isview view) (rule_body rule)) :: lst 
      else lst

let derive (debug:bool) (edb:symtable) prog = 
  (* get the view user want to define *)
  match prog with
    Prog stt_lst ->
    let query_rt = get_query_rterm (get_query prog) in
    let local_edb = Hashtbl.copy edb in
    symt_insert local_edb (Rule(query_rt,[]));
    let idb = extract_idb prog in 
    let view_rt = get_view prog in
    let cnt = build_colnamtab local_edb idb in
    let mapping:vartab = Hashtbl.create 100 in 
    List.iter (schema_mapping_of_rule cnt mapping view_rt) stt_lst; 
    let transformed_lst = List.fold_right (transform_rule view_rt cnt) stt_lst [] in 
    let new_idb = extract_idb (Prog transformed_lst) in
    let new_cnt = build_colnamtab local_edb new_idb in
    let view_def = create_view_definition view_rt new_cnt (pos_view_head_list (rename_view view_rt) new_idb new_cnt) mapping in
    deltapred_to_pred (Prog (get_query prog::view_def::transformed_lst));;