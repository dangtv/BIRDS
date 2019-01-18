(*******************************************************)
(**  
Functions to transform datalog ast to first-order logic formula and vice versa
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

(** convert the vterm type into a Fol.term *)
let rec folterm_of_vterm ae =
    match ae with 
      Const c -> Fn (string_of_const c,[]) 
    | Var v -> Fol.Var (string_of_var v)
    | Sum(f,g) -> Fn("+",[folterm_of_vterm f; folterm_of_vterm g])
    | Diff(f,g) -> Fn("-",[folterm_of_vterm f; folterm_of_vterm g])
    | Times(f,g) -> Fn("*",[folterm_of_vterm f; folterm_of_vterm g])
    | Div (f,g) -> Fn("/",[folterm_of_vterm f; folterm_of_vterm g])
    | Neg e ->  Fn("-",[folterm_of_vterm e])
    | Concat(f,g) -> Fn("^",[folterm_of_vterm f; folterm_of_vterm g])
    | BoolAnd (f,g) -> Fn("and",[folterm_of_vterm f; folterm_of_vterm g])
    | BoolOr (f,g) -> Fn("or",[folterm_of_vterm f; folterm_of_vterm g])
    | BoolNot e ->  Fn("not",[folterm_of_vterm e])
;;

let const_of_string str = 
    try  (Int (int_of_string str)) with
    (* try test () with *)
    | Failure e -> 
        try  (Real (float_of_string str)) with
        (* try test () with *)
        | Failure e -> 
            try  (Bool (bool_of_string str)) with
            (* try test () with *)
            | Failure e -> if (str = "null") then  Null else  (String str)

(** convert the Fol.term to vterm type*)
let rec vterm_of_folterm ft =
    match ft with 
      Fol.Var s -> (Var (NamedVar s))
    | Fol.Fn(c,[]) -> Const (const_of_string c)
    | Fol.Fn("/",[tm1;tm2]) -> Div (vterm_of_folterm tm1, vterm_of_folterm tm2)
    | Fol.Fn("*",[tm1;tm2]) -> Times (vterm_of_folterm tm1, vterm_of_folterm tm2)
    | Fol.Fn("-",[tm1;tm2]) -> Diff (vterm_of_folterm tm1, vterm_of_folterm tm2)
    | Fol.Fn("+",[tm1;tm2]) -> Sum (vterm_of_folterm tm1, vterm_of_folterm tm2)
    | Fol.Fn("^",[tm1;tm2]) ->  Concat(vterm_of_folterm tm1, vterm_of_folterm tm2)
    | Fol.Fn("::",[tm1;tm2]) -> Concat (vterm_of_folterm tm1, vterm_of_folterm tm2)
    | Fol.Fn(f,_) -> raise (SemErr ("unkown arithmetic operator " ^f))
;;

let rec fol_of_symtkey (idb:symtable) (cnt:colnamtab) (goal:symtkey)  =
    let rule_lst = Hashtbl.find idb goal in
    (* disjunction of all rules then we have FO formula for a idb predicate*)
    let fol_of_rule_lst (idb:symtable) (cnt:colnamtab) rules =
        let fol_list = List.map (fol_of_rule idb cnt) rules in
        let fm = match fol_list with 
            hd::bd -> if (List.length bd = 0) then hd else 
                    List.fold_left (fun form e -> Formulas.Or(form, e)) hd bd 
            | _ -> failwith "there is no rule for the idb relation" in 
        fm in
    let fm = fol_of_rule_lst idb cnt rule_lst in
    fm
and fol_of_rule (idb:symtable) (cnt:colnamtab) rule =
    let head = rule_head rule in
    let body = rule_body rule in
    let (p_rt,n_rt,all_eqs,all_ineqs) = split_terms body in
    let cols = Hashtbl.find cnt (symtkey_of_rterm head) in
    let varlst = get_rterm_varlist head in
    let subfn = fpf (List.map (fun x -> string_of_var x) varlst) (List.map (fun x -> Fol.Var x) cols) in
    let exvars = VarSet.filter (fun x -> not (is_anon x)) (VarSet.diff (get_termlst_varset body) (VarSet.of_list (get_rterm_varlist head))) in
    let conjunction_lst =  (List.map (fun x ->  fol_of_rterm x idb cnt) p_rt)@(List.map (fun x ->  Formulas.Not(fol_of_rterm x idb cnt) ) n_rt)@ (List.map (fun x -> fol_of_eq x) all_eqs) @ (List.map (fun x -> fol_of_ineq x) all_ineqs) in
    let fm = match conjunction_lst with 
        hd::bd -> if (List.length bd = 0) then hd else 
            List.fold_left (fun form e -> Formulas.And(form, e)) hd bd 
        | _ -> failwith "the body of rule contains nothing" in 
    let fm2 = subst subfn fm in
        itlist mk_exists (List.map string_of_var (VarSet.elements exvars)) fm2
and fol_of_rterm r (idb:symtable) (cnt:colnamtab)= 
    let cols = Hashtbl.find cnt (symtkey_of_rterm r) in
    let varlst = get_rterm_varlist r in
    let excols = List.fold_right2 (fun col var l -> if (is_anon var) then col::l else l) cols varlst [] in
    let subfn = List.fold_right2 (fun col var l-> if (is_anon var) then l else (col |-> Fol.Var (string_of_var var)) l) cols varlst undefined in 
    let fm = if Hashtbl.mem idb (symtkey_of_rterm r) then 
    fol_of_symtkey idb cnt (symtkey_of_rterm r) 
    else  
    Atom(R(get_rterm_predname r, (List.map (fun x -> Fol.Var x) cols))) in
    let fm2 = subst subfn fm in
    itlist mk_exists excols fm2
and fol_of_eq eq = match eq with 
    Equal(exp1, exp2) -> Atom(R("=",[folterm_of_vterm exp1; folterm_of_vterm exp2]))
    | _ -> failwith "not a equality"
and fol_of_ineq ineq = match ineq with 
    Ineq(str, exp1, exp2) -> Atom(R(str,[folterm_of_vterm exp1; folterm_of_vterm exp2]))
    | _ -> failwith "not a inequality";;

(** take a query term and rules of idb relations stored in a symtable, generate a FO formula for it *)
let fol_of_query (idb:symtable) (cnt:colnamtab) (query:rterm) =
    let qrule = rule_of_query query idb in
        let local_idb = Hashtbl.copy idb in 
        let local_cnt = Hashtbl.copy cnt in 
        symt_insert local_idb qrule;
        let key = symtkey_of_rule qrule in
        if not (Hashtbl.mem local_cnt key) then
        Hashtbl.add local_cnt key (List.map string_of_var (get_rterm_varlist (rule_head qrule)));
        fol_of_symtkey local_idb local_cnt (symtkey_of_rterm (rule_head qrule))

let fol_of_stt (debug:bool) (edb:symtable) prog =
    let query_rt = get_query_rterm (get_query prog) in
    let idb = extract_idb prog in
    preprocess_rules idb; 
    let cnt = build_colnamtab edb idb in
    let fm = fol_of_query idb cnt query_rt  in
    fm

(* take a view update datalog program and generate the FO sentence of checking whether all delta relations are disjoint *)
let disjoint_delta_sentence_of_stt (debug:bool) (edb:symtable) prog = 
    let query_rt = get_query_rterm (get_query prog) in
    let local_edb = Hashtbl.copy edb in
    symt_insert local_edb (Rule(query_rt,[]));
    let idb = extract_idb prog in
    preprocess_rules idb;
    let cnt = build_colnamtab local_edb idb in
    let delta_rt_lst = get_delta_rterms prog in
    let delta_pair_lst = 
        let pair_of_delta_insert lst ins_rel = 
            let del_rels = List.filter (is_delta_pair ins_rel) delta_rt_lst in 
            if (List.length del_rels = 0) then lst else (ins_rel, (List.hd del_rels))::lst in 
        List.fold_left pair_of_delta_insert [] delta_rt_lst in 
    
    (* get the emptiness FO sentence of a relation *)
    let disjoint_fo_sentence ins_rel del_rel = 
        let cols = List.map string_of_var (get_rterm_varlist ins_rel) in
        itlist mk_exists cols (And(fol_of_query idb cnt ins_rel, fol_of_query idb cnt del_rel)) in
    let djsjoint_sen_lst = List.map (fun (r1,r2) -> disjoint_fo_sentence r1 r2) delta_pair_lst in 
    Prop.list_disj djsjoint_sen_lst;;

(* take a view update datalog program and generate FO sentence of SourceStability constraint (put s v = s) for its view update strategy *)
let sourcestability_sentence_of_stt (debug:bool) (edb:symtable) prog =
    let query_rt = get_query_rterm (get_query prog) in
    let local_edb = Hashtbl.copy edb in
    symt_insert local_edb (Rule(query_rt,[]));
    let idb = extract_idb prog in
    preprocess_rules idb;
    let cnt = build_colnamtab local_edb idb in
    let delta_rt_lst = get_delta_rterms prog in
    (* get the emptiness FO sentence of a relation *)
    let emptiness_fo_sentence rel = 
         let cols = List.map string_of_var (get_rterm_varlist rel) in
        itlist mk_exists cols (fol_of_query idb cnt rel) in
    let delta_fo_sentence_lst = List.map emptiness_fo_sentence delta_rt_lst in 
    Prop.list_disj delta_fo_sentence_lst;;
;;

let sourcestability_theorem_of_stt (debug:bool) (edb:symtable) prog = Imp(sourcestability_sentence_of_stt debug edb prog, False);;

let get_goal_predicate freevars  goal_name = Pred("__dummy__goal_"^(goal_name), (List.map (fun x -> NamedVar x) freevars))

(** take a RANF formula and return the equivalent datalog program *)
let rec ranf2datalog fm freevars sub_name goal_name= 
    match fm with 
    Atom(R("=",[Var x; Fn (c,[])]))
    | Atom(R("=",[Fn (c,[]); Var x])) -> let goal_predicate = get_goal_predicate freevars goal_name in
        ([Rule (goal_predicate, [Equal(Var (NamedVar x), Const (const_of_string c))])], goal_predicate)
    | Atom(R(_,_))
    | Exists(_,_) 
    | Formulas.Not _ -> datalog_of_conj [fm] freevars sub_name goal_name
    | And(p,q) -> datalog_of_conj (to_conj_lst fm) freevars sub_name goal_name
    | Or(p,q) -> ((fst (ranf2datalog p (fv p) (sub_name^"_0") goal_name))@(fst (ranf2datalog q (fv p) (sub_name^"_1") goal_name)), get_goal_predicate freevars goal_name)
    | _ -> failwith ("fail to get datalog program of " ^ Fol_ex.string_of_fol_formula fm )
and datalog_of_conj conj_lst freevars sub_name goal_name= 
    let goal_predicate = get_goal_predicate freevars goal_name in
    let rec datalog_of_subfm subfm (rule_termlst, prog, sub_num) = 
        (match subfm with 
              False -> ((Equal (Const (Int 1), Const (Int 2)))::rule_termlst, prog,sub_num+1)
            | True -> ((Equal (Const (Int 1), Const (Int 1)))::rule_termlst, prog, sub_num+1)
            | Atom(R(p,args)) -> 
                if (List.mem p ["="; "<"; "<="; ">"; ">="]) then
                    (match args with 
                        [term1;term2] -> if (p = "=") then ((Equal (vterm_of_folterm term1, vterm_of_folterm term2))::rule_termlst, prog, sub_num+1)
                            else ((Ineq (p, vterm_of_folterm term1, vterm_of_folterm term2))::rule_termlst, prog, sub_num+1)
                        | _ -> failwith ("fail to get datalog predicate of " ^ Fol_ex.string_of_fol_formula subfm )
                    )
                else
                if is_relation_symbol p then
                    (* replace each term not a variable in args to an variable and add an equation of that new variable *)
                    let convert_term t (varlst,eqlst,i)= 
                        match t with 
                            Fol.Var v -> (NamedVar v::varlst, eqlst,i) 
                            | Fol.Fn (c,[]) -> (ConstVar (const_of_string c)::varlst, eqlst,i) 
                            | _ -> ((NamedVar ("_"^sub_name^(string_of_int i)))::varlst, Equal(Var (NamedVar ("_"^sub_name^"_"^(string_of_int i))), vterm_of_folterm t) ::eqlst,i+1) in
                    let varlst, eqlst,_ = List.fold_right convert_term args ([],[],0) in
                    ( (Rel (Pred(p, varlst)))::eqlst@rule_termlst, prog, sub_num+1)
                else failwith ("fail to get datalog program of " ^ Fol_ex.string_of_fol_formula subfm )
            | Formulas.Not Atom(R(p,args)) -> let t1, t2, t3 = datalog_of_subfm (Atom(R(p,args))) (rule_termlst, prog, sub_num) in 
                (match t1 with 
                    head::tail -> ((negate_term head)::tail, t2, t3) 
                    | _ -> failwith ("fail to get datalog program of " ^ Fol_ex.string_of_fol_formula subfm )
                )
            | Exists(x,p) ->
                let quants, psi = extract_ex_quants (Exists(x,p)) in 
                (match psi with 
                    | Atom(R(p,args)) -> let t1, t2, t3 = datalog_of_subfm (Atom(R(p,args))) (rule_termlst, prog, sub_num) in 
                        (match t1 with 
                            (Rel predicate)::tail -> 
                                let newvarlst = List.map (fun x -> if List.mem (string_of_var x) quants then AnonVar else x ) (get_rterm_varlist predicate) in
                                ((Rel (Pred(get_rterm_predname predicate, newvarlst)))::tail, t2, t3) 
                            | _ -> failwith ("can not obtain for datalog program of " ^ Fol_ex.string_of_fol_formula subfm )
                        )
                    | _ -> 
                        let subprog, subgoal_pred = ranf2datalog psi (fv psi) (sub_name^"_"^(string_of_int sub_num)) (sub_name^"_"^(string_of_int sub_num)) in 
                        let varlst = get_rterm_varlist subgoal_pred in
                        let newvarlst = List.map (fun x -> if List.mem (string_of_var x) quants then AnonVar else x ) varlst in
                        ((Rel (Pred(get_rterm_predname subgoal_pred, newvarlst)))::rule_termlst, subprog@prog, sub_num+1)
                )
            | Formulas.Not (Exists(x,p)) -> let t1, t2, t3 = datalog_of_subfm (Exists(x,p)) (rule_termlst, prog, sub_num) in 
                (match t1 with 
                    head::tail -> ((negate_term head)::tail, t2, t3) 
                    | _ -> failwith ("fail to get datalog program of " ^ Fol_ex.string_of_fol_formula subfm )
                )
            | Formulas.Not p -> let subprog, subgoal_pred = ranf2datalog p (fv p) (sub_name^"_"^(string_of_int sub_num)) (sub_name^"_"^(string_of_int sub_num)) in 
                ((Not (subgoal_pred))::rule_termlst, subprog@prog,  sub_num+1)
            | _ -> let subprog, subgoal_pred = ranf2datalog subfm (fv subfm) (sub_name^"_"^(string_of_int sub_num)) (sub_name^"_"^(string_of_int sub_num)) in 
                (((Rel(subgoal_pred)))::rule_termlst, subprog@prog, sub_num+1) ) in 
    let rule_termlst, prog, _ = List.fold_right datalog_of_subfm conj_lst ([],[],0) in
    ((Rule (goal_predicate, rule_termlst))::prog, goal_predicate);;

(** transform a safe range FO fomula to datalog program*)
let fol2datalog freevars fm = 
    if set_eq (setify freevars) (fv fm) then
        let lst, rt = ranf2datalog (ranf (simplify fm)) freevars "0" "0" in 
        Prog ((Query rt)::lst)
    else failwith "the list of variables must be exactly the free varilabes in FO formula";;

(** take a RANF formula and view name to return the it in view-predicate normal form *)
let rec ranf2lvnf view fm =
    push_into_quant view fm
and push_into_quant view fm =
    match fm with 
        (* Distribution with existential quantifiers *)
        | Exists(x, Or(p,q)) -> Prop.list_disj (List.map (fun f -> Exists(x, f)) (to_dis_lst (Or(p,q))))
        | Exists(x, p) ->  Exists(x, push_into_quant view p)
        
        | _ -> fm
and front_loading view fm = 
    match fm with 
        (* Commutative property of ’and’ *)
        | And(phi,Atom(R(view,args))) | And(Atom(R(view,args)), phi) ->  
            And(Atom(R(view,args)), front_loading view phi)
        | And(phi,Formulas.Not (Atom(R(view,args)))) | And(Formulas.Not (Atom(R(view,args))), phi) -> 
            And(Formulas.Not (Atom(R(view,args))), front_loading view phi)
        | And(And(Atom(R(view,args)), phi), psi) | And(psi, And(Atom(R(view,args)), phi)) ->
            And(Atom(R(view,args)), front_loading view (And(phi, psi)))
        | And(And(Formulas.Not (Atom(R(view,args))), phi), psi) | And(psi, And(Formulas.Not Atom(R(view,args)), phi)) ->
            And(Formulas.Not (Atom(R(view,args))), front_loading view (And(phi, psi))) 
        (* Commutative property of ’or’: *)
        | Or(phi, And(Atom(R(view, args)), xi)) | Or(And(Atom(R(view, args)), xi), phi) -> 
            Or(And(Atom(R(view, args)), front_loading view xi), front_loading view phi)
        | Or(phi, And(Formulas.Not(Atom(R(view, args))), xi)) | Or(And(Formulas.Not(Atom(R(view, args))), xi), phi) -> 
            Or(And(Formulas.Not(Atom(R(view, args))), front_loading view xi), front_loading view phi)
        | Or(Or(And(Atom(R(view, args)), xi), phi), psi) | Or(psi, Or(And(Atom(R(view, args)), xi), phi)) -> 
            Or (And(Atom(R(view, args)), front_loading view xi), front_loading view (Or(phi, psi)))
        | Or(Or(And(Formulas.Not(Atom(R(view, args))), xi), phi), psi) | Or(psi, Or(And(Formulas.Not(Atom(R(view, args))), xi), phi)) -> 
            Or (And(Formulas.Not(Atom(R(view, args))), front_loading view xi), front_loading view (Or(phi, psi)))
        (* Distributive laws  *)
        | And(Or(And(Atom(R(view, args)), xi), phi), psi) | And(psi, Or(And(Atom(R(view, args)), xi), phi)) -> 
            Or(And(Atom(R(view,args)), front_loading view (And(xi, psi))), front_loading view (And(phi, psi)))
        | And(Or(And(Formulas.Not(Atom(R(view, args))), xi), phi), psi) | And(psi, Or(And(Formulas.Not(Atom(R(view, args))), xi), phi)) -> 
            Or(And(Formulas.Not(Atom(R(view, args))), front_loading view (And(xi, psi))), front_loading view (And(phi, psi)))
        (* travel all subformulas inductively *)
        | And(p, q) -> 
            let p' = front_loading view p in 
            let q' = front_loading view q in 
            let fm' = And(p', q') in 
            (match fm' with 
                | And(And(Atom(R(view,args)), phi), psi) | And(psi, And(Atom(R(view,args)), phi)) ->
                And(Atom(R(view,args)), front_loading view (And(phi, psi)))
                | And(And(Formulas.Not (Atom(R(view,args))), phi), psi) | And(psi, And(Formulas.Not Atom(R(view,args)), phi)) ->
                And(Formulas.Not (Atom(R(view,args))), front_loading view (And(phi, psi)))
                | _ -> fm'
            )
        | Or(p,q) -> Or(front_loading view p, front_loading view q)
        | Exists(x, p) ->  Exists(x, front_loading view p)
        | Not(Exists(x, p)) -> Not (Exists(x, front_loading view p))
        | _ -> fm;;