(* 
@author: Vandang Tran
*)

open Expr;;
open Utils;;

let extract_rterm_constants ?(pos = 0) rt = match rt with
    | Pred (x, vl) ->
        let extract var (v_lst,e_lst,i) = match var with 
            | ConstVar const -> 
                let nvar = NumberedVar i in
                let neq = Equal (nvar,const) in
                (nvar::v_lst, neq::e_lst,i+1) 
            | _ -> (var::v_lst, e_lst, i) in
        let (vars,eqs,_) = List.fold_right extract vl ([],[],pos) in 
        ( Pred (x,vars), eqs )
    | Deltainsert (x, vl) ->
        let extract var (v_lst,e_lst,i) = match var with 
            | ConstVar const -> 
                let nvar = NumberedVar i in
                let neq = Equal (nvar,const) in
                (nvar::v_lst, neq::e_lst,i+1) 
            | _ -> (var::v_lst, e_lst, i) in
        let (vars,eqs,_) = List.fold_right extract vl ([],[],pos) in 
        ( Deltainsert (x,vars), eqs )
    | Deltadelete (x, vl) ->
        let extract var (v_lst,e_lst,i) = match var with 
            | ConstVar const -> 
                let nvar = NumberedVar i in
                let neq = Equal (nvar,const) in
                (nvar::v_lst, neq::e_lst,i+1) 
            | _ -> (var::v_lst, e_lst, i) in
        let (vars,eqs,_) = List.fold_right extract vl ([],[],pos) in 
        ( Deltadelete (x,vars), eqs )

let extract_rule_constants = function
    | Query _    -> invalid_arg "function extract_rule_constants called with a query"
    | Base _    -> invalid_arg "function extract_rule_constants called with a base relation"
    | Rule(h, b) -> 
        let (h2, h2_e) = extract_rterm_constants h in
        let extract t (t_lst,e_lst,i) = match t with
            | Rel rt -> 
                let (rt2,rt2_e) = extract_rterm_constants ~pos:i rt in
                ((Rel rt2)::t_lst, rt2_e@e_lst, i+(List.length rt2_e) )
            | _ -> (t::t_lst, e_lst, i) in
        let (b2,b2_e,_) = List.fold_right extract b ([],[],(List.length h2_e)) in
        Rule (h2,b2@h2_e@b2_e)
;;

let preprocess_rules (st:symtable) =
    let rep_rule key rules =
        let rep_lst = List.map extract_rule_constants rules in
        Hashtbl.replace st key rep_lst in
    Hashtbl.iter rep_rule st

let rule_of_query query (idb:symtable) =
    let (q2,eqs) = extract_rterm_constants query in
    let dummy = Pred ("__dummy__", get_rterm_varlist q2) in
    Rule (dummy, (Rel q2)::eqs)

let split_terms terms =
    let rec split t (pos,neg,eq,inq) = match t with
        | Rel rt -> (rt::pos,neg,eq,inq)
        | Not rt -> (pos,rt::neg,eq,inq)
        | Equal _ -> (pos,neg,t::eq,inq) 
        | Ineq _ -> (pos,neg,eq,t::inq) in
    List.fold_right split terms ([],[],[],[])