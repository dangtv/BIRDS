(** 
Functions for preprocesing of IDB rules before
SQL generation.
 *)

open Expr2
open Lib
open Utils

(****************************************************)
(*      Recursion check functions                   *)
(****************************************************)

(** Returns true if the provided term is a call to the predicate described
 by the provided key. If the recursive call is negative, an error is raised. *)
let rec check_rec_term key = function
    | Rel rt ->
        let rt_key = symtkey_of_rterm rt in
        (key_comp key rt_key) = 0
    | Not rt ->
        if (check_rec_term key (Rel rt)) then
            raise (SemErr (
                "Predicate "^(string_of_symtkey key)^
                " is negatively recursive"
            ))
        else false
    | _ -> false

(** Returns true if the provided rule contains a call to itself.
 * If the recursive call is negative, then an error is raised.
 * If there is more than one recursive call to itself, an error is raised. *)
let is_rec_rule rule =
    let key = symtkey_of_rule rule in
    let body = rule_body rule in
    let rec_eval acc rule =
        if check_rec_term key rule then acc+1 else acc
    in
    let rec_count = List.fold_left rec_eval 0 body in
    if rec_count > 1 then
        raise (SemErr(
            "Predicate "^(string_of_symtkey key)^
            " has more than one recursive call"
        ))
    else
        rec_count = 1

(** Given a list of rules that define a predicate, checks that they satisfy the recursion conditions, which are:
  - There cannot be negative recursive calls.
  - A single rule cannot make more than one recursive call.
  - There is at most one recursive rule.
  - The predicate has a base-case.
 
  If the conditions are satisfied and there is a recursive rule,
  then this recursive rule is placed at the end of the list.
 *)
let check_rec_rule rule_lst =
    let rec_split rule (r_lst,nr_lst) =
        if is_rec_rule rule then (rule::r_lst, nr_lst)
        else (r_lst, rule::nr_lst)
    in
    (*Split the rules in recursive and not revursive*)
    let (r_lst, nr_lst) = List.fold_right rec_split rule_lst ([],[]) in
    (*Check existence of base-case*)
    let key = symtkey_of_rule (List.hd rule_lst) in
    if nr_lst = [] then
        raise (SemErr(
            "Recursive predicate "^(string_of_symtkey key)^
            " does not contain a base case"
        ))
    else
    (*Check that there is no more than one recursive rule
     * and return list with recursion at the end*)
    match r_lst with
        | _::_::_ ->
            raise (SemErr (
                "Predicate "^(string_of_symtkey key)^
                " contains more than one recursive definition"
            ))
        | _ -> nr_lst@r_lst

(****************************************************)
(*      Constant extraction functions               *)
(****************************************************)

(** Extracts constants in a rterm and replaces them with
  ad-hoc NumVars which numbers start from 'pos'. The function
  returns a tuple with the new rterm and a list of equalities
  that must be satisfied by the ad-hoc variables.
 *)
let extract_rterm_constants ?(pos = 0) rt = match rt with
    | Pred (x, vl) ->
        let extract var (v_lst,e_lst,i) = match var with 
            | ConstVar const -> 
                let nvar = NumberedVar i in
                let neq = Equat (Equation("=", Var nvar,Const const)) in
                (nvar::v_lst, neq::e_lst,i+1) 
            | _ -> (var::v_lst, e_lst, i) in
        let (vars,eqs,_) = List.fold_right extract vl ([],[],pos) in 
        ( Pred (x,vars), eqs )
    | Deltainsert (x, vl) ->
        let extract var (v_lst,e_lst,i) = match var with 
            | ConstVar const -> 
                let nvar = NumberedVar i in
                let neq = Equat (Equation("=", Var nvar,Const const)) in
                (nvar::v_lst, neq::e_lst,i+1) 
            | _ -> (var::v_lst, e_lst, i) in
        let (vars,eqs,_) = List.fold_right extract vl ([],[],pos) in 
        ( Deltainsert (x,vars), eqs )
    | Deltadelete (x, vl) ->
        let extract var (v_lst,e_lst,i) = match var with 
            | ConstVar const -> 
                let nvar = NumberedVar i in
                let neq = Equat (Equation("=", Var nvar,Const const)) in
                (nvar::v_lst, neq::e_lst,i+1) 
            | _ -> (var::v_lst, e_lst, i) in
        let (vars,eqs,_) = List.fold_right extract vl ([],[],pos) in 
        ( Deltadelete (x,vars), eqs )

(** Given a rule, extracts all constants in its head and body rterms
  for replacing them with ad-hoc variables placed in the body.
  E.g. "Q(a,b,1) :- P(1,a,c) and R(b,3)." will be transformed to
  "Q(a,b,_0) :- P(_1,a,c) and R(b,_2) and _0 = 1 and _1 = 1 and _2 = 3."
 *)
let extract_rule_constants(h, b) = 
        let (h2, h2_e) = extract_rterm_constants h in
        let extract t (t_lst,e_lst,i) = match t with
            | Rel rt -> 
                let (rt2,rt2_e) = extract_rterm_constants ~pos:i rt in
                ((Rel rt2)::t_lst, rt2_e@e_lst, i+(List.length rt2_e) )
            | Not rt -> 
                let (rt2,rt2_e) = extract_rterm_constants ~pos:i rt in
                ((Not rt2)::t_lst, rt2_e@e_lst, i+(List.length rt2_e) )
            | _ -> (t::t_lst, e_lst, i) in
        let (b2,b2_e,_) = List.fold_right extract b ([],[],(List.length h2_e)) in
        (h2,b2@h2_e@b2_e)

(* let seperate_rules exp = match exp with 
    Expr.Prog lst -> List.partition (fun stt -> (match stt with Expr.Rule _ -> true | _ -> false) ) lst *)

(** Given a rule, replace AnonVar to a NamedVar*)
let anonvar2namedvar (h, b) =
        let a2n_varlist lst ind = 
            List.fold_right (fun v (l,i)  -> if (is_anon v) then ((NamedVar ("DUMMY__ANON_"^string_of_int i))::l, i+1) else (v::l), i ) lst ([], ind) in
        let a2n_term t (lst, ind)  = match t with 
            | Rel rt -> (match rt with 
                        | Pred (name, varlst) -> 
                            let nvars, newind = a2n_varlist varlst ind in 
                            (Rel (Pred (name, nvars))::lst, newind)
                        | Deltainsert (name, varlst) -> 
                            let nvars, newind = a2n_varlist varlst ind in 
                            (Rel (Deltainsert (name, nvars))::lst, newind)
                        | Deltadelete (name, varlst) -> 
                            let nvars, newind = a2n_varlist varlst ind in 
                            (Rel (Deltadelete (name, nvars))::lst, newind)
                        )
            | Not rt -> (t::lst, ind)
            (* AnonVar in a negated predicate can not be converted to NamedVar because it make program unsafe *)
            | Equat _ -> (t::lst, ind)
            | Noneq _ -> (t::lst, ind) in
        let newb,_ = List.fold_right a2n_term b ([], 0) in 
        (* print_endline (string_of_stt (Rule(h,newb)));  *)
        (h,newb)

(** Given a rule, replace string to a int defined by a mapping*)
let string2int mapping (h, b) =
        let s2i_rterm rt = match rt with 
            | Pred (name, varlst) -> 
                let new_varlst = List.map (function | ConstVar (String s) -> ConstVar (Int (mapping s)) | x -> x ) varlst in 
                (Pred (name, new_varlst))
            | Deltainsert (name, varlst) -> 
                let new_varlst = List.map (function | ConstVar (String s) -> ConstVar (Int (mapping s)) | x -> x ) varlst in 
                (Deltainsert (name, new_varlst))
            | Deltadelete (name, varlst) -> 
                let new_varlst = List.map (function | ConstVar (String s) -> ConstVar (Int (mapping s)) | x -> x ) varlst in 
                (Deltadelete (name, new_varlst)) in
        
        let s2i_eterm t = match t with 
            | Equation (op, Const (String s1), Const (String s2)) -> Equation (op, Const (Int (mapping s1)), Const (Int (mapping s2)))
            | Equation (op, Const (String s1), x) -> Equation (op, Const (Int (mapping s1)), x)
            | Equation (op, x, Const (String s2)) -> Equation (op, x, Const (Int (mapping s2)))
            | _ -> t in

        let s2i_term t = match t with 
            | Rel rt -> Rel (s2i_rterm rt) 
            | Not rt -> Not (s2i_rterm rt)
            | Equat e -> Equat (s2i_eterm e)
            | Noneq e -> Noneq (s2i_eterm e) in

        let newb  = List.map s2i_term b in 
        let newh  = s2i_rterm h in 
        (* print_endline (string_of_stt (Rule(h,newb)));  *)
        (newh,newb)


(****************************************************)
(*      Main function                               *)
(****************************************************)

(** Given a symtable, performs preprocess operations over the rules
  contained in it. This operations are:
   - Extraction of constants in predicates
   - Recursion checks (only one recursive rule,
                           only one recursive call in rules,
                           no negated recursions,
                           and base case existance)
                           
 *)
let preprocess_rules (st:symtable) =
    let rep_rule key rules =
        let rep_lst = List.map extract_rule_constants rules in
        let sorted_lst = check_rec_rule rep_lst in
        Hashtbl.replace st key sorted_lst in
    Hashtbl.iter rep_rule st

let extract_rterm_adom rt = 
    let vl = get_rterm_varlist rt in
    let extract var c_lst = match var with 
        | ConstVar const -> const::c_lst
        | _ -> c_lst in
    List.fold_right extract vl []

let extract_rule_adom (h, b) =
        let h_adom = extract_rterm_adom h in
        let extract t c_lst = match t with
            | Rel rt -> (extract_rterm_adom rt) @ c_lst
            | Not rt -> (extract_rterm_adom rt) @ c_lst
            | Equat (Equation (_, Const c1, Const c2)) 
            | Noneq (Equation(_, Const c1, Const c2)) -> c1::c2::c_lst
            | Equat (Equation (_, _, Const c)) | Equat (Equation (_, Const c, _))
            | Noneq (Equation (_, _, Const c)) | Noneq (Equation (_, Const c, _)) -> c::c_lst
            | _ -> c_lst in
        let b_adom = List.fold_right extract b [] in
        h_adom@b_adom

let extract_rules_adom rules =
    List.concat (List.map extract_rule_adom rules)

let extract_rules_string_adom rules =
    let lst = List.filter (function | String s -> true | _-> false) (extract_rules_adom rules) in 
    Lib.setify (List.map string_of_const lst)


let string2int_rules mapping rules =
    List.map (string2int mapping) rules


(** Given a symtable, replace AnonVar with a NamedVar*)
let anon2named_rules (st:symtable) =
    let rep_rule key rules =
        let named_lst = List.map anonvar2namedvar rules in
        Hashtbl.replace st key named_lst in
    Hashtbl.iter rep_rule st

(** Given a query, it returns a 'dummy' idb rule that calculates the desired output,
for example tracks2_prime(TRACK,0,RATING,ALBUM,QUANTITY) will be converted to _dummy_(TRACK,_0,RATING,ALBUM,QUANTITY):- ...
 *)
let rule_of_query query (idb:symtable) =
    let (q2,eqs) = extract_rterm_constants query in
    let dummy = Pred ("__dummy__", get_rterm_varlist q2) in
    (dummy, (Rel q2)::eqs)

(** Given a delta, it returns a 'dummy' idb rule that calculates the desired output, 
note that we take the difference of a deltainsert and its source 
and take the intersection of a deltadelete and its source.
*)
let rule_of_delta delta (idb:symtable) =
    let (q2,eqs) = extract_rterm_constants delta in
    let dummy = Pred ("__dummy__", get_rterm_varlist q2) in
    match delta with
        Deltainsert (pname, varlst) ->  (dummy, (Rel q2)::(Not (Pred(pname, get_rterm_varlist q2)))::eqs)
        | Deltadelete (pname, varlst) ->  (dummy, (Rel q2)::(Rel (Pred(pname, get_rterm_varlist q2)))::eqs)
        | _ -> raise (SemErr "the non_rec_unfold_sql_of_update is called without and delta predicate")
