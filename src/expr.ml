(* 
@author: Vandang Tran
*)

type expr = 
      | Prog of stt list
and term = 
    | Rel of rterm 
    | Equal of var * const  
    | Ineq of string * var * const 
    | Not of rterm
and const =
    | Int of int
    | Real of float
    | String of string
and rterm =  
    | Pred of string * var list 
    | Deltainsert of string * var list 
    | Deltadelete of string * var list 
and var = 
    | NamedVar of string 
    | NumberedVar of int 
    | ConstVar of const
    | AnonVar
    | AggVar of string * string
and stt =
    | Rule of rterm * term list
    | Query of rterm
    | Base of rterm
;;

(** get the predicate name of an rterm *)
let get_rterm_predname rterm = match rterm with
    | Pred (x, vl) -> x
    | Deltainsert (x, vl) -> "__dummy__delta__insert__"^ x
    | Deltadelete (x, vl) -> "__dummy__delta__delete__"^ x
;;

(** get the arity of an rterm *)
let get_arity rterm = match rterm with
    | Pred (x, vl) -> List.length vl
    | Deltainsert (x, vl) -> List.length vl
    | Deltadelete (x, vl) -> List.length vl
;;

(** get the arity of a rule *)
let get_rule_arity rule = match rule with
    | Rule (h, b) -> get_arity h
    | _ -> invalid_arg "function get_rule_arity called without a rule"
;;

(** get the predicate name of a term *)
let rec get_predname t = match t with
    | Rel r            -> get_rterm_predname r
    | _                -> invalid_arg "function get_predname called without a relation"
;;

(** get a rule's head predicate name *)
let get_rule_predname r = match r with
    | Rule(h, t) -> get_rterm_predname h
    | Query _    -> invalid_arg "function get_rule_predname called with a query"
    | Base _    -> invalid_arg "function rule_body called with a update"
;;

(** get a rule's head pred *)
let rule_head r = match r with
    | Rule(h, _) -> h
    | Query _    -> invalid_arg "function rule_head called with a query"
    | Base _    -> invalid_arg "function rule_body called with a update"
;;

(** get a rule's body list of terms *)
let rule_body r = match r with
    | Rule(_, t) -> t
    | Query _    -> invalid_arg "function rule_body called with a query"
    | Base _    -> invalid_arg "function rule_body called with a update"
;;

(** get rterm varlist *)
let get_rterm_varlist t = match t with
    | Pred (x, vl) -> vl
    | Deltainsert (x, vl) -> vl
    | Deltadelete (x, vl) -> vl
;;

(** get the list of variables of a term *)
let rec get_varlist t = match t with
    | Rel r            -> get_rterm_varlist r
    | Equal (s, i)     -> s :: []
    | Ineq  (op,s, i)  -> s :: []
    | Not r            -> get_rterm_varlist r
;;

(** Given a query, returns the rterm that is defined inside*)
let get_query_rterm (r:stt) = match r with
    | Query rt -> rt
    | _ -> invalid_arg "function get_query_rterm called without a query"
;;

(** Given a rule, returns all the positive and negative rterms
 * inside*)
let get_all_rule_rterms = function
    | Rule(_, t) ->
        let rec extract_rterm acc = function
            | Rel x -> x::acc
            | Not x -> x::acc
            | _ -> acc in
        List.fold_left extract_rterm [] t
    | Query _    -> invalid_arg "function get_all_rule_rterms called with a query"
    | Base _    -> invalid_arg "function rule_body called with a update"

(** Given a rule, returns all the negative rterms
 * inside*)
let get_all_negative_rule_rterms = function
    | Rule(_, t) ->
        let rec extract_rterm acc = function
            | Not x -> x::acc
            | _ -> acc in
        List.fold_left extract_rterm [] t
    | Query _    -> invalid_arg "function get_all_rule_rterms called with a query"
    | Base _    -> invalid_arg "function rule_body called with a update"

(** Given a rule, returns all the negative rterms
 * inside*)
let get_all_positive_rule_rterms = function
    | Rule(_, t) ->
        let rec extract_rterm acc = function
            | Rel x -> x::acc
            | _ -> acc in
        List.fold_left extract_rterm [] t
    | Query _    -> invalid_arg "function get_all_rule_rterms called with a query"
    | Base _    -> invalid_arg "function rule_body called with a update"

(** Given an equality, returns the (var,const) tuple that defines it *)
let extract_eq_tuple = function
    | Equal (v,c) -> (v,c)
    | _ -> invalid_arg "function extract_eq_tuple called without an equality"

(** Given an inequality, returns the (op,var,const) tuple that defines it *)
let extract_ineq_tuple = function
    | Ineq (s,v,c) -> (s,v,c)
    | _ -> invalid_arg "function extract_ineq_tuple called without an inequality"

(** Given an aggregated variable, returns the (function_name,var_name) tuple that defines it *)
let extract_aggvar_tuple = function
    | AggVar (fn,vn) -> (fn,vn)
    | _ -> invalid_arg "function extract_aggvar_tuple called without an aggregated var"

(*Given an equation, returns the equivalent of a negation of it*)
let negate_eq = function
    | Equal (v,c) -> Ineq ("<>",v,c)
    | Ineq ("<>",v,c) -> Equal (v,c)
    | Ineq ("<",v,c) -> Ineq (">=",v,c)
    | Ineq (">",v,c) -> Ineq ("<=",v,c)
    | Ineq ("<=",v,c) -> Ineq (">",v,c)
    | Ineq (">=",v,c) -> Ineq ("<",v,c)
    | _ -> invalid_arg "function negate_eq called without an equation"

(** Returns true if the provided argument is an aggregate variable *)
let is_aggvar = function
    | AggVar _ -> true
    | _ -> false

(** Returns true if the provided argument is an equality involving an
aggregate function*)
let is_agg_equality = function
    | Equal (AggVar _ , _ ) -> true
    | Equal _ -> false
    | _ -> invalid_arg "function is_agg_equality called without an equality"

(** Returns true if the provided argument is an inequality involving an
aggregate function*)
let is_agg_inequality = function
    | Ineq (_ , AggVar _ , _) -> true
    | Ineq _ -> false
    | _ -> invalid_arg "function is_agg_inequality called without an equality"

(** support function for smart stringify of the AST - see to_string below *)
let rec string_of_const t = match t with 
    | Int x -> string_of_int x 
    | Real x -> string_of_float x
    | String x -> x
;;

(** convert the var type into a string *)
let string_of_var r = match r with
    | NamedVar x -> x
    | NumberedVar x -> "_" ^ string_of_int x
    | AnonVar    -> "_"
    | ConstVar x -> string_of_const x
    | AggVar (fn,vn) -> fn^"("^vn^")"
;;

(** support function for smart stringify of the AST - see to_string below *)
let string_of_rterm r = match r with     
    | Pred (pn,vars) -> pn^"("^String.concat "," (List.map string_of_var vars)^")"
    | Deltainsert (pn,vars) -> "+"^ pn^"("^String.concat "," (List.map string_of_var vars)^")"
    | Deltadelete (pn,vars) -> "-"^pn^"("^String.concat "," (List.map string_of_var vars)^")"
;;

(** support function for smart stringify of the AST - see to_string below *)
let rec string_of_term t = match t with 
    | Rel r             -> string_of_rterm r
    | Equal (s, i)      -> string_of_var s ^ " = " ^ string_of_const i
    | Ineq (op,s, i)    -> string_of_var s ^ " " ^ op ^ " " ^ string_of_const i
    | Not rt            -> "not " ^ string_of_rterm rt
;;

(** support function for smart stringify of the AST - see to_string below *)
let string_of_stt st = match st with
    | Rule (p, tel)        -> string_of_rterm p ^ " :- " ^ 
                             String.concat " , " (List.map string_of_term tel) ^ ".\n"
    | Query r            -> "?- " ^ string_of_rterm r ^ ".\n"
    | Base r    -> "%s: " ^ string_of_rterm r ^ ".\n"
;;

(** smart stringify for AST *)
let to_string e = match e with
    | Prog []         -> invalid_arg "Passed empty program to stringify"
    | Prog stl        -> List.fold_right (^) (List.map string_of_stt stl) ""
;;

let str_to_namedvar = function str -> NamedVar str;;

let stringlist_to_varlist strlst = List.map str_to_namedvar strlst;; 

let string_of_prog = function
    | Prog stt_lst ->           
        String.concat "\n" (List.map string_of_stt stt_lst)