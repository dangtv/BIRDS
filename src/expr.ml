(**  Abstract Syntax Tree definition of the Datalog language, accessor/check/transformation functions on the ASTs
 *)
(* 
@author: Vandang Tran
*)

type expr = 
  | Prog of stt list
and stt =
  | Rule of rterm * term list
  | Query of rterm (* the goal predicate *)
  | Source of string * (string * stype) list (* the predicate of edb relation which is Source relation want to update*)
  | View of string * (string * stype) list
  | Constraint of rterm * term list
and rterm =  (* rterm is literal *)
  | Pred of string * var list (* string is name of predicate, var list is a list of variables*)
  | Deltainsert of string * var list (* delta predicate for insertion*)
  | Deltadelete of string * var list (* delta predicate for deletion*)
and term = (* term is one of predicate (positive or negative), equation, non-equation *)
  | Rel of rterm (* positive predicate *)
  | Equal of vterm * vterm  (* for example x = 5 *)
  | Ineq of string * vterm * vterm (* this is for other comparison, string is <>, >, <, >=, <=  *)
  | Not of rterm (* negative predicate *)
  (* need to extend eqal and ineq to arithemetic expression like the expression "col1-1 = 3+1" in select * from tracks2_prime where col1-1 = 3+1 *)
and var = 
  | NamedVar of string 
  | NumberedVar of int (* this is not used in parser *)
  | ConstVar of const (* var in a literal in allowed to be a const like int or string, for example p(X,1) or p(X, 'tran' *)
  | AnonVar (* anonimous variable *)
  | AggVar of string * string (* the first string is function, the second is variable *)
and const =
  | Int of int
  | Real of float
  | String of string
  | Bool of bool 
  | Null
and stype = Sint | Sreal | Sstring| Sbool (* data type in schema *)
(* and operator = Plus | Minus | Add | Sub | Mul multiply | Div | Lt | Gt | Le | Ge | Eq *)
and vterm (* value term *) = 
  (* arithmetic expression *)
  | Const of const | Var of var | Sum of vterm * vterm | Diff of vterm * vterm | Times of vterm * vterm 
  | Div of vterm * vterm | Neg of vterm 
  (* string expression *)
  | Concat of vterm * vterm 
  (* boolean expression *)
  | BoolAnd of vterm * vterm | BoolOr of vterm * vterm | BoolNot of vterm

(* and valexpr (* value expression *) = Armexpr of armexpr | Strexpr of strexpr
and armexpr (* arithmetic expression *) = ArmConst of const | ArmVar of var  
and strexpr (* string expression *) = StrConst of const | StrVar of var   *)

(* and armexpr (* arithmetic expression *) = Aterm of aterm | Plus of armexpr * aterm  | Minus of armexpr * aterm 
and aterm (* arithmetic term *) = Factor of factor | Times of aterm * factor | Div of  aterm * factor 
and factor (* arithmetic factor *)  =  Const of const | Neg of const |  *)

(* to be continued... *)
(* need to and arithmetic expression for term of equal and ineq (comparasion) 
https://ronsavage.github.io/SQL/sql-2003-2.bnf.html#value%20expression
*)
(* to add delta relation: delta relation have to be the head of rule
delta relation have to have the same name as a update of rterm
when add delta rule to symtable, just convert name of a delta relation by add symbol '+' before its predicate name, we can do that because now we only allow alphabet name for a normal predicate
we can get the delta rule, delta rterm by match rterm with Deltapred
we can get pred name of delta relation by change some functions of getting pred name 
 *)
;;

(****************************************************
 *
 *  AST accessor functions
 *
 ****************************************************)

(** get the predicate name of an rterm *)
let get_rterm_predname rterm = match rterm with
    | Pred (x, vl) -> x
    | Deltainsert (x, vl) -> "Δ_ins_"^ x
    | Deltadelete (x, vl) -> "Δ_del_"^ x
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
    | Source _    -> invalid_arg "function get_rule_predname called with a source schema"
    | View _ -> invalid_arg "function get_rule_predname called with a view schema"
    | Constraint _ -> invalid_arg "function get_rule_predname called with a constraint"
;;

(** get a rule's head pred *)
let rule_head r = match r with
    | Rule(h, _) -> h
    | Query _    -> invalid_arg "function rule_head called with a query"
    | Source _    -> invalid_arg "function rule_head called with a source schema"
    | View _    -> invalid_arg "function rule_head called with a view schema"
    | Constraint _    -> invalid_arg "function rule_head called with a constraint"
;;

(** get a rule's body list of terms *)
let rule_body r = match r with
    | Rule(_, t) -> t
    | Query _    -> invalid_arg "function rule_body called with a query"
    | Source _    -> invalid_arg "function rule_body called with a source schema"
    | View _ -> invalid_arg "function rule_body called with a view schema"
    | Constraint _ -> invalid_arg "function rule_body called with a constraint"
;;

(** get rterm varlist *)
let get_rterm_varlist t = match t with
    | Pred (x, vl) -> vl
    | Deltainsert (x, vl) -> vl
    | Deltadelete (x, vl) -> vl
;;

let rec get_vterm_varlist e = match e with 
    | Const const -> []
    | Var var ->  [var]
    | Sum (ae1, ae2) -> (get_vterm_varlist ae1) @ (get_vterm_varlist ae2) 
    | Diff (ae1, ae2) -> (get_vterm_varlist ae1) @ (get_vterm_varlist ae2) 
    | Times (ae1, ae2) -> (get_vterm_varlist ae1) @ (get_vterm_varlist ae2) 
    | Div (ae1, ae2) -> (get_vterm_varlist ae1) @ (get_vterm_varlist ae2) 
    | Neg ae -> get_vterm_varlist ae
    | Concat(ae1, ae2) -> (get_vterm_varlist ae1) @ (get_vterm_varlist ae2)
    | BoolAnd (ae1, ae2) -> (get_vterm_varlist ae1) @ (get_vterm_varlist ae2)
    | BoolOr (ae1, ae2) -> (get_vterm_varlist ae1) @ (get_vterm_varlist ae2)
    | BoolNot ae -> get_vterm_varlist ae

(** get the list of variables of a term *)
let rec get_term_varlist t = match t with
    | Rel r            -> get_rterm_varlist r
    | Equal (e1, e2)     -> (get_vterm_varlist e1) @ (get_vterm_varlist e2)
    | Ineq  (op,e1, e2)  -> (get_vterm_varlist e1) @ (get_vterm_varlist e2)
    | Not r            -> get_rterm_varlist r
;;

(** Given a query, returns the rterm that is defined inside*)
let get_query_rterm (r:stt) = match r with
    | Query rt -> rt
    | _ -> invalid_arg "function get_query_rterm called without a query"
;;

(** Given a schema declaration (source and view), returns the rterm that is defined inside*)
let get_schema_rterm (r:stt) = match r with
    | Source (name, lst) 
    | View (name, lst)  -> Pred(name, (List.map (fun (col,typ) -> NamedVar col) lst))
    | _ -> invalid_arg "function get_schema_rterm called without an schema"
;;

(** Given a schema declaration (source and view), returns the list of column:typ  *)
let get_schema_col_typs (r:stt) = match r with
    | Source (name, lst) -> lst
    | View (name, lst)  -> lst
    | _ -> invalid_arg "function get_schema_col_typs called without an schema"
;;

(** Given a schema declaration (source and view), returns the schema name  *)
let get_schema_name (r:stt) = match r with
    | Source (name, lst) -> name
    | View (name, lst)  -> name
    | _ -> invalid_arg "function get_schema_name called without an schema"
;;

(** Given program return all schema statement*)
let get_schema_stts prog = match prog with
    | Prog sttlst -> List.filter (fun x -> match x with Source _ -> true | View _ -> true | _ -> false) sttlst
;;

(** Given program return all source statement*)
let get_source_stts prog = match prog with
    | Prog sttlst -> List.filter (fun x -> match x with Source _ -> true | _ -> false) sttlst
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
    | Source _    -> invalid_arg "function get_all_rule_rterms called with a source schema"
    | View _ -> invalid_arg "function get_all_rule_rterms called with a view schema"
    | Constraint _ -> invalid_arg "function get_all_rule_rterms called with a constraint"

(** Given a rule, returns all the negative rterms
 * inside*)
let get_all_negative_rule_rterms = function
    | Rule(_, t) ->
        let rec extract_rterm acc = function
            | Not x -> x::acc
            | _ -> acc in
        List.fold_left extract_rterm [] t
    | Query _    -> invalid_arg "function get_all_rule_rterms called with a query"
    | Source _    -> invalid_arg "function get_all_rule_rterms called with a source schema"
    | View _ -> invalid_arg "function get_all_rule_rterms called with a view schema"
    | Constraint _ -> invalid_arg "function get_all_rule_rterms called with a constraint"

(** Given a rule, returns all the negative rterms
 * inside*)
let get_all_positive_rule_rterms = function
    | Rule(_, t) ->
        let rec extract_rterm acc = function
            | Rel x -> x::acc
            | _ -> acc in
        List.fold_left extract_rterm [] t
    | Query _    -> invalid_arg "function get_all_rule_rterms called with a query"
    | Source _    -> invalid_arg "function get_all_rule_rterms called with a source schema"
    | View _    -> invalid_arg "function get_all_rule_rterms called with a view schema"
    | Constraint _    -> invalid_arg "function get_all_rule_rterms called with a Constraint"

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

(****************************************************
 *
 *  AST check / transformation functions
 *
 ****************************************************)

(*Given an equation, returns the equivalent of a negation of it*)
let negate_eq = function
    | Equal (v,c) -> Ineq ("<>",v,c)
    | Ineq ("<>",v,c) -> Equal (v,c)
    | Ineq ("<",v,c) -> Ineq (">=",v,c)
    | Ineq (">",v,c) -> Ineq ("<=",v,c)
    | Ineq ("<=",v,c) -> Ineq (">",v,c)
    | Ineq (">=",v,c) -> Ineq ("<",v,c)
    | _ -> invalid_arg "function negate_eq called without an equation"

(*Given an term, returns the equivalent of a negation of it*)
let negate_term term= match term with
      Rel (rt) -> Not (rt)
    | Not (rt) -> Rel (rt)
    | _ -> negate_eq term

(** Returns true if the provided argument is an aggregate variable *)
let is_aggvar = function
    | AggVar _ -> true
    | _ -> false

(** Returns true if the provided argument is an anonymous variable *)
let is_anon = function
    | AnonVar -> true
    | _ -> false

(** Returns true if the provided argument is an equality involving an
aggregate function*)
let is_agg_equality = function
    | Equal (e1, e2) -> (List.length (List.filter is_aggvar ((get_vterm_varlist e1) @ (get_vterm_varlist e2)))) > 0
    | _ -> invalid_arg "function is_agg_equality called without an equality"

(** Returns true if the provided argument is an inequality involving an
aggregate function*)
let is_agg_inequality = function
    | Ineq (_ , e1, e2) -> (List.length (List.filter is_aggvar ((get_vterm_varlist e1) @ (get_vterm_varlist e2)))) > 0
    | _ -> invalid_arg "function is_agg_inequality called without an equality"

(****************************************************
 *
 *  String operations
 *
 ****************************************************)

(** support function for smart stringify of the AST - see to_string below *)
let rec string_of_const t = match t with 
    | Int x -> string_of_int x 
    | Real x -> string_of_float x
    | String x -> x
    | Bool x -> string_of_bool x
    | Null -> "null"
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
    | Pred (pn,vars) -> pn^"("^String.concat ", " (List.map string_of_var vars)^")"
    | Deltainsert (pn,vars) -> "+"^ pn^"("^String.concat ", " (List.map string_of_var vars)^")"
    | Deltadelete (pn,vars) -> "-"^pn^"("^String.concat ", " (List.map string_of_var vars)^")"
;;

(** convert the vterm type into a string *)
let string_of_vterm ae =
    (* open and close parentthesis in current expresion by using the priority of the expression containing the current expression 
    opem_paren and close_paren take two priorities of previous operator and current operator
    higher priority means earlier evaluation
    "+" give the same priority of 0 to its two terms, and priority of 0 for the term contain this "+"
    "-" give the priority 0 to the first term and priority 1 to the second term (the first term should be evaluated first), and priority of 0 for the term contain this "-"
    similarly, "*" give its two terms the priority 2, "/" give its two terms the priority 2 and 3
    "-" which is minus sign has highest priority 
     *)
  let open_paren prec op_prec = 
    if prec > op_prec then  "(" else "" in 
  let close_paren prec op_prec = 
    if prec > op_prec then  ")" else "" in
  let rec str_of prec aexp = 
    match aexp with 
      Const c -> string_of_const c
    | Var v -> string_of_var v
    | Sum(f,g) -> (open_paren prec 0)^ (str_of 0 f) ^ "+" ^ (str_of 0 g) ^ (close_paren prec 0)
    | Diff(f,g) -> (open_paren prec 0) ^ (str_of 0 f) ^  "-" ^ (str_of 1 g) ^ (close_paren prec 0)
    | Times(f,g) -> (open_paren prec 2) ^ (str_of 2 f) ^  "*" ^ (str_of 2 g) ^ (close_paren prec 2)
    | Div (f,g) -> (open_paren prec 2)^ (str_of 2 f) ^ "/" ^ (str_of 3 g) ^ (close_paren prec 2)
    | Neg e ->  (open_paren prec 4)^ "-" ^ (str_of 5 e)^(close_paren prec 4)
    | Concat(f,g) -> (open_paren prec 0)^ (str_of 0 f) ^ "^" ^ (str_of 0 g) ^ (close_paren prec 0)
    | BoolAnd (f,g) -> (open_paren prec 2) ^ (str_of 2 f) ^  "*" ^ (str_of 2 g) ^ (close_paren prec 2)
    | BoolOr (f,g) -> (open_paren prec 0)^ (str_of 0 f) ^ "+" ^ (str_of 0 g) ^ (close_paren prec 0)
    | BoolNot e ->  (open_paren prec 4)^ "-" ^ (str_of 5 e)^(close_paren prec 4)
  in str_of 0 ae;;
;;

(** support function for smart stringify of the AST - see to_string below *)
let rec string_of_term t = match t with 
    | Rel r             -> string_of_rterm r
    | Equal (e1, e2)      -> (string_of_vterm e1) ^ " = " ^ (string_of_vterm e2)
    | Ineq (op,e1, e2)    -> (string_of_vterm e1) ^ " " ^ op ^ " " ^ (string_of_vterm e2)
    | Not rt            -> "not " ^ string_of_rterm rt
;;

let string_of_stype t = match t with 
    Sint -> "int"
    | Sreal -> "real"
    | Sstring -> "string"
    | Sbool -> "bool"

(** support function for smart stringify of the AST - see to_string below *)
let string_of_stt st = match st with
    | Rule (p, tel)        -> string_of_rterm p ^ " :- " ^ 
                             String.concat " , " (List.map string_of_term tel) ^ ".\n"
    | Query r            -> "?- " ^ string_of_rterm r ^ ".\n"
    | Source (name, lst)   -> "source " ^ name ^ "(" ^ String.concat "," (List.map (fun (col,typ) -> col^":"^ (string_of_stype typ)) lst) ^ ").\n" 
    | View (name, lst)    -> "view " ^ name ^ "(" ^ String.concat "," (List.map (fun (col,typ) -> col^":"^ (string_of_stype typ)) lst) ^ ").\n"
    | Constraint (p, tel)        -> string_of_rterm p ^ " :- " ^ 
                             String.concat " , " (List.map string_of_term tel) ^ ".\n"
;;

(** smart stringify for AST *)
let to_string e = match e with
    | Prog []         -> invalid_arg "Passed empty program to stringify"
    | Prog stl        -> List.fold_right (^) (List.map string_of_stt stl) ""
;;

let str_to_namedvar = function str -> NamedVar str;;

let stringlist_to_varlist strlst = List.map str_to_namedvar strlst;; 

(** convert datalog program to string  *)
let string_of_prog = function
    | Prog stt_lst ->           
        String.concat "" (List.map string_of_stt stt_lst)

let stype_of_const c = match c with 
    Int _ -> Sint 
    | Real _ -> Sreal 
    | String _ -> Sstring 
    | Bool _ -> Sbool
    | Null -> invalid_arg "Null does not have type"

(** Take a delta rterm and return a dummy rterm of new source  *)
let get_new_source_rel_pred del_rterm = match del_rterm with 
    | Pred (x, vl) | Deltainsert (x, vl) | Deltadelete (x, vl) -> Pred("__dummy_new_"^ x,vl)

(** Take a delta rterm and return a rterm of source relation *)
let get_source_rel_pred del_rterm = match del_rterm with 
    | Pred (x, vl) | Deltainsert (x, vl) | Deltadelete (x, vl) -> Pred(x,vl)

(** take a constraint and return an equivalent rule *)
let rule_of_constraint c = match c with
    | Constraint (h,b)    -> Rule(h,b)
    | Rule _ -> invalid_arg "function rule_of_constraint called with a rule"
    | Query _    -> invalid_arg "function rule_of_constraint called with a query"
    | Source _    -> invalid_arg "function rule_of_constraint called with a source schema"
    | View _    -> invalid_arg "function rule_of_constraint called with a view schema"

let get_empty_pred = Pred ("⊥", [])
;;

let constraint2rule prog = match prog with 
    | Prog stt_lst ->
    let trans_stt t = match t with
      | Rule _ -> t 
      | Constraint _ -> rule_of_constraint t
      | Query _ -> t
      | Source _ -> t
      | View _ -> t in   
    Prog(List.map trans_stt stt_lst)         
;;

(** add a list of stt to a program *)
let add_stts lst prog = match prog with
    | Prog sttlst -> Prog(sttlst@lst)

(** insert a new statement *)
let insert_stt stt prog = add_stts [stt] prog;;

let is_rule_of_predname predname stt = match stt with 
    | Rule(h, b) -> (String.compare (get_rterm_predname h)  predname == 0)
    | Query _   
    | Source _    
    | View _ 
    | Constraint _ -> false

(** delete all rule of a predname *)
let delete_rule_of_predname predname prog = match prog with
    | Prog sttlst -> 
        (* print_endline ("delete rule of "^predname); 
        print_endline (string_of_prog (Prog (List.filter (fun x -> not (is_rule_of_predname predname x)) sttlst)) );  *)
        Prog (List.filter (fun x -> not (is_rule_of_predname predname x)) sttlst)
;;