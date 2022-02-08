type const =
  | Int of int
  | Real of float
  | String of string
  | Bool of bool 
  | Null

type var = 
  | NamedVar of string 
  | NumberedVar of int (* this is not used in parser *)
  | ConstVar of const (* var in a literal in allowed to be a const like int or string, for example p(X,1) or p(X, 'tran') *)
  | AnonVar (* anonimous variable *)
  | AggVar of string * string (* the first string is function, the second is variable *)

type vterm = (* value term *)
  | Const of const
  | Var of var
  (* arithmetic expression *)
  | BinaryOp of string * vterm * vterm (* string is one of '+', '-', '*', '/', '^' *)
  | UnaryOp of string * vterm (* string is one of '-' *)

type eterm = (* equation *)
  | Equation of string * vterm * vterm (* string is one of '=', '<>', '<', '>', '<=', '>=' *)

type rterm = (* rterm is literal *)
  | Pred of string * var list (* string is name of predicate, var list is a list of variables *)
  | Deltainsert of string * var list (* delta predicate for insertion *)
  | Deltadelete of string * var list (* delta predicate for deletion *)

type term = (* term is one of predicate (positive or negative), equation, non-equation *)
  | Rel of rterm (* positive predicate *)
  | Not of rterm (* negative predicate *)
  | Equat of eterm  (* for example x = 5 *)
  | Noneq of eterm (* for example NOT x = 5 *)

type stype = (* data type in schema *)
  | Sint
  | Sreal
  | Sstring
  | Sbool

type rule = rterm * term list

type fact = rterm

type query = rterm

type source = string * (string * stype) list

type view = string * (string * stype) list

type constraint' = rterm * term list

type primary_key = string * string list

type expr = {
  rules: rule list;
  facts: fact list;
  query: query option;
  sources: source list;
  view: view option;
  constraints: constraint' list;
  primary_keys: primary_key list;
}

type conj_query =
  | Conj_query of var list * rterm list * rterm list

val get_empty_pred: rterm

val get_empty_expr: expr

type stt =
  | Stt_Rule of rule
  | Stt_Fact of fact
  | Stt_Query of query
  | Stt_Source of source (* the predicate of edb relation which is Source relation want to update *)
  | Stt_View of view
  | Stt_Constraint of constraint'
  | Stt_Pk of primary_key (* primary key *)

val add_stt : stt -> expr -> expr
val add_stts : stt list -> expr -> expr
val add_rules : rule list -> expr -> expr
val insert_stt : stt -> expr -> expr
val get_ins_delta_pred : rterm -> rterm
val get_del_delta_pred : rterm -> rterm
val get_new_source_rel_pred : rterm -> rterm
val get_source_rel_pred : rterm -> rterm
val get_empty_pred : rterm
val get_rterm_predname : rterm -> string
val is_rule_of_predname : String.t -> rterm * 'a -> bool
val delete_rule_of_predname : String.t -> expr -> expr
val is_fact_of_predname : String.t -> rterm -> bool
val delete_fact_of_predname : String.t -> expr -> expr
val view_schema_to_source_schema : expr -> expr
val is_defined_pred : String.t -> expr -> bool
val vterm2var : vterm -> var
val get_arity : rterm -> int
val get_rule_arity : rterm * 'a -> int
val get_predname : term -> string
val get_rule_predname : rterm * 'a -> string
val rule_head : 'a * 'b -> 'a
val rule_body : 'a * 'b -> 'b
val get_rterm_varlist : rterm -> var list
val get_vterm_varlist : vterm -> var list
val get_term_varlist : term -> var list
val get_schema_rterm : string * (string * 'a) list -> rterm
val get_schema_attrs : 'a * ('b * 'c) list -> 'b list
val get_schema_col_typs : 'a * 'b -> 'b
val get_schema_name : 'a * 'b -> 'a
val get_schema_stts : expr -> view list
val get_source_stts : expr -> source list
val get_all_rule_rterms : 'a * term list -> rterm list
val get_all_negative_rule_rterms : 'a * term list -> rterm list
val get_all_positive_rule_rterms : 'a * term list -> rterm list
val extract_eq_tuple : term -> vterm * vterm
val negate_eq : eterm -> eterm
val extract_ineq_tuple : term -> string * vterm * vterm
val extract_aggvar_tuple : var -> string * string
val negate_term : term -> term
val is_aggvar : var -> bool
val is_anon : var -> bool
val is_agg_equality : term -> bool
val is_agg_inequality : term -> bool
val string_of_const : const -> string
val string_of_var : var -> string
val string_of_rterm : rterm -> string
val string_of_vterm : vterm -> string
val string_of_eterm : eterm -> string
val string_of_term : term -> string
val string_of_stype : stype -> string
val string_of_source : string * (string * stype) list -> string
val string_of_view : string * (string * stype) list -> string
val string_of_query : rterm -> string
val string_of_constraint : rterm * term list -> string
val string_of_pk : string * string list -> string
val string_of_fact : rterm -> string
val string_of_rule : rterm * term list -> string
val to_string : expr -> string
val str_to_namedvar : string -> var
val stringlist_to_varlist : string list -> var list
val string_of_prog : expr -> string
val stype_of_const : const -> stype