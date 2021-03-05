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

let get_empty_pred = Pred ("⊥", [])

let get_empty_expr = {
  rules= [];
  facts= [];
  query= None;
  sources= [];
  view= None;
  constraints= [];
  primary_keys= [];
}

type stt =
  | Stt_Rule of rule
  | Stt_Fact of fact
  | Stt_Query of query
  | Stt_Source of source (* the predicate of edb relation which is Source relation want to update *)
  | Stt_View of view
  | Stt_Constraint of constraint'
  | Stt_Pk of primary_key (* primary key *)

let add_stt stt expr = match stt with
  | Stt_Rule rule -> { expr with rules= rule :: expr.rules }
  | Stt_Fact fact -> { expr with facts= fact :: expr.facts }
  | Stt_Query query -> begin match expr.query with
      | Some _ -> invalid_arg "Query should appear at most once"
      | None -> { expr with query= Some query }
    end
  | Stt_Source source -> { expr with sources= source :: expr.sources }
  | Stt_View view -> begin match expr.view with
      | Some _ -> invalid_arg "View should appear at most once"
      | None -> { expr with view= Some view }
    end
  | Stt_Constraint constraint' -> { expr with constraints= constraint' :: expr.constraints }
  | Stt_Pk primary_key -> { expr with primary_keys= primary_key :: expr.primary_keys }