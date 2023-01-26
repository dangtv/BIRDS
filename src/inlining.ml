
open Expr
open Utils


type named_var =
  | ImNamedVar of string

type intermediate_predicate =
  | ImPred        of table_name
  | ImDeltaInsert of table_name
  | ImDeltaDelete of table_name

type intermediate_clause =
  | ImClause of intermediate_predicate * named_var list

(* The type for data of the form `(X_1, ..., X_n) -> C_1, ..., C_m.` (i.e., a rule without a predicate name) *)
type rule_abstraction = {
  binder : named_var list;
  body   : intermediate_clause list;
}

type error =
  | UnexpectedHeadVarForm of var


type state = {
  current_max : int;
}


let separate_predicate_and_vars (rterm : rterm) : intermediate_predicate * var list =
  match rterm with
  | Pred (t, vars)        -> (ImPred t, vars)
  | Deltainsert (t, vars) -> (ImDeltaInsert t, vars)
  | Deltadelete (t, vars) -> (ImDeltaDelete t, vars)


let convert_head_var (var : var) : (named_var, error) result =
  let open ResultMonad in
  match var with
  | NamedVar x -> return (ImNamedVar x)
  | _          -> err (UnexpectedHeadVarForm var)


let convert_head_rterm (rterm : rterm) : (intermediate_predicate * named_var list, error) result =
  let open ResultMonad in
  let (impred, vars) = separate_predicate_and_vars rterm in
  vars |> mapM convert_head_var >>= fun imvars ->
  return (impred, imvars)


let generate_fresh_name (state : state) : state * named_var =
  let i = state.current_max + 1 in
  let imvar = ImNamedVar (Printf.sprintf "%d" i) in
  ({ current_max = i }, imvar)


let convert_body_clause (state : state) (term : term) : state * intermediate_clause =
  failwith "TODO: implement this by using `generate_fresh_name`"


let convert_rule (state : state) (rule : rule) : (state * intermediate_predicate * rule_abstraction, error) result =
  let open ResultMonad in
  let (head, body) = rule in
  convert_head_rterm head >>= fun (impred, binder) ->
  let (state, imclause_acc) =
    body |> List.fold_left (fun (state, imclause_acc) term ->
      let (state, imclause) = convert_body_clause state term in
      (state, imclause :: imclause_acc)
    ) (state, [])
  in
  let body = List.rev imclause_acc in
  let ruleabs = { binder; body } in
  return (state, impred, ruleabs)


let inline_rules (rules : rule list) : (rule list, error) result =
  let open ResultMonad in

  (* Converts rules into intermediate ones
     by substituting each occurrence of the anonymous variable with fresh variables. *)
  let state = { current_max = 0 } in
  rules |> foldM (fun (state, improg) rule ->
    convert_rule state rule >>= fun (state, impred, ruleabs) ->
    failwith "TODO: add a mapping `(impred |-> ruleabs)` to `improg` here"
  ) (state, []) >>= fun (_state, _improg) ->

  failwith "TODO: implement this"
