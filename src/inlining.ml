
open Expr
open Utils


type named_var =
  | ImNamedVar of string

type intermediate_predicate =
  | ImPred        of table_name
  | ImDeltaInsert of table_name
  | ImDeltaDelete of table_name

type intermediate_clause =
  | ImPositive    of intermediate_predicate * named_var list
  | ImNegative    of intermediate_predicate * named_var list
  | ImEquation    of eterm
  | ImNonequation of eterm

(* The type for rule abstractions,
  i.e. data of the form `(X_1, ..., X_n) -> C_1, ..., C_m.` *)
type rule_abstraction = {
  binder : named_var list;
  body   : intermediate_clause list;
}

module RuleAbstraction = struct
  type t = rule_abstraction

  let compare = compare
end

module RuleAbstractionSet = Set.Make(RuleAbstraction)

type predicate_definition =
  intermediate_predicate * RuleAbstractionSet.t

module Predicate = struct
  type t = intermediate_predicate

  let compare (impred1 : t) (impred2 : t) : int =
    match (impred1, impred2) with
    | (ImPred t1, ImPred t2)               -> String.compare t1 t2
    | (ImPred _, _)                        -> 1
    | (_, ImPred _)                        -> -1
    | (ImDeltaInsert t1, ImDeltaInsert t2) -> String.compare t1 t2
    | (ImDeltaInsert _, _)                 -> 1
    | (_, ImDeltaInsert _)                 -> -1
    | (ImDeltaDelete t1, ImDeltaDelete t2) -> String.compare t1 t2
end

module PredicateMap = Map.Make(Predicate)

type intermediate_program = RuleAbstractionSet.t PredicateMap.t

type state = {
  current_max : int;
}

type error =
  | UnexpectedHeadVarForm of var
  | UnexpectedBodyVarForm of var


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


let convert_var (state : state) (var : var) : (state * named_var, error) result =
  let open ResultMonad in
  match var with
  | NamedVar x ->
      return (state, ImNamedVar x)

  | AnonVar ->
      let (state, imvar) = generate_fresh_name state in
      return (state, imvar)

  | _ ->
      err (UnexpectedBodyVarForm var)


let convert_rterm (state : state) (rterm : rterm) : (state * intermediate_predicate * named_var list, error) result =
  let open ResultMonad in
  let (impred, vars) = separate_predicate_and_vars rterm in
  vars |> foldM (fun (state, imvar_acc) var ->
    convert_var state var >>= fun (state, imvar) ->
    return (state, imvar :: imvar_acc)
  ) (state, []) >>= fun (state, imvar_acc) ->
  return (state, impred, List.rev imvar_acc)


let convert_body_clause (state : state) (term : term) : (state * intermediate_clause, error) result =
  let open ResultMonad in
  match term with
  | Rel rterm ->
      convert_rterm state rterm >>= fun (state, impred, imvars) ->
      return (state, ImPositive (impred, imvars))

  | Not rterm ->
      convert_rterm state rterm >>= fun (state, impred, imvars) ->
      return (state, ImNegative (impred, imvars))

  | Equat eterm ->
      return (state, ImEquation eterm)

  | Noneq eterm ->
      return (state, ImNonequation eterm)


let convert_rule (state : state) (rule : rule) : (state * intermediate_predicate * rule_abstraction, error) result =
  let open ResultMonad in
  let (head, body) = rule in
  convert_head_rterm head >>= fun (impred, binder) ->
  body |> foldM (fun (state, imclause_acc) term ->
    convert_body_clause state term >>= fun (state, imclause) ->
    return (state, imclause :: imclause_acc)
  ) (state, []) >>= fun (state, imclause_acc) ->
  let body = List.rev imclause_acc in
  let ruleabs = { binder; body } in
  return (state, impred, ruleabs)


(* Adds a mapping `(impred |-> ruleabs)` to `improg` *)
let add_rule_abstraction (impred : intermediate_predicate) (ruleabs : rule_abstraction) (improg : intermediate_program) : intermediate_program =
  match improg |> PredicateMap.find_opt impred with
  | None ->
      improg |> PredicateMap.add impred (RuleAbstractionSet.singleton ruleabs)

  | Some ruleabsset ->
      improg |> PredicateMap.add impred (ruleabsset |> RuleAbstractionSet.add ruleabs)


let resolve_dependencies_among_predicates (improg : intermediate_program) : (predicate_definition list, error) result =
  failwith "TODO: implement this"


let inline_rules (rules : rule list) : (rule list, error) result =
  let open ResultMonad in

  (* Converts rules into intermediate ones
     by substituting each occurrence of the anonymous variable with fresh variables: *)
  let state = { current_max = 0 } in
  rules |> foldM (fun (state, improg) rule ->
    convert_rule state rule >>= fun (state, impred, ruleabs) ->
    return (state, improg |> add_rule_abstraction impred ruleabs)
  ) (state, PredicateMap.empty) >>= fun (_state, improg) ->

  (* Extracts dependencies among IDB predicates and perform a topological sorting: *)
  resolve_dependencies_among_predicates improg >>= fun _sorted_rules ->
  failwith "TODO: implement this"
