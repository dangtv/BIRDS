
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

module PredicateDependencyGraph = Dependency_graph.Make(Predicate)

module Subst = Map.Make(String)

type substitution = named_var Subst.t

type intermediate_program = RuleAbstractionSet.t PredicateMap.t

type state = {
  current_max : int;
}

type error =
  | UnexpectedHeadVarForm  of var
  | UnexpectedBodyVarForm  of var
  | PredicateArityMismatch of int * int
  | CyclicDependency       of predicate_definition list


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
  (* Adds vertices corresponding to IDB predicates to the graph: *)
  let (graph, acc) =
    PredicateMap.fold (fun impred ruleabsset (graph, acc) ->
      match graph |> PredicateDependencyGraph.add_vertex impred ruleabsset with
      | Error _            -> assert false
      | Ok (graph, vertex) -> (graph, (impred, vertex, ruleabsset) :: acc)
    ) improg (PredicateDependencyGraph.empty, [])
  in

  (* Adds directed edges that represent dependencies among IDB predicates: *)
  let graph =
    acc |> List.rev |> List.fold_left (fun graph (impred_from, vertex_from, ruleabsset) ->
      let ruleabss = RuleAbstractionSet.elements ruleabsset in
      ruleabss |> List.fold_left (fun graph ruleabs ->
        ruleabs.body |> List.fold_left (fun graph clause ->
          match clause with
          | ImPositive (impred_to, _) | ImNegative (impred_to, _) ->
              begin
                match graph |> PredicateDependencyGraph.get_vertex impred_to with
                | Some vertex_to ->
                    (* If `impred_to` is an IDB predicate and thus registered to `graph` as `vertex_to`: *)
                    graph |> PredicateDependencyGraph.add_edge ~from:vertex_from ~to_:vertex_to

                | None ->
                    (* If `impred_to` is NOT an IDB predicate: *)
                    graph
              end

          | ImEquation _ | ImNonequation _ ->
              graph
        ) graph
      ) graph
    ) graph
  in

  PredicateDependencyGraph.topological_sort graph
    |> Result.map_error (fun cycle -> CyclicDependency cycle)


let substitute_argument (subst : substitution) (arg : named_var) : named_var =
  let ImNamedVar x = arg in
  match subst |> Subst.find_opt x with
  | Some var -> var
  | None     -> arg


let substitute_vterm (subst : substitution) (vterm : vterm) : vterm =
  match vterm with
  | Var (NamedVar x) ->
      begin
        match subst |> Subst.find_opt x with
        | Some (ImNamedVar y) -> Var (NamedVar y)
        | None                -> vterm
      end

  | _ ->
      vterm


let substitute_eterm (subst : substitution) (eterm : eterm) : eterm =
  let Equation (op, vt1, vt2) = eterm in
  Equation (op, vt1 |> substitute_vterm subst, vt2 |> substitute_vterm subst)


let substitute_clause (subst : substitution) (clause : intermediate_clause) : intermediate_clause =
  match clause with
  | ImPositive (impred, args) -> ImPositive (impred, args |> List.map (substitute_argument subst))
  | ImNegative (impred, args) -> ImNegative (impred, args |> List.map (substitute_argument subst))
  | ImEquation eterm          -> ImEquation (eterm |> substitute_eterm subst)
  | ImNonequation eterm       -> ImNonequation (eterm |> substitute_eterm subst)


let reduce_rule (ruleabs : rule_abstraction) (args : named_var list) : (intermediate_clause list, error) result =
  let open ResultMonad in
  let { binder; body } = ruleabs in
  match List.combine binder args with
  | exception Invalid_argument _ ->
      err (PredicateArityMismatch (List.length binder, List.length args))

  | zipped ->
      let subst =
        zipped |> List.fold_left (fun subst (ImNamedVar x, arg) ->
          subst |> Subst.add x arg
        ) Subst.empty
      in
      return (body |> List.map (substitute_clause subst))


let inline_rule_abstraction (improg_inlined : intermediate_program) (ruleabs : rule_abstraction) : (rule_abstraction list, error) result =
  let open ResultMonad in
  let { binder; body } = ruleabs in
  body |> foldM (fun (accs : (intermediate_clause list) list) (clause : intermediate_clause) ->
    match clause with
    | ImPositive (impred, args) ->
        begin
          match improg_inlined |> PredicateMap.find_opt impred with
          | Some ruleabsset ->
              let ruleabss = RuleAbstractionSet.elements ruleabsset in
              ruleabss |> mapM (fun ruleabs -> reduce_rule ruleabs args) >>= fun clausess ->
              return (accs |> List.map (fun acc ->
                clausess |> List.map (fun clauses -> List.rev_append clauses acc)
              ) |> List.concat)

          | None ->
            (* If `impred` is not an IDB predicate: *)
              return (accs |> List.map (fun acc -> clause :: acc))
        end

    | _ ->
      (* Clauses other than positive applications are not inlined: *)
        return (accs |> List.map (fun acc -> clause :: acc))

  ) [ [] ] >>= fun accs ->
  return (accs |> List.map (fun acc -> { binder; body = List.rev acc }))


let inject_rterm (impred : intermediate_predicate) (imvars : named_var list) : rterm =
  let vars = imvars |> List.map (fun (ImNamedVar x) -> NamedVar x) in
  match impred with
  | ImPred table        -> Pred (table, vars)
  | ImDeltaInsert table -> Deltainsert (table, vars)
  | ImDeltaDelete table -> Deltadelete (table, vars)


let inject_clause (clause : intermediate_clause) : term =
  match clause with
  | ImPositive (impred, args) -> Rel (inject_rterm impred args)
  | ImNegative (impred, args) -> Not (inject_rterm impred args)
  | ImEquation eterm          -> Equat eterm
  | ImNonequation eterm       -> Noneq eterm


let inject_rule (impred : intermediate_predicate) (ruleabs : rule_abstraction) : rule =
  let { binder; body } = ruleabs in
  let rterm = inject_rterm impred binder in
  let terms = body |> List.map inject_clause in
  (rterm, terms)


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
  resolve_dependencies_among_predicates improg >>= fun sorted_rules ->

  (* Performs inlining: *)
  sorted_rules |> foldM (fun improg_inlined (impred, ruleabsset) ->
    let ruleabss = RuleAbstractionSet.elements ruleabsset in
    ruleabss |> mapM (inline_rule_abstraction improg_inlined) >>= fun ruleabsss_inlined ->
    let ruleabss_inlined = List.concat ruleabsss_inlined in
    return (improg_inlined |> PredicateMap.add impred (RuleAbstractionSet.of_list ruleabss_inlined))
  ) PredicateMap.empty >>= fun improg_inlined ->

  (* Converts intermediate representations to rules: *)
  let acc =
    PredicateMap.fold (fun impred ruleabsset acc ->
      let ruleabss = RuleAbstractionSet.elements ruleabsset in
      ruleabss |> List.fold_left (fun acc ruleabs ->
        let rule = inject_rule impred ruleabs in
        rule :: acc
      ) acc
    ) improg_inlined []
  in
  return (List.rev acc)
