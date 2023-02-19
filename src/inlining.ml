
open Expr
open Utils


(** The prefix used for variables generated during inlining. *)
let generated_variable_prefix = "GenV"


type named_var =
  | ImNamedVar of string

type intermediate_predicate =
  | ImPred        of table_name
  | ImDeltaInsert of table_name
  | ImDeltaDelete of table_name

type intermediate_argument =
  | ImNamedVarArg of named_var

type intermediate_clause =
  | ImPositive    of intermediate_predicate * intermediate_argument list
  | ImNegative    of intermediate_predicate * intermediate_argument list
  | ImEquation    of eterm
  | ImNonequation of eterm

(** The type for rule abstractions,
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

type substitution = intermediate_argument Subst.t

type intermediate_program = RuleAbstractionSet.t PredicateMap.t

(** The type for states that hold information for generating fresh variables. *)
type state = {
  current_max : int;
}

type error =
  | UnexpectedHeadVarForm  of var
  | UnexpectedBodyVarForm  of var
  | PredicateArityMismatch of int * int
  | CyclicDependency       of predicate_definition list


let string_of_intermediate_predicate = function
  | ImPred table        -> Printf.sprintf "%s" table
  | ImDeltaInsert table -> Printf.sprintf "+%s" table
  | ImDeltaDelete table -> Printf.sprintf "-%s" table


let string_of_error = function
  | UnexpectedHeadVarForm var ->
      Printf.sprintf "unexpected head var form: %s" (string_of_var var)

  | UnexpectedBodyVarForm var ->
      Printf.sprintf "unexpected body var form: %s" (string_of_var var)

  | PredicateArityMismatch (expected, got) ->
      Printf.sprintf "predicate arity mismatch; expected %d but got %d" expected got

  | CyclicDependency defs ->
      let s =
        defs |> List.map (fun (impred, _) ->
          string_of_intermediate_predicate impred
        ) |> String.concat ", "
      in
      Printf.sprintf "cyclic dependency found among %s" s


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


(** Generates fresh variables for instantiation. *)
let generate_fresh_name (state : state) : state * named_var =
  let i = state.current_max + 1 in
  let imvar = ImNamedVar (Printf.sprintf "%s%d" generated_variable_prefix i) in
  ({ current_max = i }, imvar)


let convert_body_var (state : state) (var : var) : (state * intermediate_argument, error) result =
  let open ResultMonad in
  match var with
  | NamedVar x ->
      return (state, ImNamedVarArg (ImNamedVar x))

  | AnonVar ->
      (* Replaces an occurrence `_` of anonymous variables with a fresh variable: *)
      let (state, imvar) = generate_fresh_name state in
      return (state, ImNamedVarArg imvar)

  | _ ->
      err (UnexpectedBodyVarForm var)
        (* TODO: support conversion of `ConstVar` *)


let convert_body_rterm (state : state) (rterm : rterm) : (state * intermediate_predicate * intermediate_argument list, error) result =
  let open ResultMonad in
  let (impred, vars) = separate_predicate_and_vars rterm in
  vars |> foldM (fun (state, imarg_acc) var ->
    convert_body_var state var >>= fun (state, imarg) ->
    return (state, imarg :: imarg_acc)
  ) (state, []) >>= fun (state, imarg_acc) ->
  return (state, impred, List.rev imarg_acc)


let convert_body_clause (state : state) (term : term) : (state * intermediate_clause, error) result =
  let open ResultMonad in
  match term with
  | Rel rterm ->
      convert_body_rterm state rterm >>= fun (state, impred, imargs) ->
      return (state, ImPositive (impred, imargs))

  | Not rterm ->
      convert_body_rterm state rterm >>= fun (state, impred, imvars) ->
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


(** Adds a mapping `(impred |-> ruleabs)` to `improg`. *)
let add_rule_abstraction (impred : intermediate_predicate) (ruleabs : rule_abstraction) (improg : intermediate_program) : intermediate_program =
  match improg |> PredicateMap.find_opt impred with
  | None ->
      improg |> PredicateMap.add impred (RuleAbstractionSet.singleton ruleabs)

  | Some ruleabsset ->
      improg |> PredicateMap.add impred (ruleabsset |> RuleAbstractionSet.add ruleabs)


(** Performs topological sorting of IDB predicates based on the dependencies among them. *)
let resolve_dependencies_among_predicates (improg : intermediate_program) : (predicate_definition list, error) result =
  (* Adds vertices corresponding to IDB predicates to the graph: *)
  let (graph, acc) =
    PredicateMap.fold (fun impred ruleabsset (graph, acc) ->
      match graph |> PredicateDependencyGraph.add_vertex impred ruleabsset with
      | Error _            -> assert false
      | Ok (graph, vertex) -> (graph, (impred, vertex, ruleabsset) :: acc)
    ) improg (PredicateDependencyGraph.empty, [])
  in

  (* Adds directed edges that represent dependencies among IDB predicates.
     Here, `impred_to` depends on `impred_from`: *)
  let graph =
    acc |> List.rev |> List.fold_left (fun graph (impred_to, vertex_to, ruleabsset) ->
      let ruleabss = RuleAbstractionSet.elements ruleabsset in
      ruleabss |> List.fold_left (fun graph ruleabs ->
        ruleabs.body |> List.fold_left (fun graph clause ->
          match clause with
          | ImPositive (impred_from, _) | ImNegative (impred_from, _) ->
              begin
                match graph |> PredicateDependencyGraph.get_vertex impred_from with
                | Some vertex_from ->
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

  (* Performs topological sorting according to the dependency graph: *)
  PredicateDependencyGraph.topological_sort graph
    |> Result.map_error (fun cycle -> CyclicDependency cycle)


(** Basically, `intern_argument state subst X` returns `subst(X)`
    if `X` belongs to the domain of `subst`.
    Otherwise, the application generates a fresh variable `Y` and returns it,
    extending `subst` with `(X |-> Y)`. *)
let intern_argument (state : state) (subst : substitution) (imarg : intermediate_argument) : state * substitution * intermediate_argument =
  let ImNamedVarArg (ImNamedVar x) = imarg in
  match subst |> Subst.find_opt x with
  | Some imarg_to ->
      (state, subst, imarg_to)

  | None ->
      let (state, imvar) = generate_fresh_name state in
      let imarg_to = ImNamedVarArg imvar in
      let subst = subst |> Subst.add x imarg_to in
      (state, subst, imarg_to)


let substitute_vterm (state : state) (subst : substitution) (vterm : vterm) : state * substitution * vterm =
  match vterm with
  | Var (NamedVar x) ->
      begin
        match subst |> Subst.find_opt x with
        | Some (ImNamedVarArg (ImNamedVar y)) ->
            (state, subst, Var (NamedVar y))

        | None ->
            let (state, imvar) = generate_fresh_name state in
            let ImNamedVar y = imvar in
            let subst = subst |> Subst.add x (ImNamedVarArg imvar) in
            (state, subst, Var (NamedVar y))
      end

  | _ ->
      (state, subst, vterm)


let substitute_eterm (state : state) (subst : substitution) (eterm : eterm) : state * substitution * eterm =
  let Equation (op, vt1, vt2) = eterm in
  let (state, subst, vt1_to) = vt1 |> substitute_vterm state subst in
  let (state, subst, vt2_to) = vt2 |> substitute_vterm state subst in
  (state, subst, Equation (op, vt1_to, vt2_to))


let intern_argument_list (state : state) (subst : substitution) (imargs : intermediate_argument list) : state * substitution * intermediate_argument list =
  let (state, subst, imarg_to_acc) =
    imargs |> List.fold_left (fun (state, subst, imarg_to_acc) imarg ->
      let (state, subst, imarg_to) = imarg |> intern_argument state subst in
      (state, subst, imarg_to :: imarg_to_acc)
    ) (state, subst, [])
  in
  (state, subst, List.rev imarg_to_acc)


let substitute_clause (state : state) (subst : substitution) (clause : intermediate_clause) : state * substitution * intermediate_clause =
  match clause with
  | ImPositive (impred, imargs) ->
      let (state, subst, imargs_to) = imargs |> intern_argument_list state subst in
      (state, subst, ImPositive (impred, imargs_to))

  | ImNegative (impred, imargs) ->
      let (state, subst, imargs_to) = imargs |> intern_argument_list state subst in
      (state, subst, ImNegative (impred, imargs_to))

  | ImEquation eterm ->
      let (state, subst, eterm_to) = eterm |> substitute_eterm state subst in
      (state, subst, ImEquation eterm_to)

  | ImNonequation eterm ->
      let (state, subst, eterm_to) = eterm |> substitute_eterm state subst in
      (state, subst, ImNonequation eterm_to)


(** Basically, `reduce_rule state ((X_1, ..., X_n) -> C_1, ..., C_m.) [Y_1, ..., Y_n]` returns
    an array of clauses `{Y_n/X_n, ..., Y_1/X_1}(C_1, ..., C_m)`
    (where `{_/_}_` denotes the standard capture-avoiding substitution operation).
    In addition, free variables occurring in C_1, ..., C_m are instantiated by fresh variables beforehand. *)
let reduce_rule (state : state) (ruleabs : rule_abstraction) (imargs : intermediate_argument list) : (state * intermediate_clause list, error) result =
  let open ResultMonad in
  let { binder; body } = ruleabs in
  match List.combine binder imargs with
  | exception Invalid_argument _ ->
      err (PredicateArityMismatch (List.length binder, List.length imargs))

  | zipped ->
      (* Initializes a substitution by bound variables: *)
      let subst =
        zipped |> List.fold_left (fun subst (ImNamedVar x, imarg) ->
          subst |> Subst.add x imarg
        ) Subst.empty
      in
      (* Traverses body clauses and substitute variables occurring in them: *)
      let (state, _subst, clause_to_acc) =
        body |> List.fold_left (fun (state, subst, clause_to_acc) clause ->
          let (state, subst, clause_to) = clause |> substitute_clause state subst in
          (state, subst, clause_to :: clause_to_acc)
        ) (state, subst, [])
      in
      return (state, List.rev clause_to_acc)


(** `inline_rule_abstraction state improg_inlined ruleabs` performs inlining of `ruleabs`
    by using `improg_inlined`, which consists of "already inlined" definitions. *)
let inline_rule_abstraction (state : state) (improg_inlined : intermediate_program) (ruleabs : rule_abstraction) : (state * rule_abstraction list, error) result =
  let open ResultMonad in
  let { binder; body } = ruleabs in
  body |> foldM (fun ((state, accs) : state * (intermediate_clause list) list) (clause : intermediate_clause) ->
    match clause with
    | ImPositive (impred, imargs) ->
        begin
          match improg_inlined |> PredicateMap.find_opt impred with
          | Some ruleabsset ->
            (* If `impred` is an IDB predicate associated with abstractions `ruleabsset`: *)
              let ruleabss = RuleAbstractionSet.elements ruleabsset in
              ruleabss |> foldM (fun (state, clauses_acc) ruleabs ->
                reduce_rule state ruleabs imargs >>= fun (state, clauses) ->
                return (state, clauses :: clauses_acc)
              ) (state, []) >>= fun (state, clauses_acc) ->
              let clausess = List.rev clauses_acc in
              return (state, accs |> List.map (fun acc ->
                clausess |> List.map (fun clauses -> List.rev_append clauses acc)
              ) |> List.concat)

          | None ->
            (* If `impred` is not an IDB predicate: *)
              return (state, accs |> List.map (fun acc -> clause :: acc))
        end

    | _ ->
      (* Clauses other than positive applications are not inlined: *)
        return (state, accs |> List.map (fun acc -> clause :: acc))

  ) (state, [ [] ]) >>= fun (state, accs) ->
  return (state, accs |> List.map (fun acc -> { binder; body = List.rev acc }))


let inject_rterm (impred : intermediate_predicate) (imargs : intermediate_argument list) : rterm =
  let vars = imargs |> List.map (fun (ImNamedVarArg (ImNamedVar x)) -> NamedVar x) in
  match impred with
  | ImPred table        -> Pred (table, vars)
  | ImDeltaInsert table -> Deltainsert (table, vars)
  | ImDeltaDelete table -> Deltadelete (table, vars)


let inject_clause (clause : intermediate_clause) : term =
  match clause with
  | ImPositive (impred, imargs) -> Rel (inject_rterm impred imargs)
  | ImNegative (impred, imargs) -> Not (inject_rterm impred imargs)
  | ImEquation eterm            -> Equat eterm
  | ImNonequation eterm         -> Noneq eterm


let inject_rule (impred : intermediate_predicate) (ruleabs : rule_abstraction) : rule =
  let { binder; body } = ruleabs in
  let rterm = binder |> List.map (fun var -> ImNamedVarArg var) |> inject_rterm impred in
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
  ) (state, PredicateMap.empty) >>= fun (state, improg) ->

  (* Extracts dependencies among IDB predicates and perform a topological sorting: *)
  resolve_dependencies_among_predicates improg >>= fun sorted_rules ->

  (* Performs inlining: *)
  sorted_rules |> foldM (fun (state, improg_inlined) (impred, ruleabsset) ->
    let ruleabss = RuleAbstractionSet.elements ruleabsset in
    ruleabss |> foldM (fun (state, ruleabss_inlined) ruleabs ->
      ruleabs |> inline_rule_abstraction state improg_inlined >>= fun (state, ruleabss_inlined_new) ->
      return (state, List.append ruleabss_inlined ruleabss_inlined_new)
      ) (state, []) >>= fun (state, ruleabss_inlined) ->
    return (state, improg_inlined |> PredicateMap.add impred (RuleAbstractionSet.of_list ruleabss_inlined))
  ) (state, PredicateMap.empty) >>= fun (_state, improg_inlined) ->

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
