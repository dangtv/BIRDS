
open Expr
open Utils


type table_name = string

type var_name = string

type intermediate_predicate =
  | ImPred        of table_name
  | ImDeltaInsert of table_name
  | ImDeltaDelete of table_name

type intermediate_head_var =
  | ImHeadVar of var_name

type intermediate_body_var =
  | ImBodyNamedVar of var_name
  | ImBodyAnonVar

type intermediate_equation =
  | ImEquation of var_name * const


module PredicateMap = Map.Make(struct
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
end)


type predicate_map = ((intermediate_body_var list) list) PredicateMap.t

type intermediate_rule = {
  head           : intermediate_predicate * intermediate_head_var list;
  positive_terms : predicate_map;
  negative_terms : predicate_map;
  equations      : intermediate_equation list;
}

type error =
  | UnexpectedHeadVarForm   of var
  | UnexpectedBodyVarForm   of var
  | UnsupportedEquation     of eterm
  | NonequalityNotSupported of eterm


let convert_head_var (var : var) : (intermediate_head_var, error) result =
  let open ResultMonad in
  match var with
  | NamedVar x -> return (ImHeadVar x)
  | _          -> err (UnexpectedHeadVarForm var)


let convert_body_var (var : var) : (intermediate_body_var, error) result =
  let open ResultMonad in
  match var with
  | NamedVar x -> return (ImBodyNamedVar x)
  | AnonVar    -> return ImBodyAnonVar
  | _          -> err (UnexpectedBodyVarForm var)


let separate_predicate_and_vars (rterm : rterm) : intermediate_predicate * var list =
  match rterm with
  | Pred (t, vars)        -> (ImPred t, vars)
  | Deltainsert (t, vars) -> (ImDeltaInsert t, vars)
  | Deltadelete (t, vars) -> (ImDeltaDelete t, vars)


let convert_head_rterm (rterm : rterm) : (intermediate_predicate * intermediate_head_var list, error) result =
  let open ResultMonad in
  let (impred, vars) = separate_predicate_and_vars rterm in
  vars |> mapM convert_head_var >>= fun imhvars ->
  return (impred, imhvars)


let convert_body_rterm (rterm : rterm) : (intermediate_predicate * intermediate_body_var list, error) result =
  let open ResultMonad in
  let (impred, vars) = separate_predicate_and_vars rterm in
  vars |> mapM convert_body_var >>= fun imbvars ->
  return (impred, imbvars)


let convert_eterm (eterm : eterm) : (intermediate_equation, error) result =
  let open ResultMonad in
  match eterm with
  | Equation("=", Var (NamedVar x), Const c)          -> return (ImEquation (x, c))
  | Equation("=", Var (NamedVar x), Var (ConstVar c)) -> return (ImEquation (x, c))
  | Equation("=", Const c, Var (NamedVar x))          -> return (ImEquation (x, c))
  | Equation("=", Var (ConstVar c), Var (NamedVar x)) -> return (ImEquation (x, c))
  | _                                                 -> err (UnsupportedEquation eterm)


let extend_predicate_map (impred : intermediate_predicate) (args : intermediate_body_var list) (predmap : predicate_map) : predicate_map =
  let argss =
    match predmap |> PredicateMap.find_opt impred with
    | None        -> []
    | Some(argss) -> argss
  in
  predmap |> PredicateMap.add impred (args :: argss)


let convert_rule (rule : rule) : (intermediate_rule, error) result =
  let open ResultMonad in
  let (head, body) = rule in
  convert_head_rterm head >>= fun imhead ->
  body |> foldM (fun (predmap_pos, predmap_neg, eqn_acc) term ->
    match term with
    | Rel rterm ->
        convert_body_rterm rterm >>= fun (impred, imbvars) ->
        let predmap_pos = predmap_pos |> extend_predicate_map impred imbvars in
        return (predmap_pos, predmap_neg, eqn_acc)

    | Not rterm ->
        convert_body_rterm rterm >>= fun (impred, imbvars) ->
        let predmap_neg = predmap_neg |> extend_predicate_map impred imbvars in
        return (predmap_pos, predmap_neg, eqn_acc)

    | Equat eterm ->
        convert_eterm eterm >>= fun eqn ->
        return (predmap_pos, predmap_neg, eqn :: eqn_acc)

    | Noneq eterm ->
        err (NonequalityNotSupported eterm)

  ) (PredicateMap.empty, PredicateMap.empty, []) >>= fun (predmap_pos, predmap_neg, eqn_acc) ->
  return {
    head           = imhead;
    positive_terms = predmap_pos;
    negative_terms = predmap_neg;
    equations      = List.rev eqn_acc;
  }


let rule_equal (imrule1 : intermediate_rule) (imrule2 : intermediate_rule) : bool =
  true (* TODO: implement this *)


let simplify_rule_step (imrule : intermediate_rule) : intermediate_rule =
  failwith "TODO: simplify_rule_step"


let rec simplify_rule_recursively (imrule1 : intermediate_rule) : intermediate_rule =
  let imrule2 = simplify_rule_step imrule1 in
  if rule_equal imrule1 imrule2 then
  (* If the simplification reaches a fixpoint: *)
    imrule2
  else
    simplify_rule_recursively imrule2


let simplify (rules : rule list) =
  let open ResultMonad in
  rules |> mapM convert_rule >>= fun imrules ->

  (* Performs per-rule simplification: *)
  let _imrules = imrules |> List.map simplify_rule_recursively in

  failwith "TODO: simplify"
