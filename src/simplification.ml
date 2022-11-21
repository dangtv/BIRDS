
open Expr
open Utils


let const_equal (c1 : const) (c2 : const) : bool =
  match (c1, c2) with
  | (Int n1, Int n2)       -> Int.equal n1 n2
  | (Real r1, Real r2)     -> Float.equal r1 r2 (* Not conform to IEEE754’s equality *)
  | (String s1, String s2) -> String.equal s1 s2
  | (Bool b1, Bool b2)     -> Bool.equal b1 b2
  | (Null, Null)           -> true
  | _                      -> false


type table_name = string

type var_name = string

type intermediate_predicate =
  | ImPred        of table_name
  | ImDeltaInsert of table_name
  | ImDeltaDelete of table_name

type intermediate_head_var =
  | ImHeadVar of var_name


let head_var_equal (ImHeadVar x1) (ImHeadVar x2) =
  String.equal x1 x2


type intermediate_body_var =
  | ImBodyNamedVar of var_name
  | ImBodyAnonVar


let body_var_compare (imbvar1 : intermediate_body_var) (imbvar2 : intermediate_body_var) : int =
  match (imbvar1, imbvar2) with
  | (ImBodyNamedVar x1, ImBodyNamedVar x2) -> String.compare x1 x2
  | (ImBodyNamedVar _, ImBodyAnonVar)      -> 1
  | (ImBodyAnonVar, ImBodyAnonVar)         -> 0
  | (ImBodyAnonVar, ImBodyNamedVar _)      -> -1


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


module VariableMap = Map.Make(String)


type body_term_arguments = intermediate_body_var list


module BodyTermArguments = struct
  type t = body_term_arguments

  let compare =
    List.compare body_var_compare
end

module BodyTermArgumentsSet = Set.Make(BodyTermArguments)


type predicate_map = BodyTermArgumentsSet.t PredicateMap.t

type equation_map = const VariableMap.t

type intermediate_rule = {
  head_predicate : intermediate_predicate;
  head_arguments : intermediate_head_var list;
  positive_terms : predicate_map;
  negative_terms : predicate_map;
  equations      : equation_map;
}

type error =
  | UnexpectedHeadVarForm   of var
  | UnexpectedBodyVarForm   of var
  | UnsupportedEquation     of eterm
  | NonequalityNotSupported of eterm


let predicate_equal (impred1 : intermediate_predicate) (impred2 : intermediate_predicate) : bool =
  match (impred1, impred2) with
  | (ImPred t1, ImPred t2)               -> String.equal t1 t2
  | (ImDeltaInsert t1, ImDeltaInsert t2) -> String.equal t1 t2
  | (ImDeltaDelete t1, ImDeltaDelete t2) -> String.equal t1 t2
  | _                                    -> false


let predicate_map_equal : predicate_map -> predicate_map -> bool =
  PredicateMap.equal BodyTermArgumentsSet.equal


let rule_equal (imrule1 : intermediate_rule) (imrule2 : intermediate_rule) : bool =
  List.fold_left ( && ) true [
    predicate_equal imrule1.head_predicate imrule2.head_predicate;
    List.equal head_var_equal imrule1.head_arguments imrule2.head_arguments;
    predicate_map_equal imrule1.positive_terms imrule2.positive_terms;
    predicate_map_equal imrule1.negative_terms imrule2.negative_terms;
    VariableMap.equal const_equal imrule1.equations imrule2.equations;
  ]


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


let convert_eterm (eterm : eterm) : (var_name * const, error) result =
  let open ResultMonad in
  match eterm with
  | Equation("=", Var (NamedVar x), Const c)          -> return (x, c)
  | Equation("=", Var (NamedVar x), Var (ConstVar c)) -> return (x, c)
  | Equation("=", Const c, Var (NamedVar x))          -> return (x, c)
  | Equation("=", Var (ConstVar c), Var (NamedVar x)) -> return (x, c)
  | _                                                 -> err (UnsupportedEquation eterm)


let extend_predicate_map (impred : intermediate_predicate) (args : intermediate_body_var list) (predmap : predicate_map) : predicate_map =
  let argsset =
    match predmap |> PredicateMap.find_opt impred with
    | None          -> BodyTermArgumentsSet.empty
    | Some(argsset) -> argsset
  in
  predmap |> PredicateMap.add impred (argsset |> BodyTermArgumentsSet.add args)


let check_equation_map (x : var_name) (c : const) (eqnmap : equation_map) : equation_map option =
  match eqnmap |> VariableMap.find_opt x with
  | None ->
      Some (eqnmap |> VariableMap.add x c)

  | Some c0 ->
      if const_equal c0 c then
        Some eqnmap
      else
        None


(* Converts rules to intermediate ones.
   The application `convert_rule rule` returns:
   - `Error _` if `rule` is syntactically incorrect (or in unsupported form),
   - `Ok None` if it turns out that `rule` is syntactically correct but
     obviously unsatisfiable according to its equations, or
   - `Ok (Some imrule)` otherwise, i.e., if `rule` can be successfully converted to `imrule`. *)
let convert_rule (rule : rule) : (intermediate_rule option, error) result =
  let open ResultMonad in
  let (head, body) = rule in
  convert_head_rterm head >>= fun (impred_head, imhvars) ->
  body |> foldM (fun opt term ->
    match opt with
    | None ->
        return None

    | Some (predmap_pos, predmap_neg, eqnmap) ->
        begin
          match term with
          | Rel rterm ->
              convert_body_rterm rterm >>= fun (impred, imbvars) ->
              let predmap_pos = predmap_pos |> extend_predicate_map impred imbvars in
              return (Some (predmap_pos, predmap_neg, eqnmap))

          | Not rterm ->
              convert_body_rterm rterm >>= fun (impred, imbvars) ->
              let predmap_neg = predmap_neg |> extend_predicate_map impred imbvars in
              return (Some (predmap_pos, predmap_neg, eqnmap))

          | Equat eterm ->
              convert_eterm eterm >>= fun (x, c) ->
              begin
                match eqnmap |> check_equation_map x c with
                | None ->
                  (* If it turns out that the list of equations are unsatisfiable: *)
                    return None

                | Some eqnmap ->
                    return (Some (predmap_pos, predmap_neg, eqnmap))
              end

          | Noneq eterm ->
              err (NonequalityNotSupported eterm)
        end
  ) (Some (PredicateMap.empty, PredicateMap.empty, VariableMap.empty)) >>= fun opt ->
  return (opt |> Option.map (fun (predmap_pos, predmap_neg, eqnmap) ->
    {
      head_predicate = impred_head;
      head_arguments = imhvars;
      positive_terms = predmap_pos;
      negative_terms = predmap_neg;
      equations      = eqnmap;
    }))


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

  (* Converts each rule to an intermediate rule (with unsatisfiable ones removed): *)
  rules |> foldM (fun imrule_acc rule ->
    convert_rule rule >>= function
    | None        -> return imrule_acc
    | Some imrule -> return (imrule :: imrule_acc)
  ) [] >>= fun imrule_acc ->
  let imrules = List.rev imrule_acc in

  (* Performs per-rule simplification: *)
  let _imrules = imrules |> List.map simplify_rule_recursively in

  failwith "TODO: simplify"
