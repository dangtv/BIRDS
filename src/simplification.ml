
open Expr
open Utils


module Const = struct
  type t = const

  let compare (c1 : const) (c2 : const) : int =
    match (c1, c2) with
    | (Int n1, Int n2)       -> Int.compare n1 n2
    | (Int _, _)             -> 1
    | (_, Int _)             -> -1
    | (Real r1, Real r2)     -> Float.compare r1 r2
    | (Real _, _)            -> 1
    | (_, Real _)            -> -1
    | (String s1, String s2) -> String.compare s1 s2
    | (String _, _)          -> 1
    | (_, String _)          -> -1
    | (Bool b1, Bool b2)     -> Bool.compare b1 b2
    | (Bool _, _)            -> 1
    | (_, Bool _)            -> -1
    | (Null, Null)           -> 0

  (* Note: The equality of floats is NOT conform to IEEE754’s equality *)
  let equal (c1 : const) (c2 : const) : bool =
    compare c1 c2 = 0
end

module ConstSet = Set.Make(Const)


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

module PredicateSet = Set.Make(Predicate)

module VariableMap = Map.Make(String)


type body_term_arguments = intermediate_body_var list


let string_of_body_term_arguments imbvars =
  imbvars |> List.map (function
  | ImBodyNamedVar x -> x
  | ImBodyAnonVar    -> "_"
  ) |> String.concat ", "


module BodyTermArguments = struct
  type t = body_term_arguments

  let compare =
    List.compare body_var_compare
end

module BodyTermArgumentsSet = Set.Make(BodyTermArguments)


type predicate_map = BodyTermArgumentsSet.t PredicateMap.t

type constant_requirement =
  | EqualTo    of const
  | NotEqualTo of ConstSet.t

type equation_map = constant_requirement VariableMap.t

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


let constant_requirement_equal (cr1 : constant_requirement) (cr2 : constant_requirement) : bool =
  match (cr1, cr2) with
  | (EqualTo c1, EqualTo c2)             -> Const.equal c1 c2
  | (NotEqualTo cset1, NotEqualTo cset2) -> ConstSet.equal cset1 cset2
  | _                                    -> false


let predicate_equal (impred1 : intermediate_predicate) (impred2 : intermediate_predicate) : bool =
  match (impred1, impred2) with
  | (ImPred t1, ImPred t2)               -> String.equal t1 t2
  | (ImDeltaInsert t1, ImDeltaInsert t2) -> String.equal t1 t2
  | (ImDeltaDelete t1, ImDeltaDelete t2) -> String.equal t1 t2
  | _                                    -> false


let predicate_map_equal : predicate_map -> predicate_map -> bool =
  PredicateMap.equal BodyTermArgumentsSet.equal


(* Checks that `imrule1` and `imrule2` are syntactically equal
   (i.e. exactly the same including variable names). *)
let rule_equal (imrule1 : intermediate_rule) (imrule2 : intermediate_rule) : bool =
  List.fold_left ( && ) true [
    predicate_equal imrule1.head_predicate imrule2.head_predicate;
    List.equal head_var_equal imrule1.head_arguments imrule2.head_arguments;
    predicate_map_equal imrule1.positive_terms imrule2.positive_terms;
    predicate_map_equal imrule1.negative_terms imrule2.negative_terms;
    VariableMap.equal constant_requirement_equal imrule1.equations imrule2.equations;
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


let convert_body_rterm (rterm : rterm) : (intermediate_predicate * body_term_arguments, error) result =
  let open ResultMonad in
  let (impred, vars) = separate_predicate_and_vars rterm in
  vars |> mapM convert_body_var >>= fun imbvars ->
  return (impred, imbvars)


let convert_eterm ~(negated : bool) (eterm : eterm) : (var_name * constant_requirement, error) result =
  let open ResultMonad in
  begin
    match eterm with
    | Equation("=", Var (NamedVar x), Const c)           -> return (x, true, c)
    | Equation("=", Var (NamedVar x), Var (ConstVar c))  -> return (x, true, c)
    | Equation("=", Const c, Var (NamedVar x))           -> return (x, true, c)
    | Equation("=", Var (ConstVar c), Var (NamedVar x))  -> return (x, true, c)
    | Equation("<>", Var (NamedVar x), Const c)          -> return (x, false, c)
    | Equation("<>", Var (NamedVar x), Var (ConstVar c)) -> return (x, false, c)
    | Equation("<>", Const c, Var (NamedVar x))          -> return (x, false, c)
    | Equation("<>", Var (ConstVar c), Var (NamedVar x)) -> return (x, false, c)
    | _                                                  -> err (UnsupportedEquation eterm)
  end >>= fun (x, equal, c) ->
  let equal = if negated then not equal else equal in
  if equal then
    return (x, EqualTo c)
  else
    return (x, NotEqualTo (ConstSet.singleton c))


let extend_predicate_map (impred : intermediate_predicate) (args : body_term_arguments) (predmap : predicate_map) : predicate_map =
  let argsset =
    match predmap |> PredicateMap.find_opt impred with
    | None          -> BodyTermArgumentsSet.empty
    | Some(argsset) -> argsset
  in
  predmap |> PredicateMap.add impred (argsset |> BodyTermArgumentsSet.add args)


let check_equation_map (x : var_name) (cr : constant_requirement) (eqnmap : equation_map) : equation_map option =
  match eqnmap |> VariableMap.find_opt x with
  | None ->
      Some (eqnmap |> VariableMap.add x cr)

  | Some cr0 ->
      begin
        match (cr0, cr) with
        | (EqualTo c0, EqualTo c) ->
            if Const.equal c0 c then
              Some eqnmap
            else
              None

        | (NotEqualTo cset0, EqualTo c) ->
            if cset0 |> ConstSet.mem c then
              None
            else
              Some (eqnmap |> VariableMap.add x (EqualTo c))

        | (EqualTo c0, NotEqualTo cset) ->
            if cset |> ConstSet.mem c0 then
              None
            else
              Some eqnmap

        | (NotEqualTo cset0, NotEqualTo cset) ->
            Some (eqnmap |> VariableMap.add x (NotEqualTo (ConstSet.union cset0 cset)))
      end


(* Converts rules to intermediate ones.
   The application `convert_rule rule` returns:
   - `Error _` if `rule` is syntactically incorrect (or in unsupported forms),
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
              convert_eterm ~negated:false eterm >>= fun (x, cr) ->
              begin
                match eqnmap |> check_equation_map x cr with
                | None ->
                  (* If it turns out that the list of equations is unsatisfiable: *)
                    return None

                | Some eqnmap ->
                    return (Some (predmap_pos, predmap_neg, eqnmap))
              end

          | Noneq eterm ->
              convert_eterm ~negated:true eterm >>= fun (x, cr) ->
              begin
                match eqnmap |> check_equation_map x cr with
                | None ->
                  (* If it turns out that the list of equations is unsatisfiable: *)
                    return None

                | Some eqnmap ->
                    return (Some (predmap_pos, predmap_neg, eqnmap))
              end
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


let revert_head (impred : intermediate_predicate) (imhvars : intermediate_head_var list) : rterm =
  let vars = imhvars |> List.map (function ImHeadVar x -> NamedVar x) in
  match impred with
  | ImPred t        -> Pred (t, vars)
  | ImDeltaInsert t -> Deltainsert (t, vars)
  | ImDeltaDelete t -> Deltadelete (t, vars)


let revert_body_term ~(positive : bool) (impred : intermediate_predicate) (args : body_term_arguments) : term =
  let vars =
    args |> List.map (function
    | ImBodyNamedVar x -> NamedVar x
    | ImBodyAnonVar    -> AnonVar
    )
  in
  let rterm =
    match impred with
    | ImPred t        -> Pred (t, vars)
    | ImDeltaInsert t -> Deltainsert (t, vars)
    | ImDeltaDelete t -> Deltadelete (t, vars)
  in
  if positive then
    Rel rterm
  else
    Not rterm


let revert_body_terms ~(positive : bool) ((impred, argsset) : intermediate_predicate * BodyTermArgumentsSet.t) : term list =
  let argss = argsset |> BodyTermArgumentsSet.elements in
  argss |> List.map (revert_body_term ~positive impred)


let revert_rule (imrule : intermediate_rule) : rule =
  let { head_predicate; head_arguments; positive_terms; negative_terms; equations } = imrule in
  let head = revert_head head_predicate head_arguments in
  let terms_pos =
    positive_terms |> PredicateMap.bindings |> List.map (revert_body_terms ~positive:true) |> List.concat
  in
  let terms_neg =
    negative_terms |> PredicateMap.bindings |> List.map (revert_body_terms ~positive:false) |> List.concat
  in
  let terms_eq =
    equations |> VariableMap.bindings |> List.concat_map (fun (x, cr) ->
      match cr with
      | EqualTo c ->
          [ Equat (Equation ("=", Var (NamedVar x), Const c)) ]

      | NotEqualTo cset ->
          cset |> ConstSet.elements |> List.map (fun c ->
            Noneq (Equation ("=", Var (NamedVar x), Const c))
          )
    )
  in
  let body = List.concat [ terms_pos; terms_neg; terms_eq ] in
  (head, body)


type occurrence_count_map = int VariableMap.t


let increment_occurrence_count (x : var_name) (count_map : occurrence_count_map) : occurrence_count_map =
  match count_map |> VariableMap.find_opt x with
  | None       -> count_map |> VariableMap.add x 1
  | Some count -> count_map |> VariableMap.add x (count + 1)


let has_only_one_occurrence (count_map : occurrence_count_map) (x : var_name) : bool =
  match count_map |> VariableMap.find_opt x with
  | Some 1 -> true
  | _      -> false


let fold_predicate_map_for_counting (predmap : predicate_map) (count_map : occurrence_count_map) : occurrence_count_map =
  PredicateMap.fold (fun impred argsset count_map ->
    BodyTermArgumentsSet.fold (fun args count_map ->
      args |> List.fold_left (fun count_map arg ->
        match arg with
        | ImBodyNamedVar x -> count_map |> increment_occurrence_count x
        | ImBodyAnonVar    -> count_map
      ) count_map
    ) argsset count_map
  ) predmap count_map


let erase_sole_occurrences_in_predicate_map (count_map : occurrence_count_map) (predmap : predicate_map) : predicate_map =
  predmap |> PredicateMap.map (fun argsset ->
    argsset |> BodyTermArgumentsSet.map (fun args ->
      args |> List.map (fun arg ->
        match arg with
        | ImBodyNamedVar x ->
            if x |> has_only_one_occurrence count_map then
              let () = Printf.printf "**** REMOVE THE SOLE OCCURRENCE OF %s\n" x in (* TODO: remove this *)
              ImBodyAnonVar
            else
              arg

      | ImBodyAnonVar ->
          arg
      )
    )
  )


let erase_sole_occurrences (imrule : intermediate_rule) : intermediate_rule =
  let { head_predicate; head_arguments; positive_terms; negative_terms; equations } = imrule in

  (* Counts occurrence of each variables: *)
  let count_map =
    VariableMap.empty
      |> fold_predicate_map_for_counting positive_terms
      |> fold_predicate_map_for_counting negative_terms
      |> VariableMap.fold (fun x _c count_map -> count_map |> increment_occurrence_count x) equations
  in

  (* Removes variables occurring in head arguments: *)
  let count_map =
    head_arguments |> List.fold_left (fun count_map (ImHeadVar x) ->
      count_map |> VariableMap.remove x
    ) count_map
  in

  (* Converts variables that have only one occurrence with the underscore: *)
  let positive_terms = positive_terms |> erase_sole_occurrences_in_predicate_map count_map in
  let negative_terms = negative_terms |> erase_sole_occurrences_in_predicate_map count_map in
  let equations =
    VariableMap.fold (fun x c equations_new ->
      if x |> has_only_one_occurrence count_map then
        let () = Printf.printf "**** REMOVE THE SOLE OCCURRENCE OF %s IN EQ\n" x in (* TODO: remove this *)
        equations_new
      else
        equations_new |> VariableMap.add x c
    ) equations VariableMap.empty
  in
  { head_predicate; head_arguments; positive_terms; negative_terms; equations }


let is_looser ~than:(args1 : body_term_arguments) (args2 : body_term_arguments) : bool =
  let b = (* TODO: remove this line *)
  match List.combine args1 args2 with
  | exception Invalid_argument _ ->
      false

  | zipped ->
      zipped |> List.for_all (function
      | (_, ImBodyAnonVar)                     -> true
      | (ImBodyAnonVar, ImBodyNamedVar _)      -> false
      | (ImBodyNamedVar x1, ImBodyNamedVar x2) -> String.equal x1 x2
      )
  in (Printf.printf "**** LOOSENESS: (%s) |- (%s) => %B\n" (string_of_body_term_arguments args1) (string_of_body_term_arguments args2) b); b (* TODO: remove this line *)


let remove_looser_positive_terms (argsset : BodyTermArgumentsSet.t) : BodyTermArgumentsSet.t =
  let rec aux (acc : body_term_arguments list) ~(criterion : body_term_arguments) (targets : body_term_arguments list) =
    match targets |> List.filter (fun target -> not (is_looser ~than:criterion target)) with
    | [] ->
        BodyTermArgumentsSet.of_list (criterion :: acc)

    | head :: tail ->
        aux (criterion :: acc) ~criterion:head tail
  in
  (* Sorted in descending lexicographical order as to variable name lists: *)
  let argss_sorted_desc = argsset |> BodyTermArgumentsSet.elements |> List.rev in
  match argss_sorted_desc with
  | [] ->
      argsset

  | head :: tail ->
      aux [] ~criterion:head tail


let remove_looser_negative_terms (argsset : BodyTermArgumentsSet.t) : BodyTermArgumentsSet.t =
  let rec aux (acc : body_term_arguments list) ~(criterion : body_term_arguments) (targets : body_term_arguments list) =
    match targets |> List.filter (fun target -> is_looser ~than:criterion target) with
    | [] ->
        BodyTermArgumentsSet.of_list (criterion :: acc)

    | head :: tail ->
        aux (criterion :: acc) ~criterion:head tail
  in
  (* Sorted in ascending lexicographical order as to variable name lists: *)
  let argss_sorted_asc = argsset |> BodyTermArgumentsSet.elements in
  match argss_sorted_asc with
  | [] ->
      argsset

  | head :: tail ->
      aux [] ~criterion:head tail


let remove_looser_terms (imrule : intermediate_rule) : intermediate_rule =
  let { head_predicate; head_arguments; positive_terms; negative_terms; equations } = imrule in
  let positive_terms = positive_terms |> PredicateMap.map remove_looser_positive_terms in
  let negative_terms = negative_terms |> PredicateMap.map remove_looser_negative_terms in
  { head_predicate; head_arguments; positive_terms; negative_terms; equations }


let simplify_rule_step (imrule : intermediate_rule) : intermediate_rule =
  let imrule = erase_sole_occurrences imrule in

  Printf.printf "**** SOLE OCCURRENCES REMOVED: %s\n" (string_of_rule (revert_rule imrule)); (* TODO: remove this *)

  remove_looser_terms imrule


let rec simplify_rule_recursively (imrule1 : intermediate_rule) : intermediate_rule =
  let imrule2 = simplify_rule_step imrule1 in
  Printf.printf "**** STEP END: %s\n" (string_of_rule (revert_rule imrule2)); (* TODO: remove this *)
  if rule_equal imrule1 imrule2 then
  (* If the simplification reaches a fixpoint: *)
    imrule2
  else
    simplify_rule_recursively imrule2


let has_contradicting_body (imrule : intermediate_rule) : bool =
  let { positive_terms; negative_terms; _ } = imrule in
  let dom =
    PredicateSet.empty
      |> PredicateMap.fold (fun impred _ dom -> dom |> PredicateSet.add impred) positive_terms
      |> PredicateMap.fold (fun impred _ dom -> dom |> PredicateSet.add impred) negative_terms
  in
  PredicateSet.fold (fun impred found ->
    if found then
      true
    else
      match (positive_terms |> PredicateMap.find_opt impred, negative_terms |> PredicateMap.find_opt impred) with
      | (Some argsset_pos, Some argsset_neg) ->
          let argss_pos = BodyTermArgumentsSet.elements argsset_pos in
          let argss_neg = BodyTermArgumentsSet.elements argsset_neg in
          let posnegs =
            argss_pos |> List.map (fun args_pos ->
              argss_neg |> List.map (fun args_neg -> (args_pos, args_neg))
            ) |> List.concat
          in
          posnegs |> List.exists (fun (args_pos, args_neg) ->
            is_looser ~than:args_pos args_neg
          )

      | _ ->
          false
  ) dom false


let are_beta_equivalent_rules (imrule1 : intermediate_rule) (imrule : intermediate_rule) : bool =
  false (* TODO: implement this *)


let remove_duplicate_rules (imrules : intermediate_rule list) : intermediate_rule list =
  let rec aux acc imrules =
    match imrules with
    | [] ->
        List.rev acc

    | imrule_head :: imrules_tail ->
        let imrules_tail =
          imrules_tail |> List.filter (fun imrule ->
            not (are_beta_equivalent_rules imrule_head imrule)
          )
        in
        aux (imrule_head :: acc) imrules_tail
  in
  aux [] imrules


let simplify (rules : rule list) : (rule list, error) result =
  let open ResultMonad in

  (* Converts each rule to an intermediate rule (with unsatisfiable ones removed): *)
  rules |> foldM (fun imrule_acc rule ->
    convert_rule rule >>= function
    | None        -> return imrule_acc
    | Some imrule -> return (imrule :: imrule_acc)
  ) [] >>= fun imrule_acc ->
  let imrules = List.rev imrule_acc in

  Printf.printf "**** CONVERTED: %s\n" (imrules |> List.map (fun imrule -> string_of_rule (revert_rule imrule)) |> String.concat "; "); (* TODO: remove this *)

  (* Performs per-rule simplification: *)
  let imrules = imrules |> List.map simplify_rule_recursively in

  Printf.printf "**** SIMPLIFIED: %s\n" (imrules |> List.map (fun imrule -> string_of_rule (revert_rule imrule)) |> String.concat "; "); (* TODO: remove this *)

  (* Removes rules that have a contradicting body: *)
  let imrules = imrules |> List.filter (fun imrule -> not (has_contradicting_body imrule)) in

  (* Removes duplicate rules here *)
  let imrules = imrules |> remove_duplicate_rules in

  let rules = imrules |> List.map revert_rule in
  return rules
