let const_of_const2 = function
  | Expr2.Int num -> Expr.Int num
  | Expr2.Real num -> Expr.Real num
  | Expr2.String str -> Expr.String str
  | Expr2.Bool bool -> Expr.Bool bool
  | Expr2.Null -> Expr.Null

let var_of_var2 = function
  | Expr2.NamedVar str -> Expr.NamedVar str
  | Expr2.NumberedVar num -> Expr.NumberedVar num
  | Expr2.ConstVar const -> Expr.ConstVar (const_of_const2 const)
  | Expr2.AnonVar -> Expr.AnonVar
  | Expr2.AggVar (str1, str2) -> Expr.AggVar (str1, str2)

let rec vterm_of_vterm2 = function
  | Expr2.Const const -> Expr.Const (const_of_const2 const)
  | Expr2.Var var -> Expr.Var (var_of_var2 var)
  | Expr2.BinaryOp (op, lhs, rhs) ->
    let lhs = vterm_of_vterm2 lhs in
    let rhs = vterm_of_vterm2 rhs in
    begin match op with
      | "+" -> Expr.Sum (lhs, rhs)
      | "-" -> Expr.Diff (lhs, rhs)
      | "*" -> Expr.Times (lhs, rhs)
      | "/" -> Expr.Div (lhs, rhs)
      | "^" -> Expr.Concat (lhs, rhs)
      | _ -> invalid_arg ("Undefined operator: " ^ op)
    end
  | Expr2.UnaryOp (op, arg) ->
    let arg = vterm_of_vterm2 arg in
    begin match op with
      | "-" -> Expr.Neg arg
      | _ -> invalid_arg ("Undefined operator: " ^ op)
    end

let rterm_of_rterm2 = function
  | Expr2.Pred (str, vars) -> Expr.Pred (str, List.map var_of_var2 vars)
  | Expr2.Deltainsert (str, vars) -> Expr.Deltainsert (str, List.map var_of_var2 vars)
  | Expr2.Deltadelete (str, vars) -> Expr.Deltadelete (str, List.map var_of_var2 vars)

let rec term_of_term2 = function
  | Expr2.Rel rterm -> Expr.Rel (rterm_of_rterm2 rterm)
  | Expr2.Not rterm -> Expr.Not (rterm_of_rterm2 rterm)
  | Expr2.Equat eterm ->
    begin match eterm with
      | Expr2.Equation ("=", lhs, rhs) -> Expr.Equal (vterm_of_vterm2 lhs, vterm_of_vterm2 rhs)
      | Expr2.Equation (op, lhs, rhs) -> Expr.Ineq (op, vterm_of_vterm2 lhs, vterm_of_vterm2 rhs)
    end
  | Expr2.Noneq eterm -> Expr.negate_eq @@ term_of_term2 (Equat eterm)

let stype_of_stype2 = function
  | Expr2.Sint -> Expr.Sint
  | Expr2.Sreal -> Expr.Sreal
  | Expr2.Sstring -> Expr.Sstring
  | Expr2.Sbool -> Expr.Sbool

let expr_of_expr2 Expr2.{ rules; facts; query; sources; view; constraints; primary_keys; } =
  Expr.Prog (List.flatten [
    List.map (fun (rterm, terms) -> Expr.Rule (rterm_of_rterm2 rterm, List.map term_of_term2 terms)) rules;
    List.map (fun rterm -> Expr.Fact (rterm_of_rterm2 rterm)) facts;
    (match query with Some rterm -> [Expr.Query (rterm_of_rterm2 rterm)] | None -> []);
    List.map (fun (str, stypes) -> Expr.Source (str, List.map (fun (str, stype) -> (str, stype_of_stype2 stype)) stypes)) sources;
    (match view with Some (str, stypes) -> [Expr.View (str, List.map (fun (str, stype) -> (str, stype_of_stype2 stype)) stypes)] | None -> []);
    List.map (fun (rterm, terms) -> Expr.Constraint (rterm_of_rterm2 rterm, List.map term_of_term2 terms)) constraints;
    List.map (fun (str, strs) -> Expr.Pk (str, strs)) primary_keys
  ])

let conj_query_of_conj_query2 = function
  | Expr2.Conj_query (vars, rterms1, rterms2) ->
    Expr.Conj_query (
      List.map var_of_var2 vars,
      List.map rterm_of_rterm2 rterms1,
      List.map rterm_of_rterm2 rterms2
    )

let const2_of_const = function
  | Expr.Int num -> Expr2.Int num
  | Expr.Real num -> Expr2.Real num
  | Expr.String str -> Expr2.String str
  | Expr.Bool bool -> Expr2.Bool bool
  | Expr.Null -> Expr2.Null

let var2_of_var = function
  | Expr.NamedVar str -> Expr2.NamedVar str
  | Expr.NumberedVar num -> Expr2.NumberedVar num
  | Expr.ConstVar const -> Expr2.ConstVar (const2_of_const const)
  | Expr.AnonVar -> Expr2.AnonVar
  | Expr.AggVar (str1, str2) -> Expr2.AggVar (str1, str2)

let rec vterm2_of_vterm = function
  | Expr.Const const -> Expr2.Const (const2_of_const const)
  | Expr.Var var -> Expr2.Var (var2_of_var var)
  | Expr.Sum (lhs, rhs) -> Expr2.BinaryOp ("=", vterm2_of_vterm rhs, vterm2_of_vterm rhs)
  | Expr.Diff (lhs, rhs) -> Expr2.BinaryOp ("-", vterm2_of_vterm rhs, vterm2_of_vterm rhs)
  | Expr.Times (lhs, rhs) -> Expr2.BinaryOp ("*", vterm2_of_vterm rhs, vterm2_of_vterm rhs)
  | Expr.Div (lhs, rhs) -> Expr2.BinaryOp ("/", vterm2_of_vterm rhs, vterm2_of_vterm rhs)
  | Expr.Concat (lhs, rhs) -> Expr2.BinaryOp ("^", vterm2_of_vterm rhs, vterm2_of_vterm rhs)
  | Expr.Neg arg ->  Expr2.UnaryOp ("-", vterm2_of_vterm arg)
  | BoolAnd _ -> invalid_arg "BoolAnd operation is not acceptable in Expr2"
  | BoolOr _ -> invalid_arg "BoolOr operation is not acceptable in Expr2"
  | BoolNot _ -> invalid_arg "BoolNot operation is not acceptable in Expr2"

let rterm2_of_rterm = function
  | Expr.Pred (str, vars) -> Expr2.Pred (str, List.map var2_of_var vars)
  | Expr.Deltainsert (str, vars) -> Expr2.Deltainsert (str, List.map var2_of_var vars)
  | Expr.Deltadelete (str, vars) -> Expr2.Deltadelete (str, List.map var2_of_var vars)

let term2_of_term = function
  | Expr.Rel rterm -> Expr2.Rel (rterm2_of_rterm rterm)
  | Expr.Equal (lhs, rhs) -> Expr2.Equat (Expr2.Equation ("=", vterm2_of_vterm lhs, vterm2_of_vterm rhs))
  | Expr.Ineq (op, lhs, rhs) -> Expr2.Equat (Expr2.Equation (op, vterm2_of_vterm lhs, vterm2_of_vterm rhs))
  | Expr.Not rterm -> Expr2.Not (rterm2_of_rterm rterm)

let stype2_of_stype = function
  | Expr.Sint -> Expr2.Sint
  | Expr.Sreal -> Expr2.Sreal
  | Expr.Sstring -> Expr2.Sstring
  | Expr.Sbool -> Expr2.Sbool

let expr2_of_expr = function
  | Expr.Prog stts ->
    let stt2_of_stt = function
      | Expr.Rule (rterm, terms) -> Expr2.Stt_Rule (rterm2_of_rterm rterm, List.map term2_of_term terms)
      | Expr.Fact rterm -> Expr2.Stt_Fact (rterm2_of_rterm rterm)
      | Expr.Query rterm -> Expr2.Stt_Query (rterm2_of_rterm rterm)
      | Expr.Source (str, stypes) -> Expr2.Stt_Source (str, List.map (fun (str, stype) -> (str, stype2_of_stype stype)) stypes)
      | Expr.View (str, stypes) -> Expr2.Stt_View (str, List.map (fun (str, stype) -> (str, stype2_of_stype stype)) stypes)
      | Expr.Constraint (rterm, terms) -> Expr2.Stt_Constraint (rterm2_of_rterm rterm, List.map term2_of_term terms)
      | Expr.Pk (str, strs) -> Expr2.Stt_Pk (str, strs) in
    List.fold_left (fun expr stt -> Expr2.add_stt (stt2_of_stt stt) expr) Expr2.get_empty_expr stts

let conj_query2_of_conj_query = function
  | Expr.Conj_query (vars, rterms1, rterms2) ->
    Expr2.Conj_query (
      List.map var2_of_var vars,
      List.map rterm2_of_rterm rterms1,
      List.map rterm2_of_rterm rterms2
    )