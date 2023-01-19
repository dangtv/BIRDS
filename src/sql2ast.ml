open Utils

module Option = struct
  let map f = function
    | Some v -> Some (f v)
    | None -> None

  let value ~default = function
    | Some v -> v
    | None -> default

  let to_result ~none = function
    | Some v -> Ok v
    | None -> Error none
end

let ( >>= ) = ResultMonad.( >>= )

module Result = struct
  include ResultMonad

  let sequence rs = ResultMonad.mapM (fun x -> x) rs
end

type sql_binary_operator =
  | SqlPlus    (* + *)
  | SqlMinus   (* - *)
  | SqlTimes   (* * *)
  | SqlDivides (* / *)
  | SqlLor     (* || *)

type sql_unary_operator =
  | SqlNegate (* - *)

type sql_operator =
  | SqlRelEqual
  | SqlRelNotEqual
  | SqlRelGeneral of string

type sql_table_name = string

type sql_column_name = string

type sql_instance_name = string

type sql_column = sql_instance_name option * sql_column_name

type sql_vterm =
  | SqlConst    of Expr.const
  | SqlColumn   of sql_column
  | SqlUnaryOp  of sql_unary_operator * sql_vterm
  | SqlBinaryOp of sql_binary_operator * sql_vterm * sql_vterm

type sql_constraint =
  | SqlConstraint of sql_vterm * sql_operator * sql_vterm

type sql_where_clause =
  | SqlWhere of sql_constraint list

type sql_update =
  | SqlUpdateSet of sql_table_name * (sql_column * sql_vterm) list * sql_where_clause option

let string_of_sql_binary_operator = function
  | SqlPlus    -> "+"
  | SqlMinus   -> "-"
  | SqlTimes   -> "*"
  | SqlDivides -> "/"
  | SqlLor     -> "||"

let string_of_sql_unary_operator = function
  | SqlNegate -> "-"

let string_of_sql_operator = function
  | SqlRelEqual      -> "="
  | SqlRelNotEqual   -> "<>"
  | SqlRelGeneral op -> op

let string_of_sql_column (instance, column) =
  Option.value ~default:"" instance ^ "." ^ column

module ColumnVarMap = Map.Make(String)

let rec ast_vterm_of_sql_vterm colvarmap = function
  | SqlConst const ->
    Ok (Expr.Const const)
  | SqlColumn column ->
    let column_name = string_of_sql_column column in
    ColumnVarMap.find_opt column_name colvarmap
    |> Option.map (fun var -> Expr.Var var)
    |> Option.to_result ~none:("Invalid column name: " ^ column_name)
  | SqlUnaryOp (op, sql_vterm) ->
    ast_vterm_of_sql_vterm colvarmap sql_vterm >>= fun vterm ->
    Ok (Expr.UnaryOp (string_of_sql_unary_operator op, vterm))
  | SqlBinaryOp (op, left, right) ->
    ast_vterm_of_sql_vterm colvarmap left >>= fun left ->
    ast_vterm_of_sql_vterm colvarmap right >>= fun right ->
    Ok (Expr.BinaryOp (string_of_sql_binary_operator op, left, right))

let ast_terms_of_sql_where_clause colvarmap = function
  | SqlWhere sql_constraints ->
    let ast_term_of_sql_constraint = function
      | SqlConstraint (left, op, right) ->
        let op = string_of_sql_operator op in
        ast_vterm_of_sql_vterm colvarmap left >>= fun left ->
        ast_vterm_of_sql_vterm colvarmap right >>= fun right ->
        Ok (Expr.Equat (Expr.Equation (op, left, right)))
    in
    sql_constraints
    |> List.map ast_term_of_sql_constraint 
    |> Result.sequence
    
let make_tmp_table_name table_name = table_name ^ "_tmp"

module ColumnSet = Set.Make(String)
    
let update_to_datalog (update : sql_update) (instance : sql_instance_name option) (columns : sql_column_name list) : (Expr.rule list, string) result =
  let SqlUpdateSet (table_name, column_and_vterms, where_clause) = update in
  let make_column_var_list make_var =
    List.mapi (fun idx column_name ->
      let var = make_var idx column_name in
      (instance, column_name), var
    ) in
  let make_colvarmap column_var_list = column_var_list
    |> List.map (fun (col, var) -> string_of_sql_column col, var)
    |> List.to_seq
    |> ColumnVarMap.of_seq in
  let column_var_list = columns
    |> make_column_var_list (fun idx _ -> Expr.NamedVar ("V" ^ string_of_int idx)) in
  let colvarmap = make_colvarmap column_var_list in

  column_and_vterms
  |> Result.mapM (fun (sql_col, sql_vterm) ->
    ast_vterm_of_sql_vterm colvarmap sql_vterm
    >>= fun vterm ->
    let column_name = string_of_sql_column sql_col in
    ColumnVarMap.find_opt column_name colvarmap
    |> Option.to_result ~none:("Invalid column name: " ^ column_name)
    >>= fun var ->
    Result.return (Expr.Equat (Expr.Equation ("<>", Expr.Var var, vterm)))
  ) >>= fun effect_terms ->
  List.fold_right (fun (column, var) res ->
    res >>= fun (varlist, in_set) ->
    match List.assoc_opt column column_and_vterms with
    | None ->
      Ok ((var :: varlist), in_set)
    | Some _ ->
      let column_name = string_of_sql_column column in
      Ok ((var :: varlist), (ColumnSet.add column_name in_set))
  ) column_var_list (Ok ([], ColumnSet.empty))
  >>= fun (varlist, in_set) ->
  let tmp_pred = Expr.Pred (make_tmp_table_name table_name, varlist) in
  let effect_rules = effect_terms
    |> List.map (fun term -> tmp_pred, [term]) in
  let column_var_list' = columns
    |> make_column_var_list (fun idx column_name ->
      let column_name = string_of_sql_column (instance, column_name) in
      if ColumnSet.exists (fun c -> c = column_name) in_set then
        Expr.NamedVar ("V" ^ string_of_int idx ^ "_2")
      else
        Expr.NamedVar ("V" ^ string_of_int idx)
    ) in
  let colvarmap' = make_colvarmap column_var_list' in

  where_clause
  |> Option.map (ast_terms_of_sql_where_clause colvarmap)
  |> Option.value ~default:(Ok([]))
  >>= fun body ->
  let delete_pred = Expr.Deltadelete (table_name, varlist) in
  let from = Expr.Pred (table_name, varlist) in
  let delete = delete_pred, (Expr.Rel from :: body @ [Expr.Rel tmp_pred]) in

  column_and_vterms
  |> List.map (fun (column, vterm) ->
    ast_vterm_of_sql_vterm colvarmap' vterm >>= fun vterm ->
    let column_name = string_of_sql_column column in
    ColumnVarMap.find_opt column_name colvarmap
    |> Option.map (fun var -> Expr.Equat (Expr.Equation ("=", Expr.Var var, vterm)))
    |> Option.to_result ~none:("Invalid column name: " ^ column_name)
  )
  |> Result.sequence
  >>= fun body ->
  columns
  |> List.map (fun column ->
    let column_name = string_of_sql_column (instance, column) in
    ColumnVarMap.find_opt column_name colvarmap'
    |> Option.to_result ~none:("Invalid column name: " ^ column_name)
  )
  |> Result.sequence
  >>= fun delete_var_list ->
  let delete_pred = Expr.Deltadelete (table_name, delete_var_list) in
  let body = body @ [Expr.Rel delete_pred] in
  let insert_pred = Expr.Deltainsert (table_name, varlist) in
  let insert = insert_pred, body in
  Ok (effect_rules @ [delete; insert])
