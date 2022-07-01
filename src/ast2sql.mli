open Utils

val unfold_view_sql : string -> bool -> Expr.expr -> string

val unfold_delta_trigger_stt : string -> bool -> bool -> string -> string -> bool -> bool -> Expr.expr -> string

type error

type sql_query

type sql_operation

type delta_kind

val stringify_sql_query : sql_query -> string

val stringify_sql_operation : sql_operation -> string

val convert_to_operation_based_sql : colnamtab -> Expr.rule -> (delta_kind * sql_query, error) result

val convert_expr_to_operation_based_sql : Expr.expr -> (sql_operation list, error) result
