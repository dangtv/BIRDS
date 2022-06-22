open Utils

val unfold_view_sql : string -> bool -> Expr.expr -> string

val unfold_delta_trigger_stt : string -> bool -> bool -> string -> string -> bool -> bool -> Expr.expr -> string

type error

type sql_query

val convert_to_operation_based_sql : colnamtab -> Expr.rule -> (sql_query, error) result

val stringify_sql_query : sql_query -> string
