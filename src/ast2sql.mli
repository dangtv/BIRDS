open Utils

val unfold_view_sql : string -> bool -> Expr.expr -> string

val unfold_delta_trigger_stt : string -> bool -> bool -> string -> string -> bool -> bool -> Expr.expr -> string

type error

val show_error : error -> string

type sql_operation

val stringify_sql_operation : sql_operation -> string

val convert_expr_to_operation_based_sql : Expr.expr -> (sql_operation list, error) result
