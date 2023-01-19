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

val update_to_datalog : sql_update -> sql_instance_name option -> sql_column_name list -> (Expr.rule list, string) result
