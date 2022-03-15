
val source_to_lean_func_types : Expr.expr -> string list

val lean_simp_theorem_of_disjoint_delta : bool -> Expr.expr -> string

val lean_simp_theorem_of_getput : bool -> Expr.expr -> string

val lean_simp_theorem_of_putget : bool -> Expr.expr -> string

val gen_lean_code_for_theorems : string list -> string

val validity_lean_code_of_bidirectional_datalog : bool -> Expr.expr -> string
