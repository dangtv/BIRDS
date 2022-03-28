
type lean_type

type lean_theorem

val source_to_lean_func_types : Expr.expr -> (string * lean_type) list

val lean_simp_theorem_of_disjoint_delta : bool -> Expr.expr -> lean_theorem

val lean_simp_theorem_of_getput : bool -> Expr.expr -> lean_theorem

val lean_simp_theorem_of_putget : bool -> Expr.expr -> lean_theorem

val make_lean_theorem : string -> (string * lean_type) list -> Fol_ex.lean_formula -> lean_theorem

val gen_lean_code_for_theorems : lean_theorem list -> string

val validity_lean_code_of_bidirectional_datalog : bool -> Expr.expr -> string
