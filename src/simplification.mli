
open Expr

type error

val simplify : rule list -> (rule list, error) result
