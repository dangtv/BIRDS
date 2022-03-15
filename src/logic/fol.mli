
type term =
  | Var of string
  | Fn of string * term list

type fol =
  | R of string * term list

val fv : fol Formulas.formula -> string list

val variant : string -> string list -> string

val subst : (string, term) Lib.func -> fol Formulas.formula -> fol Formulas.formula

val generalize : fol Formulas.formula -> fol Formulas.formula
