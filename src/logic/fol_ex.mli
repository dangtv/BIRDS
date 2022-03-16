
val to_conj_lst : 'a Formulas.formula -> 'a Formulas.formula list

val string_of_fol_formula : Fol.fol Formulas.formula -> string

val is_relation_symbol : string -> bool

val extract_ex_quants : 'a Formulas.formula -> string list * 'a Formulas.formula

val ranf : Fol.fol Formulas.formula -> Fol.fol Formulas.formula

val normalize_comparison : Fol.fol Formulas.formula -> Fol.fol Formulas.formula

type lean_variable = string

type lean_infix_operator =
  | LeanInfixEqual    (* = *)
  | LeanInfixLt       (* < *)
  | LeanInfixGt       (* > *)
  | LeanInfixNotEqual (* ≠ *)
  | LeanInfixLeq      (* ≤ *)
  | LeanInfixGeq      (* ≥ *)
  | LeanInfixAnd
  | LeanInfixOr
  | LeanInfixImp
  | LeanInfixIff
  | LeanInfixConcat   (* ++ *)
  | LeanInfixDiv      (* / *)
  | LeanInfixMult     (* * *)
  | LeanInfixSubtr    (* - *)
  | LeanInfixAdd      (* + *)
  | LeanInfixCons     (* :: *)

(* isomorphic to `Expr.stype` *)
type lean_annot =
  | LeanAnnotInt
  | LeanAnnotRat
  | LeanAnnotString
  | LeanAnnotProp

type lean_formula =
  | LeanNull
  | LeanBool   of bool
  | LeanInt    of int
  | LeanFloat  of float
  | LeanString of string
  | LeanVar    of lean_variable
  | LeanNot    of lean_formula
  | LeanInfix  of lean_infix_operator * lean_formula * lean_formula
  | LeanApp    of string * lean_formula list
  | LeanForall of lean_variable * lean_formula
  | LeanExists of lean_variable * lean_formula
  | LeanAnnot  of lean_formula * lean_annot

val stringify_lean_formula : lean_formula -> string

val lean_formula_of_fol_formula : Fol.fol Formulas.formula -> lean_formula

val remove_trivial : Fol.fol Formulas.formula -> Fol.fol Formulas.formula

val z3_string_of_fol_formula : Fol.fol Formulas.formula -> string

val ranf2lvnf : string -> Fol.fol Formulas.formula -> Fol.fol Formulas.formula * (string list * Fol.fol Formulas.formula * Fol.fol Formulas.formula) list

val string_of_term : int -> Fol.term -> string
