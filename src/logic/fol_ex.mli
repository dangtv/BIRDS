
val to_conj_lst : 'a Formulas.formula -> 'a Formulas.formula list

val string_of_fol_formula : Fol.fol Formulas.formula -> string

val is_relation_symbol : string -> bool

val extract_ex_quants : 'a Formulas.formula -> string list * 'a Formulas.formula

val ranf : Fol.fol Formulas.formula -> Fol.fol Formulas.formula

val normalize_comparison : Fol.fol Formulas.formula -> Fol.fol Formulas.formula

val lean_string_of_fol_formula : Fol.fol Formulas.formula -> string

val remove_trivial : Fol.fol Formulas.formula -> Fol.fol Formulas.formula

val z3_string_of_fol_formula : Fol.fol Formulas.formula -> string

val ranf2lvnf : string -> Fol.fol Formulas.formula -> Fol.fol Formulas.formula * (string list * Fol.fol Formulas.formula * Fol.fol Formulas.formula) list

val string_of_term : int -> Fol.term -> string
