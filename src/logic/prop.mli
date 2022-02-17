
val psimplify1 : 'a Formulas.formula -> 'a Formulas.formula

val dnf : 'a Formulas.formula -> 'a Formulas.formula

val list_conj : 'a Formulas.formula list -> 'a Formulas.formula

val list_disj : 'a Formulas.formula list -> 'a Formulas.formula

val positive : 'a Formulas.formula -> bool

val negate : 'a Formulas.formula -> 'a Formulas.formula
