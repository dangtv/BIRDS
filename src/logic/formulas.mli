
type 'a formula =
  | False
  | True
  | Atom of 'a
  | Not of 'a formula
  | And of 'a formula * 'a formula
  | Or of 'a formula * 'a formula
  | Imp of 'a formula * 'a formula
  | Iff of 'a formula * 'a formula
  | Forall of string * 'a formula
  | Exists of string * 'a formula

val print_qformula : (int -> 'a -> unit) -> 'a formula -> unit

val atom_union : ('a -> 'b list) -> 'a formula -> 'b list

val onatoms : ('a -> 'a formula) -> 'a formula -> 'a formula

val mk_and : 'a formula -> 'a formula -> 'a formula

val mk_or : 'a formula -> 'a formula -> 'a formula

val mk_forall : string -> 'a formula -> 'a formula

val mk_exists : string -> 'a formula -> 'a formula

val strip_quant : 'a formula -> string list * 'a formula

val disjuncts : 'a formula -> 'a formula list
