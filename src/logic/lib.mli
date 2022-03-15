
(** Compose functions. *)
val ( ** ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b

(* ------------------------------------------------------------------------- *)
(* A useful idiom for "non contradictory" etc.                               *)
(* ------------------------------------------------------------------------- *)

val non : ('a -> bool) -> 'a -> bool

(* ------------------------------------------------------------------------- *)
(* Polymorphic finite partial functions via Patricia trees.                  *)
(*                                                                           *)
(* The point of this strange representation is that it is canonical (equal   *)
(* functions have the same encoding) yet reasonably efficient on average.    *)
(*                                                                           *)
(* Idea due to Diego Olivier Fernandez Pons (OCaml list, 2003/11/10).        *)
(* ------------------------------------------------------------------------- *)

type ('a,'b) func =
  | Empty
  | Leaf of int * ('a*'b) list
  | Branch of int * int * ('a, 'b) func * ('a, 'b) func

(* ------------------------------------------------------------------------- *)
(* Undefined function.                                                       *)
(* ------------------------------------------------------------------------- *)

val undefined : ('a, 'b) func

(* ------------------------------------------------------------------------- *)
(* Handy list operations.                                                    *)
(* ------------------------------------------------------------------------- *)

val ( -- ) : int -> int -> int list

(** Same as [List.hd]. *)
val hd : 'a list -> 'a

(** Same as [List.tl]. *)
val tl : 'a list -> 'a list

(** Exactly the same as [List.fold_right]. *)
val itlist : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b

(** A variant of [itlist] for non-empty lists. *)
val end_itlist : ('a -> 'a -> 'a) -> 'a list -> 'a

(** Same as [List.forall]. *)
val forall : ('a -> bool) -> 'a list -> bool

(** Same as [List.exists]. *)
val exists : ('a -> bool) -> 'a list -> bool

(** Same as [List.partition]. *)
val partition : ('a -> bool) -> 'a list -> 'a list * 'a list

(** Same as [List.filter]. *)
val filter : ('a -> bool) -> 'a list -> 'a list

(** Same as [List.length]. *)
val length : 'a list -> int

(** Same as [List.nth]. *)
val el : int -> 'a list -> 'a

(** Same as [List.map]. *)
val map : ('a -> 'b) -> 'a list -> 'b list

(** e.g. [allpairs (fun x y -> (x, y)) ["foo"; "bar"] [3; 1; 4]] evaluates to
    [[("foo", 3); ("foo", 1); ("foo", 4); ("bar", 3); ("bar", 1); ("bar", 4)]]. *)
val allpairs : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list

(* ------------------------------------------------------------------------- *)
(* Application of (presumably imperative) function over a list.              *)
(* ------------------------------------------------------------------------- *)

(** Almost the same as [List.iter]. *)
val do_list : ('a -> 'b) -> 'a list -> unit

(* ------------------------------------------------------------------------- *)
(* Bottom-up mergesort.                                                      *)
(* ------------------------------------------------------------------------- *)

val sort : ('a -> 'a -> bool) -> 'a list -> 'a list

(* ------------------------------------------------------------------------- *)
(* Set operations on ordered lists.                                          *)
(* ------------------------------------------------------------------------- *)

val setify : 'a list -> 'a list

val union : 'a list -> 'a list -> 'a list

val intersect : 'a list -> 'a list -> 'a list

val subtract : 'a list -> 'a list -> 'a list

val subset : 'a list -> 'a list -> bool

val set_eq : 'a list -> 'a list -> bool

val psubset : 'a list -> 'a list -> bool

val insert : 'a -> 'a list -> 'a list

(** Same as [List.map] (for sets). *)
val image : ('a -> 'b) -> 'a list -> 'b list

(* ------------------------------------------------------------------------- *)
(* Union of a family of sets.                                                *)
(* ------------------------------------------------------------------------- *)

val unions : 'a list list -> 'a list

(* ------------------------------------------------------------------------- *)
(* List membership. This does *not* assume the list is a set.                *)
(* ------------------------------------------------------------------------- *)

(** Equivalent to [List.mem]. *)
val mem : 'a -> 'a list -> bool

(* ------------------------------------------------------------------------- *)
(* Finding all subsets or all subsets of a given size.                       *)
(* ------------------------------------------------------------------------- *)

val allnonemptysubsets : 'a list -> 'a list list

(* ------------------------------------------------------------------------- *)
(* Application.                                                              *)
(* ------------------------------------------------------------------------- *)

val apply : ('a, 'b) func -> 'a -> 'b

val tryapplyd : ('a, 'b) func -> 'a -> 'b -> 'b

(* ------------------------------------------------------------------------- *)
(* Undefinition.                                                             *)
(* ------------------------------------------------------------------------- *)

val undefine : 'a -> ('a, 'b) func -> ('a, 'b) func

(* ------------------------------------------------------------------------- *)
(* Redefinition and combination.                                             *)
(* ------------------------------------------------------------------------- *)

val ( |-> ) : 'a -> 'b -> ('a, 'b) func -> ('a, 'b) func

(* ------------------------------------------------------------------------- *)
(* Special case of point function.                                           *)
(* ------------------------------------------------------------------------- *)

val ( |=> ) : 'a -> 'b -> ('a, 'b) func

(* ------------------------------------------------------------------------- *)
(* Idiom for a mapping zipping domain and range lists.                       *)
(* ------------------------------------------------------------------------- *)

val fpf : 'a list -> 'b list -> ('a, 'b) func
