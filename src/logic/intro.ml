(* ========================================================================= *)
(* Simple algebraic expression example from the introductory chapter.        *)
(*                                                                           *)
(* Copyright (c) 2003-2007, John Harrison. (See "LICENSE.txt" for details.)  *)
(* ========================================================================= *)

type expression =
   Var of string
 | Const of int
 | Add of expression * expression
 | Mul of expression * expression;;

(* ------------------------------------------------------------------------- *)
(* Simplification example.                                                   *)
(* ------------------------------------------------------------------------- *)

let simplify1 expr =
  match expr with
    Add(Const(m),Const(n)) -> Const(m + n)
  | Mul(Const(m),Const(n)) -> Const(m * n)
  | Add(Const(0),x) -> x
  | Add(x,Const(0)) -> x
  | Mul(Const(0),x) -> Const(0)
  | Mul(x,Const(0)) -> Const(0)
  | Mul(Const(1),x) -> x
  | Mul(x,Const(1)) -> x
  | _ -> expr;;

let rec simplify expr =
  match expr with
    Add(e1,e2) -> simplify1(Add(simplify e1,simplify e2))
  | Mul(e1,e2) -> simplify1(Mul(simplify e1,simplify e2))
  | _ -> simplify1 expr;;

(* ------------------------------------------------------------------------- *)
(* Conservatively bracketing first attempt at printer.                       *)
(* ------------------------------------------------------------------------- *)

let rec string_of_exp e =
  match e with
    Var s -> s
  | Const n -> string_of_int n
  | Add(e1,e2) -> "("^(string_of_exp e1)^" + "^(string_of_exp e2)^")"
  | Mul(e1,e2) -> "("^(string_of_exp e1)^" * "^(string_of_exp e2)^")";;

(* ------------------------------------------------------------------------- *)
(* Somewhat better attempt.                                                  *)
(* ------------------------------------------------------------------------- *)

let rec string_of_exp pr e =
  match e with
    Var s -> s
  | Const n -> string_of_int n
  | Add(e1,e2) ->
        let s = (string_of_exp 3 e1)^" + "^(string_of_exp 2 e2) in
        if 2 < pr then "("^s^")" else s
  | Mul(e1,e2) ->
        let s = (string_of_exp 5 e1)^" * "^(string_of_exp 4 e2) in
        if 4 < pr then "("^s^")" else s;;

(* ------------------------------------------------------------------------- *)
(* Install it.                                                               *)
(* ------------------------------------------------------------------------- *)

let print_exp e = Format.print_string ("<<"^string_of_exp 0 e^">>");;

(* 
(* ------------------------------------------------------------------------- *)
(* Example.                                                                  *)
(* ------------------------------------------------------------------------- *)
(* START_INTERACTIVE;;
let e = Add(Mul(Add(Mul(Const(0),Var "x"),Const(1)),Const(3)),
            Const(12));;
simplify e;;
END_INTERACTIVE;; *)

(* ------------------------------------------------------------------------- *)
(* Examples.                                                                 *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
string_of_exp <<x + 3 * y>>;;
END_INTERACTIVE;;



#install_printer print_exp;;

(* ------------------------------------------------------------------------- *)
(* Examples.                                                                 *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
<<x + 3 * y>>;;
<<(x + 3) * y>>;;
<<1 + 2 + 3>>;;
<<((1 + 2) + 3) + 4>>;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Example shows the problem.                                                *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
<<(x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10) *
  (y1 + y2 + y3 + y4 + y5 + y6 + y7 + y8 + y9 + y10)>>;;
END_INTERACTIVE;; *)
