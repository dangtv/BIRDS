open Lib;;
(* open Intro;; *)
open Formulas;;
open Fol;;
open Skolem;;
open Fol_ex;;

(** mainly a call to the above main function *)
let _ =
  (* ------------------------------------------------------------------------- *)
(* Trivial example of using the type constructors.                           *)
(* ------------------------------------------------------------------------- *)

    (* let e = Add(Mul(Add(Mul(Const(0),Var "x"),Const(1)),Const(3)), *)
            (* Const(12)) in *)
    (* let e' = simplify e in  *)
    

    let f = Fn("sqrt",[Fn("-",[Fn("1",[]);
                   Fn("cos",[Fn("power",[Fn("+",[Var "x"; Var "y"]);
                                        Fn("2",[])])])])]) in
                                        
    (* ------------------------------------------------------------------------- *)
(* Trivial example of "x + y < z".                                           *)
(* ------------------------------------------------------------------------- *)
    let fo = Forall("t",Atom(R("<",[Fn("+",[Var "x"; Var "y"]); Var "z"]))) in 
  
    let fo2 = Forall("x", Forall("y", Exists("z", Atom(R("<",[Fn("+",[Var "x"; Var "y"]); Var "z"]))))) in
    (* <<forall x. (x = 0) \/ (x = 1)>> *)
    let fo3 = Forall("x", Or(Atom(R("=",[Var "x"; Fn ("0",[])])), Atom(R("=",[Var "x"; Fn ("1",[])])))) in
    (* holds bool_interp undefined <<forall x. (x = 0) \/ (x = 1)>>;; *)
    
    let fo4 =  Forall("x", Atom(R("<",[Fn("+",[Var "x"; Var "y"]); Var "z"]))) in

    (* pnf <<(forall x. P(x) \/ R(y)) *)
      (* ==> exists y z. Q(y) \/ ~(exists z. P(z) /\ Q(z))>>;; *)
    let fo5 = Imp(Forall ("x", Or(Atom(R("P", [Var "x"])), Atom(R("R", [Var "y"]))) ), Exists("y", Exists("z", Or(Atom(R("Q",[Var "y"])), Not(Exists("z", And(Atom(R("P",[Var "z"])),Atom(R("Q",[Var "z"]))))) )))) in
    
    print_string (lean_string_of_fol_formula (subst ("x" |=> Var "x'") (subst ("y" |=> Var "x") fo4)));
    print_newline();
    print_string ("------"^(lean_string_of_fol_formula (simplify fo4))^"-----");
    print_newline();
    print_string (lean_string_of_fol_formula (Fol_ex.pnf_dnf fo5));
    (* ∃ x, ∀ z, ¬P(x) ∧ ¬R(y) ∨ Q(x) ∨ ¬P(z) ∨ ¬Q(z) *)
    (* ¬P(f_x(y)) ∧ ¬R(y) ∨ Q(c_y) ∨ ¬P(z) ∨ ¬Q(z) *)
    (* ¬P(f_x(y)) ∧ ¬R(y) ∨ Q(f_x(y)) ∨ ¬P(z) ∨ ¬Q(z) *)
    print_newline();
    print_string (lean_string_of_fol_formula (generalize fo4));
    print_newline();
    print_string (String.concat ", " (fv (subst ("x" |=> Var "x'") (subst ("y" |=> Var "x") fo4))));
    (* print_exp e';  *)
    if(holds bool_interp undefined fo3) then print_string "OK===";
;;