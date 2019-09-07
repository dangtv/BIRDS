(*******************************************************)
(**  
BX algorithms on Datalog programs
 *)
(********************************************************)
(* 
@author: Vandang Tran
*)

open Lib;;
open Formulas;;
open Fol;;
open Fol_ex;;
open Ast2fol;;
open Ast2theorem;;
open Utils;;

(* take a view update put datalog program and generate the FO sentence of checking whether view is unique or not *)
let view_uniqueness_sentence_of_stt (debug:bool) prog = 
    let fm = sourcestability_sentence_of_stt ( debug) prog in
    let view_name = Expr.get_rterm_predname (Expr.get_schema_rterm (get_view prog)) in
    let view_vars = List.map (fun x -> Expr.string_of_var x) @@ Expr.get_rterm_varlist (Expr.get_schema_rterm (get_view prog)) in
    let phi, lst = ranf2lvnf view_name fm in 
    (* we do not need vars any more because it must be all free variables in vfol and phi_i *)
    let lst2 = List.map (fun (vars, vfol, phi_i) -> 
    match vfol with 
        Atom(R(view_name,lst)) | Not(Atom(R(view_name,lst))) -> let subfn = fpf (List.map (fun x -> Fol_ex.string_of_term 0 x) lst) (List.map (fun x -> Fol.Var x) view_vars) in
        (subst subfn vfol, subst subfn phi_i) 
        | _ -> (vfol, phi_i)
    ) lst in
    if debug then (
    print_endline "===> solving sourcestability constraint to check view uniqueness";
    print_endline "______constraints from view-predicate normal form_______";
    print_endline @@ "phi: " ^(lean_string_of_fol_formula phi);
    List.iter (fun (vfol, phi_i) -> 
    print_endline @@ "false <=> : " ^ (lean_string_of_fol_formula ((vfol))) ;
    print_endline @@ ", " ^ (lean_string_of_fol_formula ((phi_i))) ^"\n";
    ) lst2;
    );

    (* contruct upper bound FO formula of view *)
    let view_upper_fol = List.fold_left (fun fm (vfol, phi_i) -> 
    match vfol with 
        Formulas.Not(Formulas.Atom(R(view_name,lst))) -> let ex_vars = subtract (fv phi_i) view_vars in Or(fm, itlist mk_exists ex_vars phi_i )
        | _ -> fm
    ) False lst2 in
    if debug then print_endline @@ "upper bound of view: "^ (lean_string_of_fol_formula ((view_upper_fol)));
    (* contruct lower bound FO formula of view *)
    let view_lower_fol = List.fold_left (fun fm (vfol, phi_i) -> 
    match vfol with 
        | Formulas.Atom(R(view_name,lst)) -> let ex_vars = subtract (fv phi_i) view_vars in Or(fm, Not(itlist mk_exists ex_vars phi_i) ) 
        | _ -> fm
    ) False lst2 in
    if debug then print_endline @@ "lower bound of view: " ^ (lean_string_of_fol_formula ((view_lower_fol)));

    (* make the equivalence sentence of checking whether upper FOL and lower FOL of view are equvalent *)
    let sentence_of_view_uniqueness = generalize (Iff(view_upper_fol, view_lower_fol)) in 
    if debug then (print_endline @@ "FO sentence of view uniqueness : " ^ (lean_string_of_fol_formula ((sentence_of_view_uniqueness)));
    print_endline "_______________________________________\n");
    sentence_of_view_uniqueness ;;

(* take a view update put datalog program and generate the theorem of checking view uniqueness *)
let lean_simp_theorem_of_view_uniqueness (debug:bool) prog = 
    if debug then (print_endline "==> generating theorem for view uniqueness";) else ();
    "theorem view_uniqueness " ^ String.concat " " (List.map (fun x -> "{"^x^"}") (source_to_lean_func_types prog)) ^
     ": " ^ (Fol_ex.lean_string_of_fol_formula (Imp (Ast2fol.constraint_sentence_of_stt debug prog, 
     view_uniqueness_sentence_of_stt debug prog)));;

(* take a view update put datalog program and derive the corresponding get datalog *)
let derive_get_datalog (debug:bool) (speedup:bool) timeout inputprog = 
    (* verifying djsdelta property *)
    let prog = constraint2rule inputprog in
    let disdelta_thm = lean_simp_theorem_of_disjoint_delta (debug) prog in
    if (not speedup) then
        (if debug then print_endline "==> verifying djsdelta property";
        let is_disjoint, disdel_mess =  verify_fo_lean (debug) timeout (gen_lean_code_for_theorems [disdelta_thm]) in
        if not (is_disjoint=0) then 
            (
                (* print_endline @@ "Well-behavedness is not validated \nExit code: " ^ string_of_int is_disjoint; *)
            raise (ChkErr ("Deltas in the datalog program are not disjoint" ^ (if (debug) then "\nError messange: "^ disdel_mess else "") ));););
    (* let fm = Or(sourcestability_sentence_of_stt ( debug) prog, Not (Ast2fol.constraint_sentence_of_stt debug prog)) in *)
    let fm = sourcestability_sentence_of_stt ( debug) prog in
    let view_name = Expr.get_rterm_predname (Expr.get_schema_rterm (get_view prog)) in
    let view_vars = List.map (fun x -> Expr.string_of_var x) @@ Expr.get_rterm_varlist (Expr.get_schema_rterm (get_view prog)) in
    let phi, lst = ranf2lvnf view_name fm in 
    (* we do not need vars any more because it must be all free variables in vfol and phi_i *)
    let lst2 = List.map (fun (vars, vfol, phi_i) -> 
    match vfol with 
        Atom(R(view_name,lst)) | Not(Atom(R(view_name,lst))) -> let subfn = fpf (List.map (fun x -> Fol_ex.string_of_term 0 x) lst) (List.map (fun x -> Fol.Var x) view_vars) in
        (subst subfn vfol, subst subfn phi_i) 
        | _ -> (vfol, phi_i)
    ) lst in
    if debug then (
    print_endline "===> solving sourcestability constraint to check view existence";
    print_endline "______constraints from view-predicate normal form_______";
    print_endline @@ "phi: " ^(lean_string_of_fol_formula phi);
    List.iter (fun (vfol, phi_i) -> 
    print_endline @@ "false <=> : " ^ (lean_string_of_fol_formula ((vfol))) ;
    print_endline @@ ", " ^ (lean_string_of_fol_formula ((phi_i))) ^"\n";
    ) lst2;
    );

    (* contruct upper bound FO formula of view *)
    let view_upper_fol = List.fold_left (fun fm (vfol, phi_i) -> 
    match vfol with 
        Formulas.Atom(R(view_name,lst)) -> let ex_vars = subtract (fv phi_i) view_vars in And(fm, Not(itlist mk_exists ex_vars phi_i) )
        | _ -> fm
    ) True lst2 in
    if debug then print_endline @@ "upper bound of view: "^ (lean_string_of_fol_formula ((view_upper_fol)));
    (* contruct lower bound FO formula of view *)
    let view_lower_fol = List.fold_left (fun fm (vfol, phi_i) -> 
    match vfol with 
        | Formulas.Not(Formulas.Atom(R(view_name,lst)))  -> let ex_vars = subtract (fv phi_i) view_vars in Or(fm, itlist mk_exists ex_vars phi_i ) 
        | _ -> fm
    ) False lst2 in
    if debug then print_endline @@ "lower bound of view: " ^ (lean_string_of_fol_formula ((view_lower_fol)));

    (* make the equivalence sentence of checking whether lower FOL of view implies upper FOL of view *)
    let sentence_of_view_existence = generalize (Imp(view_lower_fol, view_upper_fol)) in 
    if debug then (print_endline @@ "FO sentence of view existence : " ^ (lean_string_of_fol_formula ((sentence_of_view_existence)));
    print_endline "_______________________________________\n");
    if debug then (print_endline "==> generating theorem for view existence";) else ();
    
    (* checking the FO sentence of the existence of a view *)
    let theorem_of_view_existence =  "theorem view_existence " ^ String.concat " " (List.map (fun x -> "{"^x^"}") (source_view_to_lean_func_types prog)) ^
     ": " ^ (Fol_ex.lean_string_of_fol_formula (Imp (Ast2fol.constraint_sentence_of_stt debug prog, 
     And(sentence_of_view_existence, generalize (Imp (phi, False)))))) in
    let lean_code_view_existence = gen_lean_code_for_theorems [theorem_of_view_existence] in
    
    if (not speedup) then   
        (if debug then print_endline "==> verifying view existence property";  
        let exists, message =  verify_fo_lean (debug) timeout lean_code_view_existence in
        if not (exists=0) then 
            (
            (* print_endline @@ "Program is not validated \nExit code: " ^ string_of_int is_unique; *)
            raise (ChkErr ("view does not exist" ^ (if (debug) then "\nError messange: "^ message else "")));););
    (* choose the lower bound as FO formula of view *)
    let view_fo = view_lower_fol in 
    let refined_view_fo = remove_trivial (ranf (Skolem.simplify (normalize_comparison view_fo))) in
    if debug then print_endline @@ "FO formula of the view: " ^ Fol_ex.lean_string_of_fol_formula refined_view_fo ;
    if (refined_view_fo = False) then raise (ChkErr "fail to construct the view definition, make sure there is no self-join or projection on the view");
    let raw_get_ast = view_fol2datalog debug (Expr.get_schema_stts prog) view_vars (refined_view_fo ) in
    let get_ast = Ast2fol.optimize_query_datalog debug (Expr.insert_stt (Query (Expr.get_schema_rterm (get_view prog))) raw_get_ast) in
    if debug then (
        print_endline "______get datalog program:_______";
        print_endline (Expr.string_of_prog get_ast);
        print_endline "_________________________________";
    );
    (* verify putget property *)
    let get_rules exp = fst (Rule_preprocess.seperate_rules exp) in 
    let bi_prog = Expr.add_stts (get_rules get_ast) inputprog in
    let putget_thm = (lean_simp_theorem_of_putget (debug) (constraint2rule bi_prog)) in
    let lean_code_putget = gen_lean_code_for_theorems [ putget_thm ] in

    if (not speedup) then (
        if debug then print_endline "==> verifying putget property";
        let exitcode, message = verify_fo_lean (debug) (timeout) lean_code_putget in 
        if not (exitcode=0) then 
            if (exitcode = 124) then (raise (ChkErr ("Timeout"))) 
            else
            (raise (ChkErr ("Property putget is not validated \nExit code: " ^ string_of_int exitcode
            ^ (if (debug) then "\nError messange: "^ message else ""))));
    );
    
    if speedup then (
        if debug then print_endline "==> verifying all properties of putback program";
        let lean_code_all = gen_lean_code_for_theorems [ disdelta_thm; theorem_of_view_existence; putget_thm ] in
        let exitcode, message = verify_fo_lean (debug) (timeout) lean_code_all in 
        if not (exitcode=0) then 
            if (exitcode = 124) then (raise (ChkErr ("Timeout"))) 
            else
            (raise (ChkErr ("Well-behavedness is not validated \nExit code: " ^ string_of_int exitcode
            ^ (if (debug) then "\nError messange: "^ message else ""))));
    );
    print_endline @@ "-- Program is validated and the view definition is derived --";
    get_ast;;
