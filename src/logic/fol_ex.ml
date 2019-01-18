(*******************************************************)
(**  
Functions on first-order logic formulas
 *)
(********************************************************)

(* 
author: Vandang Tran
*)

open Lib;;
open Formulas;;
open Fol;;

(* ------------------------------------------------------------------------- *)
(* Printing of formulas, parametrized by atom printer.                       *)
(* ------------------------------------------------------------------------- *)

let lean_logic_character name = match name with 
    | "false" -> "false"
    | "true" -> "true"
    | "not" -> "¬"
    | "and" -> "∧"
    | "or" -> "∨"
    | "imply" -> "→"
    | "iff" -> "↔"
    | "forall" -> "∀"
    | "exists" -> "∃"
    | "quantifer_sep" -> ","
    | _ -> failwith "unkown symbol of "^name

let normal_logic_character name = match name with 
    | "false" -> "false"
    | "true" -> "true"
    | "not" -> "~"
    | "and" -> "/\\"
    | "or" -> "\\/"
    | "imply" -> "==>"
    | "iff" -> "<=>"
    | "forall" -> "forall"
    | "exists" -> "exists"
    | "quantifer_sep" -> "."
    | _ -> failwith "unkown symbol of "^name

let break_line sp = if(sp>0) then (String.concat "" (List.map (fun x -> " ") (0--(sp-1)))) else ""

let bracket p n f x y =
    (if p then  "(" else "")^
    f x y^
    (if p then  ")" else "");;

let string_of_formula log_str pfn =
  let rec string_of_formula log_str pr fm =
    match fm with
      False ->  log_str "false"
    | True ->  log_str "true"
    | Atom(pargs) -> pfn pr pargs
    | Not(p) -> bracket (pr > 10) 1 (string_of_prefix 10) (log_str "not") p
    | And(p,q) -> bracket (pr > 8) 0 (string_of_infix 8 (log_str "and")) p q
    | Or(p,q) ->  bracket (pr > 6) 0 (string_of_infix  6 (log_str "or")) p q
    | Imp(p,q) ->  bracket (pr > 4) 0 (string_of_infix 4 (log_str "imply")) p q
    | Iff(p,q) ->  bracket (pr > 2) 0 (string_of_infix 2 (log_str "iff")) p q
    | Forall(x,p) -> bracket (pr > 0) 2 string_of_qnt (log_str "forall") (strip_quant fm)
    | Exists(x,p) -> bracket (pr > 0) 2 string_of_qnt (log_str "exists") (strip_quant fm)
  and string_of_qnt qname (bvs,bod) =
    qname^ " "^
    String.concat " " bvs ^
    log_str "quantifer_sep"^ " " ^
    string_of_formula log_str 0 bod
  and string_of_prefix newpr sym p =
    sym^ string_of_formula log_str (newpr+1) p
  and string_of_infix newpr sym p q =
    string_of_formula log_str (newpr+1) p^
    (" "^sym)^ " "^
    string_of_formula log_str newpr q in
  string_of_formula log_str 0;;

let string_of_qformula pfn fm =
    string_of_formula normal_logic_character pfn fm;;

let lean_string_of_qformula pfn fm =
    string_of_formula lean_logic_character pfn fm;;


(* ------------------------------------------------------------------------- *)
(* Printing of terms.                                                        *)
(* ------------------------------------------------------------------------- *)

let rec string_of_term prec fm =
  match fm with
    Var x -> x
  | Fn("^",[tm1;tm2]) -> string_of_infix_term true prec 24 "^" tm1 tm2
  | Fn("/",[tm1;tm2]) -> string_of_infix_term true prec 22 " /" tm1 tm2
  | Fn("*",[tm1;tm2]) -> string_of_infix_term false prec 20 " *" tm1 tm2
  | Fn("-",[tm1;tm2]) -> string_of_infix_term true prec 18 " -" tm1 tm2
  | Fn("+",[tm1;tm2]) -> string_of_infix_term false prec 16 " +" tm1 tm2
  | Fn("::",[tm1;tm2]) -> string_of_infix_term false prec 14 "::" tm1 tm2
  | Fn(f,args) -> string_of_fargs f args

and string_of_fargs f args =
    f^
    if args = [] then "" else
    ( "("^
    string_of_term 0 (hd args)^ break_line 0^
    String.concat "" (List.map (fun t ->  ","^break_line 0^ string_of_term 0 t)
            (tl args))^
    ")")

and string_of_infix_term isleft oldprec newprec sym p q =
  if oldprec > newprec then "(" else ""^
  string_of_term (if isleft then newprec else newprec+1) p^
   sym^
  break_line (if String.sub sym 0 1 = " " then 1 else 0)^
  string_of_term (if isleft then newprec+1 else newprec) q^
  (if oldprec > newprec then  ")" else "");;

(* #install_printer printert;; *)

(* ------------------------------------------------------------------------- *)
(* Printing of formulas.                                                     *)
(* ------------------------------------------------------------------------- *)

let string_of_atom prec (R(p,args)) =
  if mem p ["="; "<"; "<="; ">"; ">="] && length args = 2
  then string_of_infix_term false 12 12 (" "^p) (el 0 args) (el 1 args)
  else string_of_fargs p args;;

let lean_string_of_fargs f args =
    f^
    if args = [] then "" else
    ( " "^
    string_of_term 0 (hd args)^ break_line 0^
    String.concat "" (List.map (fun t ->  " "^break_line 0^ string_of_term 0 t)
            (tl args)))

let rec lean_string_of_atom prec (R(p,args)) =
  if mem p ["="; "<"; "<="; ">"; ">="] && length args = 2
  then string_of_infix_term false 12 12 (" "^p) (el 0 args) (el 1 args)
  else lean_string_of_fargs p args;;

let string_of_fol_formula = string_of_qformula string_of_atom;;

let lean_string_of_fol_formula = lean_string_of_qformula lean_string_of_atom;;

(* ------------------------------------------------------------------------- *)
(* operations on formulas.                                                     *)
(* ------------------------------------------------------------------------- *)

let rec extract_ex_quants fm =
  match fm with
    Exists(x,p) -> let quants, body = extract_ex_quants p in 
      x::quants, body
  | _ -> [],fm;;

(* ------------------------------------------------------------------------- *)
(* Tranformation on formulas.                                                     *)
(* ------------------------------------------------------------------------- *)

let rec pnf_dnf fm = dnf_of_pnf (Skolem.pnf fm) 
and dnf_of_pnf fm = 
  match fm with 
    Forall(x,p) -> Forall(x, dnf_of_pnf p)
  | Exists(x,p) -> Exists(x, dnf_of_pnf p)
  | _ -> Prop.dnf fm;;

(** take a FO formula and return its safe-range normal form *)
let rec srnf fm = 
  match fm with
      And(p,q) -> And(srnf p,srnf q)
    | Or(p,q) -> Or(srnf p,srnf q)
    | Imp(p,q) -> Or(srnf(Not p),srnf q)
    | Iff(p,q) -> Or(And(srnf p,srnf q),And(srnf(Not p),srnf(Not q)))
    | Not(Not p) -> srnf p
    | Not(And(p,q)) -> Or(srnf(Not p),srnf(Not q))
    | Not(Or(p,q)) -> And(srnf(Not p),srnf(Not q))
    | Not(Imp(p,q)) -> And(srnf p,srnf(Not q))
    | Not(Iff(p,q)) -> Or(And(srnf p,srnf(Not q)),And(srnf(Not p),srnf q))
    | Forall(x,p) -> Not (Exists(x,srnf (Not p)))
    | Exists(x,p) -> Exists(x,srnf p)
    | Not(Forall(x,p)) -> Exists(x,srnf(Not p))
    | Not(Exists(x,p)) -> Not (Exists(x,srnf p))
    | _ -> fm;;

let is_relation_symbol str = 
  let idregex = Str.regexp "_*[A-Za-z][A-Za-z0-9_]*" in
    Str.string_match idregex str 0;;

(** take a SRNF formula and return the set of its range-restricted variables*)
let rec rr fm = 
  match fm with
    | True -> (true,[])
    | False -> (true,[])
    | Atom(R("=",[Var x; Fn (c,[])]))
    | Atom(R("=",[Fn (c,[]); Var x])) -> (true,[x])
    | Atom(R(p,args)) -> if is_relation_symbol p then (true, fv fm) else (true, [])
    | And(p,Atom(R("=",[Var x; Var y])))
    | And(Atom(R("=",[Var x; Var y])), p) -> let is_safe, rr_vars = rr p in if (intersect rr_vars [x;y]) = [] then (is_safe, rr_vars) else
     (is_safe, union rr_vars [x;y])
    | And(p,q) -> let is_safe1, rr_vars1 = rr p in 
      let is_safe2, rr_vars2 = rr q in
      (is_safe1 && is_safe2, union rr_vars1 rr_vars2)
    | Or(p,q) -> let is_safe1, rr_vars1 = rr p in 
      let is_safe2, rr_vars2 = rr q in
      (is_safe1 && is_safe2, intersect rr_vars1 rr_vars2)
    | Not(p) -> (fst (rr p),[])
    | Exists(x,p) -> let is_safe, rr_vars = rr p in 
      if mem x rr_vars then (is_safe, subtract rr_vars [x]) else (false, [])
    | _ -> (false,[]);;

let is_safe_range fm = 
  let is_safe, rr_vars = rr(srnf fm) in 
  is_safe && (set_eq rr_vars (fv fm));;

let rec to_dis_lst fm = 
  match fm with
    Or(p,q) -> union (to_dis_lst p) (to_dis_lst q)
  | _ -> [fm];;

let rec to_conj_lst fm = 
  match fm with
    And(p,q) -> union (to_conj_lst p) (to_conj_lst q)
  | _ -> [fm];;

let is_of_three_cases subfm = match subfm with 
  Or(_,_) -> if is_safe_range subfm then false else true
  | Exists(_, _) -> 
    let quants, xi = extract_ex_quants subfm in 
    if is_safe_range xi then false else true
  | Not(Exists(x, fm)) ->   
    let quants, xi = extract_ex_quants (Exists(x, fm)) in 
    if is_safe_range xi then false else true
  | _ -> false;;

(** apply three push-into procedures on a SRNF formula *)
let rec push_into_subfm conj subfm = 
  match subfm with
  (* Push-into-or *)
    Or(_,_) -> 
      let psi = Prop.list_conj conj in
      Prop.list_disj (List.map (fun x ->  And(psi, x)) (to_dis_lst subfm))
  (* Push-into-quantifier *)
  | Exists(_, _) -> 
      let psi = Prop.list_conj conj in
      let quants, xi = extract_ex_quants subfm in
      let quants' = List.map (fun x -> variant x (fv psi)) quants in
      let subfn = fpf quants (List.map (fun x -> Fol.Var x) quants') in
      itlist mk_exists quants' ( And(psi, subst subfn xi))
  (* Push-into-negated-quantifier *)
  | Not(Exists(x, fm)) ->
      And(Prop.list_conj conj, Not(push_into_subfm conj (Exists(x, fm))))
  | _ -> And(Prop.list_conj conj, subfm)
  ;;

(** take a SRNF formula and return it in RANF  *)
let rec srnf2ranf fm = 
  match fm with
    And(_,_) -> let conj_lst = to_conj_lst fm in 
        let not_self_contained_lst = List.filter is_of_three_cases conj_lst in
        if (List.length not_self_contained_lst > 0) then 
          let h = List.hd not_self_contained_lst in
            srnf2ranf (push_into_subfm (subtract conj_lst [h]) h)
        else Prop.list_conj (List.map srnf2ranf conj_lst) 
    | Or(p, q) -> Or(srnf2ranf p, srnf2ranf q) 
    | Exists(x, p) ->  Exists(x, srnf2ranf p)
    | Not(Exists(x, p)) -> Not (Exists(x, srnf2ranf p))
    | _ -> fm;;

let ranf fm = if (is_safe_range fm) then srnf2ranf (srnf fm) else failwith "the formula is not safe range" ;;