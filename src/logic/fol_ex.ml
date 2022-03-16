(*******************************************************)
(**
Functions on first-order logic formulas
 *)
(********************************************************)

(*
author: Vandang Tran
*)

open Lib
open Formulas
open Fol
open Skolem
open Prop

(* ------------------------------------------------------------------------- *)
(* Printing of formulas, parametrized by atom printer.                       *)
(* ------------------------------------------------------------------------- *)

let rec normalize_comparison (fm : fol formula) : fol formula =
  match fm with
  | Atom (R (r, args)) ->
      begin
        match r with
        | "<>" -> Not (Atom (R ("=", args)))
        | "<=" -> Not (Atom (R (">", args)))
        | ">=" -> Not (Atom (R ("<", args)))
        | _    -> fm
      end

  | Not p        -> Not (normalize_comparison p)
  | And(p, q)    -> And (normalize_comparison p, normalize_comparison q)
  | Or(p, q)     -> Or (normalize_comparison p, normalize_comparison q)
  | Imp(p, q)    -> Imp (normalize_comparison p, normalize_comparison q)
  | Iff(p, q)    -> Iff (normalize_comparison p, normalize_comparison q)
  | Forall(x, p) -> Forall (x, normalize_comparison p)
  | Exists(x, p) -> Exists (x, normalize_comparison p)
  | _            -> fm

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

let string_of_atom prec (R(p,args)) =
  if mem p ["="; "<"; "<="; ">"; ">="; "<>"] && length args = 2
  then string_of_infix_term false 12 12 (" "^p) (el 0 args) (el 1 args)
  else string_of_fargs p args;;

let string_of_fol_formula = string_of_qformula string_of_atom;;

(* ------------------------------------------------------------------------- *)
(* Printing of formulas in lean.                                             *)
(* ------------------------------------------------------------------------- *)

type lean_variable = string

type lean_infix_operator =
  | LeanInfixEqual    (* = *)
  | LeanInfixLt       (* < *)
  | LeanInfixGt       (* > *)
  | LeanInfixNotEqual (* ≠ *)
  | LeanInfixLeq      (* ≤ *)
  | LeanInfixGeq      (* ≥ *)
  | LeanInfixAnd      (* ∧ *)
  | LeanInfixOr       (* ∨ *)
  | LeanInfixImp      (* → *)
  | LeanInfixIff      (* ↔ *)
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


let stringify_lean_infix_operator = function
  | LeanInfixEqual    -> "="
  | LeanInfixLt       -> "<"
  | LeanInfixGt       -> ">"
  | LeanInfixNotEqual -> "≠"
  | LeanInfixLeq      -> "≤"
  | LeanInfixGeq      -> "≥"
  | LeanInfixAnd      -> "∧"
  | LeanInfixOr       -> "∨"
  | LeanInfixImp      -> "→"
  | LeanInfixIff      -> "↔"
  | LeanInfixConcat   -> "++"
  | LeanInfixDiv      -> "/"
  | LeanInfixMult     -> "*"
  | LeanInfixSubtr    -> "-"
  | LeanInfixAdd      -> "+"
  | LeanInfixCons     -> "::"


let stringify_lean_formula (lfm : lean_formula) : string =
  let rec aux (lfm : lean_formula) =
    match lfm with
    | LeanNull       -> "null"
    | LeanBool true  -> "true"
    | LeanBool false -> "false"
    | LeanInt n      -> string_of_int n
    | LeanFloat r    -> string_of_float r
    | LeanString s   -> Printf.sprintf "\"%s\"" s
    | LeanVar x      -> x
    | LeanNot lfm0   -> Printf.sprintf "(¬ %s)" (aux lfm0)

    | LeanInfix (op, lfm1, lfm2) ->
        let s_op = stringify_lean_infix_operator op in
        let s1 = aux lfm1 in
        let s2 = aux lfm2 in
        Printf.sprintf "(%s %s %s)" s1 s_op s2

    | LeanApp (f, lfms) ->
        let ss = lfms |> List.map aux in
        Printf.sprintf "(%s %s)" f (String.concat " " ss)

    | LeanForall (x, lfm0) -> Printf.sprintf "(∀%s, %s)" x (aux lfm0)
    | LeanExists (x, lfm0) -> Printf.sprintf "(∃%s, %s)" x (aux lfm0)

    | LeanAnnot (lfm0, styp) ->
        let annot =
          match styp with
          | LeanAnnotInt    -> "int"
          | LeanAnnotRat    -> "rat"
          | LeanAnnotProp   -> "Prop"
          | LeanAnnotString -> "string"
        in
        Printf.sprintf "(%s: %s)" (aux lfm0) annot
  in
  aux lfm

(*
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

let lean_character_of_operator o = match o with
  | "<>" -> "≠"
  | ">=" -> "≥"
  | "<=" -> "≤"
  | "=" | "<"| ">" -> o
  | _ -> failwith "unkown symbol of "^o

let lean_stype_of_string (str : string) =
  try  (ignore(int_of_string str); "int") with
    (* try test () with *)
    | Failure e ->
        try  (ignore(float_of_string str); "rat") with
        (* try test () with *)
        | Failure e ->
            try  ( ignore(bool_of_string str); "Prop") with
            | Failure e | Invalid_argument e ->  if (str = "null") then  "int" else  "string"

let lean_string_of_string (str : string) =
  try  (ignore(int_of_string str); str) with
    (* try test () with *)
    | Failure e ->
        try  (ignore(float_of_string str); str) with
        (* try test () with *)
        | Failure e ->
            try  ( ignore(bool_of_string str); str) with
            | Failure e | Invalid_argument e ->
            if (str = "null") then  str else "\""^(String.sub str 1 (String.length str -2))^"\""
*)

let lean_formula_of_literal (str : string) : lean_formula * lean_annot =
  try
    let n = int_of_string str in
    (LeanInt n, LeanAnnotInt)
  with
  | _ ->
      try
        let r = float_of_string str in
        (LeanFloat r, LeanAnnotRat)
      with
      | _ ->
          try
            let b = bool_of_string str in
            (LeanBool b, LeanAnnotProp)
          with
          | _ ->
              match str with
              | "null" -> (LeanNull, LeanAnnotInt)
              | _      -> (LeanString (String.sub str 1 (String.length str - 2)), LeanAnnotString)


let rec lean_formula_of_term (fm : term) : lean_formula =
  let iter = lean_formula_of_term in
  match fm with
  | Var x                      -> LeanVar x
  | Fn ("^", [tm1; tm2])       -> LeanInfix (LeanInfixConcat, iter tm1, iter tm2)
  | Fn ("/", [tm1; tm2])       -> LeanInfix (LeanInfixDiv, iter tm1, iter tm2)
  | Fn ("*", [tm1; tm2])       -> LeanInfix (LeanInfixMult, iter tm1, iter tm2)
  | Fn ("-", [tm1; tm2])       -> LeanInfix (LeanInfixSubtr, iter tm1, iter tm2)
  | Fn ("+", [tm1; tm2])       -> LeanInfix (LeanInfixAdd, iter tm1, iter tm2)
  | Fn ("::", [tm1; tm2])      -> LeanInfix (LeanInfixCons, iter tm1, iter tm2)
  | Fn (literal, [])           -> let (lfm, styp) = lean_formula_of_literal literal in LeanAnnot (lfm, styp)
  | Fn (f, ((_ :: _) as args)) -> LeanApp (f, List.map iter args)
(* ORIGINAL:
  match fm with
    Var x -> x
  | Fn("^",[tm1;tm2]) -> lean_string_of_infix_term true prec 24 "++" tm1 tm2
  | Fn("/",[tm1;tm2]) -> lean_string_of_infix_term true prec 22 " /" tm1 tm2
  | Fn("*",[tm1;tm2]) -> lean_string_of_infix_term false prec 20 " *" tm1 tm2
  | Fn("-",[tm1;tm2]) -> lean_string_of_infix_term true prec 18 " -" tm1 tm2
  | Fn("+",[tm1;tm2]) -> lean_string_of_infix_term false prec 16 " +" tm1 tm2
  | Fn("::",[tm1;tm2]) -> lean_string_of_infix_term false prec 14 "::" tm1 tm2
  | Fn(f,args) -> lean_string_of_fargs f args
*)

(*
and lean_string_of_fargs (f : string) (args : term list) : lean_formula =
  if args = [] then
    "("^(lean_string_of_string f)^":"^ (lean_stype_of_string f)^")"
  else
    f^ ( "("^
    lean_string_of_term 0 (hd args)^ break_line 0^
    String.concat "" (List.map (fun t ->  ","^break_line 0^ lean_string_of_term 0 t)
            (tl args))^
    ")")

and lean_string_of_infix_term isleft oldprec newprec sym p q =
  if oldprec > newprec then "(" else ""^
  lean_string_of_term (if isleft then newprec else newprec+1) p^
   sym^
  break_line (if String.sub sym 0 1 = " " then 1 else 0)^
  lean_string_of_term (if isleft then newprec+1 else newprec) q^
  (if oldprec > newprec then  ")" else "");;

let lean_string_of_fargs f args =
    f^
    if args = [] then "" else
    ( " "^
    lean_string_of_term 0 (hd args)^ break_line 0^
    String.concat "" (List.map (fun t ->  " "^break_line 0^ lean_string_of_term 0 t)
            (tl args)))
*)

let lean_formula_of_atom (R (p, args) : fol) : lean_formula =
  let iter = lean_formula_of_term in
  match (p, args) with
  | ("=", [tm1; tm2])  -> LeanInfix (LeanInfixEqual, iter tm1, iter tm2)
  | ("<", [tm1; tm2])  -> LeanInfix (LeanInfixLt, iter tm1, iter tm2)
  | (">", [tm1; tm2])  -> LeanInfix (LeanInfixGt, iter tm1, iter tm2)
  | ("<>", [tm1; tm2]) -> LeanInfix (LeanInfixNotEqual, iter tm1, iter tm2)
  | ("<=", [tm1; tm2]) -> LeanInfix (LeanInfixLeq, iter tm1, iter tm2)
  | (">=", [tm1; tm2]) -> LeanInfix (LeanInfixGeq, iter tm1, iter tm2)
  | _                  -> LeanApp (p, List.map iter args)

(* ORIGINAL:
  if mem p ["="; "<"; "<="; ">"; ">="; "<>"] && length args = 2 then
    lean_string_of_infix_term false 12 12 (" "^ lean_character_of_operator p) (el 0 args) (el 1 args)
  else
    lean_string_of_fargs p args
*)


let lean_formula_of_fol_formula (fm : fol formula) : lean_formula =
  let rec aux fm =
    match fm with
    | False        -> LeanBool false
    | True         -> LeanBool true
    | Atom pargs   -> lean_formula_of_atom pargs
    | Not p        -> LeanNot (aux p)
    | And (p, q)   -> LeanInfix (LeanInfixAnd, aux p, aux q)
    | Or (p, q)    -> LeanInfix (LeanInfixOr, aux p, aux q)
    | Imp(p, q)    -> LeanInfix (LeanInfixImp, aux p, aux q)
    | Iff(p, q)    -> LeanInfix (LeanInfixIff, aux p, aux q)
    | Forall(x, p) -> LeanForall (x, aux p)
    | Exists(x, p) -> LeanExists (x, aux p)
  in
  aux (normalize_comparison fm)

(* ------------------------------------------------------------------------- *)
(* Printing of formulas in z3.                                             *)
(* ------------------------------------------------------------------------- *)

(** get a type of a free variable in the formula
(not completed yet) *)
let rec type_of_var varname fm =
  match fm with
    False -> []
  | True -> []
  | Atom(R(r,args)) ->
    if r = "=" then (
      let v, o = List.partition (fun x -> x = varname) (List.map (string_of_term 0) args) in
      [(r^ hd o, 0)]
    )
    else
    (let lst,_ = List.fold_left
      (fun (lst, index) str -> if (string_of_term 0 str) = varname then (lst@[index], index+1) else (lst, index+1))
      ([],0)
      args in
    List.map (fun x -> (r,x)) lst)
  | Not(p) -> type_of_var varname p
  | And(p,q) -> union (type_of_var varname p)  (type_of_var varname q)
  | Or(p,q) -> union (type_of_var varname p) (type_of_var varname q)
  | Imp(p,q) -> union (type_of_var varname p) (type_of_var varname q)
  | Iff(p,q) -> union (type_of_var varname p) (type_of_var varname q)
  | Forall(x,p) -> type_of_var varname (let newx = variant x (varname::(fv fm)) in subst (x |=> Var newx) p)
  | Exists(x,p) -> type_of_var varname (let newx = variant x (varname::(fv fm)) in subst (x |=> Var newx) p)

let z3_logic_character name = match name with
    | "false" -> "false"
    | "true" -> "true"
    | "not" -> "not"
    | "and" -> "and"
    | "or" -> "∨"
    | "imply" -> "=>"
    | "iff" -> "="
    | "forall" -> "forall"
    | "exists" -> "exists"
    | "quantifer_sep" -> failwith "unkown symbol of "^name
    | _ -> failwith "unkown symbol of "^name

let z3_character_of_operator o = match o with
  | "<>" -> failwith "unkown symbol of "^o
  | ">="
  | "<="
  | "=" | "<"| ">" -> o
  | _ -> failwith "unkown symbol of "^o

let z3_string_of_string str =
  try  (ignore(int_of_string str); str) with
    (* try test () with *)
    | Failure e ->
        try  (ignore(float_of_string str); str) with
        (* try test () with *)
        | Failure e ->
            try  ( ignore(bool_of_string str); str) with
            | Failure e | Invalid_argument e ->
            if (str = "null") then  str else "\""^(String.sub str 1 (String.length str -2))^"\""

let z3_normalize_string str =
  Str.global_replace (Str.regexp  "\'") "_prime" str;;

let rec z3_string_of_term fm =
  match fm with
    Var x -> z3_normalize_string x
  | Fn(f,tms) -> if List.length tms = 0 then z3_normalize_string f
      else "(" ^z3_normalize_string f ^ " " ^ String.concat " " (List.map z3_string_of_term tms) ^ " )"

let rec z3_string_of_atom (R(p,args)) =
  if p = "<>" then "( not " ^ (z3_string_of_atom (R("=",args))) ^ ")"
  else "( "^ p ^" " ^ String.concat " " (List.map z3_string_of_term args) ^ " )"

let rec z3_string_of_qformula fm =
    match fm with
      False ->   "false"
    | True ->   "true"
    | Atom(pargs) -> z3_string_of_atom pargs
    | Not(p) -> "( not " ^ z3_string_of_qformula p ^ " )"
    | And(p,q) -> "( and " ^ z3_string_of_qformula p ^" " ^z3_string_of_qformula q ^ " )"
    | Or(p,q) ->  "( or " ^ z3_string_of_qformula p ^" " ^z3_string_of_qformula q ^ " )"
    | Imp(p,q) ->  "( => " ^ z3_string_of_qformula p ^" " ^z3_string_of_qformula q ^ " )"
    | Iff(p,q) ->  "( = " ^ z3_string_of_qformula p ^" " ^z3_string_of_qformula q ^ " )"
    (* todo: need to add a type after the x *)
    | Forall(x,p) ->
      let rname, pos = hd (type_of_var x p) in
      "( forall ((" ^z3_normalize_string x^ " " ^rname ^string_of_int pos ^")) " ^ z3_string_of_qformula p ^ " )"
    | Exists(x,p) -> let rname, pos = hd (type_of_var x p) in
      "( exists ((" ^z3_normalize_string x^ " " ^rname ^string_of_int pos ^")) " ^ z3_string_of_qformula p ^ " )"

let z3_string_of_fol_formula = z3_string_of_qformula;;

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
      let powerset = allnonemptysubsets conj in
      let is_right_candiate cand =
        let psi = Prop.list_conj cand in
        let xi' = Prop.list_disj (List.map (fun x ->  And(psi, x)) (to_dis_lst subfm)) in
        is_safe_range xi' in
      let right_candidates = List.filter is_right_candiate powerset in
      let sorted_candiates = Lib.sort (fun a b -> List.length a < List.length b) right_candidates in
      let final_cand = List.hd sorted_candiates in
      let remain = subtract conj final_cand in
      let phi_remain = Prop.list_conj remain in
      let psi = Prop.list_conj final_cand in
      let xi' = Prop.list_disj (List.map (fun x ->  And(psi, x)) (to_dis_lst subfm)) in
      And(phi_remain, xi')

  (* Push-into-quantifier *)
  | Exists(_, _) ->
    let powerset = allnonemptysubsets conj in
    let is_right_candiate cand =
      let psi = Prop.list_conj cand in
      let quants, xi = extract_ex_quants subfm in
      let quants' = List.map (fun x -> variant x (fv psi)) quants in
      let subfn = fpf quants (List.map (fun x -> Fol.Var x) quants') in
      let xi' = ( And(psi, subst subfn xi)) in
      is_safe_range xi' in
    let right_candidates = List.filter is_right_candiate powerset in
    let sorted_candiates = Lib.sort (fun a b -> List.length a < List.length b) right_candidates in
    let final_cand = List.hd sorted_candiates in
    let remain = subtract conj final_cand in
    let phi_remain = Prop.list_conj remain in
    let psi = Prop.list_conj final_cand in
    let quants, xi = extract_ex_quants subfm in
    let quants' = List.map (fun x -> variant x (fv psi)) quants in
    let subfn = fpf quants (List.map (fun x -> Fol.Var x) quants') in
    let xi' = And(psi, subst subfn xi) in
    And(phi_remain, itlist mk_exists quants' xi')
  (* Push-into-negated-quantifier *)
  | Not(Exists(x, fm)) ->
    let powerset = allnonemptysubsets conj in
    let is_right_candiate cand =
      let psi = Prop.list_conj cand in
      let quants, xi = extract_ex_quants (Exists(x, fm)) in
      let quants' = List.map (fun x -> variant x (fv psi)) quants in
      let subfn = fpf quants (List.map (fun x -> Fol.Var x) quants') in
      let xi' = ( And(psi, subst subfn xi)) in
      is_safe_range xi' in
    let right_candidates = List.filter is_right_candiate powerset in
    let sorted_candiates = Lib.sort (fun a b -> List.length a < List.length b) right_candidates in
    let final_cand = List.hd sorted_candiates in
    let phi = Prop.list_conj conj in
    let psi = Prop.list_conj final_cand in
    let quants, xi = extract_ex_quants (Exists(x, fm)) in
    let quants' = List.map (fun x -> variant x (fv psi)) quants in
    let subfn = fpf quants (List.map (fun x -> Fol.Var x) quants') in
    let xi' = And(psi, subst subfn xi) in
    And(phi, Not (itlist mk_exists quants' xi'))
      (* And(Prop.list_conj conj, Not(push_into_subfm conj (Exists(x, fm)))) *)
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

(** take a FO formula in nnf and return its dnf *)
(* todo: this function can not go to the lower level of quantifier *)
let rec pure_dnf fm =
  match fm with
    Forall(x,p) -> Forall(x,pure_dnf p)
  | Exists(x,p) -> Exists(x,pure_dnf p)
  | _ -> Prop.dnf fm;;

(** take a FO formula and return its dnf *)
let dnf fm = pure_dnf (Skolem.nnf(Skolem.simplify fm));;

(* note that in order to preserve certain order and also show the conciseness of the implementation, no tail-recursive is used *)
let ins_all_positions x l =
  let rec aux prev acc = function
    | [] -> (prev @ [x]) :: acc |> List.rev
    | hd::tl as l -> aux (prev @ [hd]) ((prev @ [x] @ l) :: acc) tl
  in
  aux [] [] l

let rec permutations = function
  | [] -> []
  | x::[] -> [[x]] (* we must specify this edge case *)
  | x::xs -> List.fold_left (fun acc p -> acc @ ins_all_positions x p ) [] (permutations xs)

let conj_trivial lits =
  let pos,neg = partition positive lits in
  let power_set_pos = if (List.length lits < 11) then allnonemptysubsets lits else List.map (fun x -> [x]) lits in
  (* let power_set_with_permutation = List.fold_left (fun lst x -> lst@(permutations x) ) [] power_set_pos in *)
  (* List.iter (fun x -> print_endline (lean_string_of_fol_formula x)) (List.map list_disj power_set_with_permutation); *)
  intersect (List.map list_conj power_set_pos) (image negate neg) <> [];;

let disj_trivial lits =
  let pos,neg = partition positive lits in
  let power_set_pos = if (List.length lits < 11) then allnonemptysubsets lits else List.map (fun x -> [x]) lits in
  (* let power_set_with_permutation = List.fold_left (fun lst x -> lst@(permutations x) ) [] power_set_pos in *)
  (* List.iter (fun x -> print_endline (lean_string_of_fol_formula x)) (List.map list_disj power_set_with_permutation); *)
  intersect (List.map list_disj power_set_pos) (image negate neg) <> [];;

let remove_trivial1 fm =
  match fm with
   And(p,q) ->
    let conj = (to_conj_lst fm) in
    (* print_endline "checking conj :"; *)
    (* List.iter (fun x -> print_endline (lean_string_of_fol_formula x)) conj; *)
    if (conj_trivial conj) then False else simplify (list_conj conj)
  | Or(p,q) ->
    let disj = (to_dis_lst fm) in
    if (disj_trivial disj) then True else simplify (list_disj disj)
  | _ -> simplify fm;;

let rec remove_trivial fm =
  match fm with
    Not p -> remove_trivial1 (Not(remove_trivial p))
  | And(p,q) -> remove_trivial1 (And(remove_trivial p,remove_trivial q))
  | Or(p,q) -> remove_trivial1 (Or(remove_trivial p,remove_trivial q))
  | Imp(p,q) -> remove_trivial1 (Imp(remove_trivial p,remove_trivial q))
  | Iff(p,q) -> remove_trivial1 (Iff(remove_trivial p,remove_trivial q))
  | Forall(x,p) -> remove_trivial1(Forall(x,remove_trivial p))
  | Exists(x,p) -> remove_trivial1(Exists(x,remove_trivial p))
  | _ -> fm;;

(** take a FOL formula and return whether it contains view predicate or not  *)
let rec is_superformula_of_view view fm = match fm with
    False -> false
  | True -> false
  | Atom(R(r,args)) -> if r = view then true else false
  | Not(p) -> is_superformula_of_view view  p
  | And(p,q) -> (is_superformula_of_view view p ) || (is_superformula_of_view view q )
  | Or(p,q) -> (is_superformula_of_view view p ) || (is_superformula_of_view view q )
  | Imp(p,q) -> (is_superformula_of_view view p ) || (is_superformula_of_view view q )
  | Iff(p,q) -> (is_superformula_of_view view p ) || (is_superformula_of_view view q )
  | Forall(x,p) -> is_superformula_of_view view p
  | Exists(x,p) -> is_superformula_of_view view p

(** take a RANF formula and view name to return the it in view-predicate normal form
view-predicate normal form is represented in the form:
(phi, a list of (a list of quantifier vars, a fol of (¬)V(vi1)∧...∧(¬)V(vimi), phi_i ) )
*)

let rec ranf2lvnf view fm = if not (is_superformula_of_view view fm) then (fm, []) else
    match fm with
        | Atom(R(r,lst)) -> if r = view then (False, [([],fm, True)]) else (fm, [])
        | Or(p, q) ->
            let phi1, lst1 = ranf2lvnf view p in
            let phi2, lst2 = ranf2lvnf view q in
            (remove_trivial(Or(phi1, phi2)), lst1@lst2)
        | Exists(x, p) ->
            let phi1, lst1 = ranf2lvnf view p in
            let add_exist var (varlst, vfol, phi_i) =
                if List.mem var varlst then (varlst, vfol, phi_i)
                else if List.mem var (fv vfol) then (union [var] varlst, vfol, phi_i)
                else (varlst, vfol, remove_trivial (mk_exists var phi_i)) in
            let newlst = List.map (add_exist x) lst1 in
            (remove_trivial (mk_exists x phi1), newlst)
        | And(_,_) -> let conj_lst = to_conj_lst fm in
            lvnf_of_conj (List.map (ranf2lvnf view) conj_lst)
        | Not(p) ->
            let phi1, lst1 = ranf2lvnf view p in
            let negate_e (varlst, vfol, phi_i) =
                (* negate_e return a fol in linear-view normal form *)
                if (List.length varlst) = 0 then
                (* no quantifiers  *)
                let dis_lst_of_v = disjuncts (nnf (Formulas.Not(vfol))) in
                let lstfol = List.map (fun x -> ([],x,True)) dis_lst_of_v in
                (Formulas.Not(phi_i), lstfol)
                (* if it has a quantifier, we can not do it
                just consider it as a phi_i (remained part of linear-view normal form)
                should raise a warning here, the formula is not linear-view
                can be optimized here, when we can choose a V in vfol which does not have free variables in varlst
                the we can pull it out
                *)
                else (remove_trivial(Formulas.Not(itlist mk_exists varlst ((Formulas.And (vfol, phi_i))))), [] ) in
            let conjlst = List.map negate_e lst1 in
            lvnf_of_conj ((remove_trivial(Formulas.Not(phi1)), [])::conjlst)

        | _ -> (fm, [])

and and_of_two_lvnf (phi1, lst1) (phi2, lst2) =
    let newlst1 = ([],True,phi1)::lst1 in
    let newlst2 = ([],True,phi2)::lst2 in
    let and_two_e (vars1, vfol1, phi_i1) (vars2, vfol2, phi_i2) =
        (* (exists vars1, vfol1 and phi_i1) and (exists vars2, vfol2 and phi_i2) *)
        let intro_var freevars v (newvs, subfn) =
            let z = variant v freevars in
            (newvs@[v], (v |-> Fol.Var z) subfn) in
        let freevars1 = subtract ((union (fv (And(vfol1, phi_i1))) (fv (And(vfol2, phi_i2))))) (union vars1 vars2) in
        let newvars1, subfn1 = itlist (intro_var freevars1) vars1 ([],undefined) in
        let newvfol1 = subst subfn1 vfol1 in
        let newphi_i1 = subst subfn1 phi_i1 in
        (* now we get
        exists newvars1, (newvfol1 and newphi_i1) and (exists vars2, vfol2 and phi_i2)
        we continue to pull the vars2 in the formula
        (newvfol1 and newphi_i1) and (exists vars2, vfol2 and phi_i2)
         *)
        let freevars2 = subtract ((union (fv (And(newvfol1, newphi_i1))) (fv (And(vfol2, phi_i2))))) vars2 in
        let newvars2, subfn2 = itlist (intro_var freevars2) vars2 ([],undefined) in
        let newvfol2 = subst subfn2 vfol2 in
        let newphi_i2 = subst subfn2 phi_i2 in
        (* now we get
        exists newvars1, exists newvars2, (newvfol1 and newphi_i1) and (newvfol2 and newvphi_i2)
         *)
        (newvars1@newvars2, remove_trivial (And(newvfol1, newvfol2)), remove_trivial(And(newphi_i1, newphi_i2))) in
    let dis_law_result = allpairs and_two_e newlst1 newlst2 in
    (* we can find the phi in this list result which is from phi1 and phi2 *)
    let phis, others = List.partition (fun x -> match x with ([], True, _) -> true | _ -> false) dis_law_result in
    let filtered_others = List.filter (fun x -> match x with (_, _, False) -> false | _ -> true) others in
    let flattedphis = List.map (fun (_,_,x) -> x) phis in
    (Prop.list_disj flattedphis, filtered_others)

and lvnf_of_conj conj_lst =
    match conj_lst with
    hd::tl ->
    List.fold_left (fun x y -> and_of_two_lvnf x y) hd tl
    | _ -> invalid_arg "function lvnf_of_conj called with a empty list";;

let ranf2lvnf view fm = ranf2lvnf view (remove_trivial fm);;
