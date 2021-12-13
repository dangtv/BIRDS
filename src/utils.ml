(**  Data structure definitions and operations for other modules.
*)
open Expr
open Parsing
open Lexing
open Printf

(** Semantic error  *)
exception SemErr of string

(** Verification error  *)
exception ChkErr of string

(** Environment error  *)
exception EnvErr of string

(** Grammar error  *)
exception ParseErr of string

(** Lexing error  *)
exception LexErr of string

(** get a concrete message (file, possition of error, error message) for parsing or lexing error  *)
let spec_error msg start finish  =
  Printf.sprintf "File \"%s\", line %d, characters %d-%d: '%s'" start.pos_fname start.pos_lnum
    (start.pos_cnum  -start.pos_bol) (finish.pos_cnum  - finish.pos_bol) msg

(** raise a parsing error *)
let spec_parse_error msg nterm =
  raise ( ParseErr (spec_error msg (rhs_start_pos nterm) (rhs_end_pos nterm)))

(** raise a lexing error *)
let spec_lex_error lexbuf =
  raise ( LexErr (spec_error (lexeme lexbuf) (lexeme_start_p lexbuf) (lexeme_end_p lexbuf)))

(***********************************************************
 *  Symtable
 *********************************************************)

(** This type defines a symtable: a hash table that stores
    the set of rules that define a program. The keys
    are rule-name & rule-arity tuples and the values are lists of
    rules' AST specifications.
*)
type symtkey = (string*int) (* string is predicate name, int is the arity of literal*)
type symtable = (symtkey, (rterm * term list) list) Hashtbl.t (* each row of a symtable is all the rules which has the same literal in head*)

(* let hash_max_size = ref 500 *)

(** Prints a symtable
*)
let print_symtable (st:symtable) =
  let print_el s = Printf.printf "%s" (string_of_rule s) in
  let print_lst _ lst = List.iter print_el lst in
  Hashtbl.iter print_lst st

(** string of a symtable
*)
let string_of_symtable (st:symtable) =
  let p_el str s = str ^ (string_of_rule s) in
  let p_lst _ lst str = (List.fold_left p_el "" lst)^str in
  Hashtbl.fold p_lst st ""

(** Receives a rterm and generates its hash key for the
    symtable

    a rterm is identified by it predicate name and number of argument (arity)
*)
let symtkey_of_rterm rt : symtkey = (get_rterm_predname rt, get_arity rt)

(** Receives a rule and generates its hash key for the  symtable
*)
let symtkey_of_rule (h, b) : symtkey =  symtkey_of_rterm h

(** Inserts a rule in the symtable *)
let symt_insert (st:symtable) rule =
    let key = symtkey_of_rule rule in
    if Hashtbl.mem st key then
      Hashtbl.replace st key ((Hashtbl.find st key)@[rule]) (* add new rule into the list of rules of this key *)
    else
      Hashtbl.add st key [rule]

(** remove all rules of a key in the symtable if the symtable have this key *)
let symt_remove (st:symtable) key =
    if Hashtbl.mem st key then
      Hashtbl.remove st key

(** Compares two keys for ordering *)
let key_comp ((k1_n,k1_a):symtkey) ((k2_n,k2_a):symtkey) =
  let comp = String.compare k1_n k2_n in
  if comp != 0 then comp
  else k1_a - k2_a

(** Given a list of keys, remove repetitions *)
let remove_repeated_keys k_lst =
  let no_rep key = function
    | [] -> [key]
    | (hd::tl) ->
      if (key_comp key hd) == 0 then (hd::tl)
      else (key::hd::tl) in
  let sorted = List.sort key_comp k_lst in
  List.fold_right no_rep sorted []

(** Given a key, returns the predicate name that belongs to the key *)
let get_symtkey_predname ((n,_):symtkey) = n

(** Given a key, returns the predicate arity that belongs to the key *)
let get_symtkey_arity ((_,a):symtkey) = a

let string_of_symtkey ((n,a):symtkey) =
  n^"/"^(string_of_int a)

(** return alias for a symtkey, just append the string with arity of the symtkey  *)
let alias_of_symtkey ((n,a):symtkey) =
  n^"_a"^(string_of_int a)

(** this type defines a table for storing sql translated from datalog for each predicate  *)
type sqltable = (symtkey, (rterm * term list) list) Hashtbl.t (* each row of a symtable is all the rules which has the same literal in head*)


(**Takes a program and extracts all rules and places them in a symtable*)
let extract_idb expr =
    let idb:symtable = Hashtbl.create (2 * (List.length expr.rules)) in
    List.iter (symt_insert idb) expr.rules;
    idb

(** conbine idb and a query in a AST  *)
let idb_query_to_ast (idb:symtable) (query:rterm) =
  let p_lst _ lst sttlst = lst @ sttlst in
  {get_empty_expr with rules = Hashtbl.fold p_lst idb []; query = Some query}

(** Takes a program and extracts all schema declarations and places them in a symtable*)
let extract_edb expr =
    let edb:symtable = Hashtbl.create (2 * (List.length expr.sources) + 2) in
    List.iter (fun x -> symt_insert edb (get_schema_rterm x,[])) expr.sources;
    edb

(***********************************************************
 *  kset (SymtkeySet)
 *********************************************************)

(**This structure defines a set of symtable keys*)
module SymtkeySet = Set.Make(
  struct
    let compare = key_comp
    type t = symtkey
  end
  )

type kset = SymtkeySet.t

(***********************************************************
 *  Colnamtab
 *********************************************************)

(** This type defines a colnamtab, which is a dictionnary that
    contains for each predicate (edb & idb) a list with the name of
    all of its columns in order. The information is stored in a
    hash table using as a keys the keys from symtables.*)
type colnamtab = (symtkey, (string list)) Hashtbl.t

(*Extracts from the edb and idb their column names and
 * stores them in a colnamtab, places them in order*)
let build_colnamtab (edb:symtable) (idb:symtable) =
  let hs:colnamtab = Hashtbl.create (2*(Hashtbl.length edb + Hashtbl.length idb)) in
  let e_cols key rules =
    let rule = List.hd rules in
    let varlist = List.map string_of_var (get_rterm_varlist (rule_head rule)) in
    Hashtbl.add hs key varlist in
  Hashtbl.iter e_cols edb;
  let i_cols key rules =
    let rec cols ind n =
      if ind<n then ("COL"^(string_of_int ind))::(cols (ind+1) n)
      else [] in
    if not (Hashtbl.mem hs key) then
      Hashtbl.add hs key (cols 0 (get_symtkey_arity key))
    else
      ()
  in
  Hashtbl.iter i_cols idb;
  hs

(** set for rterm  *)
module RtermSet = Set.Make(struct
    type t = rterm
    let compare rt1 rt2 = key_comp (symtkey_of_rterm rt1) (symtkey_of_rterm rt2)
  end)

type rtermset = RtermSet.t

(** Compares two variables for ordering *)
let var_comp var1 var2 = String.compare (string_of_var var1) (string_of_var var2)

(** set for variable  *)
module VarSet = Set.Make(struct
    type t = var
    let compare = var_comp
  end)

type varset = VarSet.t

(** Get the set of variables of a term list (maybe a rule body). *)
let rec get_termlst_varset terms =
    let lst = List.fold_right (@) (List.map get_term_varlist terms) [] in
    VarSet.of_list lst


(** Get the list of variables of a term list (maybe a rule body). *)
let rec get_termlst_vars terms =
    let lst = List.fold_right (@) (List.map get_term_varlist terms) [] in lst


(** Get the list of variables of a rterm list (maybe a rule body). *)
let rec get_rtermlst_vars rterms =
    let lst = List.fold_right (@) (List.map get_rterm_varlist rterms) [] in lst


(***********************************************************
 *  Vartab
 *********************************************************)

(** This type defines a 'vartable', it belongs to a rule and
    it is a dictionary with variable names as key, these variables
    are those that appear in the body/head of the rule.
    The value for each key is a list of variable-appearances:
    references to predicates in the rule's body where the variable is
    mentioned.
    A variable appearence is simply a string denoting
    a column of a relation in the way Table.column*)
type vartab = (string, string list) Hashtbl.t

(* Insert in a vartab the provided var_app, initializing a list in the hash if necessary. *)
let vt_insert (vt:vartab) vname va =
  if Hashtbl.mem vt vname then
    let ap_lst = Hashtbl.find vt vname in
    Hashtbl.replace vt vname (va::ap_lst)
  else
    Hashtbl.add vt vname [va]

(*Prints a vartab*)
let vt_print (vt:vartab) =
  let print_el vn alst =
    let ap_str = "["^(String.concat ", " alst)^"]" in
    Printf.printf "%s: %s\n" vn ap_str in
  Hashtbl.iter print_el vt


(** Builds a vartab out of a list of rterms and with the colnamtab.*)
let build_vartab (col_names:colnamtab) rterms =
  let vt:vartab = Hashtbl.create (2*(List.length (get_rtermlst_vars rterms ))) in
  let in_rt n rterm =
    let pname = get_rterm_predname rterm in
    let vlst = get_rterm_varlist rterm in
    let arity = get_arity rterm in
    let key = symtkey_of_rterm rterm in
    if not (Hashtbl.mem col_names key) then raise (SemErr ("not found edb or idb predicate "^string_of_symtkey key)) else
    let cols =
    try Hashtbl.find col_names key
        with Not_found -> print_endline ("Not_found in col_names the key "^string_of_symtkey key); exit 0;
    in
    let in_v cn v =
      let comp_cn =
        pname^"_a"^(string_of_int arity)^
        "_"^(string_of_int n)^"."^cn
      in
      match v with
        NamedVar _ | NumberedVar _ ->
        vt_insert vt (string_of_var v) comp_cn
      | AggVar _ -> raise (SemErr (
          "Goal "^(string_of_symtkey key)^
          " contains an aggregate function as a variable, "^
          "which is only allowed in rule heads"
        ))
      | _ -> ()
    in
    List.iter2 in_v cols vlst;
    n+1
  in
  let _ = List.fold_left in_rt 0 rterms in
  vt

(** Build a vartab (use numbers to refer to the tables and columns, used in rosette code) out of a list of rterms and with the colnamtab. *)
let build_num_vartab (col_names:colnamtab) rterms =
  let vt:vartab = Hashtbl.create (2*(List.length (get_rtermlst_vars rterms ))) in
  let in_rt n rterm =
    let pname = get_rterm_predname rterm in
    let vlst = get_rterm_varlist rterm in
    let arity = get_arity rterm in
    let key = symtkey_of_rterm rterm in
    if not (Hashtbl.mem col_names key) then raise (SemErr ("not found edb or idb predicate "^string_of_symtkey key)) else
    let rec gen_nums ind n =
      if ind<n then ((string_of_int ind))::(gen_nums (ind+1) n)
      else [] in
    let cols = gen_nums 0 arity in
    let in_v cn v =
      let comp_cn =
        "(list-ref (list-ref tuplelst "^(string_of_int n)^") "^cn^")"
      in
      match v with
        NamedVar _ | NumberedVar _ ->
        vt_insert vt (string_of_var v) comp_cn
      | AggVar _ -> raise (SemErr (
          "Goal "^(string_of_symtkey key)^
          " contains an aggregate function as a variable, "^
          "which is only allowed in rule heads"
        ))
      | _ -> ()
    in
    List.iter2 in_v cols vlst;
    n+1
  in
  let _ = List.fold_left in_rt 0 rterms in
  vt

(***********************************************************
 *  Eqtab
 *********************************************************)

(** This type defines a eqtab, it belongs to a rule and it is
    a dictionary with variable names as
    keys and constants as values. They represent equalities that
    must be satisfied by the variables*)
type eqtab = (vterm,vterm) Hashtbl.t

(** Given a list of equality ASTs, returns an eqtab with
    the equality relations as var = value.
    PRECONDITION: There should not be aggregate equalities
    in the provided list.*)
let build_eqtab eqs =
  let tuples = List.map extract_eq_tuple eqs in
  let hs:eqtab = Hashtbl.create (2*(List.length eqs)) in
  let add_rel (e1,e2) = if ((List.length ((get_vterm_varlist e1) @ (get_vterm_varlist e2))) > 0)
    then Hashtbl.add hs e1 e2
    else invalid_arg "Trying to build_eqtab with equalities containing no varialbe " in
  List.iter add_rel tuples;
  hs

(** Given a var name, returns the value and removes it from the eqtab. *)
let eqt_extract (eqt : eqtab) (e1 : vterm) : vterm =
  let lst = Hashtbl.find_all eqt e1 in
  if ((List.length lst) <> 1) then raise (SemErr ("Ambiguity of the assigments of variable "^(string_of_vterm e1)));
  let c = List.hd lst in
  (* Hashtbl.remove eqt e1;  *)
  c

(** Get the query expression,
    check if there is one query, or more than one
    @param get_query takes input assumed to be query
    @return true if there is query, otherwise error statements *)
let get_query expr =
  match expr.query with
    | Some v -> v
    | None -> raise (SemErr "The program has no query")

(** Return true if there is a query, otherwise error statements. *)
let has_query expr =
  match expr.query with
    | Some v -> true
    | None -> false

(** Takes a list of terms and splits them into positive rterms, negative terms, equalities, and inequalities. *)
let split_terms terms =
  let rec split t (pos,neg,eq,inq) = match t with
    | Rel rt -> (rt::pos,neg,eq,inq)
    | Not rt -> (pos,rt::neg,eq,inq)
    | Equat (Equation ("=",_,_)) -> (pos,neg,t::eq,inq)
    | Noneq (Equation ("<>",ae1,ae2)) -> (pos,neg,Equat (Equation ("=",ae1,ae2))::eq,inq)
    | _ -> (pos,neg,eq,t::inq) in
  List.fold_right split terms ([],[],[],[])

(** get the statement of view schema  *)
let get_view expr =
  match expr.view with
    | Some v -> v
    | None -> raise (SemErr "The program has no view")

let get_view_rterm e = get_schema_rterm (get_view e)

(** Generate a column name list [col0, col1,....]  *)
let rec gen_cols ind n =
  if ind<n then ( "COL"^(string_of_int ind))::(gen_cols (ind+1) n)
  else []

(** Generate a var list [COL0, COL1,....]  *)
let rec gen_vars ind n =
  if ind<n then (NamedVar ("COL"^(string_of_int ind)))::(gen_vars (ind+1) n)
  else []

(** Given a rterm, return this rterm as a literal of all variables *)
let variablize_rterm(rt:rterm) = match rt with
  | Pred (x, vl) -> Pred (x, (gen_vars 0 (List.length vl)))
  | Deltainsert (x, vl) -> Deltainsert (x, (gen_vars 0 (List.length vl)))
  | Deltadelete (x, vl) -> Deltadelete (x, (gen_vars 0 (List.length vl)))

(** Given a predicate name, return a new temporary name*)
let get_temp_name (name:string) = "__tmp_"^name

(** Given a rterm, return a new temporary rterm*)
let get_temp_rterm (rt:rterm) = match rt with
  | Pred (x, vl) -> Pred ("__tmp_"^x, vl)
  | Deltainsert (x, vl) -> Deltainsert ("__tmp_"^x, vl)
  | Deltadelete (x, vl) -> Deltadelete ("__tmp_"^x, vl)

(** Given a rterm, return a new temporary delta of insertion of rterm*)
let get_temp_delta_insertion_rterm (rt:rterm) = match rt with
  | Pred (x, vl) -> Pred ("__tmp_delta_ins_"^x, vl)
  | _ -> invalid_arg "function get_temp_delta_insertion_rterm called with not a Pred"

(** Given a rterm, returns a new temporary delta of deletion of rterm*)
let get_temp_delta_deletion_rterm (rt:rterm) = match rt with
  | Pred (x, vl) -> Pred ("__tmp_delta_del_"^x, vl)
  | _ -> invalid_arg "function get_temp_delta_deletion_rterm called with not a Pred"

(** Given a rterm, returns a materialized of rterm*)
let get_materializied_rterm (rt:rterm) = match rt with
  | Pred (x, vl) -> Pred ("__dummy__materialized_"^x, vl)
  | _ -> invalid_arg "function get_materializied_rterm called with not a Pred"

(** Given a rterm, rename it by adding to its name a prefix*)
let rename_rterm (prefix:string) (rt:rterm) = match rt with
  | Pred (x, vl) -> Pred (prefix^x, vl)
  | Deltainsert (x, vl) -> Deltainsert (prefix^x, vl)
  | Deltadelete (x, vl) -> Deltadelete (prefix^x, vl)

(** Given a rterm, rename it by adding to its name a postfix*)
let rename2_rterm (postfix:string) (rt:rterm) = match rt with
  | Pred (x, vl) -> Pred (x^postfix, vl)
  | Deltainsert (x, vl) -> Deltainsert (x^postfix, vl)
  | Deltadelete (x, vl) -> Deltadelete (x^postfix, vl)

(** Given a rterm, change its var list*)
let change_vars rt vs = match rt with
            Pred (n,_) -> Pred (n,vs)
            | Deltadelete (n,_) -> Deltadelete (n,vs)
            | Deltainsert (n,_) -> Deltainsert (n,vs)

let rename_term prefix t = match t with
  | Rel r             -> Rel (rename_rterm prefix r)
  | Not r            -> Not (rename_rterm prefix r)
  | _ -> t

let rename_rule prefix (p, body) = (rename_rterm prefix p, List.map (rename_term prefix) body)

let rename_fact prefix rt = rename_rterm prefix rt

let rename_rules prefix rules =
  List.map (rename_rule prefix) rules

let str_contains s1 s2 =
  let re = Str.regexp_string s2
  in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false

(** Cut a substring starting with a word *)
let cut_str_by_word s1 word =
  let re = Str.regexp_string word in
  let start = try(Str.search_forward re s1 0)
    with Not_found -> String.length s1 in
  String.sub s1 start ((String.length s1)-start)

(** print a delta predicate list  *)
let print_deltas dlst =
  let print_el s = Printf.printf "%s, " (string_of_rterm s) in
  List.iter print_el dlst

(** get the delta predicates,
    check if there is no update
    @return true if there are more than one updates, otherwise error statements *)
let get_delta_rterms expr =
    let add_delta (rtset:rtermset) = function
      | (head, lst) -> (
        match head with
          Pred _ -> rtset
          | Deltainsert _ -> RtermSet.add (variablize_rterm head) rtset
          | Deltadelete _ -> RtermSet.add (variablize_rterm head) rtset)
    in
    let delta_lst: rterm list = RtermSet.elements (List.fold_left add_delta RtermSet.empty expr.rules) in
    (* print_endline "____delta____";
       print_deltas delta_lst; *)
    match delta_lst with
    | []     -> raise (SemErr "The program has no update")
    | _::tail    -> delta_lst

(** get all source predicates *)
let get_source_rterms expr =
    let add_source (rtset:rtermset) src= RtermSet.add (get_schema_rterm src) rtset
    in
    let source_lst: rterm list = RtermSet.elements (List.fold_left add_source RtermSet.empty expr.sources) in
    source_lst

(** check if a variable is free or not, a variable is free if it is not in positive predicates*)
let is_free_var (vt:vartab) (vexp:vterm) = match vexp with
  | Var variable -> if Hashtbl.mem vt (string_of_var variable) then false else true
  | _ -> false

let is_delta_pair rt1 rt2 = match (rt1, rt2) with
    (Deltainsert (pred1, varlst1) , Deltadelete (pred2, varlst2)) -> if (String.compare pred1 pred2 = 0) && ((List.length varlst1) = (List.length varlst2)) then true else false
    | _ -> false

let is_delta_or_empty rt = match rt with
  Pred (n,vs) -> (String.compare n (get_rterm_predname (get_empty_pred))) == 0
  | Deltadelete _ -> true
  | Deltainsert _ -> true

let rules_of_symt symt = Hashtbl.fold (fun k rules lst -> rules@lst) symt []

let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

let exe_command command =
  let tmp_file = Filename.temp_file "" ".txt" in
  let status = Sys.command @@ command ^" > " ^ tmp_file ^" 2>> " ^ tmp_file in
  let message = String.concat "\n" @@ read_file tmp_file in
  status, message

let check_command_version command =
  let status, message = exe_command @@ command ^ " --version" in
  if not (status = 0) then raise (EnvErr (command ^ " is required but not installed yet! Be sure "^command ^ " can be called in the terminal."))
  else message

let verify_fo_lean debug timeout sentence =
  if  debug then (
    print_endline @@"==> verifying by Lean";
    print_endline "--------------";
    print_endline "Lean script:\n";
    print_endline sentence;
    print_endline "--------------";
    flush stdout;
  ) else ();
  let tmp_file = Filename.temp_file "" ".lean" in
  let ol =  open_out tmp_file in
  fprintf ol "%s\n" sentence;
  close_out ol;
  ignore (check_command_version "lean");
  (* check_lean_path *)
  let tmp_chklib_file = Filename.temp_file "" ".lean" in
  let chklib =  open_out tmp_chklib_file in
  fprintf chklib "%s\n" "import bx";
  close_out chklib;
  let leanstatus, leanmessage = exe_command @@ "lean "^tmp_chklib_file in
  if not (leanstatus = 0) then
    raise (EnvErr ("Lean paths to BIRDS's verification folder are not configured correctly! Please change the Lean path configuration in ~/.lean/leanpkg.path and check by 'lean --path'. More details at https://github.com/dangtv/BIRDS"))
  else ();
  ignore (check_command_version "z3");
  let status, message = exe_command @@ "timeout "^ (string_of_int timeout) ^" lean "^tmp_file in
  if (debug && (status = 0)) then print_endline @@">>> verified by lean: correct";
  status, message

let check_ros_prog debug timeout sentence =
  if  debug then (
    print_endline @@"==> generating a counterexample by Rosette";
    print_endline "--------------";
    print_endline "Racket code:\n";
    print_endline sentence;
    print_endline "--------------";
    flush stdout;
  ) else ();
  let tmp_file = Filename.temp_file "" ".rkt" in
  let ol =  open_out tmp_file in
  fprintf ol "%s\n" sentence;
  close_out ol;
  ignore (check_command_version "racket");
  (* check racket lib *)
  let tmp_chklib_file = Filename.temp_file "" ".rkt" in
  let chklib =  open_out tmp_chklib_file in
  fprintf chklib "%s\n" "#lang rosette";
  close_out chklib;
  let racketstatus, racketmessage = exe_command @@ "racket "^tmp_chklib_file in
  if not (racketstatus = 0) then
    raise (EnvErr ("Package rosette is requried but not installed correctly. Please install by 'raco pkg install rosette'. More details at https://github.com/dangtv/BIRDS"))
  else ();
  let status, message = exe_command @@ "timeout "^ (string_of_int timeout) ^" racket "^tmp_file in
  if (debug && (status = 0)) then print_endline @@">>> Checked by Rosette";
  status, message

let constraint2rule expr =
    let trans_pk (relname, attrlst) lst=
        (* generate datalog rules for a primary key *)
        let schema_stt =
          try (List.find (fun x -> relname = (get_schema_name x)) (get_schema_stts expr) )
          with Not_found -> raise (SemErr ("Not found the relation "^relname^ " in the primary key constraint \n"^ string_of_pk (relname, attrlst)))
        in
        let allattrlst = get_schema_attrs schema_stt in
        let allattrlst2 = List.map (fun x -> if (List.mem x attrlst) then x else x^"2") allattrlst in
        let nonkeyattrlst = List.filter (fun x -> not (List.mem x attrlst)) allattrlst in
        (List.map (fun x -> (get_empty_pred, [Rel (Pred(relname, List.map (fun t -> NamedVar t) allattrlst)); Rel (Pred(relname, List.map (fun t -> NamedVar t) allattrlst2)); Equat (Equation("<>", Var (NamedVar x), Var (NamedVar (x^"2"))))] )) nonkeyattrlst )@lst in
    { expr with rules = (List.fold_right trans_pk expr.primary_keys expr.constraints)@expr.rules}

(*
Color	Code
Black	0;30
Blue	0;34
Green	0;32
Cyan	0;36
Red	0;31
Purple	0;35
Brown	0;33
Blue	0;34
Green	0;32
Cyan	0;36
Red	0;31
Purple	0;35
Brown	0;33 *)
let colored_string color str = match color with
    "red" -> "\027[31m"^str^"\027[0m"
    | "black"	 -> "\027[30m"^str^"\027[0m"
    | "blue"	 -> "\027[34m"^str^"\027[0m"
    | "green"	 -> "\027[32m"^str^"\027[0m"
    | "cyan"	 -> "\027[36m"^str^"\027[0m"
    | "purple" -> "\027[35m"^str^"\027[0m"
    | "brown"	 -> "\027[33m"^str^"\027[0m"
    | _ -> str
