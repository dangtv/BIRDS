(**  Functions for stratifying a datalog program. Report an error if any of the predicates is indirectly recursive, 
or it is an incomplete program, which contains references to undefined predicates
 *)
 
open Expr;;
open Utils;;

(** Checks that all predicates in rterms are in the edb/idb,
 * and returns those that contain idb predicates*)
let check_keys (edb:symtable) (idb:symtable) keys =
    let check key =
        if Hashtbl.mem edb key then false
        else if Hashtbl.mem idb key then true
        else raise ( SemErr 
            (   "Incomplete program, predicate "^
                (string_of_symtkey key)^" not defined"
            )
        )
    in
    List.filter check keys

(** Given a symtkey, returns the keys of all IDB predicates that
 * depend on it, positively or negatively.*)
let get_idb_graph_sons (edb:symtable) (idb:symtable) (key:symtkey) =
    let rule_lst = Hashtbl.find idb key in
    let rule_rts = List.map get_all_rule_rterms rule_lst in
    let rterms = List.flatten rule_rts in
    let keys = List.map symtkey_of_rterm rterms in
    let uniq_keys = remove_repeated_keys keys in
    check_keys edb idb uniq_keys

let rec strat_dfs edb idb visit active key =
    visit := SymtkeySet.add key (!visit);
    active := SymtkeySet.add key (!active);
    let sons = get_idb_graph_sons edb idb key in
    let rec_call son =
        if SymtkeySet.mem son (!visit) then
            if (key_comp son key) != 0 && SymtkeySet.mem son (!active) then
                raise (SemErr (
                    "Predicate "^(string_of_symtkey son)^
                    " is indirectly recursive."
                ))
            else []
        else
            strat_dfs edb idb visit active son
    in
    let strat = List.flatten (List.map rec_call sons) in
    active := SymtkeySet.remove key (!active);
    strat@[key]

let stratify (edb:symtable) (idb:symtable) (query_rt:rterm) : (symtkey list) =
    let visit = ref SymtkeySet.empty in
    let active = ref SymtkeySet.empty in
    let key = symtkey_of_rterm query_rt in
    (*Check that the queried relation exists*)
    let _ = check_keys edb idb [key] in
    (*If it is an EDB query, return an empty stratification*)
    if Hashtbl.mem edb key then
        []
    else
        strat_dfs edb idb visit active key
;;

(** get all rules that are evaluated before evaluating the rules of query_rt *)
let get_preceding_rules prog query_rt =
    let edb = extract_edb prog in
    let strat =
        let temp_idb = extract_idb prog in
        Rule_preprocess.preprocess_rules temp_idb;
        stratify edb temp_idb query_rt in
    let idb = extract_idb prog in
    symt_remove idb (symtkey_of_rterm query_rt);
    Rule_preprocess.preprocess_rules idb;
    let rule_lst_lst = List.map (fun x -> if (Hashtbl.mem idb x) then Hashtbl.find idb x else []) strat in
    List.concat rule_lst_lst
;;

(** get all rules in a stratified order *)
let get_stratified_rules prog =
    let edb = extract_edb prog in
    let idb = extract_idb prog in
    let get_preds key rules lst = (Pred(get_rterm_predname (rule_head (List.hd rules)), gen_vars 0 (get_symtkey_arity key))) :: lst in
    let all_rels = Hashtbl.fold get_preds idb [] in
    let tmp = Pred("__str_tmp__",[]) in
    symt_insert idb (Rule(tmp, List.map (fun x -> Rel x) all_rels));
    Rule_preprocess.preprocess_rules idb;
    let strat = stratify edb idb tmp in
    let idb = extract_idb prog in
    symt_remove idb (symtkey_of_rterm tmp);
    let rule_lst_lst = List.map (fun x -> if (Hashtbl.mem idb x) then Hashtbl.find idb x else []) strat in
    List.concat rule_lst_lst
;;