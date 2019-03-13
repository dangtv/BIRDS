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
