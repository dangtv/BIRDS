(* 
@author: Vandang Tran
*)

(**  Evaluate the fixpoint of a Datalog program
*)

exception Eof
open Printf

(* Evaluate the fixpoint of a Datalog program, also caculate the explanation *)
let eval (log:bool) explain_lst prog = 
  let quiet = ref (not log) in
  let progress = ref false in
  let print_input = ref false in
  let print_result = ref true in (* must be true to get all the derived facts *)
  let print_saturated = ref false in
  let print_size = ref false in
  let print_version = ref false in
  let sums = ref [] in
  let patterns = (ref [] : Bottom_up.literal list ref) in
  let goals = (ref [] : Bottom_up.literal list ref) in
  let explains = ref [] in
  let files = ref [] in
  let queries = ref [] in
  let returned_explanation = ref [] in
  let returned_facts = (ref [] : Bottom_up.literal list ref) in 

  let pp_progress i total =
    Format.printf "\r%% clause %-5d / %-5d  " i total;
    Format.print_flush () in

  (** Simple goal handler (interprets 'lt') *)
  let handle_goal db lit =
    (* log: Format.printf "%% goal %a@." Bottom_up.pp_literal lit; *)
    if Bottom_up.is_negative lit then (
      (* Format.printf "========= %% goal %a@." Bottom_up.pp_literal lit;  *)
      let pos_lit = Bottom_up.negate lit in 
      let lit_holds = ref true in
        (* Bottom_up.db_fold (fun () clause ->
        if Bottom_up.is_fact clause then
          (Format.printf "  @[<h>%a@]@." Bottom_up.pp_clause clause; 
          (match clause.(0).(0) with Const s -> Format.printf "*** 1) fact of=== %s  vs  " s | _ -> ());
          (match pos_lit.(0) with Const s -> Format.printf "*** 2) fact of=== %s  \n" s | _ -> ());
          let fun_name = (fun term -> match term with Bottom_up.Const s -> Bottom_up.Symbol.to_string s | _ -> "") in
          if (Bottom_up.eq_term clause.(0).(0)  pos_lit.(0)) then (Format.printf "===found===\n"; lit_holds := false))) () db; *)

      Bottom_up.db_match db pos_lit (fun lit' -> lit_holds := false);
      if !lit_holds then ( 
        (* Format.printf "========= no fact %a@." Bottom_up.pp_literal pos_lit;   *)
        Bottom_up.db_add_fact db lit)
      (* if not (Bottom_up.db_mem db [|pos_lit|]) then ( Format.printf "========= no fact %a@." Bottom_up.pp_literal pos_lit;  Bottom_up.db_add_fact db lit) *)
    )
    else if Bottom_up.is_builtin lit then
    let compare a b =
      try
        let a = int_of_string (Bottom_up.StringSymbol.to_string a)
        and b = int_of_string (Bottom_up.StringSymbol.to_string b) in
        Stdlib.compare a b
      with Failure _ | Invalid_argument _ -> compare a b
    in
    match (Bottom_up.open_literal lit :> string * Bottom_up.term list) with
      | "lt", [Bottom_up.Const a; Bottom_up.Const b] when compare a b < 0 ->
        Bottom_up.db_add_fact db lit (* literal is true *)
      | "<", [Bottom_up.Const a; Bottom_up.Const b] when compare a b < 0 ->
        Bottom_up.db_add_fact db lit (* literal is true *)
      | "le", [Bottom_up.Const a; Bottom_up.Const b] when compare a b <= 0 ->
        Bottom_up.db_add_fact db lit (* literal is true *)
      | "<=", [Bottom_up.Const a; Bottom_up.Const b] when compare a b <= 0 ->
        Bottom_up.db_add_fact db lit (* literal is true *)
      | "gt", [Bottom_up.Const a; Bottom_up.Const b] when compare a b > 0 ->
        Bottom_up.db_add_fact db lit (* literal is true *)
      | ">", [Bottom_up.Const a; Bottom_up.Const b] when compare a b > 0 ->
        Bottom_up.db_add_fact db lit (* literal is true *)
      | "ge", [Bottom_up.Const a; Bottom_up.Const b] when compare a b >= 0 ->
        Bottom_up.db_add_fact db lit (* literal is true *)
      | ">=", [Bottom_up.Const a; Bottom_up.Const b] when compare a b >= 0 ->
        Bottom_up.db_add_fact db lit (* literal is true *)
      | "ineq", [Bottom_up.Const a; Bottom_up.Const b] when ((compare a b > 0) || (compare a b < 0)) ->
        Bottom_up.db_add_fact db lit (* literal is true *)
      | "<>", [Bottom_up.Const a; Bottom_up.Const b] when ((compare a b > 0) || (compare a b < 0)) ->
        Bottom_up.db_add_fact db lit (* literal is true *)
      | "equal", [Bottom_up.Const a; Bottom_up.Const b] when compare a b = 0 ->
        Bottom_up.db_add_fact db lit (* literal is true *)
      | "=", [Bottom_up.Const a; Bottom_up.Const b] when compare a b = 0 ->
        Bottom_up.db_add_fact db lit (* literal is true *)
      | "equal", [Bottom_up.Const a; _] 
      | "=", [Bottom_up.Const a; _] 
      | "equal", [_; Bottom_up.Const a] 
      | "=", [_; Bottom_up.Const a] ->
        Bottom_up.db_add_fact db (Bottom_up.mk_literal "=" [Bottom_up.Const a; Bottom_up.Const a]) (* literal is true *)
      | _ -> () in

  (** Handler that aggregates the number of facts with this head symbol. It adds the
      handler to the global variable [sums] *)
  let add_sum symbol =
    let count = ref 0 in
    (* print result at exit *)
    let printer () = Format.printf "%% number of fact with head %s: %d@." symbol !count in
    let handler _ = incr count in
    sums := (Bottom_up.StringSymbol.make symbol, handler, printer) :: !sums in

  (** Handler that add a goal *)
  let add_goal p =
    let lexbuf = Lexing.from_string p in
    let rterm = Parser.parse_rterm Lexer.token lexbuf in
    (* let rterm = Conversion.rterm_of_rterm2 rterm in *)
    let rterm = Bottom_up.literal_of_rterm false rterm in
    goals := rterm :: !goals in
 
  (** Add the pattern to the list of patterns to explain *)
  let add_explain p =
    let lexbuf = Lexing.from_string p in
    let rterm = Parser.parse_rterm Lexer.token lexbuf in
    (* let rterm = Conversion.rterm_of_rterm2 rterm in *)
    let rterm = Bottom_up.literal_of_rterm false rterm in
    explains := rterm :: !explains in

  (** Add a query to the list of queries to run *)
  let add_query q_str =
    try
      let lexbuf = Lexing.from_string q_str in
      let ast = Parser.parse_query Lexer.token lexbuf in
      (* let ast = Conversion.conj_query_of_conj_query2 ast in *)
      let q = Bottom_up.query_of_ast ast in
      queries := q :: !queries
    with Parsing.Parse_error ->
      failwith ("could not parse the query string " ^ q_str) in

  (** Compute the fixpoint of clauses *)
  let process_clauses clauses =
    if not !quiet then Format.printf "%% process %d clauses@." (List.length clauses);
    if !print_input then (
      List.iter (Format.printf "  clause @[<h>%a@]@." Bottom_up.pp_clause) clauses
    );
    if not !quiet then Format.printf "%% computing fixpoint...@.";
    let db = Bottom_up.db_create () in
    (* handlers *)
    List.iter (fun (symbol,handler,_) -> Bottom_up.db_subscribe_fact db symbol handler) !sums;
    (* goals *)
    Bottom_up.db_subscribe_goal db (handle_goal db);
    List.iter (fun goal -> Bottom_up.db_goal db goal) !goals;
    (* add clauses one by one *)
    let total = List.length clauses in
    List.iteri
      (fun i clause ->
        if !progress then pp_progress i total;
        Bottom_up.db_add db clause)
      clauses;
    if not !quiet then Format.printf "%% done.@.";
    (* print fixpoint of set after application of clauses *)
    if !print_size then (
      Format.printf "%% size of saturated set: %d@." (Bottom_up.db_size db)
    );
    if !print_saturated then (
      Bottom_up.db_fold (fun () clause ->
          Format.printf "  @[<h>%a@]@." Bottom_up.pp_clause clause) () db
    ) else if !print_result then (
      Bottom_up.db_fold (fun () clause ->
        if Bottom_up.is_fact clause then
          (if (not !quiet) then Format.printf "<fact>  @[<h>%a@]@." Bottom_up.pp_clause clause;
          returned_facts := (clause.(0)):: !returned_facts))
          () db;
    );
    (* print aggregates *)
    List.iter (fun (_,_,printer) -> printer ()) !sums;
    (* print patterns *)
    List.iter (fun pattern ->
      if (not !quiet) then Format.printf "%% facts matching pattern %a:@." Bottom_up.pp_literal pattern;
      Bottom_up.db_match db pattern
        (fun fact -> if (not !quiet) then Format.printf "<fact>  @[<h>%a.@]@." Bottom_up.pp_literal fact; returned_facts := fact:: !returned_facts;))
      !patterns;
    (* run queries *)
    List.iter (fun (vars, lits, neg) ->
      let set = Bottom_up.Query.ask ~neg db vars lits in
      let l = Bottom_up.Query.to_list set in
      if not !quiet then Format.printf "%% query plan: @[<h>%a@]@." Bottom_up.Query.pp_plan set;
      Format.printf "%% @[<v2>query answer:@ ";
      List.iter
        (fun terms ->
          Array.iteri
            (fun i t ->
              (if i > 0 then Format.printf ", %a" else Format.printf "%a") Bottom_up.pp_term t)
            terms;
          Format.printf "@;")
        l;
      Format.printf "@]@.")
    !queries;
    (* get explaination trees *)
    let rec complete_explanation fact = 
      (* premises *)
      let clause, premises = Bottom_up.db_premises db fact in
      if (List.length premises = 0) then [] else 
        let fist_premise = (fact, clause, premises) in 
        let inductive_premises = List.map complete_explanation premises in 
        fist_premise :: (List.concat inductive_premises)
        (*  *)
      in
    let explanation_summary fact = 
    (* explanation *)
        let edb_facts = Bottom_up.db_explain db fact in
        let all_clauses = List.map (fun (fact, clause, premises) -> clause ) (List.rev (complete_explanation fact)) in 
        (edb_facts, all_clauses) in
    (* print explanations *)
    List.iter (fun pattern ->
      Bottom_up.db_match db pattern 
        (fun fact ->
          let edb_facts, all_clauses = explanation_summary fact in 
          if (not !quiet) then 
          (Format.printf "  (Summary) explain @[<h>%a@] by: \n    @[<h>" Bottom_up.pp_literal fact;
          List.iter (fun fact' -> Format.printf "%a " Bottom_up.pp_literal fact') edb_facts;
          Format.printf "\n    ~~~~~~~~~~~~~~~~~~~~~\n";
          List.iter (fun clause -> Format.printf "         || @[<h>%a@] \n" Bottom_up.pp_clause clause) all_clauses;
          Format.printf "         \\/  \n";
          Format.printf "    ~~~~~~~~~~~~~~~~~~~~~\n";
          Format.printf "    @[<h>%a@]" Bottom_up.pp_literal fact;
          Format.printf "@]@.");

          (* premises *)
          (* Format.printf "  premises of @[<h>%a@]: @[<h>" Bottom_up.pp_literal fact;
          let clause, premises = Bottom_up.db_premises db fact in
          List.iter (fun fact' -> Format.printf "%a, " Bottom_up.pp_literal fact') premises;
          Format.printf " with @[<h>%a@]" Bottom_up.pp_clause clause;
          Format.printf "@]@."; *)
          if (not !quiet) then Format.printf "\n  (Details) explain @[<h>%a@] by: @[<h>\n" Bottom_up.pp_literal fact;
          let detail = List.rev(complete_explanation fact) in
          if (not !quiet) then List.iter (fun (fact, clause, premises) -> 
            (* Format.printf "    premises of @[<h>%a@]: @[<h>" Bottom_up.pp_literal fact; *)
            Format.printf "    @[<h>";
            List.iter (fun fact' -> Format.printf "%a " Bottom_up.pp_literal fact') premises;
            Format.printf "\n    ------------------ %a\n" Bottom_up.pp_clause clause;
            Format.printf "    @[<h>%a@]" Bottom_up.pp_literal fact;
            Format.printf "@]\n@.";
          ) (detail);
          returned_explanation := (fact, edb_facts, all_clauses, detail) :: !returned_explanation
          )
        )
      !explains;
    (* print memory usage *)
    let stat = Gc.quick_stat () in
    if not !quiet then (
      Format.printf "%% max_heap_size: %d; minor_collections: %d; major collections: %d@."
        stat.Gc.top_heap_words stat.Gc.minor_collections stat.Gc.major_collections;
    );
    () in

  let stratified_rules = Stratification.get_stratified_rules prog in
  (* let clauses = List.filter (fun x -> match x with Expr2.Rule _ -> List.length (Expr2.rule_body x) > 0 | _ -> true) clauses in *)
  if log then (
    print_endline "_____stratified rules for evalation____";
    print_endline (Expr2.string_of_prog {Expr2.get_empty_expr with rules = stratified_rules});
    print_endline "_________________________"
  );
  let rterm_list = List.map (Bottom_up.literal_of_rterm false) explain_lst in
  (* patterns := rterm_list @ !patterns; *)
  (* print_result := true; *)
  (* explains := (Bottom_up.literal_of_rterm false (Expr2.Pred ("+s1", [Expr2.ConstVar (Expr2.Int 0); Expr2.ConstVar (Expr2.Int (-2)) ]))) :: !explains;
  explains := (Bottom_up.literal_of_rterm false (Expr2.Pred ("s1", [Expr2.ConstVar (Expr2.Int 0); Expr2.ConstVar (Expr2.Int (-2)) ]))) :: !explains; *)
  explains := rterm_list @ !explains;
  let clauses = (List.map Bottom_up.clause_of_fact prog.facts) @ (List.map Bottom_up.clause_of_rule stratified_rules) in
  process_clauses clauses; 
  let get_original_rule clause= 
  let all_rules = prog.rules in 
  let matched_rules = List.filter (fun x -> clause = Bottom_up.clause_of_rule x ) all_rules in 
  if (List.length matched_rules > 0) then List.hd matched_rules else Bottom_up.rule_of_clause clause in 
  (List.map (fun x -> (Bottom_up.rterm_of_literal x)) (!returned_facts),
  List.map (fun (fact, edb_facts, all_clauses, detail) -> (
    (Bottom_up.rterm_of_literal fact ),
    List.map (fun x ->(Bottom_up.rterm_of_literal x )) edb_facts,
    List.map (fun cl -> get_original_rule cl) all_clauses,
    List.map (fun (f, cl, premises) -> ((Bottom_up.rterm_of_literal f ), get_original_rule cl, List.map (fun x -> (Bottom_up.rterm_of_literal x )) premises ) ) detail
    )
  ) (!returned_explanation)
  )