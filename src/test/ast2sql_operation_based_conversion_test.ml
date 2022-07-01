
open Utils
open Expr


type test_case = {
  title    : string;
  expr     : expr;
  expected : string;
}

type test_result =
  | Pass
  | Fail of { expected : string; got : string }


let run_test (test_case : test_case) : (test_result, Ast2sql.error) result =
  let open ResultMonad in
  let expr = test_case.expr in
  let expected = test_case.expected in

  Ast2sql.convert_expr_to_operation_based_sql expr >>= fun sql_operations ->
  let got = sql_operations |> List.map Ast2sql.stringify_sql_operation |> String.concat " " in

  if String.equal got expected then
    return Pass
  else
    return (Fail { expected; got })


(* Runs all the test cases in the given list, prints every result,
   and returns whether a failure has occurred. *)
let run_tests (test_cases : test_case list) : bool =
  test_cases |> List.fold_left (fun has_failed test_case ->
    let title = test_case.title in
    match run_test test_case with
    | Ok Pass ->
        Printf.printf "- %s: OK\n" title;
        has_failed

    | Ok (Fail { expected; got }) ->
        Printf.printf "! %s: FAILED\n" title;
        Printf.printf "expected:\n\"%s\"\n" expected;
        Printf.printf "got:\n\"%s\"\n" got;
        true

    | Error _ ->
        Printf.printf "FAILED (error)\n";
        true
  ) false


let main () =
  let test_cases =
    [
      {
        title =
          "ed and eed";
        expr =
          {
            rules = [
              (* "+eed(E, D) :- ed(E, D), D = 'A', E != 'Joe', ¬eed(E, D)." *)
              Deltainsert ("eed", [ NamedVar "E"; NamedVar "D" ]), [
                Rel (Pred ("ed", [ NamedVar "E"; NamedVar "D" ]));
                Equat (Equation ("=", Var (NamedVar "D"), Const (String "'A'")));
                Equat (Equation ("<>", Var (NamedVar "E"), Const (String "'Joe'")));
                Not (Pred ("eed", [ NamedVar "E"; NamedVar "D" ]));
              ];

              (* "-eed(E, D) :- ed(V1, D), eed(E, D), E = 'Joe', D = 'A', V1 != 'Joe', ¬eed(V1, D)." *)
              Deltadelete ("eed", [ NamedVar "E"; NamedVar "D" ]), [
                Rel (Pred ("ed", [ NamedVar "V1"; NamedVar "D" ]));
                Rel (Pred ("eed", [ NamedVar "E"; NamedVar "D" ]));
                Equat (Equation ("=", Var (NamedVar "E"), Const (String "'Joe'")));
                Equat (Equation ("=", Var (NamedVar "D"), Const (String "'A'")));
                Equat (Equation ("<>", Var (NamedVar "V1"), Const (String "'Joe'")));
                Not (Pred ("eed", [ NamedVar "V1"; NamedVar "D" ]));
              ];

              (* "+ed(E, D) :- ed(V1, D), E = 'Joe', D = 'A', V1 != 'Joe', ¬ed(E, D), ¬eed(V1, D)." *)
              Deltainsert ("ed", [ NamedVar "E"; NamedVar "D" ]), [
                Rel (Pred ("ed", [ NamedVar "V1"; NamedVar "D" ]));
                Equat (Equation ("=", Var (NamedVar "E"), Const (String "'Joe'")));
                Equat (Equation ("=", Var (NamedVar "D"), Const (String "'A'")));
                Equat (Equation ("<>", Var (NamedVar "V1"), Const (String "'Joe'")));
                Not (Pred ("ed", [ NamedVar "E"; NamedVar "D" ]));
                Not (Pred ("eed", [ NamedVar "V1"; NamedVar "D" ]));
              ];
            ];
            facts = [];
            query = None;
            sources = [
              ("ed", [ ("emp_name", Sstring); ("dept_name", Sstring) ]);
              ("eed", [ ("emp_name", Sstring); ("dept_name", Sstring) ]);
            ];
            view = None;
            constraints = [];
            primary_keys = [];
          };
        expected =
          let query1 =
            String.concat " " [
              "SELECT ed0.emp_name AS emp_name, 'A' AS dept_name FROM ed AS ed0 WHERE";
              "ed0.dept_name = 'A' AND ed0.emp_name <> 'Joe' AND";
              "NOT EXISTS ( SELECT * FROM eed AS t WHERE t.emp_name = ed0.emp_name AND t.dept_name = 'A' )";
            ]
          in
          let query2 =
            String.concat " " [
              "SELECT 'Joe' AS emp_name, 'A' AS dept_name FROM ed AS ed0, eed AS eed1 WHERE";
              "ed0.dept_name = 'A' AND eed1.dept_name = 'A' AND eed1.emp_name = 'Joe' AND ed0.emp_name <> 'Joe' AND";
              "NOT EXISTS ( SELECT * FROM eed AS t WHERE t.emp_name = ed0.emp_name AND t.dept_name = 'A' )";
            ]
          in
          let query3 =
            String.concat " " [
              "SELECT 'Joe' AS emp_name, 'A' AS dept_name FROM ed AS ed0 WHERE";
              "ed0.dept_name = 'A' AND ed0.emp_name <> 'Joe' AND";
              "NOT EXISTS ( SELECT * FROM ed AS t WHERE t.emp_name = 'Joe' AND t.dept_name = 'A' ) AND";
              "NOT EXISTS ( SELECT * FROM eed AS t WHERE t.emp_name = ed0.emp_name AND t.dept_name = 'A' )";
            ]
          in
          String.concat " " [
            Printf.sprintf "CREATE TEMPORARY TABLE temp0 AS %s;" query1;
            Printf.sprintf "CREATE TEMPORARY TABLE temp1 AS %s;" query2;
            Printf.sprintf "CREATE TEMPORARY TABLE temp2 AS %s;" query3;
            "INSERT INTO temp0 SELECT * FROM temp0 AS inst;";
            "DELETE FROM temp1 WHERE EXISTS ( SELECT * FROM temp1 AS inst );";
            "INSERT INTO temp2 SELECT * FROM temp2 AS inst;";
          ]
      };
    ]
  in
  run_tests test_cases
