
open Utils
open Expr


type table_definition = table_name * column_name list

type test_case = {
  title    : string;
  tables   : table_definition list;
  rule     : rule;
  expected : string;
}

type test_result =
  | Pass
  | Fail of { expected : string; got : string }


let make_colnamtab (defs : table_definition list) : colnamtab =
  let colnamtab : colnamtab = Hashtbl.create 32 in
  defs |> List.iter (fun (table, cols) ->
    let arity = List.length cols in
    Hashtbl.add colnamtab (table, arity) cols
  );
  colnamtab


let single (test_case : test_case) =
  let open ResultMonad in
  let colnamtab = make_colnamtab test_case.tables in
  let rule = test_case.rule in
  let expected = test_case.expected in

  Ast2sql.convert_to_operation_based_sql colnamtab rule >>= fun sql ->
  let got = Ast2sql.stringify_sql_query sql in

  if String.equal got expected then
    return Pass
  else begin
    return (Fail { expected; got })
  end


let test (test_cases : test_case list) : bool =
  test_cases |> List.fold_left (fun has_failed test_case ->
    let title = test_case.title in
    match single test_case with
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


let () =
  let test_cases =
    [
      {
        title = "3rd rule";
        tables = [
          ("ed", ["emp_name"; "dept_name"]);
          ("eed", ["emp_name"; "dept_name"]);
        ];
        (* "+ed(E, D) :- ed(V1, D), E = 'Joe', D = 'A', V1 != 'Joe', ¬ed(E, D), ¬eed(V1, D)." *)
        rule =
          (Deltainsert ("ed", [ NamedVar "E"; NamedVar "D" ]), [
            Rel (Pred ("ed", [ NamedVar "V1"; NamedVar "D" ]));
            Equat (Equation ("=", Var (NamedVar "E"), Const (String "'Joe'")));
            Equat (Equation ("=", Var (NamedVar "D"), Const (String "'A'")));
            Equat (Equation ("<>", Var (NamedVar "V1"), Const (String "'Joe'")));
            Not (Pred ("ed", [ NamedVar "E"; NamedVar "D" ]));
            Not (Pred ("eed", [ NamedVar "V1"; NamedVar "D" ]));
          ]);
        expected =
          String.concat " " [
            "SELECT 'Joe' AS emp_name, 'A' AS dept_name FROM ed AS ed0 WHERE";
            "ed0.dept_name = 'A' AND ed0.emp_name <> 'Joe' AND";
            "NOT EXISTS ( SELECT * FROM ed AS t WHERE t.emp_name = 'Joe' AND t.dept_name = 'A' ) AND";
            "NOT EXISTS ( SELECT * FROM eed AS t WHERE t.emp_name = ed0.emp_name AND t.dept_name = 'A' )";
          ];
      };
    ]
  in
  let has_failed = test test_cases in
  exit (if has_failed then 1 else 0)
