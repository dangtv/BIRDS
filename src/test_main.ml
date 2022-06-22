
open Utils
open Expr


type table_definition = table_name * column_name list

type test_case = {
  tables   : table_definition list;
  rule     : rule;
  expected : string;
}


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

  if String.equal got expected then begin
    Printf.printf "OK\n";
    return ()
  end else begin
    Printf.printf "FAILED\n";
    Printf.printf "expected:\n\"%s\"\n" expected;
    Printf.printf "got:\n\"%s\"\n" got;
    return ()
  end


let () =
  let test_case =
    {
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
        ]
    }
  in
  match single test_case with
  | Ok ()   -> ()
  | Error _ -> ()
