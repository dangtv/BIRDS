open Utils

type test_case = {
  title : string;
  input : Sql2ast.sql_update * Sql2ast.sql_instance_name option * Sql2ast.sql_column_name list;
  expected : Expr.rule list
}

type failed_body = { expected : string; actual : string }

type test_result =
  | Pass
  | Failed of failed_body

let run_test {input; expected; _} =
  let open ResultMonad in
  let string_of_rules rules = rules
    |> List.map Expr.string_of_rule
    |> String.concat "; "
  in
  let (update, instance, columns) = input in
  Sql2ast.update_to_datalog update instance columns >>= fun actual ->
  let actual_str = string_of_rules actual in
  let expected_str = string_of_rules expected in
  if String.equal actual_str expected_str then
    return Pass
  else
    return (Failed { expected= expected_str; actual= actual_str })

let run_tests (test_cases : test_case list) : bool =
  test_cases |> List.fold_left (fun has_failed test_case ->
    let title = test_case.title in
    match run_test test_case with
    | Ok Pass ->
        Printf.printf "- %s: OK\n" title;
        has_failed

    | Ok (Failed { expected; actual }) ->
        Printf.printf "! %s: FAILED\n" title;
        Printf.printf "expected:\n\"%s\"\n" expected;
        Printf.printf "actual:\n\"%s\"\n" actual;
        true

    | Error _ ->
        Printf.printf "! %s: FAILED (error)\n" title;
        true
  ) false

let main () =
  run_tests [
    {
      title= "sample";
      (*
       * SQL:
       *   UPDATE
       *     ced
       *   SET
       *     dname = 'R&D'
       *   WHERE
       *     dname = 'Dev'
       *
       * datalog:
       *   ced_tmp(V0, V1) :- V1 <> R&D.
       *   -ced(V0, V1) :- ced(V0, V1), V1 = Dev, ced_tmp(V0, V1).
       *   +ced(V0, V1) :- V1 = R&D, -ced(V0, V1_2)
       *
       *)
      input= (
        Sql2ast.SqlUpdateSet (
          "ced",
          [(None, "dname"), Sql2ast.SqlConst (Expr.String "R&D")],
          Some (Sql2ast.SqlWhere ([
            Sql2ast.SqlConstraint (
              Sql2ast.SqlColumn (None, "dname"),
              Sql2ast.SqlRelEqual,
              Sql2ast.SqlConst (Expr.String "Dev")
            )
          ]))
        ),
        None,
        ["ename"; "dname"]
      );
      expected= [
        (
          Expr.Pred ("ced_tmp", [Expr.NamedVar "V0"; Expr.NamedVar "V1"]),
          [Expr.Equat (Expr.Equation ("<>", (Expr.Var (Expr.NamedVar "V1")), (Expr.Var (Expr.ConstVar (Expr.String "R&D")))))]
        );
        (
          Expr.Deltadelete ("ced", [Expr.NamedVar "V0"; Expr.NamedVar "V1"]),
          [
            Expr.Rel (Expr.Pred ("ced", [Expr.NamedVar "V0"; Expr.NamedVar "V1"]));
            Expr.Equat (Expr.Equation ("=", (Expr.Var (Expr.NamedVar "V1")), (Expr.Var (Expr.ConstVar (Expr.String "Dev")))));
            Expr.Rel (Expr.Pred ("ced_tmp", [Expr.NamedVar "V0"; Expr.NamedVar "V1"]))
          ]
        );
        (
          Expr.Deltainsert ("ced", [Expr.NamedVar "V0"; Expr.NamedVar "V1"]),
          [
            Expr.Equat (Expr.Equation ("=", (Expr.Var (Expr.NamedVar "V1")), (Expr.Var (Expr.ConstVar (Expr.String "R&D")))));
            Expr.Rel (Expr.Deltadelete ("ced", [Expr.NamedVar "V0"; Expr.NamedVar "V1_2"]));
          ]
        )
      ]
    };
    {
      title= "Update multi columns";
      (*
       * SQL:
       *   UPDATE
       *     t
       *   SET
       *     c1 = 'v1',
       *     c3 = 'v3',
       *     c5 = 'v5'
       *   WHERE
       *         c2 = 'v2'
       *     AND c3 = 'v100'
       *
       * datalog:
       *   t_tmp(V0, V1, V2, V3, V4, V5) :- V0 <> v1.
       *   t_tmp(V0, V1, V2, V3, V4, V5) :- V2 <> v3.
       *   t_tmp(V0, V1, V2, V3, V4, V5) :- V4 <> v5.
       *   -t(V0, V1, V2, V3, V4, V5) :- t(V0, V1, V2, V3, V4, V5), V1 = v2, V2 = v100, t_tmp(V0, V1, V2, V3, V4, V5).
       *   +t(V0, V1, V2, V3, V4, V5) :- V0 = v1, V2 = v3, V4 = v5, -t(V0_2, V1, V2_2, V3, V4_2, V5).
       *
       *)
      input= (
        Sql2ast.SqlUpdateSet (
          "t",
          [
            (None, "c1"), Sql2ast.SqlConst (Expr.String "v1");
            (None, "c3"), Sql2ast.SqlConst (Expr.String "v3");
            (None, "c5"), Sql2ast.SqlConst (Expr.String "v5")
          ],
          Some (Sql2ast.SqlWhere ([
            Sql2ast.SqlConstraint (
              Sql2ast.SqlColumn (None, "c2"),
              Sql2ast.SqlRelEqual,
              Sql2ast.SqlConst (Expr.String "v2")
            );
            Sql2ast.SqlConstraint (
              Sql2ast.SqlColumn (None, "c3"),
              Sql2ast.SqlRelEqual,
              Sql2ast.SqlConst (Expr.String "v100")
            )
          ]))
        ),
        None,
        ["c1"; "c2"; "c3"; "c4"; "c5"; "c6"]
      );
      expected= [
        (
          Expr.Pred ("t_tmp", [Expr.NamedVar "V0"; Expr.NamedVar "V1"; Expr.NamedVar "V2"; Expr.NamedVar "V3"; Expr.NamedVar "V4"; Expr.NamedVar "V5"]),
          [Expr.Equat (Expr.Equation ("<>", (Expr.Var (Expr.NamedVar "V0")), (Expr.Var (Expr.ConstVar (Expr.String "v1")))))]
        );
        (
          Expr.Pred ("t_tmp", [Expr.NamedVar "V0"; Expr.NamedVar "V1"; Expr.NamedVar "V2"; Expr.NamedVar "V3"; Expr.NamedVar "V4"; Expr.NamedVar "V5"]),
          [Expr.Equat (Expr.Equation ("<>", (Expr.Var (Expr.NamedVar "V2")), (Expr.Var (Expr.ConstVar (Expr.String "v3")))))]
        );
        (
          Expr.Pred ("t_tmp", [Expr.NamedVar "V0"; Expr.NamedVar "V1"; Expr.NamedVar "V2"; Expr.NamedVar "V3"; Expr.NamedVar "V4"; Expr.NamedVar "V5"]),
          [Expr.Equat (Expr.Equation ("<>", (Expr.Var (Expr.NamedVar "V4")), (Expr.Var (Expr.ConstVar (Expr.String "v5")))))]
        );
        (
          Expr.Deltadelete ("t", [Expr.NamedVar "V0"; Expr.NamedVar "V1"; Expr.NamedVar "V2"; Expr.NamedVar "V3"; Expr.NamedVar "V4"; Expr.NamedVar "V5"]),
          [
            Expr.Rel (Expr.Pred ("t", [Expr.NamedVar "V0"; Expr.NamedVar "V1"; Expr.NamedVar "V2"; Expr.NamedVar "V3"; Expr.NamedVar "V4"; Expr.NamedVar "V5"]));
            Expr.Equat (Expr.Equation ("=", (Expr.Var (Expr.NamedVar "V1")), (Expr.Var (Expr.ConstVar (Expr.String "v2")))));
            Expr.Equat (Expr.Equation ("=", (Expr.Var (Expr.NamedVar "V2")), (Expr.Var (Expr.ConstVar (Expr.String "v100")))));
            Expr.Rel (Expr.Pred ("t_tmp", [Expr.NamedVar "V0"; Expr.NamedVar "V1"; Expr.NamedVar "V2"; Expr.NamedVar "V3"; Expr.NamedVar "V4"; Expr.NamedVar "V5"]))
          ]
        );
        (
          Expr.Deltainsert ("t", [Expr.NamedVar "V0"; Expr.NamedVar "V1"; Expr.NamedVar "V2"; Expr.NamedVar "V3"; Expr.NamedVar "V4"; Expr.NamedVar "V5"]),
          [
            Expr.Equat (Expr.Equation ("=", (Expr.Var (Expr.NamedVar "V0")), (Expr.Var (Expr.ConstVar (Expr.String "v1")))));
            Expr.Equat (Expr.Equation ("=", (Expr.Var (Expr.NamedVar "V2")), (Expr.Var (Expr.ConstVar (Expr.String "v3")))));
            Expr.Equat (Expr.Equation ("=", (Expr.Var (Expr.NamedVar "V4")), (Expr.Var (Expr.ConstVar (Expr.String "v5")))));
            Expr.Rel (Expr.Deltadelete ("t", [Expr.NamedVar "V0_2"; Expr.NamedVar "V1"; Expr.NamedVar "V2_2"; Expr.NamedVar "V3"; Expr.NamedVar "V4_2"; Expr.NamedVar "V5"]));
          ]
        )
      ]
    };
    {
      title= "Use other columns";
      (*
       * SQL:
       *   UPDATE
       *     t
       *   SET
       *     c1 = c2,
       *     c2 = c3
       *
       * datalog:
       *   t_tmp(V0, V1, V2, V3) :- V0 <> V1.
       *   t_tmp(V0, V1, V2, V3) :- V1 <> V2.
       *   -t(V0, V1, V2, V3) :- t(V0, V1, V2, V3), t_tmp(V0, V1, V2, V3).
       *   +t(V0, V1, V2, V3) :- V0 = V1_2, V1 = V2, -t(V0_2, V1_2, V2, V3).
       *
       *)
      input= (
        Sql2ast.SqlUpdateSet (
          "t",
          [
            (None, "c1"), Sql2ast.SqlColumn (None, "c2");
            (None, "c2"), Sql2ast.SqlColumn (None, "c3")
          ],
          Some (Sql2ast.SqlWhere ([]))
        ),
        None,
        ["c1"; "c2"; "c3"; "c4"]
      );
      expected= [
        (
          Expr.Pred ("t_tmp", [Expr.NamedVar "V0"; Expr.NamedVar "V1"; Expr.NamedVar "V2"; Expr.NamedVar "V3"]),
          [Expr.Equat (Expr.Equation ("<>", (Expr.Var (Expr.NamedVar "V0")), (Expr.Var (Expr.NamedVar "V1"))))]
        );
        (
          Expr.Pred ("t_tmp", [Expr.NamedVar "V0"; Expr.NamedVar "V1"; Expr.NamedVar "V2"; Expr.NamedVar "V3"]),
          [Expr.Equat (Expr.Equation ("<>", (Expr.Var (Expr.NamedVar "V1")), (Expr.Var (Expr.NamedVar "V2"))))]
        );
        (
          Expr.Deltadelete ("t", [Expr.NamedVar "V0"; Expr.NamedVar "V1"; Expr.NamedVar "V2"; Expr.NamedVar "V3"]),
          [
            Expr.Rel (Expr.Pred ("t", [Expr.NamedVar "V0"; Expr.NamedVar "V1"; Expr.NamedVar "V2"; Expr.NamedVar "V3"]));
            Expr.Rel (Expr.Pred ("t_tmp", [Expr.NamedVar "V0"; Expr.NamedVar "V1"; Expr.NamedVar "V2"; Expr.NamedVar "V3"]))
          ]
        );
        (
          Expr.Deltainsert ("t", [Expr.NamedVar "V0"; Expr.NamedVar "V1"; Expr.NamedVar "V2"; Expr.NamedVar "V3"]),
          [
            Expr.Equat (Expr.Equation ("=", (Expr.Var (Expr.NamedVar "V0")), (Expr.Var (Expr.NamedVar "V1_2"))));
            Expr.Equat (Expr.Equation ("=", (Expr.Var (Expr.NamedVar "V1")), (Expr.Var (Expr.NamedVar "V2"))));
            Expr.Rel (Expr.Deltadelete ("t", [Expr.NamedVar "V0_2"; Expr.NamedVar "V1_2"; Expr.NamedVar "V2"; Expr.NamedVar "V3"]));
          ]
        )
      ]
    }
  ]
