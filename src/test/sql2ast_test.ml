open Utils

type test_case = {
  title : string;
  input : Sql2ast.sql_update * Sql2ast.sql_column_name list;
  expected : Expr.rule list
}

type failed_body = { expected : string; actual : string }

type test_result =
  | Pass
  | Failed of failed_body

let run_test {input; expected; _} =
  let open ResultMonad in
  let string_of_rules rules =
    rules
      |> List.map Expr.string_of_rule
      |> String.concat "; "
  in
  let (update, columns) = input in
  Sql2ast.update_to_datalog update columns >>= fun actual ->
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

    | Error error ->
        Printf.printf "! %s: FAILED (%s)\n" title @@ Sql2ast.string_of_error error;
        true
  ) false

let main () =
  let open Sql2ast in
  let open Expr in
  run_tests [
    {
      title = "sample";
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
       *   ced_tmp(GenV1, GenV2) :- GenV2 <> R&D.
       *   -ced(GenV1, GenV2) :- ced(GenV1, GenV2), GenV2 = Dev, ced_tmp(GenV1, GenV2).
       *   +ced(GenV1, GenV2) :- GenV2 = R&D, -ced(GenV1, GenV2_2)
       *
       *)
      input = (
        SqlUpdateSet (
          "ced",
          [(None, "dname"), SqlConst (String "R&D")],
          Some (SqlWhere ([
            SqlConstraint (
              SqlColumn (None, "dname"),
              SqlRelEqual,
              SqlConst (String "Dev")
            )
          ]))
        ),
        ["ename"; "dname"]
      );
      expected = [
        (
          Pred ("ced_tmp", [NamedVar "GenV1"; NamedVar "GenV2"]),
          [Equat (Equation ("<>", (Var (NamedVar "GenV2")), (Var (ConstVar (String "R&D")))))]
        );
        (
          Deltadelete ("ced", [NamedVar "GenV1"; NamedVar "GenV2"]),
          [
            Rel (Pred ("ced", [NamedVar "GenV1"; NamedVar "GenV2"]));
            Equat (Equation ("=", (Var (NamedVar "GenV2")), (Var (ConstVar (String "Dev")))));
            Rel (Pred ("ced_tmp", [NamedVar "GenV1"; NamedVar "GenV2"]))
          ]
        );
        (
          Deltainsert ("ced", [NamedVar "GenV1"; NamedVar "GenV2"]),
          [
            Equat (Equation ("=", (Var (NamedVar "GenV2")), (Var (ConstVar (String "R&D")))));
            Rel (Deltadelete ("ced", [NamedVar "GenV1"; NamedVar "GenV2_2"]));
          ]
        )
      ]
    };
    {
      title = "Update multi columns";
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
       *   t_tmp(GenV1, GenV2, GenV3, GenV4, GenV5, GenV6) :- GenV1 <> v1.
       *   t_tmp(GenV1, GenV2, GenV3, GenV4, GenV5, GenV6) :- GenV3 <> v3.
       *   t_tmp(GenV1, GenV2, GenV3, GenV4, GenV5, GenV6) :- GenV5 <> v5.
       *   -t(GenV1, GenV2, GenV3, GenV4, GenV5, GenV6) :- t(GenV1, GenV2, GenV3, GenV4, GenV5, GenV6), GenV2 = v2, GenV3 = v100, t_tmp(GenV1, GenV2, GenV3, GenV4, GenV5, GenV6).
       *   +t(GenV1, GenV2, GenV3, GenV4, GenV5, GenV6) :- GenV1 = v1, GenV3 = v3, GenV5 = v5, -t(GenV1_2, GenV2, GenV3_2, GenV4, GenV5_2, GenV6).
       *
       *)
      input = (
        SqlUpdateSet (
          "t",
          [
            (None, "c1"), SqlConst (String "v1");
            (None, "c3"), SqlConst (String "v3");
            (None, "c5"), SqlConst (String "v5")
          ],
          Some (SqlWhere ([
            SqlConstraint (
              SqlColumn (None, "c2"),
              SqlRelEqual,
              SqlConst (String "v2")
            );
            SqlConstraint (
              SqlColumn (None, "c3"),
              SqlRelEqual,
              SqlConst (String "v100")
            )
          ]))
        ),
        ["c1"; "c2"; "c3"; "c4"; "c5"; "c6"]
      );
      expected = [
        (
          Pred ("t_tmp", [NamedVar "GenV1"; NamedVar "GenV2"; NamedVar "GenV3"; NamedVar "GenV4"; NamedVar "GenV5"; NamedVar "GenV6"]),
          [Equat (Equation ("<>", (Var (NamedVar "GenV1")), (Var (ConstVar (String "v1")))))]
        );
        (
          Pred ("t_tmp", [NamedVar "GenV1"; NamedVar "GenV2"; NamedVar "GenV3"; NamedVar "GenV4"; NamedVar "GenV5"; NamedVar "GenV6"]),
          [Equat (Equation ("<>", (Var (NamedVar "GenV3")), (Var (ConstVar (String "v3")))))]
        );
        (
          Pred ("t_tmp", [NamedVar "GenV1"; NamedVar "GenV2"; NamedVar "GenV3"; NamedVar "GenV4"; NamedVar "GenV5"; NamedVar "GenV6"]),
          [Equat (Equation ("<>", (Var (NamedVar "GenV5")), (Var (ConstVar (String "v5")))))]
        );
        (
          Deltadelete ("t", [NamedVar "GenV1"; NamedVar "GenV2"; NamedVar "GenV3"; NamedVar "GenV4"; NamedVar "GenV5"; NamedVar "GenV6"]),
          [
            Rel (Pred ("t", [NamedVar "GenV1"; NamedVar "GenV2"; NamedVar "GenV3"; NamedVar "GenV4"; NamedVar "GenV5"; NamedVar "GenV6"]));
            Equat (Equation ("=", (Var (NamedVar "GenV2")), (Var (ConstVar (String "v2")))));
            Equat (Equation ("=", (Var (NamedVar "GenV3")), (Var (ConstVar (String "v100")))));
            Rel (Pred ("t_tmp", [NamedVar "GenV1"; NamedVar "GenV2"; NamedVar "GenV3"; NamedVar "GenV4"; NamedVar "GenV5"; NamedVar "GenV6"]))
          ]
        );
        (
          Deltainsert ("t", [NamedVar "GenV1"; NamedVar "GenV2"; NamedVar "GenV3"; NamedVar "GenV4"; NamedVar "GenV5"; NamedVar "GenV6"]),
          [
            Equat (Equation ("=", (Var (NamedVar "GenV1")), (Var (ConstVar (String "v1")))));
            Equat (Equation ("=", (Var (NamedVar "GenV3")), (Var (ConstVar (String "v3")))));
            Equat (Equation ("=", (Var (NamedVar "GenV5")), (Var (ConstVar (String "v5")))));
            Rel (Deltadelete ("t", [NamedVar "GenV1_2"; NamedVar "GenV2"; NamedVar "GenV3_2"; NamedVar "GenV4"; NamedVar "GenV5_2"; NamedVar "GenV6"]));
          ]
        )
      ]
    };
    {
      title = "Use other columns";
      (*
       * SQL:
       *   UPDATE
       *     t
       *   SET
       *     c1 = c2,
       *     c2 = c3
       *
       * datalog:
       *   t_tmp(GenV1, GenV2, GenV3, GenV4) :- GenV1 <> GenV2.
       *   t_tmp(GenV1, GenV2, GenV3, GenV4) :- GenV2 <> GenV3.
       *   -t(GenV1, GenV2, GenV3, GenV4) :- t(GenV1, GenV2, GenV3, GenV4), t_tmp(GenV1, GenV2, GenV3, GenV4).
       *   +t(GenV1, GenV2, GenV3, GenV4) :- GenV1 = GenV2_2, GenV2 = GenV3, -t(GenV1_2, GenV2_2, GenV3, GenV4).
       *
       *)
      input = (
        SqlUpdateSet (
          "t",
          [
            (None, "c1"), SqlColumn (None, "c2");
            (None, "c2"), SqlColumn (None, "c3")
          ],
          Some (SqlWhere ([]))
        ),
        ["c1"; "c2"; "c3"; "c4"]
      );
      expected = [
        (
          Pred ("t_tmp", [NamedVar "GenV1"; NamedVar "GenV2"; NamedVar "GenV3"; NamedVar "GenV4"]),
          [Equat (Equation ("<>", (Var (NamedVar "GenV1")), (Var (NamedVar "GenV2"))))]
        );
        (
          Pred ("t_tmp", [NamedVar "GenV1"; NamedVar "GenV2"; NamedVar "GenV3"; NamedVar "GenV4"]),
          [Equat (Equation ("<>", (Var (NamedVar "GenV2")), (Var (NamedVar "GenV3"))))]
        );
        (
          Deltadelete ("t", [NamedVar "GenV1"; NamedVar "GenV2"; NamedVar "GenV3"; NamedVar "GenV4"]),
          [
            Rel (Pred ("t", [NamedVar "GenV1"; NamedVar "GenV2"; NamedVar "GenV3"; NamedVar "GenV4"]));
            Rel (Pred ("t_tmp", [NamedVar "GenV1"; NamedVar "GenV2"; NamedVar "GenV3"; NamedVar "GenV4"]))
          ]
        );
        (
          Deltainsert ("t", [NamedVar "GenV1"; NamedVar "GenV2"; NamedVar "GenV3"; NamedVar "GenV4"]),
          [
            Equat (Equation ("=", (Var (NamedVar "GenV1")), (Var (NamedVar "GenV2_2"))));
            Equat (Equation ("=", (Var (NamedVar "GenV2")), (Var (NamedVar "GenV3"))));
            Rel (Deltadelete ("t", [NamedVar "GenV1_2"; NamedVar "GenV2_2"; NamedVar "GenV3"; NamedVar "GenV4"]));
          ]
        )
      ]
    }
  ]
