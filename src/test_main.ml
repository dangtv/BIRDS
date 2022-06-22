
open Utils
open Expr


let make_colnamtab (defs : (table_name * column_name list) list) : colnamtab =
  let colnamtab : colnamtab = Hashtbl.create 32 in
  defs |> List.iter (fun (table, cols) ->
    let arity = List.length cols in
    Hashtbl.add colnamtab (table, arity) cols
  );
  colnamtab


let main () =
  let open ResultMonad in
  let colnamtab =
    make_colnamtab [
      ("ed", ["emp_name"; "dept_name"]);
      ("eed", ["emp_name"; "dept_name"]);
    ]
  in

  (* "+ed(E, D) :- ed(V1, D), E = 'Joe', D = 'A', V1 != 'Joe', ¬ed(E, D), ¬eed(V1, D)." *)
  let rule =
    let head = Deltainsert ("ed", [ NamedVar "E"; NamedVar "D" ]) in
    let body =
      [
        Rel (Pred ("ed", [ NamedVar "V1"; NamedVar "D" ]));
        Equat (Equation ("=", Var (NamedVar "E"), Const (String "'Joe'")));
        Equat (Equation ("=", Var (NamedVar "D"), Const (String "'A'")));
        Equat (Equation ("<>", Var (NamedVar "V1"), Const (String "'Joe'")));
        Not (Pred ("ed", [ NamedVar "E"; NamedVar "D" ]));
        Not (Pred ("eed", [ NamedVar "V1"; NamedVar "D" ]));
      ]
    in
    (head, body)
  in

  let expected =
    "SELECT 'Joe' AS emp_name, 'A' AS dept_name FROM ed AS ed0 WHERE ed0.dept_name = 'A' AND ed0.emp_name <> 'Joe' AND NOT EXISTS ( SELECT * FROM ed AS t WHERE t.emp_name = 'Joe' AND t.dept_name = 'A' ) AND NOT EXISTS ( SELECT * FROM eed AS t WHERE t.emp_name = ed0.emp_name AND t.dept_name = 'A' )"
  in

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
  match main () with
  | Ok ()   -> ()
  | Error _ -> ()
