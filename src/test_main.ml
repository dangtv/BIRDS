
let () =
  let has_failed =
    List.exists (fun b -> b) [
      Ast2sql_operation_based_conversion_test.main ();
      (* You can add more tests here *)
    ]
  in
  exit (if has_failed then 1 else 0)
