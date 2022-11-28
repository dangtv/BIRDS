
open Expr


type test_case = {
  title    : string;
  input    : rule list;
  expected : rule list;
}


let run_test (test_case : test_case) =
  let title = test_case.title in
  match Simplification.simplify test_case.input with
  | Ok got ->
      Printf.printf "- %s: OK\n" title;
      false

  | Error _ ->
      Printf.printf "- %s: FAILED (error)\n" title;
      true


let main () =
  run_test {
    title    = "empty";
    input    = [];
    expected = [];
  }
