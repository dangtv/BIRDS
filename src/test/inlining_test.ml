
open Utils
open Expr


type test_case = {
  title    : string;
  input    : rule list;
  expected : string;
}

type test_result =
  | Pass
  | Fail of { expected : string; got : string }


let run_test (test_case : test_case) : (test_result, Inlining.error) result =
  let open ResultMonad in
  let expected = test_case.expected in

  Inlining.inline_rules test_case.input >>= fun rules_output ->
  let got = rules_output |> List.map string_of_rule |> String.concat "" in

  if String.equal got test_case.expected then
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

    | Error e ->
        Printf.printf "! %s: FAILED (%s)\n" title (Inlining.string_of_error e);
        true
  ) false


let make_lines ss =
  ss |> List.map (fun s -> s ^ "\n") |> String.concat ""


let main () =
  let test_cases =
    [
      {
        title = "inlining the empty program";
        input = [];
        expected = "";
      };
      {
        title = "minimal inlining";
        input = [
          (Deltainsert ("foo", [ NamedVar "X" ]), [
            Rel (Pred ("bar", [ NamedVar "X" ]));
          ]);
          (Pred ("bar", [ NamedVar "Y" ]), [
            Rel (Pred ("qux", [ NamedVar "Y" ]));
          ]);
        ];
        (* Input:
             +foo(X) :- bar(X).
             bar(Y) :- qux(Y). *)
        expected =
          make_lines [
            "+foo(X) :- qux(X).";
            "bar(Y) :- qux(Y).";
          ];
      };
      {
        title = "inlining rules with anonymous variables";
        input = [
          (Deltainsert ("foo", [ NamedVar "X" ]), [
            Rel (Pred ("bar", [ NamedVar "X"; AnonVar ]));
          ]);
          (Pred ("bar", [ NamedVar "A"; NamedVar "B" ]), [
            Rel (Pred ("qux", [ NamedVar "A"; NamedVar "B"; AnonVar ]));
          ]);
        ];
        (* Input:
            +foo(X) :- bar(X, _).
            bar(A, B) :- qux(A, B, _). *)
        expected =
          make_lines [
            "+foo(X) :- qux(X, GenV1, GenV2).";
            "bar(A, B) :- qux(A, B, GenV2).";
          ];
      };
    ]
  in
  run_tests test_cases
