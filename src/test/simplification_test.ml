
open Utils
open Expr


type test_case = {
  title    : string;
  input    : rule list;
  expected : rule list;
}

type test_result =
  | Pass
  | Fail of { expected : string; got : string }


let run_test (test_case : test_case) =
  let open ResultMonad in
  Simplification.simplify test_case.input >>= fun got ->
  let s_got = got |> List.map string_of_rule |> String.concat "; " in
  let s_expected = test_case.expected |> List.map string_of_rule |> String.concat "; " in
  if String.equal s_got s_expected then
    return Pass
  else
    return (Fail { expected = s_expected; got = s_got })


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
        Printf.printf "! %s: FAILED (error)\n" title;
        true
  ) false


let main () =
  let track = NamedVar "TRACK" in
  let date = NamedVar "DATE" in
  let rating = NamedVar "RATING" in
  let album = NamedVar "ALBUM" in
  run_tests [
    {
      title    = "empty";
      input    = [];
      expected = [];
    };
    {
      title = "(1)";
      input = [
        (* (1):
          -tracks(TRACK, DATE, RATING, ALBUM) :-
            albums(ALBUM, _),
            albums(ALBUM, V6845),
            tracks(TRACK, DATE, RATING, ALBUM),
            tracks(TRACK, DATE, RATING, ALBUM),
            RATING = 1. *)
        (Deltadelete ("tracks", [ track; date; rating; album ]), [
          Rel (Pred ("albums", [ album; AnonVar ]));
          Rel (Pred ("albums", [ album; NamedVar "V6845" ]));
          Rel (Pred ("tracks", [ track; date; rating; album ]));
          Rel (Pred ("tracks", [ track; date; rating; album ]));
          Equat (Equation ("=", Var rating, Const (Int 1)));
        ]);
      ];
      expected = [
        (* (1) simplified:
          -tracks(TRACK, DATE, RATING, ALBUM) :-
            albums(ALBUM, _),
            tracks(TRACK, DATE, RATING, ALBUM),
            RATING = 1. *)
        (Deltadelete ("tracks", [ track; date; rating; album ]), [
          Rel (Pred ("albums", [ album; AnonVar ]));
          Rel (Pred ("tracks", [ track; date; rating; album ]));
          Equat (Equation ("=", Var rating, Const (Int 1)));
        ])
      ];
    };
  ]
