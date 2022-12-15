
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
  let quantity = NamedVar "QUANTITY" in
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
    {
      title = "(2): erased by contradiction";
      input = [
        (* (2):
          -tracks(TRACK, DATE, RATING, ALBUM) :-
            albums(ALBUM, V34),
            albums(ALBUM, V6846),
            tracks(TRACK, DATE, RATING, ALBUM),
            tracks(V31, V32, V33, ALBUM),
            tracks(TRACK, DATE, RATING, ALBUM),
            RATING = 1,
            not tracks(V31, V32, V33, ALBUM). *)
        (Deltadelete ("tracks", [ track; date; rating; album ]), [
          Rel (Pred ("albums", [ album; NamedVar "V34" ]));
          Rel (Pred ("albums", [ album; NamedVar "V6846" ]));
          Rel (Pred ("tracks", [ track; date; rating; album ]));
          Rel (Pred ("tracks", [ NamedVar "V31"; NamedVar "V32"; NamedVar "V33"; album ]));
          Rel (Pred ("tracks", [ track; date; rating; album ]));
          Equat (Equation ("=", Var rating, Const (Int 1)));
          Not (Pred ("tracks", [ NamedVar "V31"; NamedVar "V32"; NamedVar "V33"; album ]));
        ]);
      ];
      expected = [];
    };
    {
      title = "(7)";
      input = [
        (* (7):
          -albums(ALBUM, QUANTITY) :-
            albums(ALBUM, QUANTITY),
            albums(ALBUM, QUANTITY),
            tracks(_, _, _, ALBUM),
            tracks(V6853, V6854, V6855, ALBUM),
            V6855 = 1. *)
        (Deltadelete ("albums", [ album; quantity ]), [
          Rel (Pred ("albums", [ album; quantity ]));
          Rel (Pred ("albums", [ album; quantity ]));
          Rel (Pred ("tracks", [ AnonVar; AnonVar; AnonVar; album ]));
          Rel (Pred ("tracks", [ NamedVar "V6853"; NamedVar "V6854"; NamedVar "V6855"; album ]));
          Equat (Equation ("=", Var (NamedVar "V6855"), Const (Int 1)));
        ]);
      ];
      expected = [
        (* (7) simplified:
          -albums(ALBUM, QUANTITY) :-
            albums(ALBUM, QUANTITY),
            tracks(_, _, V6855, ALBUM),
            V6855 = 1. *)
        (Deltadelete ("albums", [ album; quantity ]), [
          Rel (Pred ("albums", [ album; quantity ]));
          Rel (Pred ("tracks", [ AnonVar; AnonVar; NamedVar "V6855"; album ]));
          Equat (Equation ("=", Var (NamedVar "V6855"), Const (Int 1)));
        ]);
      ];
    };
    {
      title = "(32)";
      input = [
        (* (32):
          -albums(ALBUM, QUANTITY) :-
            albums(ALBUM, QUANTITY),
            albums(ALBUM, QUANTITY),
            tracks(TRACK, DATE, RATING, ALBUM),
            tracks(V6847, V6848, V6849, ALBUM),
            V6849 = 1,
            not RATING = 1. *)
        (Deltadelete ("albums", [ album; quantity ]), [
          Rel (Pred ("albums", [ album; quantity ]));
          Rel (Pred ("albums", [ album; quantity ]));
          Rel (Pred ("tracks", [ track; date; rating; album ]));
          Rel (Pred ("tracks", [ NamedVar "V6847"; NamedVar "V6848"; NamedVar "V6849"; album ]));
          Equat (Equation ("=", Var (NamedVar "V6849"), Const (Int 1)));
          Noneq (Equation ("=", Var rating, Const (Int 1)));
        ]);
      ];
      expected = [
        (* (32) simplified:
          -albums(ALBUM, QUANTITY) :-
            albums(ALBUM, QUANTITY),
            tracks(_, _, RATING, ALBUM),
            tracks(_, _, V6849, ALBUM),
            not RATING = 1,
            V6849 = 1. *)
        (Deltadelete ("albums", [ album; quantity ]), [
          Rel (Pred ("albums", [ album; quantity ]));
          Rel (Pred ("tracks", [ AnonVar; AnonVar; rating; album ]));
          Rel (Pred ("tracks", [ AnonVar; AnonVar; NamedVar "V6849"; album ]));
          Noneq (Equation ("=", Var rating, Const (Int 1)));
          Equat (Equation ("=", Var (NamedVar "V6849"), Const (Int 1)));
        ]);
      ];
    };
  ]
