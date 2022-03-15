open OUnit2
open Util
open Database
open Time

let time_tests =
  [
    ( "Current local time" >:: fun _ ->
      print_endline
        ("The current local time is " ^ string_of_now ~local:true) );
    ( "Current time in GMT" >:: fun _ ->
      print_endline
        ("The current time in GMT is " ^ string_of_now ~local:false) );
  ]

let database_tests = []

let suite =
  "test suite for Util" >::: List.flatten [ time_tests; database_tests ]

let _ = run_test_tt_main suite
