open OUnit2
open Util
open Database

let parser_tests = [ ("test" >:: fun _ -> assert_equal 1 1) ]
let database_tests = []

let suite =
  "test suite for Util"
  >::: List.flatten [ parser_tests; database_tests ]

let _ = run_test_tt_main suite
