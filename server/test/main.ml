open OUnit2
open Server
open Packager
open Parser
open Processor

let packager_tests = [ ("test" >:: fun _ -> assert_equal 1 1) ]
let parser_tests = []
let processor_tests = []

let suite =
  "test suite for Server"
  >::: List.flatten [ packager_tests; parser_tests; processor_tests ]

let _ = run_test_tt_main suite
