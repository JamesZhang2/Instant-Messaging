open OUnit2
open Client
open Controller
open Interface
open Network
open Packager
open Parser

let controller_tests = []
let interface_tests = []
let network_tests = []
let packager_tests = [ ("test" >:: fun _ -> assert_equal 1 1) ]
let parser_tests = []

let suite =
  "test suite for Server"
  >::: List.flatten
         [
           controller_tests;
           interface_tests;
           network_tests;
           packager_tests;
           parser_tests;
         ]

let _ = run_test_tt_main suite
