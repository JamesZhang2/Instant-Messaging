open OUnit2
open Client
open Controller
open Interface
open Network
open Packager
open Parser
open Util.Time

(******************** Controller Tests ********************)

let controller_tests = []

(******************** Interface Tests ********************)

let interface_tests = []

(******************** Network Tests ********************)

let network_tests = []

(******************** Packager Tests ********************)

(** [remove_time j] removes the value of the time field from the json
    string [j]. *)
let remove_time j =
  Str.global_replace (time_regex |> Str.regexp) "<removed>" j

(** [equal_ignore_time s1 s2] is true if s1 and s2 are equal with the
    value of the time field removed. *)
let equal_ignore_time s1 s2 = remove_time s1 = remove_time s2

let send_msg_1 =
  "{\"type\": \"SendMessage\",\"sender\": \"Alice\",\"time\": \
   \"8:00:00 3/1/2022\",\"receiver\": \"Bob\",\"message\": \"Hello \
   world!\"}"

let packager_tests =
  [
    ( "packing SendMessage" >:: fun _ ->
      assert_equal send_msg_1
        (pack_send_msg "Alice" "Bob" "Hello world!")
        ~cmp:equal_ignore_time ~printer:remove_time );
  ]

(******************** Parser Tests ********************)

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
