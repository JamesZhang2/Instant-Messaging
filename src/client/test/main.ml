open OUnit2
open Client
open Controller
open Interface
open Network
open Packager
open Parser

(******************** Controller Tests ********************)

let controller_tests = []

(******************** Interface Tests ********************)

let interface_tests = []

(******************** Network Tests ********************)

let network_tests = []

(******************** Packager Tests ********************)

(** Returns true if [time] is in the right time format and false
    otherwise. The right time format is (h)h:mm:ss (m)m/(d)d/yyyy, where
    the hour, month and day fields do not start with a 0 (unless hour is
    0) while the minute and second fields must have exactly two digits.
    The hour field must be between 0 and 23, inclusive. *)
let chk_time time =
  (* TODO: This does not check invalid dates like 2/31/2022. *)
  let hr = "\\([1]?[0-9]\\|2[0-3]\\)" in
  let min = "\\([0-5][0-9]\\)" in
  let sec = min in
  let month = "\\([1-9]\\|1[0-2]\\)" in
  let day = "\\([1-9]\\|[1-2][0-9]\\|3[0-1]\\)" in
  let yr = "\\([1-9][0-9][0-9][0-9]\\)" in
  Str.string_match
    (Str.regexp
       (Printf.sprintf "^%s:%s:%s %s/%s/%s$" hr min sec month day yr))
    time 0

let chk_time_test
    (name : string)
    (time : string)
    (well_formatted : bool) =
  name >:: fun _ -> assert_equal well_formatted (chk_time time)

let chk_time_tests =
  [
    chk_time_test "nonzero hour doesn't start with 0"
      "01:20:20 1/1/2001" false;
    chk_time_test "zero hour allowed" "0:20:20 1/1/2001" true;
    chk_time_test "minute must have 2 digits" "10:3:20 1/1/2001" false;
    chk_time_test "second must have 2 digits" "10:20:5 1/1/2001" false;
    chk_time_test "minute must be < 60" "10:60:15 1/1/2001" false;
    chk_time_test "second must be < 60" "10:15:60 1/1/2001" false;
    chk_time_test "month must be <= 12" "10:15:15 13/1/2001" false;
    chk_time_test "day must be <= 31" "10:15:15 1/32/2001" false;
    chk_time_test "first second of a day" "0:00:00 9/1/2019" true;
    chk_time_test "last second of a day" "23:59:59 9/1/2019" true;
    chk_time_test "first day of a year" "8:00:00 1/1/2000" true;
    chk_time_test "last day of a year" "8:00:00 12/31/2000" true;
  ]

let packager_tests = chk_time_tests @ []

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
