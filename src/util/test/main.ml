open OUnit2
open Util
open Time

(******************** Time Tests ********************)

let chk_time_test
    (name : string)
    (time : string)
    (well_formatted : bool) =
  name >:: fun _ -> assert_equal well_formatted (chk_time time)

let time_tests =
  [
    chk_time_test "year must have 4 digits" "123-01-01 10:01:01" false;
    chk_time_test "month must be <= 12" "2001-13-01 10:01:01" false;
    chk_time_test "day must be <= 31" "2001-01-32 10:01:01" false;
    chk_time_test "nonzero hour starts with 0" "2001-01-01 01:20:20"
      true;
    chk_time_test "zero hour allowed" "2001-01-01 00:20:20" true;
    chk_time_test "hour must have 2 digits" "2001-01-01 9:10:10" false;
    chk_time_test "minute must have 2 digits" "2001-01-01 10:9:10" false;
    chk_time_test "second must have 2 digits" "2001-01-01 10:10:9" false;
    chk_time_test "hour must be < 24" "2001-01-01 24:01:01" false;
    chk_time_test "minute must be < 60" "2001-01-01 23:60:01" false;
    chk_time_test "second must be < 60" "2001-01-01 23:59:60" false;
    chk_time_test "first second of a day" "2019-09-01 00:00:00" true;
    chk_time_test "last second of a day" "2019-09-01 23:59:59" true;
    chk_time_test "first day of a year" "2000-01-01 08:00:00" true;
    chk_time_test "last day of a year" "2000-12-31 08:00:00" true;
    chk_time_test "earliest time allowed" Time.earliest_time true;
    chk_time_test "latest time allowed" Time.latest_time true;
    chk_time_test "Current local time is correctly formatted"
      (string_of_now ~local:true)
      true;
    chk_time_test "Current time in GMT is correctly formatted"
      (string_of_now ~local:true)
      true;
  ]

let suite = "test suite for Util" >::: List.flatten [ time_tests ]
let _ = run_test_tt_main suite
