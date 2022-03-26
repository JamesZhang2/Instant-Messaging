open OUnit2
open Util
open Database
open Time

(******************** Time Tests ********************)

let chk_time_test
    (name : string)
    (time : string)
    (well_formatted : bool) =
  name >:: fun _ -> assert_equal well_formatted (chk_time time)

let time_tests =
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
    chk_time_test "Current local time is correctly formatted"
      (string_of_now ~local:true)
      true;
    chk_time_test "Current time in GMT is correctly formatted"
      (string_of_now ~local:true)
      true;
  ]

(******************** Database Tests ********************)

let database_tests = []

(******************** Crypto Tests ********************)

let rsa_test msg =
  let open Crypto in
  let key = pke_gen () in
  let ctxt = pke_enc (get_pub key) msg in
  pke_dec (get_priv key) ctxt

let crypto_test (name : string) (msg : string) : test =
  name >:: fun _ -> assert_equal msg (rsa_test msg)

let crypto_tests =
  [
    crypto_test "RSA: hello world" "hello world";
    crypto_test "RSA: 123456789" "123456789";
    crypto_test "RSA: hellohellohellohellohello"
      "hellohellohellohellohello";
    crypto_test "RSA: 7932roud49yewfhds273p98y1e fwyodsro3q!ewfoi3--;"
      "7932roud49yewfhds273p98y1e fwyodsro3q!ewfoi3--;";
    crypto_test "RSA: " "";
  ]

let suite =
  "test suite for Util"
  >::: List.flatten [ time_tests; database_tests; crypto_tests ]

let _ =
  run_test_tt_main suite;
  db_main ()
