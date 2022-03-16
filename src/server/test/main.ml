open OUnit2
open Server
open Packager
open Parser
open Processor
open Yojson.Basic

(******************** Client Packager Tests ********************)

let packager_tests = []

(******************** Server Parser Tests ********************)

let parser_dir = "data/parser/"

let friend_accept_1 =
  from_file (parser_dir ^ "friend_accept_1.json") |> to_string |> parse

let friend_reject_1 =
  from_file (parser_dir ^ "friend_reject_1.json") |> to_string |> parse

let friend_req_1 =
  from_file (parser_dir ^ "friend_req_1.json") |> to_string |> parse

let get_msg_1 =
  from_file (parser_dir ^ "get_msg_1.json") |> to_string |> parse

let login_1 =
  from_file (parser_dir ^ "login_1.json") |> to_string |> parse

let register_1 =
  from_file (parser_dir ^ "register_1.json") |> to_string |> parse

let send_msg_1 =
  from_file (parser_dir ^ "send_msg_1.json") |> to_string |> parse

let parser_type_test
    (name : string)
    (parsed_t : Parser.t)
    (expected_output : Parser.pkt_type) =
  name >:: fun _ -> assert_equal expected_output (pkt_type parsed_t)

let parser_tests =
  [
    ( "Time of send_msg_1 is correct" >:: fun _ ->
      assert_equal "8:00:00 3/1/2022" (time send_msg_1)
        ~printer:(fun x -> x) );
    ( "Time of get_msg_1 is correct" >:: fun _ ->
      assert_equal "19:15:53 11/16/2022" (time get_msg_1)
        ~printer:(fun x -> x) );
    ( "Sender of get_msg_1 is Alice" >:: fun _ ->
      assert_equal "Alice" (sender get_msg_1) ~printer:(fun x -> x) );
    ( "Sender of login_1 is Catherine" >:: fun _ ->
      assert_equal "Catherine" (sender login_1) ~printer:(fun x -> x) );
    parser_type_test "Type of friend_accept_1 is correct"
      friend_accept_1
      (FriendReqReply ("Bob", true));
    parser_type_test "Type of friend_reject_1 is correct"
      friend_reject_1
      (FriendReqReply ("Bob", false));
    parser_type_test "Type of friend_req_1 is correct" friend_req_1
      (FriendReq ("Bob", "Hi, I'm Alice."));
    parser_type_test "Type of get_msg_1 is correct" get_msg_1 GetMessage;
    parser_type_test "Type of login_1 is correct" login_1
      (Login "OCaml!");
    parser_type_test "Type of register_1 is correct" register_1
      (Register "OCaml!");
    parser_type_test "Type of send_msg_1 is correct" send_msg_1
      (SendMessage ("Bob", "Hi"));
  ]

(******************** Server Packager Tests ********************)

(**[test func name expected input]*)
let test func name expected input =
  name >:: fun _ ->
  assert_equal expected (func input) ~printer:(fun x -> x)

let error_expected_1 =
  "{\n\
   \t\"type\" : \"Error\", \n\
   \t\"time\" : \"time\", \n\
   \t\"message\" : \"error message\"\n\
   }"

let error_expected_2 =
  "{\n\
   \t\"type\" : \"Error\", \n\
   \t\"time\" : \"time\", \n\
   \t\"message\" : \"\"\n\
   }"

let post_expected_2 =
  "{\n\
   \t\"type\" : \"Post\", \n\
   \t\"time\" : \"time\", \n\
   \t\"message\" : \"Post Message\"\n\
   }"

let get_expected_1 =
  "{\n\
   \t\"type\" : \"Get\", \n\
   \t\"time\" : \"time\", \n\
   \t\"message\" : [\n\
   {\n\
   \t\"sender\" : \"sender\", \n\
   \t\"receiver\" : \"receiver\", \n\
   \t\"time\" : \"time\", \n\
   \t\"msg_type\" : \"Message\", \n\
   \t\"message\" : \"message\"\n\
   }\n\
   ]\n\
   }"

let get_expected_2 =
  "{\n\
   \t\"type\" : \"Get\", \n\
   \t\"time\" : \"time\", \n\
   \t\"message\" : [\n\
   {\n\
   \t\"sender\" : \"sender1\", \n\
   \t\"receiver\" : \"receiver1\", \n\
   \t\"time\" : \"time1\", \n\
   \t\"msg_type\" : \"Message\", \n\
   \t\"message\" : \"message1\"\n\
   }, \n\
   {\n\
   \t\"sender\" : \"sender2\", \n\
   \t\"receiver\" : \"receiver2\", \n\
   \t\"time\" : \"time2\", \n\
   \t\"msg_type\" : \"FriendReq\", \n\
   \t\"message\" : \"message2\"\n\
   }\n\
   ]\n\
   }"

let get_expected_3 =
  "{\n\
   \t\"type\" : \"Get\", \n\
   \t\"time\" : \"time\", \n\
   \t\"message\" : [\n\n\
   ]\n\
   }"

let error_tests = []

let packager_tests =
  [
    test error_response "error_test 1" error_expected_1 "error message";
    test error_response "error_test 2" error_expected_2 "";
    test post_method_response "post_test 1" post_expected_2
      "Post Message";
    test get_method_response "get test 1" get_expected_1
      [ make_message "sender" "receiver" "time" Message "message" ];
    test get_method_response "get test 2" get_expected_2
      [
        make_message "sender1" "receiver1" "time1" Message "message1";
        make_message "sender2" "receiver2" "time2" FriendReq "message2";
      ];
    test get_method_response "get test 3" get_expected_3 [];
  ]

(******************** Processor Tests ********************)

let processor_tests = []

let suite =
  "test suite for Server"
  >::: List.flatten
         [
           packager_tests; parser_tests; processor_tests; packager_tests;
         ]

let _ = run_test_tt_main suite
