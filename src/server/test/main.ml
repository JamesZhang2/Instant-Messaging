open OUnit2
open Server
open Packager
open Parser
open Processor
open Yojson.Basic

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

let packager_tests = []

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

let processor_tests = []

let suite =
  "test suite for Server"
  >::: List.flatten [ packager_tests; parser_tests; processor_tests ]

let _ = run_test_tt_main suite
