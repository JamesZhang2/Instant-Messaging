open OUnit2
open Server
open Database
open Packager
open Parser
open Processor
open Util

(******************** Server Database Tests ********************)

(** Warning: To prevent bad data from going into the server database,
    remember to change [test] to [true] in database.ml before running
    the tests. *)

let test_add_n_diff n =
  for i = 1 to n do
    let added =
      add_user (string_of_int i) "pwd" "key" "2022-03-27 11:54:50"
      |> fst
    in
    assert added
  done

let test_add_n_same n =
  let added =
    add_user "Alice" "pwd" "key" "2022-03-27 11:54:50" |> fst
  in
  assert added;

  for i = 1 to n do
    let added =
      add_user "Alice" "pwd" "key" "2022-03-27 11:54:50" |> fst
    in
    assert (not added)
  done

let run_database_tests () =
  test_add_n_diff 100;
  test_add_n_same 100

(******************** Server Parser Tests ********************)

let parser_dir = "data/parser/"

let to_parser_t filename =
  let open Yojson.Basic in
  from_file (parser_dir ^ filename) |> to_string |> parse

let friend_accept_1 = to_parser_t "friend_accept_1.json"
let friend_reject_1 = to_parser_t "friend_reject_1.json"
let friend_req_1 = to_parser_t "friend_req_1.json"
let get_msg_1 = to_parser_t "get_msg_1.json"
let login_1 = to_parser_t "login_1.json"
let register_1 = to_parser_t "register_1.json"
let send_msg_1 = to_parser_t "send_msg_1.json"

let parser_type_test
    (name : string)
    (parsed_t : Parser.t)
    (expected_output : Parser.pkt_type) =
  name >:: fun _ -> assert_equal expected_output (pkt_type parsed_t)

let parser_tests =
  [
    ( "Time of send_msg_1 is correct" >:: fun _ ->
      assert_equal "2022-03-01 08:00:00" (time send_msg_1)
        ~printer:(fun x -> x) );
    ( "Time of get_msg_1 is correct" >:: fun _ ->
      assert_equal "2022-11-16 19:15:53" (time get_msg_1)
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

(** [remove_time j] removes the value of the time field from the json
    string [j]. *)
let remove_time j =
  Str.global_replace (Time.time_regex |> Str.regexp) "<removed>" j

(** [equal_ignore_time s1 s2] is true if s1 and s2 are equal with the
    value of the time field removed. *)
let equal_ignore_time s1 s2 = remove_time s1 = remove_time s2

(**[test func name expected input]*)
let test func name expected input =
  name >:: fun _ ->
  assert_equal expected (func input)
    ~printer:(fun x -> x)
    ~cmp:equal_ignore_time

let error_expected_1 =
  "{\n\
   \t\"type\" : \"Error\", \n\
   \t\"time\" : \"2000-01-01 08:00:00\", \n\
   \t\"message\" : \"error message\"\n\
   }"

let error_expected_2 =
  "{\n\
   \t\"type\" : \"Error\", \n\
   \t\"time\" : \"2000-01-01 08:00:00\", \n\
   \t\"message\" : \"\"\n\
   }"

let post_expected_2 =
  "{\n\
   \t\"type\" : \"Post\", \n\
   \t\"time\" : \"2000-01-01 08:00:00\", \n\
   \t\"message\" : \"Post Message\"\n\
   }"

let get_expected_1 =
  "{\n\
   \t\"type\" : \"GetMsg\", \n\
   \t\"time\" : \"2000-01-01 08:00:00\", \n\
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
   \t\"type\" : \"GetMsg\", \n\
   \t\"time\" : \"2000-01-01 08:00:00\", \n\
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
   \t\"type\" : \"GetMsg\", \n\
   \t\"time\" : \"2000-01-01 08:00:00\", \n\
   \t\"message\" : [\n\n\
   ]\n\
   }"

let error_tests = []

let packager_tests =
  let open Util in
  [
    test error_response "error_test 1" error_expected_1 "error message";
    test error_response "error_test 2" error_expected_2 "";
    test post_method_response "post_test 1" post_expected_2
      "Post Message";
    test get_method_response "get test 1" get_expected_1
      [ Msg.make_msg "sender" "receiver" "time" Msg.Message "message" ];
    test get_method_response "get test 2" get_expected_2
      [
        Msg.make_msg "sender1" "receiver1" "time1" Msg.Message
          "message1";
        Msg.make_msg "sender2" "receiver2" "time2" Msg.FriendReq
          "message2";
      ];
    test get_method_response "get test 3" get_expected_3 [];
  ]

(******************** Processor Tests ********************)

let processor_tests = []

let suite =
  "test suite for Server"
  >::: List.flatten [ parser_tests; processor_tests; packager_tests ]

let _ =
  create_tables ();
  run_test_tt_main suite;
  run_database_tests ()
