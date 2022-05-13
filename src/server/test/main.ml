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

(** Note: We are not using ounit to test the database because ounit
    tests are parallel, whereas we need the database tests to be
    sequential. For instance, we must first add a user before testing
    that the user exists in the database. *)

(** [assert_false x] asserts that [x] is [false]. *)
let assert_false x = assert (not x)

(** [assert_same_elts lst1 lst2] asserts that [lst1] and [lst2] have the
    same elements, regardless of order. *)
let assert_same_elts lst1 lst2 =
  assert_equal (List.sort compare lst1) (List.sort compare lst2)

let injection_str = "Robert\'); DROP TABLE users; --"
(* SQL injection attack *)

let strange_chars = "`~!@#$%^&*()-_=+[]{}\\|;:\'\",.<>/?\t\n"

let test_add_n_diff n =
  for i = 1 to n do
    let added =
      add_user (string_of_int i) "pwd" "key" "2022-03-27 11:54:50"
    in
    assert added
  done

let test_add_n_same n =
  let added = add_user "Alice" "apple" "key A" "2022-03-27 11:54:50" in
  assert added;

  for i = 1 to n do
    let added = add_user "Alice" "pear" "42" "2022-03-27 11:54:50" in
    assert (not added)
  done

let add_more_users () =
  let added =
    add_user "Bob" "banana" "key B" "2022-03-27 14:55:20"
    && add_user "Catherine" "cherry" "key C" "2022-03-27 14:58:33"
    && add_user injection_str strange_chars strange_chars
         "2022-03-29 19:12:28"
  in

  assert added

let test_user_exists () =
  assert (user_exists "Alice");
  assert (user_exists "Bob");
  assert_false (user_exists "Foo")

let test_user_key () =
  assert_equal (user_key "Alice") "key A";
  assert_equal (user_key "Bob") "key B";
  assert_equal (user_key "Catherine") "key C"

let test_chk_pwd_true () = assert (chk_pwd "Alice" "apple")
let test_chk_pwd_false () = assert_false (chk_pwd "Alice" "watermelon")

let test_chk_pwd_unknown () =
  assert_raises (UnknownUser "Foo") (fun _ -> chk_pwd "Foo" "banana")

let test_chk_pwd () =
  test_chk_pwd_true ();
  test_chk_pwd_false ();
  test_chk_pwd_unknown ()

let msg_alice_bob : Msg.t =
  Msg.make_msg "Alice" "Bob" "2022-03-28 23:09:02" Message "Hello Bob"

let msg_bob_alice_1 : Msg.t =
  Msg.make_msg "Bob" "Alice" "2022-03-28 23:10:14" Message "Hi Alice"

let msg_bob_alice_2 : Msg.t =
  Msg.make_msg "Bob" "Alice" "2022-03-28 23:10:35" Message
    "How's it going?"

let msg_catherine_alice : Msg.t =
  Msg.make_msg "Catherine" "Alice" "2022-03-27 10:14:41" Message
    ("Hey Alice, this is Catherine!" ^ injection_str ^ strange_chars)

let msg_foo_alice : Msg.t =
  Msg.make_msg "Foo" "Alice" "2022-03-28 23:09:02" Message "Hi"

let msg_bob_bar : Msg.t =
  Msg.make_msg "Bob" "Bar" "2022-03-28 23:09:02" Message "Hi"

let msg_bad_time : Msg.t =
  Msg.make_msg "Alice" "Bob" "2022-03-28 24:09:02" Message "Hi"

let test_add_msg () =
  assert (add_msg msg_alice_bob);
  assert (add_msg msg_catherine_alice);
  assert (add_msg msg_bob_alice_1);
  assert (add_msg msg_bob_alice_2);
  assert_raises (UnknownUser "Foo") (fun _ -> add_msg msg_foo_alice);
  assert_raises (UnknownUser "Bar") (fun _ -> add_msg msg_bob_bar);
  assert_raises MalformedTime (fun _ -> add_msg msg_bad_time)

let chk_alice_bob_msg () =
  match get_new_msg "Bob" with
  | [ msg ] ->
      assert_equal
        ( Msg.sender msg,
          Msg.receiver msg,
          Msg.time msg,
          Msg.msg_type msg,
          Msg.content msg )
        ("Alice", "Bob", "2022-03-28 23:09:02", Message, "Hello Bob")
  | _ -> assert false

let test_get_msg () =
  assert_equal
    (get_new_msg_since "Alice" "2022-03-28 00:00:00" |> List.length)
    2;
  assert_equal
    (get_new_msg_since "Alice" "2022-03-28 00:00:00" |> List.length)
    0;
  assert_equal (get_new_msg "Alice" |> List.length) 1;
  assert_equal (get_new_msg "Alice" |> List.length) 0;
  assert_equal
    (get_msg_since "Alice" "2022-03-28 23:10:20" |> List.length)
    1;
  assert_equal (get_msg "Alice" |> List.length) 3;
  chk_alice_bob_msg ();
  assert_raises (UnknownUser "Foo") (fun _ -> get_msg "Foo");
  assert_raises (UnknownUser "Foo") (fun _ ->
      get_msg_since "Foo" "2022-03-28 00:00:00");
  assert_raises (UnknownUser "Bar") (fun _ -> get_new_msg "Bar");
  assert_raises (UnknownUser "Bar") (fun _ ->
      get_new_msg_since "Bar" "2022-03-28 00:00:00");
  assert_raises MalformedTime (fun _ ->
      get_msg_since "Alice" "2022-12-34 08:00:00");
  assert_raises MalformedTime (fun _ ->
      get_new_msg_since "Catherine" "2022-12-01 12:59:60")

let fr_alice_bob : Msg.t =
  Msg.make_msg "Alice" "Bob" "2022-03-30 17:29:34" FriendReq
    "Let's be best friends!"

let fr_catherine_alice : Msg.t =
  Msg.make_msg "Catherine" "Alice" "2022-03-30 20:08:04" FriendReq
    (injection_str ^ strange_chars)

let fr_foo_alice : Msg.t =
  Msg.make_msg "Foo" "Alice" "2022-03-30 20:08:04" FriendReq "Hi"

let fr_alice_bar : Msg.t =
  Msg.make_msg "Alice" "Bar" "2022-03-30 20:08:04" FriendReq "Hi"

let bob_accepts_alice : Msg.t =
  Msg.make_msg "Bob" "Alice" "2022-03-30 20:08:05"
    (FriendReqRep (true, "key"))
    "True"

let alice_rejects_catherine : Msg.t =
  Msg.make_msg "Alice" "Catherine" "2022-03-30 20:08:06"
    (FriendReqRep (false, ""))
    "False"

let test_friend_requests () =
  (* Status check before any friend requests *)
  assert_false (fr_exist "Alice" "Bob");
  assert_false (is_friend "Alice" "Bob");
  assert_equal (friends_of "Alice") [];

  (* New friend requests *)
  assert (new_fr fr_alice_bob);
  assert (add_msg fr_alice_bob);
  assert (new_fr fr_catherine_alice);
  assert (add_msg fr_catherine_alice);
  assert_raises (UnknownUser "Foo") (fun _ -> new_fr fr_foo_alice);
  assert_raises (UnknownUser "Bar") (fun _ -> new_fr fr_alice_bar);

  (* Status check for pending requests *)
  assert (fr_exist "Alice" "Bob");
  assert_false (fr_exist "Bob" "Alice");
  assert_false (is_friend "Alice" "Bob");
  assert_equal (get_new_msg "Bob" |> List.length) 1;
  assert (fr_exist "Catherine" "Alice");
  assert_false (fr_exist "Alice" "Catherine");
  assert_false (is_friend "Catherine" "Alice");
  assert_equal (get_new_msg "Alice" |> List.length) 1;
  assert_equal (friends_of "Alice") [];
  assert_equal (friends_of "Bob") [];
  assert_equal (friends_of "Catherine") [];

  (* Accept *)
  assert (fr_accept "Alice" "Bob");
  (* Alice is accepted by Bob *)
  assert (add_msg bob_accepts_alice);
  assert (is_friend "Alice" "Bob");
  assert (is_friend "Bob" "Alice");
  assert_false (fr_exist "Alice" "Bob");
  assert_false (fr_exist "Bob" "Alice");
  assert_equal (get_new_msg "Alice" |> List.length) 1;
  assert_equal (friends_of "Alice") [ "Bob" ];
  assert_equal (friends_of "Bob") [ "Alice" ];

  (* Reject *)
  assert (fr_reject "Catherine" "Alice");
  (* Catherine is rejected by Alice *)
  assert (add_msg alice_rejects_catherine);
  assert_false (is_friend "Catherine" "Alice");
  assert_false (is_friend "Alice" "Catherine");
  assert_false (fr_exist "Catherine" "Alice");
  assert_false (fr_exist "Alice" "Catherine");
  assert_equal (get_new_msg "Catherine" |> List.length) 1;
  assert_equal (friends_of "Alice") [ "Bob" ];
  assert_equal (friends_of "Catherine") []

let test_groupchat () =
  (* Create a groupchat *)
  assert_false (gc_exists "CS3110");
  assert (create_groupchat "CS3110" "OCaml" "Alice");
  assert (gc_exists "CS3110");
  assert (is_in_gc "CS3110" "Alice");
  assert_false (is_in_gc "CS3110" "Bob");
  assert (check_gc_password "CS3110" "OCaml");
  assert_false (check_gc_password "CS3110" "Java");
  assert_equal (members_of_gc "CS3110") [ "Alice" ];
  assert_false (add_member_gc "CS3110" "Alice");
  assert_equal (gc_of_user "Alice") [ "CS3110" ];

  (* Add a new member to a groupchat *)
  assert_equal (gc_of_user "Bob") [];
  assert (add_member_gc "CS3110" "Bob");
  assert_same_elts (members_of_gc "CS3110") [ "Alice"; "Bob" ];
  assert_equal (gc_of_user "Bob") [ "CS3110" ];
  assert_false (create_groupchat "CS3110" "Fun" "Bob")

let run_database_tests () =
  test_add_n_diff 100;
  test_add_n_same 100;
  add_more_users ();
  test_user_exists ();
  test_chk_pwd ();
  test_user_key ();
  test_add_msg ();
  test_get_msg ();
  test_friend_requests ();
  test_groupchat ();
  print_endline "All database tests passed!"

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
    parser_type_test "Type of get_msg_1 is correct" get_msg_1
      (GetMessage "unread");
    parser_type_test "Type of login_1 is correct" login_1
      (Login "OCaml!");
    parser_type_test "Type of register_1 is correct" register_1
      (Register ("OCaml!", "123412341234"));
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
