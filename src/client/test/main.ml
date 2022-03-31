open OUnit2
open Client
open Database
open Controller
open Interface
open Network
open Packager
open Parser
open Util

(******************** Controller Tests ********************)

let controller_tests = []

(******************** Interface Tests ********************)

let interface_tests = []

(******************** Network Tests ********************)

let network_tests = []

(******************** Client Database Tests ********************)

let put = Printf.sprintf

(** [cmb_rc lst] combines the response list to one response. *)
let cmb_rc (lst : (bool * string) list) =
  let rec inner acc = function
    | [] -> acc
    | h :: t -> (
        match acc with
        | f1, msg1 -> (
            match h with
            | f2, msg2 ->
                inner
                  ( f1 && f2,
                    if msg1 <> "" then msg1 ^ "\n" ^ msg2 else msg2 )
                  t))
  in
  inner (true, "") lst

let request_test_setup () =
  match
    let a = init_dbs () in
    let b = create_dbs "alice" "666" in
    let c =
      add_request "alice"
        (Msg.make_msg "alice" "bob" "2000-01-01 08:00:00" FriendReq
           "hello to bob")
        (Some "123") None
    in
    let d =
      add_request "alice"
        (Msg.make_msg "alice" "charlie" "2000-01-02 08:00:00" FriendReq
           "hello to charlie")
        (Some "123") (Some false)
    in
    let e =
      add_request "alice"
        (Msg.make_msg "alice" "david" "1999-12-31 08:00:00" FriendReq
           "hello to david")
        None (Some true)
    in
    let f =
      add_request "alice"
        (Msg.make_msg "eve" "alice" "1999-12-31 15:00:00" FriendReq
           "hello from eve")
        (Some "123") None
    in
    let g = update_request "alice" "bob" true in
    let h = update_request "alice" "david" false in
    let i = update_request "alice" "eve" false in
    cmb_rc [ a; b; c; d; e; f; g; h; i ]
  with
  | f, m ->
      if f then print_endline ("TRUE: " ^ m)
      else print_endline ("FALSE: " ^ m);
      f

let in_req_test name =
  if is_in_req "alice" name then (
    print_endline (put "%s is in Alice's list" name);
    true)
  else (
    print_endline (put "%s is NOT in Alice's list" name);
    false)

let all_reqs_test () =
  let lst = get_all_reqs "alice" in
  List.length lst = 4

let all_frd_test () =
  let lst = get_all_frds "alice" in
  List.length lst = 2

let request_tests () =
  let a = request_test_setup () in
  let b = in_req_test "bob" in
  let c = not (in_req_test "zooo") in
  let d = all_frd_test () && all_reqs_test () in
  a && b && c && d

let add_msgs_test () =
  let a =
    add_msg "alice"
      (Msg.make_msg "alice" "bob" "2000-01-01 03:00:00" Message
         "hello at 3")
  in
  let b =
    add_msg "alice"
      (Msg.make_msg "alice" "eve" "2000-01-01 05:00:00" Message
         "hello at 5")
  in
  let c =
    add_msg "alice"
      (Msg.make_msg "bob" "alice" "2000-01-01 06:00:00" Message
         "hello at 6")
  in
  let d =
    add_msg "alice"
      (Msg.make_msg "alice" "bob" "2000-01-01 08:00:00" Message
         "hello at 8")
  in
  let e =
    add_msg "alice"
      (Msg.make_msg "eve" "alice" "2000-01-01 09:00:00" Message
         "hello at 9")
  in
  let f =
    add_msg "alice"
      (Msg.make_msg "alice" "bob" "2000-01-02 11:00:00" Message
         "hello at 11")
  in
  let g =
    add_msg "alice"
      (Msg.make_msg "bob" "alice" "2000-01-02 08:00:00" Message
         "hello on the second day")
  in
  match cmb_rc [ a; b; c; d; e; f; g ] with
  | f, m ->
      if f then (
        print_endline ("TRUE: " ^ m);
        f)
      else (
        print_endline ("FALSE: " ^ m);
        f)

let all_msgs_test () =
  let lst = get_all_msgs_since "alice" "2000-01-01 08:00:00" in
  List.length lst = 3

let msgs_by_frd_test () =
  let lst = get_msgs_by_frd_since "alice" "eve" "2000-01-01 00:00:00" in
  List.length lst = 2

let msg_tests () =
  let a = add_msgs_test () in
  let b = all_msgs_test () in
  let c = msgs_by_frd_test () in
  a && b && c

let db_test () =
  let a = request_tests () in
  let b = msg_tests () in
  if a && b then print_endline "DATABASE TEST ON CLIENT SIDE FINISHED. "
  else print_endline "DATABSAE TEST ON CLIENT SIDE FAILED. "

(******************** Packager Tests ********************)

(** [remove_time j] removes the value of the time field from the json
    string [j]. *)
let remove_time j =
  Str.global_replace (Time.time_regex |> Str.regexp) "<removed>" j

(** [equal_ignore_time s1 s2] is true if s1 and s2 are equal with the
    value of the time field removed. *)
let equal_ignore_time s1 s2 = remove_time s1 = remove_time s2

let send_msg_1 =
  "{\"type\": \"SendMessage\",\"sender\": \"Alice\",\"time\": \
   \"2000-01-01 08:00:00\",\"receiver\": \"Bob\",\"message\": \"Hello \
   world!\"}"

let get_msg_1 =
  "{\"type\": \"GetMessage\",\"sender\": \"Charlie\",\"time\": \
   \"2000-01-01 08:00:00\"}"

let register_1 =
  "{\"type\": \"Register\",\"sender\": \"Daniel\",\"time\": \
   \"2000-01-01 08:00:00\",\"password\": \"tail recursion\",\"key\": \
   \"key\"}"

let login_1 =
  "{\"type\": \"Login\",\"sender\": \"Elizabeth\",\"time\": \
   \"2000-01-01 08:00:00\",\"password\": \"stack overflow\"}"

let friend_req_1 =
  "{\"type\": \"FriendReq\",\"sender\": \"Frank\",\"time\": \
   \"2000-01-01 08:00:00\",\"receiver\": \"George\",\"message\": \"I'm \
   Frank\"}"

let friend_req_reply_1 =
  "{\"type\": \"FriendReqReply\",\"sender\": \"Harry\",\"time\": \
   \"2000-01-01 08:00:00\",\"receiver\": \"Ian\",\"accepted\": true}"

let friend_req_reply_2 =
  "{\"type\": \"FriendReqReply\",\"sender\": \"Jason\",\"time\": \
   \"2000-01-01 08:00:00\",\"receiver\": \"Kate\",\"accepted\": false}"

let packager_tests =
  [
    ( "packing SendMessage" >:: fun _ ->
      assert_equal send_msg_1
        (pack_send_msg "Alice" "Bob" "Hello world!")
        ~cmp:equal_ignore_time ~printer:remove_time );
    ( "packing GetMessage" >:: fun _ ->
      assert_equal get_msg_1
        (pack_get_msg "Charlie" "unread")
        ~cmp:equal_ignore_time ~printer:remove_time );
    ( "packing Register" >:: fun _ ->
      assert_equal register_1
        (pack_register "Daniel" "tail recursion" "key")
        (* key ungenerated*)
        ~cmp:equal_ignore_time ~printer:remove_time );
    ( "packing Login" >:: fun _ ->
      assert_equal login_1
        (pack_login "Elizabeth" "stack overflow")
        ~cmp:equal_ignore_time ~printer:remove_time );
    ( "packing FriendReq" >:: fun _ ->
      assert_equal friend_req_1
        (pack_friend_req "Frank" "George" "I'm Frank")
        ~cmp:equal_ignore_time ~printer:remove_time );
    ( "packing FriendReqReply accepted" >:: fun _ ->
      assert_equal friend_req_reply_1
        (pack_friend_req_reply "Harry" "Ian" true)
        ~cmp:equal_ignore_time ~printer:remove_time );
    ( "packing FriendReqReply rejected" >:: fun _ ->
      assert_equal friend_req_reply_2
        (pack_friend_req_reply "Jason" "Kate" false)
        ~cmp:equal_ignore_time ~printer:remove_time );
  ]

(******************** Parser Tests ********************)

(** [test name func checker expected input] checks whether checking the
    property through [checker] after running the [input] through
    function [func] yields the expected result [expected]. *)
let test name func checker expected input =
  [
    (name >:: fun _ -> assert_equal expected (input |> func |> checker));
  ]

let error_input_1 =
  "{\n\
   \t\"type\" : \"Error\", \n\
   \t\"time\" : \"time\", \n\
   \t\"message\" : \"error message\"\n\
   }"

let post_input =
  "{\n\
   \t\"type\" : \"Post\", \n\
   \t\"time\" : \"time\", \n\
   \t\"message\" : \"Post Message\"\n\
   }"

let get_input_1 =
  "{\n\
   \t\"type\" : \"GetMsg\", \n\
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

let get_input_2 =
  "{\n\
   \t\"type\" : \"GetMsg\", \n\
   \t\"time\" : \"time\", \n\
   \t\"message\" : [\n\
   {\n\
   \t\"sender\" : \"sender2\", \n\
   \t\"receiver\" : \"receiver2\", \n\
   \t\"time\" : \"time2\", \n\
   \t\"msg_type\" : \"FriendReq\", \n\
   \t\"message\" : \"message2\"\n\
   }\n\
   ]\n\
   }"

(** [get_test name expected input property] *)
let get_test name expected input property =
  let resp = parse input in
  match get_type resp with
  | ErrorResponse msg
  | PostMethResponse msg ->
      [ (name >:: fun _ -> assert_equal expected msg) ]
  | GetMsgResponse body ->
      List.map
        (fun x -> name >:: fun _ -> assert_equal expected (property x))
        body

let get_test_type name expected input property =
  let resp = parse input in
  match get_type resp with
  | GetMsgResponse body ->
      List.map
        (fun x -> name >:: fun _ -> assert_equal expected (property x))
        body
  | _ -> assert false

let parser_tests =
  List.flatten
    [
      test "error test" parse get_type (ErrorResponse "error message")
        error_input_1;
      test "post test" parse get_type (PostMethResponse "Post Message")
        post_input;
      get_test "get test 1" "sender" get_input_1 Msg.sender;
      get_test "get test 1 message" "message" get_input_1 Msg.content;
      get_test "get test 1 time" "time" get_input_1 Msg.time;
      get_test_type "get test 1 type" Msg.Message get_input_1
        Msg.msg_type;
      get_test_type "get test 2 type" Msg.FriendReq get_input_2
        Msg.msg_type;
    ]

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
