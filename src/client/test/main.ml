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

let get_msg_1 =
  "{\"type\": \"GetMessage\",\"sender\": \"Charlie\",\"time\": \
   \"8:00:00 3/1/2022\"}"

let register_1 =
  "{\"type\": \"Register\",\"sender\": \"Daniel\",\"time\": \"8:00:00 \
   3/1/2022\",\"password\": \"tail recursion\"}"

let login_1 =
  "{\"type\": \"Login\",\"sender\": \"Elizabeth\",\"time\": \"8:00:00 \
   3/1/2022\",\"password\": \"stack overflow\"}"

let friend_req_1 =
  "{\"type\": \"FriendReq\",\"sender\": \"Frank\",\"time\": \"8:00:00 \
   3/1/2022\",\"receiver\": \"George\",\"message\": \"I'm Frank\"}"

let friend_req_reply_1 =
  "{\"type\": \"FriendReqReply\",\"sender\": \"Harry\",\"time\": \
   \"8:00:00 3/1/2022\",\"receiver\": \"Ian\",\"accepted\": true}"

let friend_req_reply_2 =
  "{\"type\": \"FriendReqReply\",\"sender\": \"Jason\",\"time\": \
   \"8:00:00 3/1/2022\",\"receiver\": \"Kate\",\"accepted\": false}"

let packager_tests =
  [
    ( "packing SendMessage" >:: fun _ ->
      assert_equal send_msg_1
        (pack_send_msg "Alice" "Bob" "Hello world!")
        ~cmp:equal_ignore_time ~printer:remove_time );
    ( "packing GetMessage" >:: fun _ ->
      assert_equal get_msg_1
        (pack_get_msg "Charlie")
        ~cmp:equal_ignore_time ~printer:remove_time );
    ( "packing Register" >:: fun _ ->
      assert_equal register_1
        (pack_register "Daniel" "tail recursion")
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
