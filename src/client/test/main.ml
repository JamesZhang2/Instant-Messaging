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

(**[test name func checker expected input] checks whether checking the
   property through [checker] after running the [input] through function
   [func] yields the expected result [expected]*)
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
   \t\"type\" : \"Get\", \n\
   \t\"time\" : \"time\", \n\
   \t\"message\" : [\n\
   {\n\
   \t\"sender\" : \"sender\", \n\
   \t\"receiver\" : \"receiver\", \n\
   \t\"time\" : \"time\", \n\
   \t\"message\" : \"message\"\n\
   }\n\
   ]\n\
   }"

(** [get_test name expected input property]*)
let get_test name expected input property =
  let resp = parse input in
  match get_type resp with
  | ErrorResponse msg
  | PostMethResponse msg ->
      [ (name >:: fun _ -> assert_equal expected msg) ]
  | GetMethResponse body ->
      List.map
        (fun x -> name >:: fun _ -> assert_equal expected (property x))
        body

let parser_tests =
  "parser_tests"
  >::: List.flatten
         [
           test "error test" parse get_type
             (ErrorResponse "error message") error_input_1;
           test "error test time" parse get_time "time" error_input_1;
           test "post test" parse get_type
             (PostMethResponse "Post Message") post_input;
           get_test "get test 1" "sender" get_input_1 msg_sender;
           get_test "get test 1 message" "message" get_input_1 msg_body;
           get_test "get test 1 time" "time" get_input_1 msg_time;
         ]

let suite =
  "test suite for Server"
  >::: List.flatten
         [
           controller_tests;
           interface_tests;
           network_tests;
           packager_tests;
           [ parser_tests ];
         ]

let _ = run_test_tt_main suite
