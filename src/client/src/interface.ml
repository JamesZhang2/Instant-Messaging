open Controller
open Util

(** begin new line if 1, if 0 then it's a prompt*)
let str_format prompt str =
  if prompt = 0 then "> " ^ str ^ "\n" else "> " ^ str ^ "\n> "

(**[begin_print] prompts the user for a new command.*)
let begin_print =
  "What would you like to do : " |> str_format 0
  |> ANSITerminal.print_string [ ANSITerminal.cyan ]

(** [illegal_command str] prints the string [str] corresponding to an
    illegal input*)
let illegal_command str =
  str |> str_format 1
  |> ANSITerminal.print_string [ ANSITerminal.magenta ]

(**[print_message msg] prints one message represented by Controller type
   [msg]*)
let print_message msg =
  let sender = "from: " ^ Msg.sender msg in
  let time = "time: " ^ Msg.time msg in
  let message = "\n" ^ Msg.content msg in
  sender |> str_format 0 |> print_endline;
  time |> str_format 0 |> print_endline;
  message |> str_format 0 |> print_endline

(** Prints all strings in [lst]*)
let printlist lst =
  let func x =
    print_endline x;
    x
  in
  List.map func lst

(** [print_messages msg_list] prints the list of messages [msg_list]*)
let rec print_messages msg_list =
  match msg_list with
  | [] -> ()
  | h :: t ->
      print_message h;
      print_messages t

(** [bool_print (check, msg)] check if the first element is true then
    print the message, if false, assume an error and print an error
    message*)
let bool_print (check, msg) =
  if check then msg |> str_format 1 |> print_string
  else
    "Request failed" |> str_format 1
    |> ANSITerminal.print_string [ ANSITerminal.magenta ]

let help_print () =
  "[SendMsg sender receiver message] : sends a message from sender to \
   receiver" |> str_format 0 |> print_string;
  "[GetMsg receiver] : gets messages to receiver" |> str_format 0
  |> print_string;
  "[Register username password] : registers a user" |> str_format 0
  |> print_string;
  "[Login username password] : logs a user in" |> str_format 0
  |> print_string;
  "[FriendReq sender receiver message] : sends a friend request from \
   sender to receiver" |> str_format 0 |> print_string;
  "[FriendReqReply sender receiver message] : replies a message from \
   receiver to sender" |> str_format 1 |> print_string;
  "[ReadAll] : Reads all recent messages" |> str_format 1
  |> print_string;
  "[Read from <friend>] : reads all recent messages from <friend> "
  |> str_format 1 |> print_string;
  "[Friends] : Shows the list of friends of current logged in user "
  |> str_format 1 |> print_string

let rec main () =
  begin_print;
  let read = read_line () in
  match Command.parse read with
  | exception Command.Malformed ->
      illegal_command "Command Illegal: ";
      main ()
  | Help ->
      help_print ();
      main ()
  | SendMsg (sender, receiver, msg) ->
      let resp = Controller.send_msg receiver msg in
      bool_print resp;
      main ()
  | GetMsg sender ->
      let check, msg = Controller.update_msg () in
      if check then print_messages msg else bool_print (false, "");
      main ()
  | Register (username, password) ->
      let resp = Controller.register username password in
      bool_print resp;
      main ()
  | Login (username, password) ->
      let check, msg = Controller.login username password in
      if check then print_messages msg else bool_print (false, "");
      main ()
  | FriendReq (sender, receiver, msg) ->
      let resp = Controller.friend_req receiver msg in
      bool_print resp;
      main ()
  | FriendReqRep (sender, receiver, accepted) ->
      let resp = Controller.friend_req_reply receiver accepted in
      bool_print resp;
      main ()
  | ReadAll ->
      let check, messages = Controller.read_msg () in
      if check then print_messages messages
      else bool_print (false, "Message History Fetch Unsuccessful");
      main ()
  | ReadFrom friend ->
      let check, messages = Controller.read_msg_from friend in
      if check then print_messages messages
      else bool_print (false, "Friend History fetch unsuccessful")
  | ListFriend ->
      let check, friends = Controller.lst_of_friends () in
      if check then
        let _ = printlist friends in
        ()
      else bool_print (false, "Unable to fetch list of friends");
      main ()
  | Quit -> exit 0

let run () =
  help_print ();
  main ()
