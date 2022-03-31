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
  let msg_type =
    match Msg.msg_type msg with
    | Message -> "Message "
    | FriendReq -> "Friend Request "
    | FriendReqRep (bo, key) ->
        "Friend Request Response " ^ string_of_bool bo
  in
  let sender = "from: " ^ Msg.sender msg in
  let time = "time: " ^ Msg.time msg in
  let message = "\n> " ^ Msg.content msg in
  msg_type |> str_format 0 |> print_string;
  sender |> str_format 0 |> print_string;
  time |> str_format 0 |> print_string;
  message ^ "\n" |> str_format 1 |> print_string

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
    msg |> str_format 1
    |> ANSITerminal.print_string [ ANSITerminal.magenta ]

let print_help () =
  match current_user () with
  | None ->
      "[Register username password] : registers a new user"
      |> str_format 0 |> print_string;
      "[Login username password] : logs in an existing user"
      |> str_format 0 |> print_string;
      "[Help] : displays instructions" |> str_format 0 |> print_string;
      "[Quit] : quits the IM program" |> str_format 1 |> print_string
  | Some user ->
      "You are currently logged in as " ^ user
      |> str_format 0 |> print_string;
      "[SendMsg receiver message] : sends a message to a friend"
      |> str_format 0 |> print_string;
      "[GetMsg] : gets all your new messages" |> str_format 0
      |> print_string;
      "[Register username password] : registers a new user"
      |> str_format 0 |> print_string;
      "[Login username password] : switches to another user"
      |> str_format 0 |> print_string;
      "[FriendReq receiver message] : sends a friend request to \
       another user" |> str_format 0 |> print_string;
      "[Approve user] : approves a friend request from user"
      |> str_format 0 |> print_string;
      "[Reject user] : rejects a friend request from user"
      |> str_format 0 |> print_string;
      "[ReadMsg] : Reads all recent messages" |> str_format 0
      |> print_string;
      "[Read from <friend>] : reads all recent messages from <friend> "
      |> str_format 0 |> print_string;
      "[FriendRequests] : reads all recent friend reequests"
      |> str_format 0 |> print_string;
      "[Friends] : Shows the list of friends of current logged in user "
      |> str_format 0 |> print_string;
      "[Help] : displays instructions" |> str_format 0 |> print_string;
      "[Quit] : quits the IM program" |> str_format 1 |> print_string

let rec main () =
  begin_print;
  let maybe_user = current_user () in
  let read = read_line () in
  match Command.parse maybe_user read with
  | exception Command.Malformed ->
      illegal_command "Command Illegal: ";
      main ()
  | Help ->
      print_help ();
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
      print_help ();
      main ()
  | Login (username, password) ->
      (let check, msg = Controller.login username password in
       if check then print_messages msg
       else
         let message =
           match msg with
           | [] -> "error"
           | h :: t -> Msg.content h
         in
         bool_print (false, message));
      main ()
  | Logout ->
      let msg = Controller.logout () in
      msg |> str_format 1 |> print_string;
      main ()
  | FriendReq (sender, receiver, msg) ->
      let resp = Controller.friend_req receiver msg in
      bool_print resp;
      main ()
  | FriendReqRep (sender, receiver, accepted) ->
      let resp = Controller.friend_req_reply receiver accepted in
      bool_print resp;
      main ()
  | ReadMsg ->
      let check, messages = Controller.read_msg () in
      if check then print_messages messages
      else bool_print (false, "Message History Fetch Unsuccessful");
      main ()
  | ReadMsgFrom friend ->
      let check, messages = Controller.read_msg_from friend in
      if check then print_messages messages
      else bool_print (false, "Friend History fetch unsuccessful")
  | ReadFR ->
      let check, messages = Controller.read_FR () in
      if check then print_messages messages
      else bool_print (false, "Friend Request fetch unsuccessful");
      main ()
  | ListFriend ->
      let check, friends = Controller.lst_of_friends () in
      if check then
        let _ = printlist friends in
        ()
      else bool_print (false, "Unable to fetch list of friends");
      main ()
  | Quit -> exit 0

let run () =
  print_help ();
  main ()
