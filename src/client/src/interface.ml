open Controller

module Command = struct
  exception Malformed

  type parameters = string list

  type command =
    | SendMsg of string * string * string
    | GetMsg of string
    | Register of string * string
    | Login of string * string
    | FriendReq of string * string * string
    | FriendReqRep of string * string * string
    | Help
    | Quit

  (** [parse str] Parses a string command into command type. Raises:
      [Malformed] if the command does not match with any of the types*)
  let parse str =
    let str_list = String.split_on_char ' ' str in
    match str_list with
    | [] -> raise Malformed
    | [ "quit" ] -> Quit
    | [ "help" ] -> Help
    | [ "SendMsg"; sender; receiver; msg ] ->
        SendMsg (sender, receiver, msg)
    | [ "GetMsg"; sender ] -> GetMsg sender
    | [ "Register"; username; password ] -> Register (username, password)
    | [ "Login"; username; password ] -> Login (username, password)
    | [ "FriendReq"; sender; receiver; msg ] ->
        FriendReq (sender, receiver, msg)
    | [ "FriendReqReply"; sender; receiver; accepted ] ->
        FriendReqRep (sender, receiver, accepted)
    | _ -> raise Malformed
end

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
  let sender = "from: " ^ msg.sender in
  let time = "time: " ^ msg.time in
  let message = "\n" ^ msg.body in
  sender |> str_format 0 |> print_endline;
  time |> str_format 0 |> print_endline;
  message |> str_format 0 |> print_endline

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
   receiver" |> str_format 1 |> print_string;
  "[GetMsg receiver] : gets messages to receiver" |> str_format 1
  |> print_string;
  "[Login username password] : Logs a message from sender to receiver"
  |> str_format 1 |> print_string;
  "[Register username password] : registers a user" |> str_format 1
  |> print_string;
  "[Login username password] : logs a user in" |> str_format 1
  |> print_string;
  "[FriendReq sender receiver message] : sends a friend request from \
   sender to receiver" |> str_format 1 |> print_string;
  "[FriendReqReply sender receiver message] : replies a message from \
   receiver to sender" |> str_format 1 |> print_string

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
      let resp = Controller.send_msg sender receiver msg in
      bool_print resp;
      main ()
  | GetMsg sender ->
      let check, msg = Controller.get_msg sender in
      if check then print_messages msg else bool_print (false, "");
      main ()
  | Register (username, password) ->
      let resp = Controller.register username password in
      bool_print resp;
      main ()
  | Login (username, password) ->
      let resp = Controller.login username password in
      bool_print resp;
      main ()
  | FriendReq (sender, receiver, msg) ->
      let resp = Controller.friend_req sender receiver msg in
      bool_print resp;
      main ()
  | FriendReqRep (sender, receiver, accepted) ->
      let resp =
        Controller.friend_req_reply sender receiver (accepted = "true")
      in
      bool_print resp;
      main ()
  | Quit -> exit 0

let run = main ()
