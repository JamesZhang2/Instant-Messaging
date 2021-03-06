open Controller
open Util

(** begin new line if 1, if 0 then it's a prompt. *)
let str_format prompt str =
  if prompt = 0 then "> " ^ str ^ "\n" else "> " ^ str ^ "\n> "

(** [print_color str color] prints [str] with [color]. *)
let print_color str color =
  str |> str_format 0 |> ANSITerminal.print_string [ color ]

(** [begin_print ()] prompts the user for a new command. *)
let begin_print () =
  print_color "What would you like to do : " ANSITerminal.cyan

(** [print_prompt str] prints [str] as a prompt. *)
let print_prompt str = str_format 0 str |> print_string

(** [bool_print (check, msg)] check if the first element is true then
    print the message, if false, assume an error and print an error
    message*)
let bool_print (check, msg) =
  if check then print_color msg ANSITerminal.green
  else print_color msg ANSITerminal.magenta;
  print_string "> "

(**[print_message msg] prints one message represented by Controller type
   [msg]*)
let print_message msg =
  let msg_type =
    match Msg.msg_type msg with
    | Message -> "Message"
    | FriendReq -> "Friend Request"
    | FriendReqRep (accepted, key) ->
        "Friend Request " ^ if accepted then "Accepted" else "Rejected"
    | GCMessage -> "Groupchat Message"
    | GCRequest -> "Groupchat Request"
    | GCReqRep accepted ->
        "Groupchat Request "
        ^ if accepted then "Accepted" else "Rejected"
  in
  let sender = "from " ^ Msg.sender msg ^ " to " ^ Msg.receiver msg in
  let time = "time: " ^ Msg.time msg in
  let message = "\n> " ^ Msg.content msg in
  if Msg.sender msg <> "" then (
    print_prompt msg_type;
    print_prompt sender;
    print_prompt time);
  message ^ "\n" |> str_format 1 |> print_string

(** [print_messages msg_list] prints the list of messages [msg_list]*)
let print_messages msg_list =
  match msg_list with
  | [] -> "Already up to date." |> str_format 1 |> print_string
  | lst -> List.map print_message lst |> ignore

(** Prints all strings in [lst]*)
let print_list lst =
  if lst = [] then print_endline "None"
  else List.map print_endline lst |> ignore

(** [help_logged_out] is the list of help messages when the user is
    logged out. *)
let help_logged_out =
  [
    "[Register <username> <password>] : registers a new user";
    "[Login <username> <password>] : logs in an existing user";
    "[Help] : displays instructions";
    "[Quit] : quits the IM program";
  ]

(** [help_logged_in] is the list of help messages when the user is
    logged in. *)
let help_logged_in =
  [
    "Note: You need to run [GetNewMsg] to keep everything up to date.";
    "-------------------- Messages --------------------";
    "[SendMsg <receiver> <message>] : sends a message to a friend";
    "[GetNewMsg] : gets all your new messages and notifications from \
     the server";
    "[ReadAllMsg] : reads all your recent local messages";
    "[Read from <friend>] : reads all recent local messages from a \
     friend";
    "-------------------- Friends ---------------------";
    "[FriendReq <receiver> <message>] : sends a friend request to \
     another user";
    "[Accept <user>] : accepts a friend request from user";
    "[Reject <user>] : rejects a friend request from user";
    "[FriendRequests] : reads all recent friend reequests";
    "[Friends] : shows a list of all your friends";
    "-------------------- Groupchats ------------------";
    "[CreateGC <groupchat> <password>] : creates a new groupchat";
    "[JoinGC <groupchat> <password>] : tries to join a groupchat";
    "[ReadGC <groupchat>] : reads all recent messages from a groupchat";
    "[SendGC <groupchat> <message>] : sends a message to a groupchat";
    "[Groupchats] : shows a list of all your groupchats";
    "[Members <groupchat>] : shows a list of all users in a groupchat";
    "-------------------- Account ---------------------";
    "[Register <username> <password>] : registers a new user and \
     switches to that user";
    "[Login <username> <password>] : switches to another user";
    "[Logout] : logs out";
    "[Help] : displays instructions";
    "[Quit] : quits the IM program";
  ]

let print_help () =
  begin_print ();
  match current_user () with
  | None ->
      print_color "You are currently not logged in" ANSITerminal.yellow;
      List.map print_prompt help_logged_out |> ignore;
      print_string "> "
  | Some user ->
      print_color
        ("You are currently logged in as " ^ user)
        ANSITerminal.green;
      List.map print_prompt help_logged_in |> ignore;
      print_string "> "

let rec main () =
  let logged_in =
    match current_user () with
    | Some _ -> true
    | None -> false
  in
  let cmd = read_line () in
  (match Command.parse logged_in cmd with
  | exception Command.Malformed -> bool_print (false, "Command Illegal.")
  | Help -> print_help ()
  | SendMsg (receiver, msg) ->
      let resp = Controller.send_msg receiver msg in
      bool_print resp
  | GetNewMsg ->
      let check, messages = Controller.update_msg () in
      if check then print_messages messages else bool_print (false, "")
  | ReadAllMsg ->
      let check, messages = Controller.read_msg () in
      if check then print_messages messages
      else bool_print (false, "Message History Fetch Unsuccessful")
  | ReadMsgFrom friend ->
      let check, messages = Controller.read_msg_from friend in
      if check then print_messages messages
      else bool_print (false, "Friend History fetch unsuccessful")
  | Register (username, password) ->
      let resp = Controller.register username password in
      bool_print resp;
      print_help ()
  | Login (username, password) ->
      let check, msg = Controller.login username password in
      if check then print_messages msg
      else
        let message =
          match msg with
          | [] -> "error"
          | h :: t -> Msg.content h
        in
        bool_print (false, message)
  | Logout ->
      let msg = Controller.logout () in
      msg |> str_format 1 |> print_string
  | FriendReq (receiver, message) ->
      let resp = Controller.friend_req receiver message in
      bool_print resp
  | FriendReqRep (receiver, accepted) ->
      let resp = Controller.friend_req_reply receiver accepted in
      bool_print resp
  | ReadFR ->
      let check, messages = Controller.read_fr () in
      if check then print_messages messages
      else bool_print (false, "Friend Request fetch unsuccessful")
  | ListFriends ->
      let check, friends = Controller.lst_of_friends () in
      if check then (
        print_list friends;
        print_string "> ")
      else bool_print (false, "Unable to fetch list of friends")
  | CreateGC (gcid, password) ->
      let resp = Controller.create_groupchat gcid password in
      bool_print resp
  | JoinGC (gcid, password) ->
      let resp = Controller.join_gc gcid password in
      bool_print resp
  | ReadGC gcid ->
      let check, messages = Controller.read_gc_msg gcid in
      if check then print_messages messages
      else
        let message =
          match messages with
          | [] -> "error"
          | h :: t -> Msg.content h
        in
        bool_print (false, message)
  | SendGC (gcid, message) ->
      let resp = Controller.send_gc_msg gcid message in
      bool_print resp
  | ListGC ->
      let check, groupchats = Controller.lst_of_gc () in
      if check then (
        print_list groupchats;
        print_string "> ")
      else bool_print (false, "Unable to fetch list of groupchats")
  | GCMembers gcid ->
      let check, members = Controller.members_of_gc gcid true in
      if check then (
        print_list members;
        print_string "> ")
      else bool_print (false, "Unable to fetch list of members")
  | Quit -> exit 0);
  main ()

let run () =
  print_help ();
  main ()
