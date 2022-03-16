open Controller

module Command = struct
  exception Malformed

  type parameters = string list

  type command =
    | SendMsg of parameters
    | GetMsg of parameters
    | Register of parameters
    | Login of parameters
    | FriendReq of parameters
    | FriendReqRep of parameters
    | Quit

  (** [parse str] Parses a string command into command type. Raises:
      [Malformed] if the command does not match with any of the types*)
  let parse str =
    let str_list = String.split_on_char ' ' str in
    match str_list with
    | [] -> raise Malformed
    | [ h ] -> raise Malformed
    | h :: t -> (
        match h with
        | "SendMsg" -> SendMsg t
        | "GetMsg" -> GetMsg t
        | "Register" -> Register t
        | "Login" -> Login t
        | "FriendReq" -> FriendReq t
        | "FriendReqReply" -> FriendReqRep t
        | "quit" -> Quit
        | _ -> raise Malformed)
end

(**[send_msg lst] takes the parameters in [lst] and sends the message to
   the server and convert result to a print statement*)
let send_msg lst =
  let first, fstlist = (List.hd lst, List.tl lst) in
  let second, sndlist = (List.hd fstlist, List.tl fstlist) in
  let third = List.hd sndlist in
  Controller.send_msg first second third

(** [get_msg str] sends a get request for all messages to be received by
    receiver [str]*)
let get_msg str = Controller.get_msg str

(** [register lst] takes the parameters in [lst] and registers a user
    using this information*)
let register lst =
  let first, fstlist = (List.hd lst, List.tl lst) in
  let second = List.hd fstlist in
  Controller.register first second

(** [login lst] takes the parameters in [lst] and logins a user using
    this information*)
let login lst =
  let first, fstlist = (List.hd lst, List.tl lst) in
  let second = List.hd fstlist in
  Controller.login first second

(**[friend_req lst] takes the parameters in [lst] and sends the
   friend_request to the server*)
let friend_req lst =
  let first, fstlist = (List.hd lst, List.tl lst) in
  let second, sndlist = (List.hd fstlist, List.tl fstlist) in
  let third = List.hd sndlist in
  Controller.friend_req first second third

(**[friend_req lst] takes the parameters in [lst] and sends the
   friend_request to the server and converts to a print statement*)
let friend_rep lst =
  let first, fstlist = (List.hd lst, List.tl lst) in
  let second, sndlist = (List.hd fstlist, List.tl fstlist) in
  let third = if List.hd sndlist = "true" then true else false in
  Controller.friend_req_reply first second third

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

let bool_print (check, msg) =
  if check then msg |> str_format 1 |> print_string
  else
    "Request failed" |> str_format 1
    |> ANSITerminal.print_string [ ANSITerminal.magenta ]

let rec main () =
  begin_print;
  let read = read_line () in
  match Command.parse read with
  | exception Command.Malformed ->
      illegal_command "Command Illegal: ";
      main ()
  | SendMsg parameters ->
      let resp = send_msg parameters in
      bool_print resp;
      main ()
  | GetMsg parameters ->
      let check, msg = get_msg (List.hd parameters) in
      if check then print_messages msg else bool_print (false, "");
      main ()
  | Register parameters ->
      let resp = register parameters in
      bool_print resp;
      main ()
  | Login parameters ->
      let resp = register parameters in
      bool_print resp;
      main ()
  | FriendReq parameters ->
      let resp = register parameters in
      bool_print resp;
      main ()
  | FriendReqRep parameters ->
      let resp = friend_req parameters in
      bool_print resp;
      main ()
  | Quit -> exit 0

let run = main ()
