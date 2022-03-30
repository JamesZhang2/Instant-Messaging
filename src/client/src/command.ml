exception Malformed

type parameters = string list

type command =
  | SendMsg of string * string * string
  | GetMsg of string
  | Register of string * string
  | Login of string * string
  | FriendReq of string * string * string
  | FriendReqRep of string * string * bool
  | ReadAll
  | ReadFrom of string
  | ListFriend
  | Help
  | Quit

(** [parse str] Parses a string command into command type. Raises:
    [Malformed] if the command does not match with any of the types*)
let parse str =
  let str_list =
    str |> String.split_on_char ' ' |> List.filter (fun s -> s <> "")
  in
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
  | [ "FriendReqReply"; sender; receiver; "true" ] ->
      FriendReqRep (sender, receiver, true)
  | [ "FriendReqReply"; sender; receiver; "false" ] ->
      FriendReqRep (sender, receiver, false)
  | [ "ReadAll" ] -> ReadAll
  | [ "Read"; "from"; sender ] -> ReadFrom sender
  | [ "Friends" ] -> ListFriend
  | _ -> raise Malformed
