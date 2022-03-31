exception Malformed

type parameters = string list

type command =
  | SendMsg of string * string * string
  | GetMsg of string
  | Register of string * string
  | Login of string * string
  | Logout
  | FriendReq of string * string * string
  | FriendReqRep of string * string * bool
  | ReadMsg
  | ReadMsgFrom of string
  | ReadFR
  | ListFriend
  | Help
  | Quit

(** [parse str] Parses a string command into command type. Raises:
    [Malformed] if the command does not match with any of the types*)
let parse ~(maybe_user : string option) str =
  let str_list =
    str |> String.split_on_char ' ' |> List.filter (fun s -> s <> "")
  in
  match maybe_user with
  | None -> (
      match str_list with
      | [ "Quit" ]
      | [ "quit" ] ->
          Quit
      | [ "Help" ]
      | [ "help" ] ->
          Help
      | [ "Register"; username; password ] ->
          Register (username, password)
      | [ "Login"; username; password ] -> Login (username, password)
      | _ -> raise Malformed)
  | Some user -> (
      match str_list with
      | [ "Quit" ]
      | [ "quit" ] ->
          Quit
      | [ "Help" ]
      | [ "help" ] ->
          Help
      | "SendMsg" :: receiver :: t ->
          SendMsg (user, receiver, String.concat " " t)
      | [ "GetMsg" ] -> GetMsg user
      | [ "Register"; username; password ] ->
          Register (username, password)
      | [ "Login"; username; password ] -> Login (username, password)
      | [ "FriendReq"; receiver; msg ] -> FriendReq (user, receiver, msg)
      | [ "Approve"; receiver ] -> FriendReqRep (user, receiver, true)
      | [ "Reject"; receiver ] -> FriendReqRep (user, receiver, false)
      | [ "ReadMsg" ] -> ReadMsg
      | [ "Read"; "from"; sender ] -> ReadMsgFrom sender
      | [ "FriendRequests" ] -> ReadFR
      | [ "Friends" ] -> ListFriend
      | [ "Logout" ] -> Logout
      | _ -> raise Malformed)
