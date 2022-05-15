exception Malformed

type t =
  | SendMsg of string * string
  | GetNewMsg
  | GetAllMsg
  | ReadMsgFrom of string
  | Register of string * string
  | Login of string * string
  | Logout
  | FriendReq of string * string
  | FriendReqRep of string * bool
  | ReadFR
  | ListFriends
  | JoinGC of string * string
  | ReadGC of string
  | SendGC of string * string
  | ListGC
  | GCMembers of string
  | Help
  | Quit

(** [parse str] Parses a string command into command type. Raises:
    [Malformed] if the command does not match with any of the types*)
let parse logged_in str =
  let str_list =
    str |> String.split_on_char ' ' |> List.filter (fun s -> s <> "")
  in
  if not logged_in then
    match str_list with
    | [ "Quit" ]
    | [ "quit" ] ->
        Quit
    | [ "Help" ]
    | [ "help" ] ->
        Help
    | [ "Register"; username; password ] -> Register (username, password)
    | [ "Login"; username; password ] -> Login (username, password)
    | _ -> raise Malformed
  else
    match str_list with
    | [ "Quit" ]
    | [ "quit" ] ->
        Quit
    | [ "Help" ]
    | [ "help" ] ->
        Help
    | "SendMsg" :: receiver :: t ->
        SendMsg (receiver, String.concat " " t)
    | [ "GetNewMsg" ] -> GetNewMsg
    | [ "GetAllMsg" ] -> GetAllMsg
    | [ "Read"; "from"; sender ] -> ReadMsgFrom sender
    | "FriendReq" :: receiver :: t ->
        FriendReq (receiver, String.concat " " t)
    | [ "Accept"; receiver ] -> FriendReqRep (receiver, true)
    | [ "Reject"; receiver ] -> FriendReqRep (receiver, false)
    | [ "FriendRequests" ] -> ReadFR
    | [ "Friends" ] -> ListFriends
    | [ "JoinGC"; gcid; password ] -> JoinGC (gcid, password)
    | [ "ReadGC"; gcid ] -> ReadGC gcid
    | "SendGC" :: gcid :: t -> SendGC (gcid, String.concat " " t)
    | [ "Groupchats" ] -> ListGC
    | [ "Members"; gcid ] -> GCMembers gcid
    | [ "Register"; username; password ] -> Register (username, password)
    | [ "Login"; username; password ] -> Login (username, password)
    | [ "Logout" ] -> Logout
    | _ -> raise Malformed
