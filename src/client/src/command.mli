exception Malformed

type parameters = string list

type command =
  | SendMsg of string * string * string
  | GetNewMsg of string
  | GetAllMsg
  | ReadMsgFrom of string
  | Register of string * string
  | Login of string * string
  | Logout
  | FriendReq of string * string * string
  | FriendReqRep of string * string * bool
  | ReadFR
  | ListFriend
  | Help
  | Quit

val parse : maybe_user:string option -> string -> command
(** [parse maybe_user str] Parses a string command into command type.
    Raises: [Malformed] if the command does not match with any of the
    types*)
