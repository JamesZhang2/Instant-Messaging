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

val parse : string -> command
(** [parse str] Parses a string command into command type. Raises:
    [Malformed] if the command does not match with any of the types*)
