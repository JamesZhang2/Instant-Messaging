(** A module for parsing commands from the user interface. *)

exception Malformed
(** The exception raised when a command is malformed. *)

(** The variant type for commands. *)
type command =
  | SendMsg of string * string  (** receiver, message *)
  | GetNewMsg
  | GetAllMsg
  | ReadMsgFrom of string  (** sender *)
  | Register of string * string  (** username, password *)
  | Login of string * string  (** username, password *)
  | Logout
  | FriendReq of string * string  (** receiver, message *)
  | FriendReqRep of string * bool  (** receiver, accepted *)
  | ReadFR
  | ListFriends
  | JoinGC of string * string  (** gcid, password *)
  | ReadGC of string  (** gcid *)
  | SendGC of string * string  (** gcid, message *)
  | ListGC
  | GCMembers of string  (** gcid *)
  | Help
  | Quit

val parse : bool -> string -> command
(** [parse logged_in str] parses a string command into command type,
    based on whether a user is logged in or not. Raises: [Malformed] if
    the command does not match with any of the types*)
