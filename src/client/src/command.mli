(** A module for parsing commands from the user interface.

    RI: Usernames (including senders and receivers), passwords, and
    gcids can't have spaces, while messages can have spaces. *)

exception Malformed
(** The exception raised when a command is malformed. *)

(** The variant type for commands. *)
type t =
  | SendMsg of string * string  (** receiver, message *)
  | GetNewMsg
  | ReadAllMsg
  | ReadMsgFrom of string  (** sender *)
  | Register of string * string  (** username, password *)
  | Login of string * string  (** username, password *)
  | Logout
  | FriendReq of string * string  (** receiver, message *)
  | FriendReqRep of string * bool  (** receiver, accepted *)
  | ReadFR
  | ListFriends
  | CreateGC of string * string  (** gcid, password *)
  | JoinGC of string * string  (** gcid, password *)
  | ReadGC of string  (** gcid *)
  | SendGC of string * string  (** gcid, message *)
  | ListGC
  | GCMembers of string  (** gcid *)
  | Help
  | Quit

val parse : bool -> string -> t
(** [parse logged_in str] parses a string command into command type,
    based on whether a user is logged in or not.

    Raises: [Malformed] if the command does not match with any of the
    types. *)
