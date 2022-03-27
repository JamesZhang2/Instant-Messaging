(** This module is a database for the server. It stores all user info
    and all the messages. *)

open Util

val add_user : string -> string -> string -> bool * string
(** [add_user username pwd time] attempts to add user [username] with
    password [pwd] and registration time [time] to the user database.
    Returns [(true, feedback)] if a new user is successfully added,
    [(false, err_msg)] otherwise.*)

type chk_user =
  | UserOK
  | UnknownUser of string
  | WrongPwd of string
  | MalformedUserTime of string
      (** for logging in checking whether user and password is valid*)

val chk_pwd : string -> string -> chk_user
(** [chk_pwd username pwd] is [UserOK] if the user database contains a
    user with name [username] and password [pwd], [UnknownUser user] if
    the user is not found, and [WrongPwd pwd] if the password supplied
    does not match the password of the user in the database. *)

(* type chk_msg = | MsgOK | Error *)

val add_msg : Msg.t -> bool
(** [add_msg message] attempts to add a message to the database.
    Returns: true if the messages are added successfully, false
    otherwise. *)

val get_msg_since : string -> string -> Msg.t list
(** [get_msg_since receiver time] is a list of all messages sent to
    [receiver] after [time]. *)

val new_fr : string -> string -> string -> string -> bool
(** [new_fr sender receiver time msg] creates a new line of friend
    request in database, with [sender] being the requester and
    [receiver] being the receiving side on this friend request. Returns:
    true if the line is successfully added, false otherwise. *)

val fr_exist : string -> string -> bool
(** [fr_exist sender receiver] determines whether an undecided friend
    request from [sender] to [receiver] exists. Note: this one must be
    pending, the approved column must be null, instead of rejected or
    approved. Returns [true] if such a line does exist, [false]
    otherwise*)

val fr_approve : string -> string -> bool
(** [fr_approve sender receiver] approves the friend request from
    [sender] to [receiver], returns true if the operation is sucessful,
    and false otherwise. Raises [Not_found] if no such friend request
    exist that is un-determined. *)

val fr_reject : string -> string -> bool
(** [fr_reject sender receiver] rejects the friend request from [sender]
    to [receiver], returns true if the operation is sucessful, and false
    otherwise. Raises [Not_found] if no such friend request exist that
    is un-determined. *)
