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
