(** This module is a database for the server. It stores all user info
    and all the messages. *)

val create_user_db : unit -> bool * string
(** [create_user_db ()] creates a new database for user info. Returns
    [(true, feedback)] if a new database is successfully created,
    [(false, err_msg)] otherwise. *)

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

val user_exists : string -> bool
(** [user_exists username] is [true] if [username] exists in the user
    database, and [false] otherwise. *)

val chk_pwd : string -> string -> chk_user
(** [chk_pwd username pwd] is [UserOK] if the user database contains a
    user with name [username] and password [pwd], [UnknownUser user] if
    the user is not found, and [WrongPwd pwd] if the password supplied
    does not match the password of the user in the database. *)

val del_user : string -> string -> string -> chk_user
(** [del_user username pwd time] attempts to delete user [username] with
    password [pwd] from the user database. [time] is the time that the
    request to close the account occured. Returns [UserOK] if the user
    is successfully deleted, [UnknownUser user] if the user is not
    found, [WrongPwd pwd] if the password supplied does not match the
    password of the user in the database, and [MalformedUserTime time]
    if the time is malformed. *)

val create_msg_db : unit -> bool * string
(** [create_msg_db ()] creates a new database for messages. Returns
    [(true, feedback)] if a new database is successfully created,
    [(false, err_msg)] otherwise. *)

type chk_msg =
  | MsgOK
  | UnknownSender of string
  | UnknownReceiver of string
  | UnkonwnMsgType of string
  | MalformedMsgTime of string

val add_msg : string -> string -> string -> string -> string -> chk_msg
(** [add_msg sender receiver time msg_type message] attempts to add a
    message to the database. Returns [MsgOK] if the message is
    successfully added, [UnknownSender sender] if the sender is not in
    the user database, [UnknownReceiver receiver] if the receiver is not
    in the user database, [UnkonwnMsgType msg_type] if the message type
    is unknown, and [MalformedMsgTime time] if the time is malformed.*)

type msg_type =
  | FriendReq
  | Message
      (** [msg_type] is the type of a message stored on server end. It
          can either be a [FriendReq] or a direct [Message]*)

type msg = {
  sender : string;
  receiver : string;
  time : string;
  msg_type : msg_type;
  message : string;
}

val get_msg_since : string -> string -> msg list
(** [get_msg_since receiver time] is a list of all messages sent to
    [receiver] after [time]. *)
