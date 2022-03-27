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
(** [add_msg message] attempts to add a direct message to the database.

    Requires: [Msg.msg_type message = Message].

    Returns: true if the messages are added successfully, false
    otherwise. *)

val get_msg_since : string -> string -> Msg.t list
(** [get_msg_since receiver time] is a list of all messages sent to
    [receiver] after [time]. *)

(** Three different relationships between users A and B:

    - no relationship (FF)
    - pending friend request (TF, FT)
    - friends (TT) *)

val new_fr : Msg.t -> bool
(** [new_fr req] creates a new friend request in the database, with
    [Msg.sender req] being the requester and [Msg.receiver req] being
    the receiving side on this friend request.

    Requires: [Msg.msg_type req = FriendReq], there is no pending
    request between the sender and the receiver, and sender and receiver
    are not friends with each other.

    Returns: true if the line is successfully added, false otherwise. *)

val fr_exist : string -> string -> bool
(** [fr_exist sender receiver] determines whether a pending friend
    request from [sender] to [receiver] exists.

    Returns: [true] if there is a pending friend request, and [false]
    otherwise. *)

val is_friend : string -> string -> bool
(** [is_friend sender receiver] determines whether [sender] and
    [receiver] are friends.

    Returns: [true] if [sender] and [receiver] are friends, and [false]
    otherwise. *)

val fr_approve : string -> string -> bool
(** [fr_approve sender receiver] approves the friend request from
    [sender] to [receiver].

    Returns: [true] if the operation is sucessful, and [false]
    otherwise.

    Raises: [Not_found] if there is no pending request between the
    sender and the receiver. *)

val fr_reject : string -> string -> bool
(** [fr_reject sender receiver] rejects the friend request from [sender]
    to [receiver].

    Returns: [true] if the operation is sucessful, and [false]
    otherwise.

    Raises: [Not_found] if there is no pending request between the
    sender and the receiver. *)

val friends_of : string -> string list
(** [friends_of user] is a list of all users that are friends with
    [user]. *)
