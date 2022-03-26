(** This module is a database for the server. It stores all user info
    and all the messages. *)

open Util

type state =
  | True
  | False
  | Null

type msg_type =
  | Sent
  | Received

exception MalformedTime
exception IncorrectUser
exception DBNotExist

val create_dbs : string -> Crypto.k -> bool * string
(** [create_dbs username key] creates a new database for user info.
    Returns [(true, feedback)] if a new database is successfully
    created, [(false, err_msg)] otherwise. *)

val delete_dbs : unit -> unit
(** [delete_dbs ()] deletes the current database if exist. *)

val add_request :
  string -> string -> string -> msg_type -> state -> bool * string
(** [add_request username time msg req_type req_state] attempts to add a
    freind request related to [username] at [time] with [msg]. Raises
    [MalformedTime] if [time] format is incorrect. .*)

val update_request : string -> bool -> bool * string
(** [update_request username req_state] updates the request state to
    [req_state]. Raises [IncorrectUser] if [username] does not have a
    request in the table. Requires [req_state] is [True] or [False], and
    the request in the tableReturns [(true, feedback)] if the request is
    successfully added, [(false, err_msg)] otherwise currently has state
    [Null]. Returns [(true, feedback)] if the request is successfully
    updates, [(false, err_msg)] otherwise. *)

val add_friend : string -> Crypto.pk -> bool * string
(** [add_friend username key] adds a friend with [username] and public
    key [key]. Raises [IncorrectUser] if [username] has been added to
    the friend list. Returns [(true, feedback)] if the friend is
    successfully added, [(false, err_msg)] otherwise. *)

val add_msg : string -> string -> string -> msg_type -> bool * string
(** [add_msg username time msg msg_type] adds a message [msg] relative
    to friend [username] at [time] with [msg_type]. Raises
    [IncorrectUser] if [username] is not in the database. Raises
    [MalformedTime] if [time] format is incorrect. Returns
    [(true, feedback)] if the message is successfully added,
    [(false, err_msg)] otherwise.*)

val get_username : unit -> string
(** [get_username()] is the username of current client. Raises
    [DBNotExist] if database has not been created. *)

val get_all_reqs : unit -> Msg.msg_type list
(** [get_all_reqs()] is a list of all freind requests. Raises
    [DBNotExist] if database has not been created.*)

val get_all_msgs : unit -> Msg.msg_type list
(** [get_all_msgs()] is a list of all messages related to the client.
    Raises [DBNotExist] if database has not been created.*)

val get_all_msgs_since : string -> Msg.msg_type list
(** [get_all_msgs_since time] is a list of all messages related to the
    client before [time]. Raises [MalformedTime] if [time] format is
    incorrect. Raises [DBNotExist] if database has not been created.*)

val get_msgs_by_frd : string -> Msg.msg_type list
(** [get_msgs_by_frd username] is a list of all messages between current
    client and friend [username]. Raises [IncorrectUser] if [username]
    is not a friend or is themselves. Raises [DBNotExist] if database
    has not been created.*)

val get_msgs_by_frd_since : string -> string -> Msg.msg_type list
(** [get_msgs_by_frd_since username time] is a list of all messages
    between the client and friend [username]. Raises [IncorrectUser] if
    [username] is not a friend or is themselves. Raises [MalformedTime]
    if [time] format is incorrect. Raises [DBNotExist] if database has
    not been created.*)

val get_req_by_name : string -> Msg.msg_type option
(** [get_req_by_name username] is [Some req] where [req] is the friend
    request related to [username] if there is such a request, [None]
    otherwise. Raises [IncorrectUser] if [username] is not a friend or
    is themselves. Raises [DBNotExist] if database has not been created.*)

val isFriend : string -> bool
(**[isFriend username] is whether [username] is in the friend list.
   Riases [DBNotExist] if database has not been created. *)

val isInRequest : string -> bool
(** [isInRequest username] is whether [username] is in the friend
    request list. Raises [DBNotExist] if database has not been created. *)
