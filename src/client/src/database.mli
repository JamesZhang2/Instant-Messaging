(** This module is a database for the client. It stores all user info
    and all the messages. *)

open Util

type msg_dir =
  | Sent
  | Received

exception MalformedTime
exception IncorrectUser
exception DBNotExist

val init_dbs : unit -> bool * string
(** [init_dbs()] initializes the client table. Returns
    [(true, feedback)] if the new database is successfully created,
    [(false, err_msg)] if the table has already been created before or
    an issue is encountered. *)

val create_dbs : string -> Crypto.k -> bool * string
(** [create_dbs username key] creates a new database for a specific
    user. Returns [(true, feedback)] if a new database is successfully
    created, [(false, err_msg)] if the table has already been created
    before or an issue is encountered. *)

val add_request :
  string -> string -> string -> msg_dir -> bool option -> bool * string
(** [add_request username time msg req_type req_state] attempts to add a
    freind request related to [username] at [time] with [msg]. Raises
    [MalformedTime] if [time] format is incorrect.

    [req_state] is [None] means the client sent an request but haven't
    receive a response; [Some true] means they are friends; [Some false]
    means the client is rejected.*)

val update_request : string -> bool -> bool * string
(** [update_request username req_state] updates the request state to
    [req_state]. Raises [IncorrectUser] if [username] does not have a
    request in the table. Requires [req_state] is [true] or [false], and
    the request in the tableReturns [(true, feedback)] if the request
    currently has state [Null]. Returns [(true, feedback)] if the
    request is successfully updated, [(false, err_msg)] otherwise. *)

val add_msg : string -> string -> string -> msg_dir -> bool * string
(** [add_msg username time msg msg_type] adds a message [msg] relative
    to friend [username] at [time] with [msg_dir]. Raises
    [IncorrectUser] if [username] is not in the database. Raises
    [MalformedTime] if [time] format is incorrect. Returns
    [(true, feedback)] if the message is successfully added,
    [(false, err_msg)] otherwise.*)

val get_username : unit -> string
(** [get_username()] is the username of current client. Raises
    [DBNotExist] if database has not been created. *)

val get_all_reqs : unit -> Msg.t list
(** [get_all_reqs()] is a list of all freind requests. Raises
    [DBNotExist] if database has not been created.*)

val get_all_frds : unit -> Msg.t list
(** [get_all_frds()] is a list of all freind. Raises [DBNotExist] if
    database has not been created.*)

val get_all_msgs : unit -> Msg.t list
(** [get_all_msgs()] is a list of all messages related to the client.
    Raises [DBNotExist] if database has not been created.*)

val get_all_msgs_since : string -> Msg.t list
(** [get_all_msgs_since time] is a list of all messages related to the
    client before [time]. Raises [MalformedTime] if [time] format is
    incorrect. Raises [DBNotExist] if database has not been created.*)

val get_msgs_by_frd : string -> Msg.t list
(** [get_msgs_by_frd username] is a list of all messages between current
    client and friend [username]. Raises [IncorrectUser] if [username]
    is not a friend or is themselves. Raises [DBNotExist] if database
    has not been created.*)

val get_msgs_by_frd_since : string -> string -> Msg.t list
(** [get_msgs_by_frd_since username time] is a list of all messages
    between the client and friend [username]. Raises [IncorrectUser] if
    [username] is not a friend or is themselves. Raises [MalformedTime]
    if [time] format is incorrect. Raises [DBNotExist] if database has
    not been created.*)

val get_req_by_name : string -> Msg.t option
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
