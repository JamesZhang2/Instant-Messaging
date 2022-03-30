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

val add_request : string -> Msg.t -> bool option -> bool * string
(** [add_request client req req_state] attempts to add a freind request
    related to [client] with current state [req_state]. Requires: sender
    and receiver are not friends, [client] is either the sender or
    receiver. Raises [IncorrectUser] if either requires clause is false.
    Returns [(true, feedback)] if a new friend request is successfully
    added, [(false, err_msg)] if the table has already been created
    before or an issue is encountered. *)

val update_request : string -> string -> bool -> bool * string
(** [update_request client username req_state] updates the request state
    to [req_state]. Raises [IncorrectUser] if [client] is not in the
    database or [client] does not have a request with [username] in
    table. Returns [(true, feedback)] if the request is successfully
    updated, [(false, err_msg)] otherwise. *)

val add_msg : string -> Msg.t -> bool * string
(** [add_msg client msg] adds a message [msg] to the [client] table.
    Requires: sender and receiver are friends. Raises [IncorrectUser] if
    [client] is not in the database. Returns [(true, feedback)] if the
    message is successfully added, [(false, err_msg)] otherwise. *)

(* val get_username : unit -> string (** [get_username()] is the
   username of current client. Raises [DBNotExist] if database has not
   been created. *) *)
(* I think this may be not necessary *)

val get_all_reqs : string -> Msg.t list
(** [get_all_reqs client] is a list of all freind requests in [client]
    table. Raises [IncorrectUser] if [client] is not a vlaid. Raises
    [DBNotExist] if the table has not been created. *)

val get_all_frds : string -> Msg.t list
(** [get_all_frds client] is a list of all freinds in [client] table.
    Raises [IncorrectUser] if [client] is not a vlaid. Raises
    [DBNotExist] if the table has not been created.*)

val get_all_msgs : string -> Msg.t list
(** [get_all_msgs client] is a list of all messages in [client] table.
    Raises [IncorrectUser] if [client] is not a valid. Raises
    [DBNotExist] if database has not been created.*)

val get_all_msgs_since : string -> string -> Msg.t list
(** [get_all_msgs_since client time] is a list of all messages in
    [client] table before [time]. Raises [IncorrectUser] if [client] is
    not a vlaid. Raises [MalformedTime] if [time] format is incorrect.
    Raises [DBNotExist] if database has not been created.*)

val get_msgs_by_frd : string -> string -> Msg.t list
(** [get_msgs_by_frd client frd] is a list of all messages between
    [client] and friend [frd]. Raises [IncorrectUser] if [client] is not
    valid or [username] is not [client]'s friend. Raises [DBNotExist] if
    database has not been created.*)

val get_msgs_by_frd_since : string -> string -> string -> Msg.t list
(** [get_msgs_by_frd_since client frd time] is a list of all messages
    between [client] and friend [frd]. Raises [IncorrectUser] if
    [client] is not valid or [username] is not [client]'s friend. Raises
    [MalformedTime] if [time] format is incorrect. Raises [DBNotExist]
    if database has not been created. *)

val get_req_by_name : string -> string -> Msg.t option
(** [get_req_by_name client username] is [Some req] where [req] is the
    friend request between [client] and [username] in [client] table if
    there is such a request, [None] otherwise. Raises [IncorrectUser] if
    [client] is not valid. Raises [DBNotExist] if database has not been
    created.*)

val isFriend : string -> string -> bool
(**[isFriend client username] is whether [username] is in the [client]'s
   friend list. Riases [DBNotExist] if database has not been created. *)

val isInRequest : string -> string -> bool
(** [isInRequest client username] is whether [username] is in the
    [client]'s friend request list. Raises [DBNotExist] if database has
    not been created. *)

val is_client : string -> bool
(** [is_client username] determines whether the device has records on
    this [username]*)
