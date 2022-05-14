(** This module is a database for the client. It stores all user info
    and all the messages. *)

open Util

exception MalformedTime
(** The exception raised when time is not correctly formatted. *)

val init_dbs : unit -> bool * string
(** [init_dbs()] initializes the client table. Returns
    [(true, feedback)] if the new database is successfully created,
    [(false, err_msg)] if the table has already been created before or
    an issue is encountered. *)

val create_dbs : string -> string -> bool * string
(** [create_dbs username key] creates a new database for a specific
    user. Returns [(true, feedback)] if a new database is successfully
    created, [(false, err_msg)] if the table has already been created
    before or an issue is encountered. *)

val add_request :
  string -> Msg.t -> string option -> bool option -> bool * string
(** [add_request client req key req_state] attempts to add a freind
    request related to [client] with current state [req_state] and [key]
    as public key of this possible . Requires: sender and receiver are
    not friends, [client] is either the sender or receiver. Returns
    [(true, feedback)] if a new friend request is successfully added,
    [(false, err_msg)] if the table has already been created before or
    an issue is encountered. *)

val update_request : string -> string -> bool -> bool * string
(** [update_request client username req_state] updates the request state
    to [req_state]. Returns [(true, feedback)] if the request is
    successfully updated, [(false, err_msg)] otherwise. *)

val add_msg : string -> Msg.t -> bool * string
(** [add_msg client msg] adds a message [msg] to the [client] table.
    Requires: sender and receiver are friends. Returns
    [(true, feedback)] if the message is successfully added,
    [(false, err_msg)] otherwise. *)

val get_all_reqs : string -> Msg.t list
(** [get_all_reqs client] is a list of all freind requests in [client]
    table. Raises [IncorrectUser] if [client] is not a vlaid. Raises
    [DBNotExist] if the table has not been created. *)

val get_all_frds : string -> string list
(** [get_all_frds client] is a list of all freinds in [client] table.
    Raises [IncorrectUser] if [client] is not a vlaid. Raises
    [DBNotExist] if the table has not been created.*)

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

val is_frd : string -> string -> bool
(**[is_frd client username] is whether [username] is in the [client]'s
   friend list. Riases [DBNotExist] if database has not been created. *)

val is_in_req : string -> string -> bool
(** [is_in_req client username] is whether [username] is in the
    [client]'s friend request list. Raises [DBNotExist] if database has
    not been created. *)

val is_client : string -> bool
(** [is_client username] determines whether the device has records on
    this [username]*)

(**************)

val add_groupchat : string -> string -> string list -> bool
(** [add_groupchat id username member_list] adds a groupchat to
    database, with [id] being the groupchat id, [username] being the
    person joining, and [member_list] being the current members in the
    groupchat. Returns [true] if successfully added, [false] otherwise*)

val create_groupchat : string -> string -> bool
(** [create_groupchat id username] creates a new groupchat with [id],
    initiated by [username]. [username] should be the only person in
    this groupchat [id]*)

val add_member_gc : string -> string -> bool
(** [add_member_gc id new_member] adds a new member [new_member] to the
    groupchat [id]. Returns [true] if successfully added, [false]
    otherwise *)

val is_in_gc : string -> string -> bool
(** [is_in_gc id username] checks whether [username] is in the groupchat
    [id]. Returns [true] if [username] is in [id], [false] otherwise*)

val is_gc : string -> bool
(** [is_gc gc] checks whether [gc] is an existing groupchat. Returns
    [true] if so, [false] otherwise*)

val add_msg_to_gc : Msg.t -> bool
(** [send_msg_to_gc id username msg] adds the message [msg] to the
    groupchat [id], sent by [username]. Returns [true] if message is
    successfully added, [false] otherwise.*)

val get_msg_gc_since : string -> string -> string -> Msg.t list
(** [get_msg_gc_since username id time] is a list of all messages in
    groupchat [id] since [time]. Raises [IncorrectUser] if [username] is
    not valid or [username] is not in groupchat [id]. Raises
    [DBNotExist] if database has not been created.*)

val gc_of_user : string -> string list
(** [gc_of_user username] is the list of groupchats that [username] is
    in. The list returned is a list of groupchat ids.

    Requires: [username] is a valid existing user *)

val members_of_gc : string -> string list
(** [member_of_gc id] is the list of member usernames in groupchat [id].

    Requires: [id] is the id of an existing groupchat*)
