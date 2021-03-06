(** This module is a database for the client. It stores all user info
    and all the messages. *)

open Util

exception MalformedTime
(** The exception raised when time is not correctly formatted. *)

val init_dbs : unit -> bool * string
(** [init_dbs()] initializes the client table if not exists. Returns
    [(true, feedback)] if the new database is successfully created,
    [(false, err_msg)] if an issue is encountered. *)

val create_dbs : string -> string -> bool * string
(** [create_dbs username key] creates a new database for a specific user
    if not exists. Returns [(true, feedback)] if a new database is
    successfully created, [(false, err_msg)] if an issue is encountered. *)

val add_request :
  string -> Msg.t -> string option -> bool option -> bool * string
(** [add_request client req key req_state] attempts to add a freind
    request related to [client] with current state [req_state] and [key]
    as public key. Returns [(true, feedback)] if a new friend request is
    successfully added, [(false, err_msg)] if an issue is encountered.

    Requires: all arguments are valid; [client] exists, sender and
    receiver are not friends, [client] is either the sender or receiver.*)

val update_request : string -> string -> bool -> bool * string
(** [update_request client username req_state] updates the request state
    to [req_state]. Returns [(true, feedback)] if the request is
    successfully updated, [(false, err_msg)] otherwise.

    Requires: all arguments are valid; [client] exists, request for
    [username] was added. *)

val add_msg : string -> Msg.t -> bool * string
(** [add_msg client msg] adds a message [msg] to the [client] table.
    Returns [(true, feedback)] if the message is successfully added,
    [(false, err_msg)] otherwise.

    Requires: all arguments are valid; [client] exists, sender and
    receiver are friends. *)

val get_all_reqs : string -> Msg.t list
(** [get_all_reqs client] is a list of all freind requests in [client]
    table.

    Requires: [client] exists. *)

val get_all_frds : string -> string list
(** [get_all_frds client] is a list of all freinds in [client] table.

    Requires: [client] exists. *)

val get_all_msgs_since : string -> string -> Msg.t list
(** [get_all_msgs_since client time] is a list of all messages in
    [client] table before [time].

    Requires: [client] exists.

    Raises [MalformedTime] if [time] format is incorrect.*)

val get_msgs_by_frd : string -> string -> Msg.t list
(** [get_msgs_by_frd client frd] is a list of all messages between
    [client] and friend [frd].

    Requires: all arguments are valid; [client] exists, [client] and
    [frd] are friends. *)

val get_msgs_by_frd_since : string -> string -> string -> Msg.t list
(** [get_msgs_by_frd_since client frd time] is a list of all messages
    between [client] and friend [frd].

    Requires: all arguments are valid; [client] exists, [client] and
    [frd] are friends.

    Raises [MalformedTime] if [time] format is incorrect. *)

val get_req_by_name : string -> string -> Msg.t option
(** [get_req_by_name client username] is [Some req] where [req] is the
    friend request between [client] and [username] in [client] table if
    there is such a request, [None] otherwise.

    Requires: all arguments are valid; [client] exists. *)

val is_frd : string -> string -> bool
(**[is_frd client username] is whether [username] is in the [client]'s
   friend list.

   Requires: all arguments are valid; [client] exists. *)

val is_in_req : string -> string -> bool
(** [is_in_req client username] is whether [username] is in the
    [client]'s friend request list.

    Requires: all arguments are valid; [client] exists. *)

val is_client : string -> bool
(** [is_client username] is whether the device has records on this
    [username].

    Requires: [init_dbs()] has been called. *)

val add_groupchat : string -> string -> string list -> bool
(** [add_groupchat client id member_list] adds a groupchat to database,
    with [id] being the groupchat id, [client] being the person joining,
    and [member_list] being the current members in the groupchat.
    Returns [true] if successfully added, [false] otherwise.

    Requires: all arguments are valid; [client] exists, [id] is new,
    [client] is in [member_list]. *)

val create_groupchat : string -> string -> bool
(** [create_groupchat client id] creates a new groupchat with [id],
    initiated by [username]. [client] should be the only person in this
    groupchat [id]. Returns [true] if successfully created, [false]
    otherwise.

    Requires: all arguments are valid; [client] exists, [id] is new.*)

val add_member_gc : string -> string -> string list -> bool
(** [add_member_gc client id new_members] adds list of new members
    [new_members] to the groupchat [id]. Returns [true] if successfully
    added, [false] otherwise.

    Requires: all arguments are valid; [client] exists, [id] is a valid
    groupchat, all member in [new_members] are not in [id]. *)

val is_in_gc : string -> string -> string -> bool
(** [is_in_gc client id username] checks whether [username] is in the
    groupchat [id]. Returns [true] if [username] is in [id], [false]
    otherwise.

    Requires: all arguments are valid; [client] exists, [id] is a valid
    groupchat. *)

val is_gc : string -> string -> bool
(** [is_gc client gc] checks whether [gc] is an existing groupchat.
    Returns [true] if so, [false] otherwise.

    Requires: all arguments are valid; [client] exists. *)

val add_msg_to_gc : string -> Msg.t -> bool
(** [add_msg_to_gc client msg] adds the message [msg] to the groupchat
    [Msg.receiver] (id), sent by [Msg.sender] (username). Returns [true]
    if message is successfully added, [false] otherwise.

    Requires: all arguments are valid; [client] exists, [msg] is valid. *)

val get_msg_gc_since : string -> string -> string -> Msg.t list
(** [get_msg_gc_since client id time] is a list of all messages in
    groupchat [id] since [time].

    Requires: all arguments are valid; [client] exists, [id] is a valid
    groupchat.

    Raises [MalformedTime] if [time] format is incorrect.*)

val gc_of_user : string -> string list
(** [gc_of_user client] is the list of groupchats that [username] is in.
    The list returned is a list of groupchat ids.

    Requires: [client] exists. *)

val members_of_gc : string -> string -> string list
(** [member_of_gc client id] is the list of member usernames in
    groupchat [id].

    Requires: [id] is the id of an existing groupchat. *)
