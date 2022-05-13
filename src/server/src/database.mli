(** This module is a database for the server. It stores all user info
    and all the messages. *)

open Util

exception MalformedTime
(** The exception raised when the given time is malformed. *)

exception UnknownUser of string
(** The exception raised when the given user is not in the database. *)

exception UnknownGCID of string
(** The exception raised when the given groupchat id is not found in the
    database. *)

exception NoAccess of string * string
(** The exception raised when the given user does not have access to the
    given groupchat. The first string is a GCID, the second string is a
    user. *)

val create_tables : unit -> unit
(** [create_tables ()] creates the tables needed for the database. *)

val add_user : string -> string -> string -> string -> bool
(** [add_user username pwd key time] attempts to add user [username]
    with password [pwd], public key [key], and registration time [time]
    to the user database.

    Returns: [true] if a new user is successfully added, [false]
    otherwise.

    Raises: [MalformedTime] if the given time is malformed. *)

val user_exists : string -> bool
(** [user_exists username] is [true] if [username] exists in the
    database, and [false] otherwise. *)

val user_key : string -> string
(** [user_key username] is the public key associated with [username].
    Raises: [UnknownUser username] if the given user is not found in the
    database. *)

val chk_pwd : string -> string -> bool
(** [chk_pwd username pwd] is [true] if the database contains a user
    with name [username] and password [pwd], and [false] if the password
    supplied does not match the password of the user in the database.

    Raises: [UnknownUser username] if the given user is not found in the
    database. *)

val add_msg : Msg.t -> bool
(** [add_msg message] attempts to add a message to the database.

    Requires: [Msg.msg_type message] is [Message], [FriendReq], or
    [FriendReqReply]. If [Msg.msg_type message] is [FriendReqReply],
    then its content must be either ["True"] or ["False"] based on
    whether the friend request is accepted or rejected.

    Returns: true if the messages are added successfully, false
    otherwise.

    Raises: [MalformedTime] if the given time is malformed;
    [UnknownUser username] if either the sender or the receiver is not
    found in the database. *)

val get_msg : string -> Msg.t list
(** [get_msg receiver] is a list of all messages sent to [receiver],
    ordered by time sent in ascending order.

    Messages include direct messages, friend requests, friend request
    replies, and groupchat messages.

    Raises: [UnknownUser username] if the receiver is not found in the
    database. *)

val get_msg_since : string -> string -> Msg.t list
(** [get_msg_since receiver time] is a list of all messages sent to
    [receiver] after [time], ordered by time sent in ascending order.

    Messages include direct messages, friend requests, friend request
    replies, and groupchat messages.

    Raises: [MalformedTime] if the given time is malformed;
    [UnknownUser username] if the receiver is not found in the database. *)

val get_new_msg : string -> Msg.t list
(** [get_new_msg receiver] is a list of all messages sent to [receiver]
    that have not been retrieved, ordered by time sent in ascending
    order.

    Messages include direct messages, friend requests, friend request
    replies, and groupchat messages.

    Raises: [UnknownUser username] if the receiver is not found in the
    database. *)

val get_new_msg_since : string -> string -> Msg.t list
(** [get_new_msg_since receiver time] is a list of all messages sent to
    [receiver] after [time] that have not been retrieved, ordered by
    time sent in ascending order.

    Messages include direct messages, friend requests, friend request
    replies, and groupchat messages.

    Raises: [MalformedTime] if the given time is malformed;
    [UnknownUser username] if the receiver is not found in the database. *)

(** Three different relationships between users A and B:

    - no relationship (FF)
    - pending friend request (TF, FT)
    - friends (TT) *)

val new_fr : Msg.t -> bool
(** [new_fr req] creates a new friend request in the database, with
    [Msg.sender req] being the requester and [Msg.receiver req] being
    the receiving side on this friend request.

    Requires:

    - [Msg.msg_type req = Msg.FriendReq]
    - There is no pending request between the sender and the receiver
    - The sender and receiver are not friends with each other.

    Returns: [true] if the line is successfully added, and [false]
    otherwise.

    Raises: [UnknownUser username] if either the sender or the receiver
    is not found in the database. *)

val fr_exist : string -> string -> bool
(** [fr_exist sender receiver] determines whether a pending friend
    request from [sender] to [receiver] exists.

    Returns: [true] if there is a pending friend request (and that
    sender and receiver are not already friends), and [false] otherwise.

    Raises: [UnknownUser username] if either the sender or the receiver
    is not found in the database. *)

val is_friend : string -> string -> bool
(** [is_friend sender receiver] determines whether [sender] and
    [receiver] are friends.

    Returns: [true] if [sender] and [receiver] are friends, and [false]
    otherwise.

    Raises: [UnknownUser username] if either the sender or the receiver
    is not found in the database. *)

val fr_accept : string -> string -> bool
(** [fr_accept sender receiver] accepts the friend request from [sender]
    to [receiver].

    Returns: [true] if the operation is sucessful, and [false]
    otherwise.

    Raises: [Not_found] if there is no pending request between the
    sender and the receiver; [UnknownUser username] if either the sender
    or the receiver is not found in the database. *)

val fr_reject : string -> string -> bool
(** [fr_reject sender receiver] rejects the friend request from [sender]
    to [receiver].

    Returns: [true] if the operation is sucessful, and [false]
    otherwise.

    Raises: [Not_found] if there is no pending request between the
    sender and the receiver; [UnknownUser username] if either the sender
    or the receiver is not found in the database. *)

val friends_of : string -> string list
(** [friends_of user] is a list of all users that are friends with
    [user].

    Raises: [UnknownUser user] if user is not found in the database. *)

(*********************** Groupchat ***********************)

val create_groupchat : string -> string -> string -> bool
(** [create_groupchat id password username] creates a new groupchat with
    [id] and [password] being password, initiated by [username].
    [username] should be the only person in this groupchat [id] by
    default. Returns [true] if the groupchat is successfully created,
    [false] otherwise.

    Raises: [UnknownUser username] if the given user is not found in the
    database. *)

val gc_exists : string -> bool
(** [gc_exists id] checks whether the groupchat [id] exists. Returns
    [true] if it does, [false] otherwise. *)

val check_gc_password : string -> string -> bool
(** [check_gc_password id password] checks whether [password] is the
    correct entrance password to the groupchat [gc].

    Raises: [UnknownGCID gcid] if the given groupchat id is not found in
    the database. *)

val add_member_gc : string -> string -> bool
(** [add_member_gc id new_member] adds a new member [new_member] to the
    groupchat [id]. Returns [true] if successfully added, [false]
    otherwise.

    Raises: [UnknownUser username] if the given user is not found in the
    database; [UnknownGCID gcid] if the given groupchat id is not found
    in the database. *)

val is_in_gc : string -> string -> bool
(** [is_in_gc id username] checks whether [username] is in the groupchat
    [id]. Returns [true] if [username] is in [id], [false] otherwise.

    Raises: [UnknownUser username] if the given user is not found in the
    database; [UnknownGCID gcid] if the given groupchat id is not found
    in the database. *)

val add_msg_to_gc : Msg.t -> bool
(** [send_msg_to_gc msg] adds the message [msg] to a groupchat. The
    receiver field of [msg] is the id of the groupchat. Returns [true]
    if message is successfully added, [false] otherwise.

    Requires: [Msg.msg_type msg] is [GCMessage].

    Raises: [UnknownUser username] if the given user is not found in the
    database; [UnknownGCID gcid] if the given groupchat id is not found
    in the database; [NoAccess (username, gcid)] if the given user does
    not have access to the given groupchat. *)

val gc_of_user : string -> string list
(** [gc_of_user username] is the list of groupchats that [username] is
    in. The list returned is a list of groupchat ids.

    Raises: [UnknownUser username] if the given user is not found in the
    database. *)

val members_of_gc : string -> string list
(** [member_of_gc id] is the list of member usernames in groupchat [id].

    Requires: [UnknownGCID gcid] if the given groupchat id is not found
    in the database. *)
