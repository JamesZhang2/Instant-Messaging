(** The controller is called by the client's interface to complete
    operations. *)

open Util

exception IllegalResponse

val send_msg : string -> string -> bool * string
(** [send_msg receiver msg] sends [msg] from [currently login user] to
    [receiver]. Returns: [(true, feedback)] if the message is
    successfully sent, [(false, feedback)] otherwise. *)

val update_msg : ?amount:string -> unit -> bool * Msg.t list
(** [update_msg ?amount ()] fetches all unread messages sent to
    [currently logged in user], unless a time string is passed into
    [?amount]. Returns [(true, msglist)] if request successful, else
    [(false, \[\])]. Requires: a user is logged in. *)

val register : string -> string -> bool * string
(** [register username password] registers a user with [username] and
    [password]. Returns: [(true, feedback)] if the user is successfully
    registered, [(false, error_msg)] otherwise. *)

val login : string -> string -> bool * Msg.t list
(** [login username password] tries to log in to user [username] with
    [password]. Returns: [(true, new_msg list)] if the user is logged
    in, [(false, error_msg)] otherwise.

    Effects: updates all messages and new status *)

val logout : unit -> string
(** [logout] logs out the current user. *)

val friend_req : string -> string -> bool * string
(** [friend_req receiver msg] sends a friend request from [sender], the
    currently logged in user to [receiver] with message [msg]. Returns:
    [(true, feedback)] if the request is sent, [(false, error_msg)]
    otherwise. *)

val friend_req_reply : string -> bool -> bool * string
(** [friend_req_reply receiver accepted] accepts the friend request from
    [receiver], or sends the [receiver] the response if [accepted] is
    true, and rejects the friend request from [receiver] if [accepted]
    is false. Returns [(true, feedback)] if it is successfully sent,
    else [(false, error_msg)]. *)

val read_msg : unit -> bool * Msg.t list
(** [read_msg receiver] fetches all local messages since 3/29/2022
    17:00:00 as a list of Msg.t. Returns [(true, lst)] if it is
    successfully fetched, else [(false, \[\])]. *)

val read_msg_from : string -> bool * Msg.t list
(** [read_msg_from friend] returns all local messages sent from [friend]
    to currently logged in user. Requires: [username] is logged in, and
    friend is a valid friend. *)

val read_fr : unit -> bool * Msg.t list
(** [read_fr ()] returns all local friend requests. *)

val lst_of_friends : unit -> bool * string list
(** [lst_of_friends ()] returns a list of friends of the current logged
    in user. *)

val current_user : unit -> string option
(** [current_user ()] is [Some user] if [user] is the user currently
    logged in, or [None] if no user is currently logged in. *)

val join_gc : string -> string -> bool * string
(** [join_gc gc password] attempts to join the groupchat [gc] with
    [password] with the current username. Returns [(true, feedback)] if
    successfully joined, [(false, feedback)] otherwise. *)

val read_gc_msg : string -> bool * Msg.t list
(** [read_gc_msg gc] returns all local messages in groupchat [gc] to
    currently logged in user. Requires: [username] is logged in, and is
    in groupchag [gc]. *)

val send_gc_msg : string -> string -> bool * string
(** [send_gc_msg gc msg] sends [msg] from the currently logged in user
    to [gc]. Returns: [(true, feedback)] if the message is successfully
    sent, [(false, feedback)] otherwise. *)

val lst_of_gc : unit -> bool * string list
(** [lst_of_gc ()] returns a list of groupchats the current logged in
    user is in. Returns: [(true, lst)] if the query is successful,
    [(false, feedback)] otherwise. *)

val members_of_gc : string -> bool -> bool * string list
(** [members_of_gc gcid is_command] returns a list of members of
    groupchat [gcid]. Returns: [(true, lst)] if the query is successful,
    [(false, feedback)] otherwise.

    Requires: [is_command] is true if this function is called from
    interface. *)

val create_groupchat : string -> string -> bool * string
(** [create_groupchat id password] creates a new groupchat with [id] as
    identification, and [password] as entrance password. *)
