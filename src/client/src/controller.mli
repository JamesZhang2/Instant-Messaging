open Util
(** The controller is called by the client's interface to complete
    operations. *)

exception IllegalResponse

val send_msg : string -> string -> bool * string
(** [send_msg receiver msg] sends [msg] from [currently login user] to
    [receiver]. Returns: [(true, feedback)] if the message is
    successfully sent, [(false, feedback)] otherwise. *)

val update_msg : ?amount:string -> unit -> bool * Msg.t list
(** [update_msg ?amount ()] fetches all unread messages sent to
    [currently logged in user], unless a time string is passed into
    [?amount]. Returns [(true, msglist)] if request successful, else
    [(false, \[\])]. Requires: a user is logged in*)

val register : string -> string -> bool * string
(** [register username password] registers a user with [username] and
    [password]. Returns: [(true, feedback)] if the user is successfully
    registered, [(false, error_msg)] otherwise. *)

val login : string -> string -> bool * Msg.t list
(** [login username password] tries to log in to user [username] with
    [password]. Returns: [(true, new_msg list)] if the user is logged
    in, [(false, error_msg)] otherwise. *)

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
    else [(false, error_msg)]*)

val read_msg : unit -> bool * Msg.t list
(** [read_msg receiver] fetches all local messages since 3/29/2022
    17:00:00 as a list of Msg.t. Returns [(true, lst)] if it is
    successfully fetched, else [(false, \[\])]*)

val read_msg_from : string -> bool * Msg.t list
(** [read_msg_from friend] returns all local messages sent from [friend]
    to currently logged in user. Requires: [username] is logged in, and
    friend is a valid friend*)

val lst_of_friends : unit -> bool * string list
(** [lst_of_friends ()] returns a list of friends of the current logged
    in user. *)
