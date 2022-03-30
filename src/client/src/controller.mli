open Util
(** The controller is called by the client's interface to complete
    operations. *)

exception IllegalResponse

val send_msg : string -> string -> string -> bool * string
(** [send_msg sender receiver msg] sends [msg] from [sender] to
    [receiver]. Returns: [(true, feedback)] if the message is
    successfully sent, [(false, feedback)] otherwise. *)

val update_msg : ?amount:string -> string -> bool * Msg.t list
(** [update_msg ?amount reciever] fetches all unread messages sent to
    [receiver], unless a time string is passed into [?amount]. Returns
    [(true, msglist)] if request successful, else [(false, \[\])]*)

val register : string -> string -> bool * string
(** [register username password] registers a user with [username] and
    [password]. Returns: [(true, feedback)] if the user is successfully
    registered, [(false, error_msg)] otherwise. *)

val login : string -> string -> bool * Msg.t list
(** [login username password] tries to log in to user [username] with
    [password]. Returns: [(true, new_msg list)] if the user is logged
    in, [(false, error_msg)] otherwise. *)

val friend_req : string -> string -> string -> bool * string
(** [friend_req sender receiver msg] sends a friend request from
    [sender] to [receiver] with message [msg]. Returns:
    [(true, feedback)] if the request is sent, [(false, error_msg)]
    otherwise. *)

val friend_req_reply : string -> string -> bool -> bool * string
(** [friend_req_reply sender receiver accepted] accepts the friend
    request from [receiver], or sends the [receiver] the response if
    [accepted] is true, and rejects the friend request from [receiver]
    if [accepted] is false. Returns [(true, feedback)] if it is
    successfully sent, else [(false, error_msg)]*)
