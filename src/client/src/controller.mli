open Util
(** The controller is called by the client's interface to complete
    operations. *)

type msg = {
  sender : string;
  receiver : string;
  time : string;
  body : string;
}

exception IllegalResponse

val send_msg : string -> string -> string -> bool * string
(** [send_msg sender receiver msg] sends [msg] from [sender] to
    [receiver]. Returns: [(true, feedback)] if the message is
    successfully sent, [(false, feedback)] otherwise. *)

val get_msg : string -> bool * Msg.t list
(** [get_msg reciever] fetches all the messages sent to [receiver].
    Returns [(true, msglist)] if request successful, else
    [(false, \[\])]*)

val register : string -> string -> bool * string
(** [register username password] registers a user with [username] and
    [password]. Returns: [(true, feedback)] if the user is successfully
    registered, [(false, error_msg)] otherwise. *)

val login : string -> string -> bool * string
(** [login username password] tries to log in to user [username] with
    [password]. Returns: [(true, feedback)] if the user is logged in,
    [(false, error_msg)] otherwise. *)

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
