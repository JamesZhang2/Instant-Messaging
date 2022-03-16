(** The controller is called by the client's interface to complete
    operations. *)

type msg = {
  sender : string;
  receiver : string;
  time : string;
  body : string;
}

exception IllegalResponse

val send_msg : string -> string -> string -> bool
(** [send_msg sender receiver msg] sends [msg] from [sender] to
    [receiver]. Returns: true if the message is successfully sent, false
    otherwise. *)

val get_msg : string -> msg list
(** [get_msg reciever] fetches all the messages sent to [receiver]. *)

val register : string -> string -> bool
(** [register username password] registers a user with [username] and
    [password]. Returns: true if the user is successfully registered,
    false otherwise. *)

val login : string -> string -> bool * string
(** [login username password] tries to log in to user [username] with
    [password]. Returns: [(true, "")] if the user is logged in,
    [(false, error_msg)] otherwise. *)

val friend_req : string -> string -> string -> bool
(** [friend_req sender receiver msg] sends a friend request from
    [sender] to [receiver] with message [msg]. Returns: true if the
    request is sent, false otherwise. *)

val friend_req_reply : string -> string -> bool -> bool
(** [friend_req_reply sender receiver accepted] accepts the friend
    request from [receiver] if [accepted] is true, and rejects the
    friend request from [receiver] if [accepted] is false.*)
