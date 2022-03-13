(** Packages the operations of the client into json strings. *)

type msg = {
  sender : string;
  receiver : string;
  time : string;
  body : string;
}

val send_message : string -> string -> string -> bool
(** [send_message sender receiver msg] sends [msg] from [sender] to
    [receiver]. Returns: true if the message is successfully sent, false
    otherwise. *)

val get_message : string -> msg list
(** [get_message sender] Returns: true if the message is successfully
    sent, false otherwise. *)

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
