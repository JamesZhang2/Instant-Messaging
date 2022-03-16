(** This module converts a response message from the server into json*)

type msg = {
  sender : string;
  receiver : string;
  time : string;
  message : string;
}
(** If the client sends a get request, type that each message will be
    transformed into*)

val post_method_response : string -> string
(** [post_method_response message] Takes the response body text
    [message] and transforms it into json. Requires: the response is not
    an error*)

val get_method_response : msg list -> string
(** [get_method_responde messages] Takes a list of messages in form of
    type msg and transforms it into a json. *)

val error_response : string -> string
(** [error_response message] takes an error message and converts into
    json.*)
