(** This module converts a response message from the server into json*)

open Util

val post_method_response : string -> string
(** [post_method_response message] Takes the response body text
    [message] and transforms it into json. Requires: the response is not
    an error*)

val get_method_response : Msg.t list -> string
(** [get_method_responde messages] Takes a list of messages and
    transforms it into a json. *)

val error_response : string -> string
(** [error_response message] takes an error message and converts into
    json.*)
