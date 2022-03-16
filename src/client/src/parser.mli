(** This module processes a response body from the server*)

exception SyntaxError
(**Raised when the json syntax is incorrect. *)

type t
(** Abstract type representing a reponse from *)

type msg
(** Represents one message entry that the server may send back as a list
    per user request. *)

type response_type =
  | GetMethResponse of msg list
  | PostMethResponse of string
  | ErrorResponse of string
      (** Represents the type of data that the response is for. *)

val parse : string -> t
(** [parse json] returns the representation of the message that the json
    entails*)

val get_type : t -> response_type
(** [get_type parsert] returns the response type of the parsed
    representation [parsert]*)

val msg_type : msg -> string
(** [msg_type t] determines what type this message is. "friend request"
    if a response to a friend request, "message" if a normal message,
    "other" if any other type of message*)

val msg_sender : msg -> string
(** [msg_sender msg] determines which user is the message from*)

val get_time : t -> string
(** [get_time t] is the time that the response [t] was sent by the
    server*)

val msg_time : msg -> string
(** [msg_time msg] is the time that hte message [msg] was sent by the
    other user*)

val msg_body : msg -> string
(** [msg_body msg] is the actual body of the message [msg]*)
