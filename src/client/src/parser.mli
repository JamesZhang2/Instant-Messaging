(** This module processes a response body from the server*)

open Util

exception SyntaxError
(** Raised when the json syntax is incorrect. *)

type t
(** Abstract type representing a reponse from. *)

type response_type =
  | GetMsgResponse of Msg.t list
  | PostMethResponse of string
  | ErrorResponse of string
      (** Represents the type of data that the response is for. *)

val parse : string -> t
(** [parse json] returns the representation of the message that the json
    entails. *)

val get_type : t -> response_type
(** [get_type parsert] returns the response type of the parsed
    representation [parsert]. *)

val get_plain : t -> string
(** [get_plain parsert] returns the plain message body of the parsed
    representation of response. *)
