(** This module processes a response body from the server*)

type t
(** Abstract type representing a reponse from *)

type response_type =
  | GetMethResponse of string
  | PostMethResponse of {
      sender : string;
      time : string;
      message : string;
    }  (** Represents the type of data that the response is for. *)

val parse : string -> t
(** [parse json] returns the representation of the message that the json
    entails*)

val get_type : t -> response_type
(** [get_type parsert] returns the response type of the parsed
    representation [parsert]*)
