(** This module parses a json string to an internal type that represents
    a packet of information. *)

type t
(** Represents a packet of information. *)

type pkt_type =
  | Message
  | FriendReq
  | Registration
  | Error

val parse : string -> t
(** [parse json] is the packet of information parsed from [json]. *)

val pkt_type : t -> pkt_type
(** [pkt_type pkt] is the type of the packet [pkt]. *)

val sender : t -> string
(** [sender pkt] is the user name that sent the packet [pkt]. *)

val receiver : t -> string option
(** [receiver pkt] is [Some user] if [user] is the user name of the
    indended receiver, or [None] if there is no receiver. *)

val time : t -> string
(** [time pkt] is the time that [pkt] is sent. *)

val body : t -> string
(** [body pkt] is the body of [pkt]. *)
