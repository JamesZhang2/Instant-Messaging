(** This module parses a json string to an internal type that represents
    a packet of information. The client uses this module to pack
    requests into json strings. The server uses this module to parse
    json strings into packets. *)

type t
(** Represents a packet of information. *)

exception SyntaxError of string

type pkt_type =
  | SendMessage of string * string (* Receiver, Message *)
  | GetMessage
  | Registration of string (* Password, (TODO: add public key later) *)
  | Login of string (* Password *)
  | FriendReq of string * string (* Receiver, Message *)
  | FriendReqReply of bool * string (* Accept or Reject, Receiver *)
  | Error
(* TODO: Do we need this? *)

val parse : string -> t
(** [parse json] is the packet of information parsed from [json].
    Raises: [SyntaxError] if [json] is Malformed. *)

val pkt_type : t -> pkt_type
(** [pkt_type pkt] is the type of the packet [pkt]. *)

val sender : t -> string
(** [sender pkt] is the user name that sent the packet [pkt]. *)

val time : t -> string
(** [time pkt] is the time that [pkt] is sent. *)

val body : t -> string
(** [body pkt] is the body of [pkt]. *)
