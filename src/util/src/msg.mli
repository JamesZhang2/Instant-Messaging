(** A module for messages sent and received by users (including direct
    messages and friend requests). *)

(** [msg_type] is the type of a message. It can either be a [FriendReq]
    or a direct [Message]*)
type msg_type =
  | FriendReq
  | FriendReqRep
  | Message

type t
(** Abstract type for a message. *)

val make_msg : string -> string -> string -> msg_type -> string -> t
(**[make_msg sender receiver time msg_type message] creates a message
   according to the given fields. *)

val sender : t -> string
(** [sender msg] is the user that sends the message. *)

val receiver : t -> string
(** [receiver msg] is the user that receives the message. *)

val time : t -> string
(** [time msg] is the time that the message is sent. *)

val msg_type : t -> msg_type
(** [msg_type msg] is the type of the message. *)

val content : t -> string
(** [content msg] is the content of the message. *)
