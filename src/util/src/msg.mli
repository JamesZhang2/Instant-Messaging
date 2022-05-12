(** A module for messages sent and received by users (including direct
    messages and friend requests). *)

(** [msg_type] is the type of a message. It can either be a [FriendReq]
    or a direct [Message], or a [FriendReqRep (true, key)], or a
    [FriendReqRep (false, "")]. For [FriendReqRep], the sender
    accepts/rejects the friend request from receiver. *)
type msg_type =
  | FriendReq
  | FriendReqRep of (bool * string)
    (* Whether the request is accepted. If accepted, key string;
       otherwise, empty string. *)
  | Message (* Direct message *)
  | GCMessage (* Groupchat message *)
  | GCRequest (* Groupchat request *)
  | GCReqRep of bool (* Whether the groupchat request is accepted. *)

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

val string_of_msg : t -> string
(** [string_of_msg msg] is a string representation of a message. *)
