(** This module converts a response message from the server into json*)

type msg_type =
  | FriendReq
  | Message
      (** [msg_type] is the type of a message stored on server end. It
          can either be a [FriendReq] or a direct [Message]*)

type msg
(** If the client sends a get request, type that each message will be
    transformed into*)

val make_msg : string -> string -> string -> msg_type -> string -> msg
(**[make_message sender receiver time msg_type message] makes a
   [Packager.msg] type out of the fields *)

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
