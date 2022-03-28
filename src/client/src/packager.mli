(** Packages operations from the Controller into json strings so that
    they can be sent to the server. *)

val pack_send_msg : string -> string -> string -> string
(** [pack_send_msg sender receiver msg] is the json string that encodes
    sending [msg] from [sender] to [receiver]. *)

val pack_get_msg : string -> string
(** [pack_get_msg sender] is the json string that encodes getting all
    messages that are sent to [sender]. *)

val pack_register : string -> string -> string -> string
(** [pack_register username password key] is the json string that
    encodes registering a user with [username], [password], and public
    [key]. *)

val pack_login : string -> string -> string
(** [pack_login username password] is the json string that encodes
    logging in to user [username] with [password]. *)

val pack_friend_req : string -> string -> string -> string
(** [friend_req sender receiver msg] is the json string that encodes
    sending a friend request from [sender] to [receiver] with message
    [msg]. *)

val pack_friend_req_reply : string -> string -> bool -> string
(** [pack_friend_req_reply sender receiver accepted] is the json string
    that encodes accepting or rejecting a friend request from [receiver]
    to [sender]. *)
