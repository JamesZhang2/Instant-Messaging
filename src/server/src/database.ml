open Util

(** User database columns:

    - username: string Not NULL
    - password: string not null
    - public key: string not null
    - register date: date not null

    Messages Database columns:

    - sender: string not null
    - receiver: string not null
    - type: string //fr or dm
    - content: string not null
    - time : time not null
    - retrieved: boolean not null *)
let create_user_db () = failwith "Unimplemented"

let add_user username pwd time = failwith "Unimplemented"

type chk_user =
  | UserOK
  | UnknownUser of string
  | WrongPwd of string
  | MalformedUserTime of string

let user_exists username = failwith "Unimplemented"
let chk_pwd username pwd = failwith "Unimplemented"
let del_user = failwith "Unimplemented"
(* TODO: Should we actually delete the row from the database, or should
   we add a column [time deleted]? *)

let create_msg_db () = failwith "Unimplemented"

type chk_msg =
  | MsgOK
  | UnknownSender of string
  | UnknownReceiver of string
  | UnkonwnMsgType of string
  | MalformedMsgTime of string

let add_msg sender receiver time msg_type message =
  failwith "Unimplemented"

let get_msg_since receiver time = failwith "Unimplemented"
