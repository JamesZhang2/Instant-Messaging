(** [pack prop value] is the json string with property [prop] and value
    [value]. *)
let pack prop value = Printf.sprintf "\"%s\": \"%s\"" prop value

(** [pack_type t] is the json string that encodes the type of the
    package. *)
let pack_type t = pack "type" t

(** [pack_sender sender] is the json string that encodes the sender. *)
let pack_sender sender = pack "sender" sender

(** [pack_time ()] is the json string that encodes the current time. *)
let pack_time () = pack "time" (Util.Time.string_of_now ~local:true)

(** [pack_receiver receiver] is the json string that encodes the
    receiver. *)
let pack_receiver receiver = pack "receiver" receiver

let pack_send_msg sender receiver msg =
  Printf.sprintf "{%s,%s,%s,%s,%s}"
    (pack_type "SendMessage")
    (pack_sender sender) (pack_time ())
    (pack_receiver receiver)
    (pack "message" msg)

let pack_get_msg sender amount =
  Printf.sprintf "{%s,%s,%s,%s}"
    (pack_type "GetMessage")
    (pack_sender sender) (pack_time ()) (pack "message" amount)

let pack_register username password key =
  Printf.sprintf "{%s,%s,%s,%s,%s}" (pack_type "Register")
    (pack_sender username) (pack_time ())
    (pack "password" password)
    (pack "key" key)

let pack_login username password =
  Printf.sprintf "{%s,%s,%s,%s}" (pack_type "Login")
    (pack_sender username) (pack_time ())
    (pack "password" password)

let pack_friend_req sender receiver msg =
  Printf.sprintf "{%s,%s,%s,%s,%s}" (pack_type "FriendReq")
    (pack_sender sender) (pack_time ())
    (pack_receiver receiver)
    (pack "message" msg)

let pack_friend_req_reply sender receiver accepted =
  Printf.sprintf "{%s,%s,%s,%s,%s}"
    (pack_type "FriendReqReply")
    (pack_sender sender) (pack_time ())
    (pack_receiver receiver)
    (Printf.sprintf "\"%s\": %B" "accepted" accepted)

let pack_fetch_key username =
  Printf.sprintf "{%s,%s, %s, %s}" (pack_type "FetchKey")
    (pack_sender "unimportant")
    (pack_time ())
    (pack "username" username)
