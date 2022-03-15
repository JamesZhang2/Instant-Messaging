(** [pack_time ()] is the json string that encodes the current time. *)
let pack_time () =
  "\"time\": \"" ^ Util.Time.string_of_now ~local:true ^ "\""

let pack_send_msg sender receiver msg = failwith "Unimplemented"
let pack_get_msg sender = failwith "Unimplemented"
let pack_register username password = failwith "Unimplemented"
let pack_login username password = failwith "Unimplemented"
let pack_friend_req sender receiver msg = failwith "Unimplemented"

let pack_friend_req_reply sender receiver accepted =
  failwith "Unimplemented"
