type msg_dir =
  | Sent
  | Received

exception MalformedTime
exception IncorrectUser
exception DBNotExist

let is_client = failwith "Unimplemented"
let init_dbs = failwith "Unimplemented"
let create_dbs = failwith "Unimplemented"
let add_request = failwith "Unimplemented"
let update_request = failwith "Unimplemented"
let add_msg = failwith "Unimplemented"
let get_all_reqs = failwith "Unimplemented"
let get_all_frds = failwith "Unimplemented"
let get_all_msgs = failwith "Unimplemented"
let get_all_msgs_since = failwith "Unimplemented"
let get_msgs_by_frd = failwith "Unimplemented"
let get_msgs_by_frd_since = failwith "Unimplemented"
let get_req_by_name = failwith "Unimplemented"
let isFriend = failwith "Unimplemented"
let isInRequest = failwith "Unimplemented"
