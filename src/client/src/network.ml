type t
(** The abstract type of response from the server. *)

type req_method =
  | Post
  | Get

let request = failwith "Unimplemented"
let status = failwith "Unimplemented"
let response_body = failwith "Unimplemented"
let response_header = failwith "Unimplemented"
