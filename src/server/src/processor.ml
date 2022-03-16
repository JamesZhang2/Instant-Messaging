type t = {
  status : string;
  body : string;
  headers : (string * string) list;
}

exception UnknownMethod of string

type req_method =
  | Post
  | Get

(* For debugging *)
let print_headers headers =
  List.map (fun (a, b) -> print_endline (a ^ ": " ^ b)) headers
  |> ignore

(** [header body] is the header containing content-length for the
    [body]. *)
let header body =
  [ ("content-length", body |> String.length |> string_of_int) ]

let handle_send_msg req_meth sender time receiver msg =
  failwith "Unimplemented"

let handle_get_msg req_meth sender time = failwith "Unimplemented"

let handle_register req_meth sender time password =
  failwith "Unimplemented"

let handle_login req_meth sender time password =
  failwith "Unimplemented"

let handle_friend_req req_meth sender time receiver msg =
  failwith "Unimplemented"

let handle_friend_req_reply req_meth sender time receiver accepted =
  failwith "Unimplemented"

let handle meth headers body =
  let req_meth =
    match meth with
    | "POST" -> Post
    | "GET" -> Get
    | m -> raise (UnknownMethod m)
  in
  let parsed_body = Parser.parse body in
  let sender = Parser.sender parsed_body in
  let time = Parser.time parsed_body in
  let res_body =
    match Parser.pkt_type parsed_body with
    | SendMessage (receiver, msg) ->
        handle_send_msg req_meth sender time receiver msg
    | GetMessage -> handle_get_msg req_meth sender time
    | Register password -> handle_register req_meth sender time password
    | Login password -> handle_login req_meth sender time password
    | FriendReq (receiver, msg) ->
        handle_friend_req req_meth sender time receiver msg
    | FriendReqReply (receiver, accepted) ->
        handle_friend_req_reply req_meth sender time receiver accepted
  in
  let res_headers = header res_body in
  { status = "201"; headers = res_headers; body = res_body }

let status res = res.status
let response_body res = res.body
let response_headers res = res.headers
