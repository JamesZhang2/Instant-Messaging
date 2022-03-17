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
  if req_meth <> Post then
    Packager.error_response "SendMessage should use POST method"
  else
    (* TODO: Put this event into the database *)
    let res =
      Printf.sprintf "%s sent a message to %s at %s: %s\n" sender
        receiver time msg
    in
    print_endline res;
    Packager.post_method_response res

let handle_get_msg req_meth sender time =
  if req_meth <> Post then
    Packager.error_response "GetMessage should use POST method"
  else begin
    (* TODO: Retrieve messages from the database *)
    print_endline
      (Printf.sprintf "%s wants to get their messages at %s\n" sender
         time);
    Packager.get_method_response []
  end

let handle_register req_meth sender time password =
  if req_meth <> Post then
    Packager.error_response "Register should use POST method"
  else
    (* TODO: Put this event into the database *)
    let res =
      Printf.sprintf "%s registers with password %s at %s\n" sender
        password time
    in
    print_endline res;
    Packager.post_method_response res

let handle_login req_meth sender time password =
  if req_meth <> Post then
    Packager.error_response "Login should use POST method"
  else
    (* TODO: Put this event into the database *)
    let res =
      Printf.sprintf "%s logs in with password %s at %s\n" sender
        password time
    in
    print_endline res;
    Packager.post_method_response res

let handle_friend_req req_meth sender time receiver msg =
  if req_meth <> Post then
    Packager.error_response "FriendReq should use POST method"
  else
    (* TODO: Put this event into the database *)
    let res =
      Printf.sprintf "%s wants to friend %s at %s: %s\n" sender receiver
        time msg
    in
    print_endline res;
    Packager.post_method_response res

let handle_friend_req_reply req_meth sender time receiver accepted =
  if req_meth <> Post then
    Packager.error_response "FriendReqReply should use POST method"
  else
    (* TODO: Put this event into the database *)
    let res =
      Printf.sprintf "%s %s the friend request from %s at %s\n" sender
        (if accepted then "accepted" else "rejected")
        receiver time
    in
    print_endline res;
    Packager.post_method_response res

let handle meth headers body =
  if body = "sleep" then
    let res_body =
      Packager.error_response "Server busy, try again later"
    in
    { status = "503"; headers = header res_body; body = res_body }
  else
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
      | Register password ->
          handle_register req_meth sender time password
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
