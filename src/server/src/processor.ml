open Database
open Util

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
    let msg = Msg.make_msg sender receiver time Util.Msg.Message msg in
    match add_msg msg with
    | exception UnknownUser x ->
        Packager.error_response ("User " ^ x ^ " does not exist")
    | exception MalformedTime ->
        Packager.error_response ("Malformed time" ^ time)
    | true -> Packager.post_method_response "Message successfully sent"
    | false -> Packager.error_response "Message send unsuccessful."

let handle_get_msg req_meth receiver time =
  if req_meth <> Post then
    Packager.error_response "GetMessage should use POST method"
  else
    match get_msg receiver with
    | exception UnknownUser x ->
        Packager.error_response ("Unknown User " ^ x)
    | lst -> Packager.get_method_response lst

let handle_register req_meth username time password public_key =
  if req_meth <> Post then
    Packager.error_response "Register should use POST method"
  else
    match add_user username password public_key time with
    | true, x ->
        Packager.post_method_response
          ("Welcome to IM system, " ^ username)
    | false, x ->
        Packager.post_method_response
          "Registration unsuccessful, please try another username"
    | exception MalformedTime ->
        Packager.error_response ("Malformed time " ^ time)

let handle_login req_meth sender time password =
  if req_meth <> Post then
    Packager.error_response "Login should use POST method"
  else if user_exists sender then
    match chk_pwd sender password with
    | true -> Packager.post_method_response (user_key sender)
    | false -> Packager.post_method_response "Incorrect Password"
  else Packager.post_method_response "Incorrect Username: "

(** [fr_approve_msg sender receiver time] sends approval message to
    receiver with sender's key *)
let fr_approve_msg sender receiver time =
  let sender_key = user_key sender in
  let msg_receiver =
    Msg.make_msg sender receiver time
      (FriendReqRep (true, sender_key))
      ""
  in
  add_msg msg_receiver

let handle_friend_req req_meth sender time receiver msg =
  if req_meth <> Post then
    Packager.error_response "FriendReq should use POST method"
  else
    match is_friend sender receiver with
    (* check already friends exists*)
    | exception UnknownUser x ->
        Packager.error_response ("Unknown User " ^ x)
    | true ->
        Packager.error_response
          ("You are already friends with" ^ receiver)
    | false -> (
        match (fr_exist receiver sender, fr_exist sender receiver) with
        (* Check if friend request already exist*)
        | exception UnknownUser x ->
            Packager.error_response ("Unknown User " ^ x)
        | true, false ->
            (* if reverse fr exist*)
            let _ = fr_approve receiver sender in
            let _ = fr_approve_msg sender receiver time in
            let _ = fr_approve_msg receiver sender time in
            (* send friend req msg to both*)
            Packager.post_method_response
              ("FriendRequest to " ^ receiver ^ " successfully sent")
        | false, _ ->
            let msg = Msg.make_msg sender receiver time FriendReq msg in
            (* notification msg*)
            let _ = new_fr msg in
            (*new fr in db*)
            Packager.post_method_response
              ("Your friend request to " ^ receiver ^ " is sent")
        | true, true ->
            Packager.error_response
              ("You are already friends with" ^ receiver))

let handle_friend_req_reply req_meth sender time receiver accepted =
  if req_meth <> Post then
    Packager.error_response "FriendReqReply should use POST method"
  else
    match
      (is_friend sender receiver, fr_exist receiver sender)
      (* check whether sender and receiver are already friends, check if
         a request from receiver to sender exist*)
    with
    | exception UnknownUser x ->
        Packager.error_response ("Unknown User " ^ x)
    | true, _ ->
        Packager.post_method_response
          ("You are already friends with" ^ receiver)
    | false, true -> (
        let successful =
          if accepted then fr_approve receiver sender
          else fr_reject receiver sender
        in
        match (successful, accepted) with
        | false, _ ->
            Packager.error_response
              ("Operation Unsuccessful, friend request from" ^ receiver
             ^ "still pending")
        | true, true ->
            let _ = fr_approve receiver sender in
            let _ = fr_approve_msg receiver sender time in
            Packager.post_method_response (user_key receiver)
        | true, false ->
            let _ = fr_reject receiver sender in
            let msg_receiver =
              Msg.make_msg sender receiver time
                (FriendReqRep (false, ""))
                ("Friend Request to " ^ receiver ^ " rejected")
            in
            let _ = add_msg msg_receiver in
            Packager.post_method_response
              ("You rejected " ^ receiver
             ^ "'s friend request succesfully "))
    | false, false -> Packager.error_response "No such friend request"

(** [parse req_meth body] parses the body [body] with request method
    [req_meth] and returns a Lwt.t of the resulting type [t]*)
let parse req_meth body =
  let parsed_body = Parser.parse body in
  let sender = Parser.sender parsed_body in
  let time = Parser.time parsed_body in
  let res_body =
    match Parser.pkt_type parsed_body with
    | SendMessage (receiver, msg) ->
        handle_send_msg req_meth sender time receiver msg
    | GetMessage -> handle_get_msg req_meth sender time
    | Register (password, key) ->
        handle_register req_meth sender time password key
    | Login password -> handle_login req_meth sender time password
    | FriendReq (receiver, msg) ->
        handle_friend_req req_meth sender time receiver msg
    | FriendReqReply (receiver, accepted) ->
        handle_friend_req_reply req_meth sender time receiver accepted
  in
  let res_headers = header res_body in
  { status = "201"; headers = res_headers; body = res_body }
  |> Lwt.return

let handle meth (headers : (string * string) list) (body : string Lwt.t)
    =
  let req_meth =
    match meth with
    | "POST" -> Post
    | "GET" -> Get
    | m -> raise (UnknownMethod m)
  in
  Lwt.bind body (parse req_meth)

let status res = res.status |> Lwt.return
let response_body res = res.body |> Lwt.return
let status_body res = (res.status, res.body) |> Lwt.return
let response_headers res = res.headers
