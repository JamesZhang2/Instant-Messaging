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
  (* let _ = print_endline msg in *)
  if req_meth <> Post then
    (* let _ = print_endline "if branch" in *)
    Packager.error_response "SendMessage should use POST method"
  else
    (* let _ = print_endline "else branch" in *)
    let msg = Msg.make_msg sender receiver time Util.Msg.Message msg in
    if add_msg msg then
      Packager.post_method_response "Message successfully sent"
    else
      Packager.error_response
        "Message can't be sent, please try again later"

let handle_get_msg req_meth receiver time amount =
  if req_meth <> Post then
    Packager.error_response "GetMessage should use POST method"
  else
    let func =
      if amount = "unread" then fun _ -> get_new_msg receiver
      else fun _ -> get_msg_since receiver amount
    in
    Packager.get_method_response (func ())

let handle_register req_meth username time password public_key =
  if req_meth <> Post then
    Packager.error_response "Register should use POST method"
  else if add_user username password public_key time then
    Packager.post_method_response
      ("Welcome to the IM system, " ^ username)
  else
    Packager.error_response
      "Registration unsuccessful, please try another username"

let handle_login req_meth sender time password =
  if req_meth <> Post then
    Packager.error_response "Login should use POST method"
  else if not (user_exists sender) then
    Packager.error_response "The specified user does not exist"
  else if not (chk_pwd sender password) then
    Packager.error_response "Incorrect Password"
  else Packager.post_method_response (user_key sender)

(** [add_fr_accept_msg sender receiver time] creates an approval message
    to receiver with sender's key and adds the message to the database. *)
let add_fr_accept_msg sender receiver time =
  let sender_key = "" in
  let msg_receiver =
    Msg.make_msg sender receiver time
      (FriendReqRep (true, sender_key))
      "True"
  in
  add_msg msg_receiver

let handle_friend_req req_meth sender time receiver msg =
  if req_meth <> Post then
    Packager.error_response "FriendReq should use POST method"
  else if
    (* Check if sender and receiver are already friends *)
    is_friend sender receiver
  then
    Packager.error_response ("You are already friends with " ^ receiver)
  else
    (* Check if friend request already exists *)
    match (fr_exist receiver sender, fr_exist sender receiver) with
    | true, false ->
        (* reverse fr exists *)
        let _ = fr_accept receiver sender in
        let _ = add_fr_accept_msg sender receiver time in
        (* send friend req msg to receiver*)
        Packager.post_method_response
          ("Your friend request to " ^ receiver
         ^ " is sent and accepted")
    | false, _ ->
        let msg = Msg.make_msg sender receiver time FriendReq msg in
        (* notification msg *)
        let _ = new_fr msg in
        let _ = add_msg msg in
        (* new fr in db *)
        Packager.post_method_response
          ("Your friend request to " ^ receiver ^ " is sent")
    | true, true ->
        let _ = add_fr_accept_msg sender receiver time in
        let _ = add_fr_accept_msg receiver sender time in
        Packager.error_response ("You are now friends with" ^ receiver)

(** [handle_friend_req_reply req_meth sender time receiver accepted]*)
let handle_friend_req_reply req_meth sender time receiver accepted =
  if req_meth <> Post then
    Packager.error_response "FriendReqReply should use POST method"
  else
    match
      (is_friend sender receiver, fr_exist receiver sender)
      (* check whether sender and receiver are already friends, check if
         a request from receiver to sender exist*)
    with
    | true, _ ->
        (* print_endline "already friend branch"; *)
        Packager.post_method_response
          ("You are already friends with " ^ receiver)
    | false, true -> (
        (* print_endline "fr branch"; *)
        let successful =
          if accepted then fr_accept receiver sender
          else fr_reject receiver sender
        in
        match (successful, accepted) with
        | false, _ ->
            Packager.error_response
              ("Operation Unsuccessful, friend request from" ^ receiver
             ^ "still pending")
        | true, true ->
            (* let _ = fr_accept_msg receiver sender time in *)
            let _ = add_fr_accept_msg sender receiver time in
            (* print_endline "got there 2"; *)
            Packager.post_method_response
              ("You are now friends with " ^ receiver)
        | true, false ->
            let msg_receiver =
              Msg.make_msg sender receiver time
                (FriendReqRep (false, ""))
                ("Friend Request to " ^ receiver ^ " rejected")
            in
            let _ = add_msg msg_receiver in
            Packager.post_method_response
              ("You rejected " ^ receiver
             ^ "'s friend request succesfully "))
    | false, false ->
        (* print_endline "no fr branch"; *)
        Packager.error_response "No such friend request"

let handle_fetch_key username =
  let key = user_key username in
  Packager.post_method_response key

(** [parse req_meth body] parses the body [body] with request method
    [req_meth] and returns a Lwt.t of the resulting type [t]*)
let parse req_meth body =
  let parsed_body = Parser.parse body in
  let sender = Parser.sender parsed_body in
  let time = Parser.time parsed_body in
  let get_res_body () =
    match Parser.pkt_type parsed_body with
    | SendMessage (receiver, msg) ->
        handle_send_msg req_meth sender time receiver msg
    | GetMessage message -> handle_get_msg req_meth sender time message
    | Register (password, key) ->
        handle_register req_meth sender time password key
    | Login password -> handle_login req_meth sender time password
    | FriendReq (receiver, msg) ->
        handle_friend_req req_meth sender time receiver msg
    | FriendReqReply (receiver, accepted) ->
        handle_friend_req_reply req_meth sender time receiver accepted
    | FetchKey username -> handle_fetch_key username
  in
  let res_body =
    match get_res_body () with
    | exception UnknownUser x ->
        Packager.error_response ("Unknown user " ^ x)
    | exception MalformedTime ->
        Packager.error_response ("Malformed time " ^ time)
    | exception x -> Packager.error_response "Unknown exception"
    | response -> response
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
