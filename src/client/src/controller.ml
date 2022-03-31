open Database
open Packager
open Parser
open Network
open Util

(** [is_successful status] is true if [status] is between 200 and 299
    inclusive, and false otherwise. *)
let is_successful status = status / 100 = 2

(** It is the memory location that stores the current key of user*)
let key_ref = ref (Crypto.sym_gen ())

(** memory documentation of the current user*)
let username_ref = ref ""

(** For debug purposes *)
let use_encryption = true

exception IllegalResponse

(** [header body] is the header containing content-length for the
    [body]. *)
let header body =
  [ ("content-length", body |> String.length |> string_of_int) ]

(** [option_unpack op] unpacks an option into its string*)
let option_unpack op =
  match op with
  | None -> ""
  | Some x -> x

(** [bool_post_parse raw_response] converts a response [raw_response] to
    post request to a tuple. Returns [(true, msg)] if the request is
    successful, [(false, error_msg)] otherwise*)
let bool_post_parse raw_response =
  let raw_body =
    raw_response |> Network.response_body |> option_unpack
  in
  (* if not status then (false, "Network Error") else *)
  let body = Parser.parse raw_body in
  match Parser.get_type body with
  | ErrorResponse x -> (false, x)
  | GetMsgResponse x -> (false, "don't use bool_post_parse")
  | PostMethResponse x -> (true, x)
(* let message = Parser.get_plain body in (Parser.get_type body,
   message) *)

(** [fetch_key user] fetches the key for [user] from server*)
let fetch_key user =
  let fetchkey = Packager.pack_fetch_key user in
  let fetch_resp = Network.request "POST" ~body:fetchkey in
  let fetch_success, key = bool_post_parse fetch_resp in
  if not fetch_success then (false, "Unable to find user " ^ user)
  else (true, key)

(** [db_op meth input] does the database operation [meth] until a
    success is returned. *)
let db_op meth input =
  let success, resp = meth input in
  if success then () else print_endline resp

let send_msg receiver msg =
  if !username_ref = "" then (false, "Incorrect user login credential")
  else
    let sender = !username_ref in
    let encrypted_msg =
      if use_encryption then Util.Crypto.(sym_enc (sym_gen ()) msg)
      else msg
    in
    let packed_msg =
      Packager.pack_send_msg sender receiver encrypted_msg
    in
    let raw_response = Network.request "POST" ~body:packed_msg in
    let success, resp = bool_post_parse raw_response in
    (* if message sent sucessfully, add message to database. *)
    if success then
      let message =
        Msg.make_msg sender receiver
          (Time.string_of_now true)
          Message msg
      in
      db_op (add_msg sender) message
    else ();
    (success, resp)

(** [msg_processor receiver msg] Processes the incoming messages*)
let msg_processor receiver msg =
  let ptext = Crypto.sym_dec !key_ref (Msg.content msg) in
  let decrypt =
    Msg.make_msg (Msg.sender msg) receiver (Msg.time msg)
      (Msg.msg_type msg) ptext
  in
  (let msg_type = Msg.msg_type msg in
   match msg_type with
   | Message -> db_op (add_msg receiver) decrypt
   | FriendReqRep (approve, key) ->
       let from = Msg.content msg in
       db_op (update_request receiver from) approve
   | FriendReq ->
       let sender = Msg.sender msg in
       let success, key = fetch_key sender in
       let add_key = if not success then None else Some key in
       db_op (add_request receiver decrypt add_key) None);
  decrypt

let update_msg ?(amount = "unread") () =
  if "" = !username_ref then (false, [])
  else
    let receiver = !username_ref in
    let request = Packager.pack_get_msg receiver amount in
    let raw_response = Network.request "POST" ~body:request in
    let raw_body =
      match Network.response_body raw_response with
      | None -> "No Response Message"
      | Some msg -> msg
    in
    let body = Parser.parse raw_body in
    match Parser.get_type body with
    | ErrorResponse x -> (false, [])
    | PostMethResponse x -> raise IllegalResponse
    | GetMsgResponse lst ->
        (*processes the fetched messages*)
        let new_lst = List.map (msg_processor receiver) lst in
        (true, new_lst)

let register username password =
  let crypto = Util.Crypto.sym_gen () in
  let key = crypto |> Util.Crypto.get_pub_str in
  let message = Packager.pack_register username password key in
  let raw_response = Network.request "POST" ~body:message in
  let successful, resp = bool_post_parse raw_response in
  if successful then (
    let _ = key_ref := crypto in
    username_ref := username;
    db_op init_dbs ();
    db_op (create_dbs username) (Crypto.get_pub_str crypto))
  else ();
  (successful, resp)

let login username password =
  let message = Packager.pack_login username password in
  let raw_response = Network.request "POST" ~body:message in
  let raw_body = Network.response_body raw_response in
  match raw_body with
  | None -> (true, [])
  | Some raw_body' -> (
      match raw_body' |> Parser.parse |> Parser.get_type with
      | ErrorResponse x -> (false, [ Msg.make_msg "" "" "" Message x ])
      | GetMsgResponse x -> raise IllegalResponse
      | PostMethResponse x ->
          key_ref := Crypto.pub_from_str x;
          username_ref := username;
          db_op init_dbs ();
          db_op (create_dbs username) (Crypto.get_pub_str !key_ref);
          let success, messages =
            if is_client username then update_msg ()
            else update_msg ~amount:"2022-03-29 17:00:00" ()
            (* hard coded time: TODO change later*)
          in
          let login_notification =
            print_endline "get there";
            Msg.make_msg "" "" "" Message "Login Successful"
          in
          (success, login_notification :: messages))

let logout () =
  username_ref := "";
  "You have logged out"

let friend_req receiver msg =
  if "" = !username_ref then (false, "User Not Logged in")
    (* else if isFriend !username_ref receiver then (true, "Already
       Friends") *)
    (*TODO: put back in later*)
  else
    let fetch_success, key = fetch_key receiver in
    if not fetch_success then (false, "Unable to find user " ^ receiver)
    else
      let sender = !username_ref in
      let encrypt = Crypto.sym_enc (Crypto.pub_from_str key) msg in
      let message = Packager.pack_friend_req sender receiver encrypt in
      let raw_response = Network.request "POST" ~body:message in
      let success, resp = bool_post_parse raw_response in
      let req =
        Msg.make_msg sender receiver
          (Time.string_of_now true)
          FriendReq msg
      in
      let _ =
        if success then
          let _ = add_request sender req (Some key) None in
          ()
        else ()
      in
      (success, resp)

let friend_req_reply receiver accepted =
  if "" = !username_ref then (false, "User not logged in")
  else
    let sender = !username_ref in
    let message =
      Packager.pack_friend_req_reply sender receiver accepted
    in
    let raw_response = Network.request "POST" ~body:message in
    let successful, resp = bool_post_parse raw_response in
    if successful then
      if accepted then
        let _ = update_request sender receiver true in
        (successful, resp)
      else
        let _ = update_request sender receiver false in
        (successful, resp)
    else (successful, resp)

let incorrect_usermsg =
  [
    Msg.make_msg "server" !username_ref "2022-03-29 17:00:00" Message
      "Incorrect User";
  ]

let read_msg () =
  if "" = !username_ref then (false, incorrect_usermsg)
  else
    let username = !username_ref in
    match get_all_msgs_since username "2022-03-29 17:00:00" with
    | exception IncorrectUser -> (false, incorrect_usermsg)
    | messages -> (true, messages)

let read_msg_from sender =
  if "" = !username_ref then (false, incorrect_usermsg)
  else
    let username = !username_ref in
    match
      get_msgs_by_frd_since username sender "2022-03-29 17:00:00"
    with
    | exception IncorrectUser -> (false, incorrect_usermsg)
    | messages -> (true, messages)

let read_FR () =
  if "" = !username_ref then (false, incorrect_usermsg)
  else
    let username = !username_ref in
    match get_all_reqs username with
    | exception IncorrectUser -> (false, incorrect_usermsg)
    | messages -> (true, messages)

let lst_of_friends () =
  if "" = !username_ref then (false, [ "User Not Logged In" ])
  else
    let lst = get_all_frds !username_ref in
    (true, lst)