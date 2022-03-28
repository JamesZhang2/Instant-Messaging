open Database
open Packager
open Parser
open Network
open Util

type msg = {
  sender : string;
  receiver : string;
  time : string;
  body : string;
}

(** [is_successful status] is true if [status] is between 200 and 299
    inclusive, and false otherwise. *)
let is_successful status = status / 100 = 2

(** It is the memory locatio that stores the current key of user*)
let key_ref = ref (Crypto.sym_gen ())

(** memory documentation of the current user*)
let username_ref = ref "none"

(** For debug purposes *)
let use_encryption = false

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
  let status = is_successful (Network.status raw_response) in
  let raw_body =
    raw_response |> Network.response_body |> option_unpack
  in
  let body = Parser.parse raw_body in
  let message = Parser.get_plain body in
  (status, message)

(** [db_op meth input] does the database operation [meth] until a
    success is returned. *)
let rec db_op meth input =
  let success, resp = meth input in
  if success then () else db_op meth input

let send_msg sender receiver msg =
  if receiver <> !username_ref then
    (false, "Incorrect user login credential")
  else
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
  (let msg_type = Msg.msg_type msg in
   match msg_type with
   | Message -> db_op (add_msg receiver) msg
   | FriendReqRep (approve, key) ->
       let from = Msg.content msg in
       db_op (update_request receiver from) approve
   | FriendReq -> db_op (add_request receiver msg) None);
  msg

let get_msg receiver =
  if receiver <> !username_ref then (false, [])
  else
    let request = Packager.pack_get_msg receiver in
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
    | GetMsgResponse lst -> (true, lst)

let register username password =
  let crypto = Util.Crypto.sym_gen () in
  let key = crypto |> Util.Crypto.get_pub_str in
  let message = Packager.pack_register username password key in
  let raw_response = Network.request "POST" ~body:message in
  let successful, resp = bool_post_parse raw_response in
  if successful then (
    let _ = key_ref := crypto in
    username_ref := username;
    db_op (create_dbs username) crypto)
  else ();
  (successful, resp)

let login username password =
  let message = Packager.pack_login username password in
  let raw_response = Network.request "POST" ~body:message in
  let raw_body = Network.response_body raw_response in
  match raw_body with
  | None -> (true, "")
  | Some raw_body' -> (
      match raw_body' |> Parser.parse |> Parser.get_type with
      | ErrorResponse x -> (false, x)
      | GetMsgResponse x -> raise IllegalResponse
      | PostMethResponse x ->
          key_ref := Crypto.pub_from_str x;
          username_ref := username;
          (true, x))

let friend_req sender receiver msg =
  if sender <> !username_ref then (false, "User Not Logged in")
  else
    let message = Packager.pack_friend_req sender receiver msg in
    let raw_response = Network.request "POST" ~body:message in
    let success, resp = bool_post_parse raw_response in
    let req =
      Msg.make_msg sender receiver
        (Time.string_of_now true)
        FriendReq msg
    in
    let _ =
      if success then
        let _ = add_request sender req None in
        ()
      else ()
    in
    (success, resp)

let friend_req_reply sender receiver accepted =
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
