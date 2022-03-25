open Packager
open Parser
open Network

type msg = {
  sender : string;
  receiver : string;
  time : string;
  body : string;
}

(** [is_successful status] is true if [status] is between 200 and 299
    inclusive, and false otherwise. *)
let is_successful status = status / 100 = 2

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

let send_msg sender receiver msg =
  let encrypted_msg =
    if use_encryption then Util.Crypto.(sym_enc (sym_gen ()) msg)
    else msg
  in
  let packed_msg =
    Packager.pack_send_msg sender receiver encrypted_msg
  in
  let raw_response = Network.request "POST" ~body:packed_msg in
  bool_post_parse raw_response

(** [parser_msg_controller receiver msg] is the controller msg
    representation of the parser [msg] type *)
let parser_msg_controller receiver msg =
  {
    sender = Parser.msg_sender msg;
    receiver;
    time = Parser.msg_time msg;
    body =
      (let unpacked_msg = Parser.msg_plain msg in
       if use_encryption then
         Util.Crypto.(sym_dec (sym_gen ()) unpacked_msg)
       else unpacked_msg);
  }

let get_msg receiver =
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
  | GetMsgResponse lst ->
      (true, List.map (parser_msg_controller receiver) lst)

let register username password =
  let message = Packager.pack_register username password in
  let raw_response = Network.request "POST" ~body:message in
  bool_post_parse raw_response

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
      | PostMethResponse x -> (true, x))

let friend_req sender receiver msg =
  let message = Packager.pack_friend_req sender receiver msg in
  let raw_response = Network.request "POST" ~body:message in
  bool_post_parse raw_response

let friend_req_reply sender receiver accepted =
  let message =
    Packager.pack_friend_req_reply sender receiver accepted
  in
  let raw_response = Network.request "POST" ~body:message in
  bool_post_parse raw_response
