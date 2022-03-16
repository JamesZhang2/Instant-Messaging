open Packager
open Parser
open Network

type msg = {
  sender : string;
  receiver : string;
  time : string;
  body : string;
}

exception IllegalResponse

let header body =
  [ ("content-length", body |> String.length |> string_of_int) ]

let send_msg sender receiver msg =
  let msg = Packager.pack_send_msg sender receiver msg in
  let raw_response =
    Network.request Post ~body:msg ~header:(header msg)
  in
  let status = Network.status raw_response in
  status / 100 = 2

(** [parser_msg_controller msg receiver] is the controller msg
    representation of the parser [msg] type *)
let parser_msg_controller receiver msg =
  {
    sender = Parser.msg_sender msg;
    receiver;
    time = Parser.msg_time msg;
    body = Parser.msg_plain msg;
  }

let get_msg receiver =
  let request = Packager.pack_get_msg receiver in
  let raw_response =
    Network.request Get ~body:request ~header:(header request)
  in
  let raw_body =
    match Network.response_body raw_response with
    | None -> "No Response Message"
    | Some msg -> msg
  in
  let body = Parser.parse raw_body in
  match Parser.get_type body with
  | ErrorResponse x -> raise IllegalResponse
  | PostMethResponse x -> raise IllegalResponse
  | GetMethResponse lst -> List.map (parser_msg_controller receiver) lst

let register username password =
  let message = Packager.pack_register username password in
  let request =
    Network.request Post ~body:message ~header:(header message)
  in
  let raw_response = Network.status request in
  raw_response / 100 = 2

let login username password =
  let message = Packager.pack_login username password in
  let request =
    Network.request Post ~body:message ~header:(header message)
  in
  let raw_response = Network.response_body request in
  match raw_response with
  | None -> (true, "")
  | Some raw_body -> (
      match raw_body |> Parser.parse |> Parser.get_type with
      | ErrorResponse x -> (false, x)
      | GetMethResponse x -> raise IllegalResponse
      | PostMethResponse x -> (true, ""))

let friend_req sender receiver msg =
  let message = Packager.pack_friend_req sender receiver msg in
  let request =
    Network.request Post ~body:message ~header:(header message)
  in
  let raw_response = Network.status request in
  raw_response / 100 = 2

let friend_req_reply sender receiver accepted =
  let message =
    Packager.pack_friend_req_reply sender receiver accepted
  in
  let request =
    Network.request Post ~body:message ~header:(header message)
  in
  let status = Network.status request in
  status / 100 = 2
