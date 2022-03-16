open Packager
open Parser
open Network

type msg = {
  sender : string;
  receiver : string;
  time : string;
  body : string;
}

exception EmptyBody

let send_msg sender receiver msg =
  let msg = Packager.pack_send_msg sender receiver msg in
  let raw_response = Network.request "Post" ~body:msg ~header:[] in
  let raw_body = Network.response_body raw_response in
  let status = Network.status raw_response in
  (* let body = *)
  status / 100 = 2
(* in let response = body |> Parser.parse |> Parser.get_plain in
   response *)

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
  let raw_response = Network.request "Get" ~body:request ~header:[] in
  let raw_body =
    match Network.response_body raw_response with
    | None -> "No Response Message"
    | Some msg -> msg
  in
  let body = Parser.parse raw_body in
  match Parser.get_type body with
  | ErrorResponse x -> raise EmptyBody
  | PostMethResponse x -> raise EmptyBody
  | GetMethResponse lst -> List.map (parser_msg_controller receiver) lst

let register = failwith "Unimplemented"
let login = failwith "Unimplemented"
let friend_req = failwith "Unimplemented"
let friend_req_reply = failwith "Unimplemented"
