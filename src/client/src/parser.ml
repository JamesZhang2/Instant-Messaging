open Yojson.Basic

exception SyntaxError

type message_type =
  | Message of string
  | FriendReq of string

type msg = {
  sender : string;
  time : string;
  message : message_type;
}

type response_type =
  | GetMethResponse of msg list
  | PostMethResponse of string
  | ErrorResponse of string

type t = {
  time : string;
  response : response_type;
}

let rec parse_messages msg_list =
  match msg_list with
  | [] -> []
  | h :: t ->
      let assoc = Yojson.Basic.Util.to_assoc h in

      let util x = List.assoc x assoc |> Yojson.Basic.Util.to_string in
      let sender = util "sender" in
      let msg_type = util "msg_type" in
      let time = util "time" in
      let message = util "message" in
      let complete_type =
        if msg_type = "Message" then Message message
        else FriendReq message
      in
      { sender; time; message = complete_type } :: parse_messages t

let parse json =
  let _ = print_endline json in
  let conversion = Yojson.Basic.from_string json in
  let largest = Yojson.Basic.Util.to_assoc conversion in
  let res_type =
    largest |> List.assoc "type" |> Yojson.Basic.Util.to_string
  in
  let time =
    largest |> List.assoc "time" |> Yojson.Basic.Util.to_string
  in
  let body_t = largest |> List.assoc "message" in
  if res_type = "Post" then
    let msg = Yojson.Basic.Util.to_string body_t in
    { time; response = PostMethResponse msg }
  else if res_type = "Get" then
    let json_list = Yojson.Basic.Util.to_list body_t in
    let body = parse_messages json_list in
    { time; response = GetMethResponse body }
  else if res_type = "Error" then
    let msg = Yojson.Basic.Util.to_string body_t in
    { time; response = ErrorResponse msg }
  else raise SyntaxError

let get_type t = t.response
let get_time t = t.time
let msg_body msg = msg.message
let msg_sender msg = msg.sender
let msg_time (msg : msg) = msg.time

let msg_type msg =
  match msg.message with
  | Message _ -> "Message"
  | FriendReq _ -> "FriendReq"

let msg_plain msg =
  match msg.message with
  | Message x -> x
  | FriendReq x -> x

let get_plain t =
  match t.response with
  | GetMethResponse x -> "List of Messages"
  | ErrorResponse x -> x
  | PostMethResponse x -> x
