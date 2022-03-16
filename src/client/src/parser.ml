open Yojson.Basic

exception SyntaxError

type msg = {
  purpose : string;
  sender : string;
  time : string;
  message : string;
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
      let purp = "purpose" in
      let util x = List.assoc x assoc |> Yojson.Basic.Util.to_string in
      let sender = util "sender" in
      let time = util "time" in
      let message = util "message" in
      { purpose = purp; sender; time; message } :: parse_messages t

let parse json =
  let conversion = Yojson.Basic.from_string json in
  let largest = Yojson.Basic.Util.to_assoc conversion in
  let res_type =
    largest |> List.assoc "type" |> Yojson.Basic.Util.to_string
  in
  let body_t = largest |> List.assoc "message" in
  if res_type = "Post" then
    let msg = Yojson.Basic.Util.to_string body_t in
    { time = "unimplemented"; response = PostMethResponse msg }
  else if res_type = "Get" then
    let json_list = Yojson.Basic.Util.to_list body_t in
    let body = parse_messages json_list in
    { time = "Unimplemented"; response = GetMethResponse body }
  else if res_type = "Error" then
    let msg = Yojson.Basic.Util.to_string body_t in
    { time = "unimplemented"; response = ErrorResponse msg }
  else raise SyntaxError

let get_type t = t.response
let msg_type msg = msg.purpose
let msg_sender msg = msg.sender
let get_time t = t.time
let msg_body msg = msg.message
