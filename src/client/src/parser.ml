open Util

exception SyntaxError

type response_type =
  | GetMsgResponse of Msg.t list
  | PostMethResponse of string
  | ErrorResponse of string

type t = {
  time : string;
  response : response_type;
}

(** [fr_rep_tup turns msg into a tup]*)
let fr_rep_tup msg =
  let accepted = String.get msg 0 in
  let key = String.sub msg 1 (String.length msg - 1) in
  (accepted = 'T', key)

let rec parse_messages msg_list =
  match msg_list with
  | [] -> []
  | h :: t ->
      let assoc = Yojson.Basic.Util.to_assoc h in
      let util str =
        List.assoc str assoc |> Yojson.Basic.Util.to_string
      in
      let sender = util "sender" in
      let receiver = util "receiver" in
      let msg_type = util "msg_type" in
      let time = util "time" in
      let message = util "message" in
      let complete_msg =
        if msg_type = "Message" then
          Msg.make_msg sender receiver time Msg.Message message
        else if msg_type = "FriendReq" then
          Msg.make_msg sender receiver time Msg.FriendReq message
        else
          Msg.make_msg sender receiver time
            (Msg.FriendReqRep (fr_rep_tup message))
            ""
      in
      complete_msg :: parse_messages t

let parse json =
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
  else if res_type = "GetMsg" then
    let json_list = Yojson.Basic.Util.to_list body_t in
    let body = parse_messages json_list in
    { time; response = GetMsgResponse body }
  else if res_type = "Error" then
    let msg = Yojson.Basic.Util.to_string body_t in
    { time; response = ErrorResponse msg }
  else raise SyntaxError

let get_type t = t.response

let get_plain t =
  match t.response with
  | GetMsgResponse x -> "List of Messages"
  | ErrorResponse x -> x
  | PostMethResponse x -> x

let to_str_list str =
  str |> Yojson.Basic.from_string |> Yojson.Basic.Util.to_list
  |> List.map Yojson.Basic.to_string
