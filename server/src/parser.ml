open Yojson.Basic

exception SyntaxError of string

type pkt_type =
  | SendMessage of string * string (* Receiver, Message *)
  | GetMessage
  | Registration of string (* Password, (TODO: add public key later) *)
  | Login of string (* Password *)
  | FriendReq of string * string (* Receiver, Message *)
  | FriendReqReply of bool * string (* Accept or Reject, Receiver *)
  | Error

type t = {
  pkt_type : pkt_type;
  sender : string;
  time : string;
}

let parse json = json |> from_string |> failwith "Unimplemented"
let pkt_type pkt = pkt.pkt_type
let sender pkt = pkt.sender
let time pkt = pkt.time
