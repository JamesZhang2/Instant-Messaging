open Yojson.Basic

type pkt_type =
  | Message
  | FriendReq
  | Registration
  | Error

type t = {
  pkt_type : pkt_type;
  sender : string;
  receiver : string option;
  time : string;
  body : string;
}

let parse json = json |> from_string |> failwith "Unimplemented"
let pkt_type pkt = pkt.pkt_type
let sender pkt = pkt.sender
let receiver pkt = pkt.receiver
let time pkt = pkt.time
let body pkt = pkt.body
