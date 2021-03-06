open Yojson.Basic
open Yojson.Basic.Util

exception SyntaxError of string

type pkt_type =
  | SendMessage of string * string
  | GetMessage of string
  | Register of string * string
  | Login of string
  | FriendReq of string * string
  | FriendReqReply of string * bool
  | FetchKey of string
  | FetchGCMem of string
  | SendGCMsg of string * string
  | GCReq of string * string
  | CreateGC of string * string * string

type t = {
  pkt_type : pkt_type;
  sender : string;
  time : string;
}

(** [get_str_val j property] is the string value associated with
    [property] in [j]. Requires: [j] contains [property] and its
    corresponding value is a string. *)
let get_str_val j property = j |> member property |> to_string

(** [parse_common j] is a packet initialized with the sender and time
    from j, but pkt_type is set to a dummy value and should be replaced.
    Requires: [j] must contain sender and time. *)
let parse_common j =
  {
    pkt_type =
      GetMessage "str" (* dummy value, should be replaced later *);
    sender = get_str_val j "sender";
    time = get_str_val j "time";
  }

(** [parse_send_msg j] parses a package whose type is "SendMessage". The
    next few functions are similar. *)
let parse_send_msg j =
  let receiver = get_str_val j "receiver" in
  let msg = get_str_val j "message" in
  { (parse_common j) with pkt_type = SendMessage (receiver, msg) }

let parse_get_msg j =
  {
    (parse_common j) with
    pkt_type = GetMessage (get_str_val j "message");
  }

let parse_register j =
  let password = get_str_val j "password" in
  let key = get_str_val j "key" in
  { (parse_common j) with pkt_type = Register (password, key) }

let parse_login j =
  let password = get_str_val j "password" in
  { (parse_common j) with pkt_type = Login password }

let parse_friend_req j =
  let receiver = get_str_val j "receiver" in
  let msg = get_str_val j "message" in
  { (parse_common j) with pkt_type = FriendReq (receiver, msg) }

let parse_friend_req_reply j =
  let receiver = get_str_val j "receiver" in
  let accepted = j |> member "accepted" |> to_bool in
  {
    (parse_common j) with
    pkt_type = FriendReqReply (receiver, accepted);
  }

let parse_fetch_key j =
  let username = get_str_val j "username" in
  { (parse_common j) with pkt_type = FetchKey username }

let parse_send_gc_msg j =
  let receiver = get_str_val j "receiver" in
  let msg = get_str_val j "message" in
  { (parse_common j) with pkt_type = SendGCMsg (receiver, msg) }

let parse_gc_req j =
  let receiver = get_str_val j "receiver" in
  let msg = get_str_val j "message" in
  { (parse_common j) with pkt_type = GCReq (receiver, msg) }

let parse_gc_mem j =
  let receiver = get_str_val j "receiver" in
  { (parse_common j) with pkt_type = FetchGCMem receiver }

let parse_create_gc j =
  let gc = get_str_val j "receiver" in
  let creator = get_str_val j "sender" in
  let pass = get_str_val j "message" in
  { (parse_common j) with pkt_type = CreateGC (creator, gc, pass) }

let parse json =
  let j = from_string json in
  let type' = get_str_val j "type" in
  match type' with
  | "SendMessage" -> parse_send_msg j
  | "GetMessage" -> parse_get_msg j
  | "Register" -> parse_register j
  | "Login" -> parse_login j
  | "FriendReq" -> parse_friend_req j
  | "FriendReqReply" -> parse_friend_req_reply j
  | "FetchKey" -> parse_fetch_key j
  | "GCRequest" -> parse_gc_req j
  | "GCMessage" -> parse_send_gc_msg j
  | "FetchMem" -> parse_gc_mem j
  | "CreateGC" -> parse_create_gc j
  | _ -> raise (SyntaxError "parse")

let pkt_type pkt = pkt.pkt_type
let sender pkt = pkt.sender
let time pkt = pkt.time
