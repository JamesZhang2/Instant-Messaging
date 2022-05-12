type msg_type =
  | FriendReq
  | FriendReqRep of (bool * string)
  | Message
  | GCMessage
  | GCRequest
  | GCReqRep of bool

type t = {
  sender : string;
  receiver : string;
  time : string;
  msg_type : msg_type;
  content : string;
}

let make_msg sender receiver time msg_type content =
  { sender; receiver; time; msg_type; content }

let sender msg = msg.sender
let receiver msg = msg.receiver
let time msg = msg.time
let msg_type msg = msg.msg_type
let content msg = msg.content

let string_of_msg msg =
  let sender = msg.sender in
  let receiver = msg.receiver in
  let time = msg.time in
  let content = msg.content in
  let msg_type_str =
    match msg.msg_type with
    | Message -> "Message"
    | FriendReq -> "Friend Request"
    | FriendReqRep (accepted, key) ->
        if accepted then
          "Friend Request Reply: Accepted with key " ^ key
        else "Friend Request Reply: Rejected"
    | GCMessage -> "Groupchat Message"
    | GCRequest -> "Groupchat Request"
    | GCReqRep accepted ->
        "Friend Request Reply: "
        ^ if accepted then "Accepted" else "Rejected"
  in
  Printf.sprintf
    "{sender: %s, receiver: %s, time: %s, msg_type: %s, content: %s}"
    sender receiver time msg_type_str content
