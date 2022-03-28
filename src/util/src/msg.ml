type msg_type =
  | FriendReq
  | FriendReqRep
  | Message

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
