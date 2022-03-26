let del_user = failwith "Unimplemented"
(* TODO: Should we actually delete the row from the database, or should
   we add a column [time deleted]? *)

type msg_type =
  | FriendReq
  | Message

type msg = {
  sender : string;
  receiver : string;
  time : string;
  msg_type : msg_type;
  message : string;
}