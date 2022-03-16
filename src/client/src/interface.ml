open Controller

module Command = struct
  exception Malformed

  type parameters = string list

  type command =
    | SendMsg of parameters
    | GetMsg of parameters
    | Register of parameters
    | Login of parameters
    | FriendReq of parameters
    | FriendReqRep of parameters

  (** [parse str] Parses a string command into command type*)
  let parse str =
    let str_list = String.split_on_char ' ' str in
    match str_list with
    | [] -> raise Malformed
    | [ h ] -> raise Malformed
    | h :: t -> (
        match h with
        | "SendMsg" -> SendMsg t
        | "GetMsg" -> GetMsg t
        | "Register" -> Register t
        | "Login" -> Login t
        | "FriendReq" -> FriendReq t
        | "FriendReqReply" -> FriendReqRep t
        | _ -> raise Malformed)
end

let bool_print check =
  if check then "Request sent successfully" else "Request failed"

(**[send_msg lst] takes the parameters in [lst] and sends the message to
   the server and convert result to a print statement*)
let send_msg lst =
  let first, fstlist = (List.hd lst, List.tl lst) in
  let second, sndlist = (List.hd fstlist, List.tl fstlist) in
  let third = List.hd sndlist in
  Controller.send_msg first second third

(** [get_msg str] sends a get request for all messages to be received by
    receiver [str]*)
let get_msg str = Controller.get_msg str

(** [register lst] takes the parameters in [lst] and registers a user
    using this information*)
let register lst =
  let first, fstlist = (List.hd lst, List.tl lst) in
  let second = List.hd fstlist in
  Controller.register first second

(** [login lst] takes the parameters in [lst] and logins a user using
    this information*)
let login lst =
  let first, fstlist = (List.hd lst, List.tl lst) in
  let second = List.hd fstlist in
  Controller.login first second

(**[friend_req lst] takes the parameters in [lst] and sends the
   friend_request to the server*)
let friend_req lst =
  let first, fstlist = (List.hd lst, List.tl lst) in
  let second, sndlist = (List.hd fstlist, List.tl fstlist) in
  let third = List.hd sndlist in
  Controller.friend_req first second third

(**[friend_req lst] takes the parameters in [lst] and sends the
   friend_request to the server and converts to a print statement*)
let friend_rep lst =
  let first, fstlist = (List.hd lst, List.tl lst) in
  let second, sndlist = (List.hd fstlist, List.tl fstlist) in
  let third = if List.hd sndlist = "true" then true else false in
  Controller.friend_req_reply first second third

let run = failwith "Unimplemented"
