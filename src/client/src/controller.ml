open Database
open Packager
open Parser
open Network
open Util

(** [is_successful status] is true if [status] is between 200 and 299
    inclusive, and false otherwise. *)
let is_successful status = status / 100 = 2

(** memory documentation of the current user*)
let username_ref = ref ""

exception IllegalResponse

(** [header body] is the header containing content-length for the
    [body]. *)
let header body =
  [ ("content-length", body |> String.length |> string_of_int) ]

(** [network_unpack none_ret some_func netop] gets the body of a
    returned [Network.t]. If there is a body, apply [some_func] to the
    body and return the result, otherwise, return [none_ret]*)
let network_unpack none_ret some_func netop =
  match Network.response_body netop with
  | None -> none_ret
  | Some x -> some_func x

(** [current_time ()] is the current time in string format*)
let current_time () = Time.string_of_now true

(** [bool_post_parse raw_response] converts a response [raw_response] to
    post request to a tuple. Returns [(true, msg)] if the request is
    successful, [(false, error_msg)] otherwise*)
let bool_post_parse raw_response =
  let raw_body =
    raw_response |> network_unpack "No Response" (fun x -> x)
  in
  let body = Parser.parse raw_body in
  match Parser.get_type body with
  | ErrorResponse x -> (false, x)
  | GetMsgResponse x -> (false, "don't use bool_post_parse")
  | PostMethResponse x -> (true, x)

(** [fetch_key user] fetches the key for [user] from server*)
let fetch_key user =
  let fetchkey = Packager.pack_fetch_key user in
  let fetch_resp = Network.request "POST" ~body:fetchkey in
  let fetch_success, key = bool_post_parse fetch_resp in
  if not fetch_success then (false, "Unable to find user " ^ user)
  else (true, key)

(** [send_msg_master receiver msg packer msg_type db_meth rel_checker]*)
let send_msg_master receiver msg packer msg_type db_meth =
  let sender = !username_ref in
  let packed_msg = packer sender receiver msg in
  let raw_response = Network.request "POST" ~body:packed_msg in
  let success, resp = bool_post_parse raw_response in
  if success then
    let message =
      Msg.make_msg sender receiver (current_time ()) msg_type msg
    in
    db_meth message |> ignore
  else ();
  (success, resp)

let send_msg receiver msg =
  if !username_ref = "" then (false, "Incorrect user login coredential")
  else if not (is_client receiver) then (false, "Not A Friend")
  else if not (is_frd !username_ref receiver) then
    (false, "You are not friends with " ^ receiver)
  else
    let sender = !username_ref in
    send_msg_master receiver msg Packager.pack_send_msg Message
      (add_msg sender)

let send_gc_msg gc msg =
  if !username_ref = "" then (false, "Incorrect user login coredential")
  else if not (is_gc !username_ref gc) then (false, "invalid gc")
  else if not (is_in_gc !username_ref gc !username_ref) then
    (false, "You are not in this groupchat")
  else
    send_msg_master gc msg Packager.pack_send_gc_msg GCMessage
      (add_msg_to_gc !username_ref)

(** [msg_processor receiver msg] Processes the incoming messages*)
let msg_processor receiver msg =
  let ptext = Msg.content msg in
  (* let _ = print_endline "got there" in *)
  let decrypt =
    Msg.make_msg (Msg.sender msg) (Msg.receiver msg) (Msg.time msg)
      (Msg.msg_type msg) ptext
  in
  (let msg_type = Msg.msg_type msg in
   match msg_type with
   | Message -> add_msg receiver decrypt |> ignore
   | FriendReqRep (accepted, key) ->
       let from = Msg.sender msg in
       update_request receiver from accepted |> ignore
   | FriendReq ->
       let sender = Msg.sender msg in
       let success, key = fetch_key sender in
       let add_key = if not success then None else Some key in
       add_request receiver decrypt add_key None |> ignore
   | GCMessage -> add_msg_to_gc !username_ref msg |> ignore
   | GCRequest -> failwith "GCRequest Shouldn't be received by a client"
   | GCReqRep b ->
       if b then add_member_gc (Msg.sender msg) receiver |> ignore
       else ());
  decrypt

let members_of_gc gcid is_command =
  if "" = !username_ref then (false, [ "Must log in first" ])
  else if not (is_gc !username_ref gcid) then
    (false, [ "Invalid groupchat" ])
  else if
    (not (is_in_gc !username_ref gcid !username_ref)) && is_command
  then (false, [ "You are not in this groupchat" ])
  else
    let json = Packager.pack_fetch_gcmem !username_ref gcid in
    let fetch_resp = Network.request "POST" ~body:json in
    let fetch_success, lststring = bool_post_parse fetch_resp in
    if not fetch_success then (false, [ " Fetch failed " ])
    else
      let total_lst = Parser.to_str_list lststring in
      if is_gc !username_ref gcid then
        let local_lst = members_of_gc !username_ref gcid in
        let add_to_local =
          List.filter (fun x -> not (List.mem x local_lst)) total_lst
        in
        add_member_gc !username_ref gcid add_to_local |> ignore
      else add_groupchat !username_ref gcid total_lst |> ignore;
      (true, total_lst)

(** [update_member_of_gc gcid] updates the local groupchat [gcid] with
    the list of members on server*)
let update_member_of_gc gcid =
  let _ = members_of_gc gcid false in
  ()

let update_msg ?(amount = "unread") () =
  if "" = !username_ref then (false, [])
  else
    let receiver = !username_ref in
    let request = Packager.pack_get_msg receiver amount in
    let raw_response = Network.request "POST" ~body:request in
    let body_type =
      raw_response
      |> network_unpack "No Response" (fun x -> x)
      |> Parser.parse |> Parser.get_type
    in
    match body_type with
    | ErrorResponse x -> (false, [])
    | PostMethResponse x -> raise IllegalResponse
    | GetMsgResponse lst ->
        (*processes the fetched messages*)
        let new_lst = List.map (msg_processor receiver) lst in
        (true, new_lst)

let register username password =
  let message = Packager.pack_register username password "" in
  let raw_response = Network.request "POST" ~body:message in
  let successful, resp = bool_post_parse raw_response in
  if successful then
    (username_ref := username;
     init_dbs () |> ignore;
     create_dbs username "")
    |> ignore
  else ();
  (successful, resp)

let login username password =
  let message = Packager.pack_login username password in
  let raw_response = Network.request "POST" ~body:message in
  let process_func raw_body' =
    match raw_body' |> Parser.parse |> Parser.get_type with
    | ErrorResponse x -> (false, [ Msg.make_msg "" "" "" Message x ])
    | GetMsgResponse x -> raise IllegalResponse
    | PostMethResponse x ->
        username_ref := username;
        init_dbs () |> ignore;
        create_dbs username "" |> ignore;
        (* update messages*)
        let success, messages =
          if is_client username then update_msg ()
          else update_msg ~amount:"2022-03-29 17:00:00" ()
        in
        (*update gc members*)
        List.iter update_member_of_gc (gc_of_user username);
        let login_notification =
          Msg.make_msg "" "" "" Message "Login Successful"
        in
        (success, login_notification :: messages)
  in
  raw_response |> network_unpack (true, []) process_func

let logout () =
  username_ref := "";
  "You have logged out"

let friend_req receiver msg =
  if "" = !username_ref then (false, "User Not Logged in")
  else if !username_ref = receiver then (false, "Friend Request to self")
  else
    let fetch_success, key = fetch_key receiver in
    if not fetch_success then (false, "Unable to find user " ^ receiver)
    else
      let sender = !username_ref in
      let message = Packager.pack_friend_req sender receiver msg in
      let raw_response = Network.request "POST" ~body:message in
      let success, resp = bool_post_parse raw_response in
      let req =
        Msg.make_msg sender receiver (current_time ()) FriendReq msg
      in
      (* add_friend request to database*)
      if success then add_request sender req (Some key) None |> ignore
      else ();
      (* updates messages*)
      update_msg () |> ignore;
      (success, resp)

let friend_req_reply receiver accepted =
  if "" = !username_ref then (false, "User not logged in")
  else if is_frd !username_ref receiver then
    (false, "You are already friends with " ^ receiver)
  else
    let sender = !username_ref in
    let message =
      Packager.pack_friend_req_reply sender receiver accepted
    in
    let raw_response = Network.request "POST" ~body:message in
    let successful, resp = bool_post_parse raw_response in
    if successful then update_request sender receiver accepted |> ignore
    else ();
    (successful, resp)

let join_gc gc password =
  if "" = !username_ref then (false, "User not logged in")
  else
    let sender = !username_ref in
    let message = Packager.pack_join_gc sender gc password in
    let raw_response = Network.request "POST" ~body:message in
    let successful, resp = bool_post_parse raw_response in
    if successful then
      let _, memlst = members_of_gc gc false in
      let _ = add_groupchat !username_ref gc memlst in
      let _ = update_member_of_gc gc in
      let _ = update_msg () in
      (successful, "You have successfully joined the groupchat")
    else (successful, resp)

(** [incorrect_usermsg] is the returner when if a user is incorrect*)
let incorrect_usermsg =
  [
    Msg.make_msg "server" !username_ref "2022-03-29 17:00:00" Message
      "Incorrect User";
  ]

(** [not_friends_msg] is the returner if two clients are not friends*)
let not_friends_msg =
  [
    Msg.make_msg "friend check" !username_ref "2022-03-29 17:00:00"
      Message "Not Friends";
  ]

let read_msg () =
  if "" = !username_ref then (false, incorrect_usermsg)
  else
    let username = !username_ref in
    match get_all_msgs_since username "2022-03-29 17:00:00" with
    | messages -> (true, List.rev messages)

let read_msg_from sender =
  if "" = !username_ref then (false, incorrect_usermsg)
  else if not (is_frd !username_ref sender) then (false, not_friends_msg)
  else
    let username = !username_ref in
    match
      get_msgs_by_frd_since username sender "2022-03-29 17:00:00"
    with
    | messages -> (true, List.rev messages)

(**[invalid_gc_msg gc msg] is a error message using [msg] with
   associated with groupchat [gc]*)
let invalid_gc_msg gc msg =
  [ Msg.make_msg gc !username_ref (current_time ()) GCMessage msg ]

let read_gc_msg gc =
  if "" = !username_ref then (false, incorrect_usermsg)
  else if not (is_gc !username_ref gc) then
    (false, invalid_gc_msg gc "Invalid Groupchat")
  else if not (is_in_gc !username_ref gc !username_ref) then
    (false, invalid_gc_msg gc "You are not in this group chat")
  else
    let username = !username_ref in
    match get_msg_gc_since username gc "2022-03-29 17:00:00" with
    | messages -> (true, List.rev messages)

let read_fr () =
  if "" = !username_ref then (false, incorrect_usermsg)
  else
    let username = !username_ref in
    match get_all_reqs username with
    | messages ->
        ( true,
          List.filter
            (fun x -> not (is_frd (Msg.sender x) (Msg.receiver x)))
            messages )

let lst_of_friends () =
  if "" = !username_ref then (false, [ "User Not Logged In" ])
  else
    let lst = get_all_frds !username_ref in
    (true, lst)

let lst_of_gc () =
  if "" = !username_ref then (false, [ "User not logged in" ])
  else
    let lst = gc_of_user !username_ref in
    (true, lst)

let create_groupchat id password =
  if "" = !username_ref then (false, "Must Log in first")
  else
    let json = Packager.pack_create_gc !username_ref id password in
    let raw_response = Network.request "POST" ~body:json in
    let successful, resp = bool_post_parse raw_response in
    if not successful then (false, resp)
    else
      let _ = create_groupchat !username_ref id in
      (successful, resp)

let current_user () =
  if !username_ref = "" then None else Some !username_ref
