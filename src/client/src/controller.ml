open Database
open Packager
open Parser
open Network
open Util

(** [is_successful status] is true if [status] is between 200 and 299
    inclusive, and false otherwise. *)
let is_successful status = status / 100 = 2

(** It is the memory location that stores the current key of user*)
let key_ref = ref (Crypto.sym_gen ())

(** memory documentation of the current user*)
let username_ref = ref ""

(** For debug purposes *)
let use_encryption = false

exception IllegalResponse

(** [header body] is the header containing content-length for the
    [body]. *)
let header body =
  [ ("content-length", body |> String.length |> string_of_int) ]

(** [option_unpack op] unpacks an option into its string*)
let option_unpack op =
  match op with
  | None -> ""
  | Some x -> x

(** [bool_post_parse raw_response] converts a response [raw_response] to
    post request to a tuple. Returns [(true, msg)] if the request is
    successful, [(false, error_msg)] otherwise*)
let bool_post_parse raw_response =
  let raw_body =
    raw_response |> Network.response_body |> option_unpack
  in
  (* if not status then (false, "Network Error") else *)
  let body = Parser.parse raw_body in
  match Parser.get_type body with
  | ErrorResponse x -> (false, x)
  | GetMsgResponse x -> (false, "don't use bool_post_parse")
  | PostMethResponse x -> (true, x)
(* let message = Parser.get_plain body in (Parser.get_type body,
   message) *)

(** [fetch_key user] fetches the key for [user] from server*)
let fetch_key user =
  let fetchkey = Packager.pack_fetch_key user in
  let fetch_resp = Network.request "POST" ~body:fetchkey in
  let fetch_success, key = bool_post_parse fetch_resp in
  if not fetch_success then (false, "Unable to find user " ^ user)
  else (true, key)

(** [db_op meth input] does the database operation [meth] until a
    success is returned. *)
let db_op meth input =
  let _ = meth input in
  ()
(* if success then () else print_endline ("Error" ^ resp) *)

(** [encrypt key msg] encrypts the message using the given symmetric
    [key]*)
let encrypt key msg =
  if use_encryption then Util.Crypto.(sym_enc key msg) else msg

(** [send_msg_master receiver msg packer msg_type db_meth rel_checker]*)
let send_msg_master receiver msg packer msg_type db_meth rel_checker =
  let sender = !username_ref in
  if not (rel_checker receiver sender) then
    (false, "You are not authorized to send message to " ^ receiver)
  else
    let encrypted_msg = encrypt (Util.Crypto.sym_gen ()) msg in
    let packed_msg = packer sender receiver encrypted_msg in
    let raw_response = Network.request "POST" ~body:packed_msg in
    let success, resp = bool_post_parse raw_response in
    if success then
      let message =
        Msg.make_msg sender receiver
          (Time.string_of_now true)
          msg_type msg
      in
      db_op db_meth message
    else ();
    (success, resp)

let send_msg receiver msg =
  if !username_ref = "" then (false, "Incorrect user login coredential")
  else
    let sender = !username_ref in
    send_msg_master receiver msg Packager.pack_send_msg Message
      (add_msg sender) is_frd
(* if !username_ref = "" then (false, "Incorrect user login credential")
   else let sender = !username_ref in if not (is_frd sender receiver)
   then (false, "You are not friends with " ^ receiver) else let
   encrypted_msg = if use_encryption then Util.Crypto.(sym_enc (sym_gen
   ()) msg) else msg in let packed_msg = Packager.pack_send_msg sender
   receiver encrypted_msg in let raw_response = Network.request "POST"
   ~body:packed_msg in let success, resp = bool_post_parse raw_response
   in (* if message sent sucessfully, add message to database. *) if
   success then let message = Msg.make_msg sender receiver
   (Time.string_of_now true) Message msg in db_op (add_msg sender)
   message else (); (success, resp) *)

let send_gc_msg gc msg =
  if !username_ref = "" then (false, "Incorrect user login coredential")
  else if not (is_gc !username_ref gc) then (false, "invalid gc")
  else
    send_msg_master gc msg Packager.pack_send_gc_msg GCMessage
      (add_msg_to_gc !username_ref)
      (is_in_gc !username_ref)
(* if !username_ref = "" then (false, "Incorrect user login credential")
   else let sender = !username_ref in if not (is_in_gc gc sender) then
   (false, "You are not in this groupchat") else let encrypted_msg = if
   use_encryption then Util.Crypto.(sym_enc (sym_gen ()) msg) else msg
   in let packed_msg = Packager.pack_send_gc_msg sender gc encrypted_msg
   in let raw_response = Network.request "POST" ~body:packed_msg in let
   success, resp = bool_post_parse raw_response in if success then let
   message = Msg.make_msg sender gc (Time.string_of_now true) GCMessage
   msg in db_op add_msg_to_gc message else (); (success, resp) *)

(** [msg_processor receiver msg] Processes the incoming messages*)
let msg_processor receiver msg =
  let ptext =
    if use_encryption then
      try Crypto.sym_dec !key_ref (Msg.content msg) with
      | x -> "message"
    else Msg.content msg
  in
  (* let _ = print_endline "got there" in *)
  let decrypt =
    Msg.make_msg (Msg.sender msg) receiver (Msg.time msg)
      (Msg.msg_type msg) ptext
  in
  (let msg_type = Msg.msg_type msg in
   match msg_type with
   | Message -> db_op (add_msg receiver) decrypt
   | FriendReqRep (accepted, key) ->
       let from = Msg.sender msg in
       db_op (update_request receiver from) accepted
   | FriendReq ->
       let sender = Msg.sender msg in
       let success, key = fetch_key sender in
       let add_key = if not success then None else Some key in
       db_op (add_request receiver decrypt add_key) None
   | GCMessage -> db_op (add_msg_to_gc !username_ref) msg
   | GCRequest -> failwith "GCRequest Shouldn't be received by a client"
   | GCReqRep b ->
       if b then db_op (add_member_gc (Msg.sender msg)) receiver else ());
  decrypt

let members_of_gc gcid =
  if "" = !username_ref then (false, [ "Must log in first" ])
  else
    let json = Packager.pack_fetch_gcmem !username_ref gcid in
    let fetch_resp = Network.request "POST" ~body:json in
    let fetch_success, lststring = bool_post_parse fetch_resp in
    if not fetch_success then (false, [ " Fetch failed " ])
    else
      let total_lst = Parser.to_str_list lststring in
      (if is_gc !username_ref gcid then
       let local_lst = members_of_gc !username_ref gcid in
       let add_to_local =
         List.filter (fun x -> not (List.mem x local_lst)) total_lst
       in
       let _ = add_member_gc !username_ref gcid add_to_local in
       ()
      else
        let _ = add_groupchat !username_ref gcid total_lst in
        ());
      (true, total_lst)

(** [update_member_of_gc gcid] updates the local groupchat [gcid] with
    the list of members on server*)
let update_member_of_gc gcid =
  let _ = members_of_gc gcid in
  ()

let update_msg ?(amount = "unread") () =
  if "" = !username_ref then (false, [])
  else
    let receiver = !username_ref in
    let request = Packager.pack_get_msg receiver amount in
    let raw_response = Network.request "POST" ~body:request in
    let raw_body =
      match Network.response_body raw_response with
      | None -> "No Response Message"
      | Some msg -> msg
    in
    (* let _ = print_endline raw_body in *)
    let body = Parser.parse raw_body in
    match Parser.get_type body with
    | ErrorResponse x -> (false, [])
    | PostMethResponse x -> raise IllegalResponse
    | GetMsgResponse lst ->
        (*processes the fetched messages*)
        let new_lst = List.map (msg_processor receiver) lst in
        (true, new_lst)

let register username password =
  let crypto = Util.Crypto.sym_gen () in
  let key = crypto |> Util.Crypto.get_pub_str in
  let message = Packager.pack_register username password key in
  let raw_response = Network.request "POST" ~body:message in
  let successful, resp = bool_post_parse raw_response in
  if successful then (
    let _ = key_ref := crypto in
    username_ref := username;
    db_op init_dbs ();
    db_op (create_dbs username) (Crypto.get_pub_str crypto))
  else ();
  (successful, resp)

let login username password =
  let message = Packager.pack_login username password in
  let raw_response = Network.request "POST" ~body:message in
  let raw_body = Network.response_body raw_response in
  match raw_body with
  | None -> (true, [])
  | Some raw_body' -> (
      (* print_endline raw_body'; *)
      match raw_body' |> Parser.parse |> Parser.get_type with
      | ErrorResponse x -> (false, [ Msg.make_msg "" "" "" Message x ])
      | GetMsgResponse x -> raise IllegalResponse
      | PostMethResponse x ->
          key_ref := Crypto.pub_from_str x;
          username_ref := username;
          db_op init_dbs ();
          db_op (create_dbs username) (Crypto.get_pub_str !key_ref);
          (* update messages*)
          let success, messages =
            if is_client username then update_msg ()
            else update_msg ~amount:"2022-03-29 17:00:00" ()
            (* hard coded time: TODO change later*)
          in
          (*update gc members*)
          let _ = List.iter update_member_of_gc (gc_of_user username) in
          let login_notification =
            (* print_endline "get there"; *)
            Msg.make_msg "" "" "" Message "Login Successful"
          in
          (success, login_notification :: messages))

let logout () =
  username_ref := "";
  "You have logged out"

let friend_req receiver msg =
  if "" = !username_ref then (false, "User Not Logged in")
  else if !username_ref = receiver then
    (false, "Cannot send friend request to yourself")
    (* else if isFriend !username_ref receiver then (true, "Already
       Friends") *)
    (*TODO: put back in later*)
  else
    let fetch_success, key = fetch_key receiver in
    if not fetch_success then (false, "Unable to find user " ^ receiver)
    else
      let sender = !username_ref in
      let encrypt =
        encrypt (Crypto.pub_from_str key) msg
        (* if use_encryption then Crypto.sym_enc (Crypto.pub_from_str
           key) msg else msg *)
      in
      let message = Packager.pack_friend_req sender receiver encrypt in
      let raw_response = Network.request "POST" ~body:message in
      let success, resp = bool_post_parse raw_response in
      let req =
        Msg.make_msg sender receiver
          (Time.string_of_now true)
          FriendReq msg
      in
      let _ =
        if success then
          let _ = add_request sender req (Some key) None in
          ()
        else ()
      in
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
    if successful then
      if accepted then
        let _ = update_request sender receiver true in
        (successful, resp)
      else
        let _ = update_request sender receiver false in
        (successful, resp)
    else (successful, resp)

let join_gc gc password =
  if "" = !username_ref then (false, "User not logged in")
  else
    let sender = !username_ref in
    let message = Packager.pack_join_gc sender gc password in
    let raw_response = Network.request "POST" ~body:message in
    let successful, resp = bool_post_parse raw_response in
    if successful then
      let _ = update_member_of_gc gc in
      let _ = update_msg () in
      (successful, "You have successfully joined the groupchat")
    else (successful, resp)

let incorrect_usermsg =
  [
    Msg.make_msg "server" !username_ref "2022-03-29 17:00:00" Message
      "Incorrect User";
  ]

let read_msg () =
  if "" = !username_ref then (false, incorrect_usermsg)
  else
    let username = !username_ref in
    match get_all_msgs_since username "2022-03-29 17:00:00" with
    (* | exception IncorrectUser -> (false, incorrect_usermsg) *)
    | messages -> (true, List.rev messages)

let read_msg_from sender =
  if "" = !username_ref then (false, incorrect_usermsg)
  else
    let username = !username_ref in
    match
      get_msgs_by_frd_since username sender "2022-03-29 17:00:00"
    with
    (* | exception IncorrectUser -> (false, incorrect_usermsg) *)
    | messages -> (true, List.rev messages)

let read_gc_msg gc =
  if "" = !username_ref then (false, incorrect_usermsg)
  else if not (is_in_gc !username_ref gc !username_ref) then
    ( false,
      [
        Msg.make_msg gc !username_ref
          (Time.string_of_now true)
          GCMessage "Not in groupchat";
      ] )
  else
    let username = !username_ref in
    match get_msg_gc_since username gc "2022-03-29 17:00:00" with
    (* | exception IncorrectUser -> (false, incorrect_usermsg) *)
    | messages -> (true, List.rev messages)

let read_fr () =
  if "" = !username_ref then (false, incorrect_usermsg)
  else
    let username = !username_ref in
    match get_all_reqs username with
    (* | exception IncorrectUser -> (false, incorrect_usermsg) *)
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
      let _ = db_op (create_groupchat !username_ref) id in
      (successful, resp)

let current_user () =
  if !username_ref = "" then None else Some !username_ref
