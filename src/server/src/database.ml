(** Users table columns:

    - username: TEXT NOT NULL
    - password: TEXT NOT NULL
    - public_key: TEXT NOT NULL
    - time_registered: TEXT NOT NULL

    Messages table columns:

    - sender: TEXT NOT NULL
    - receiver: TEXT NOT NULL
    - content: TEXT NOT NULL
    - time: TEXT NOT NULL
    - type: TEXT NOT NULL
    - retrieved: BOOLEAN NOT NULL
    - gcid: TEXT NULLABLE

    Friends table columns:

    - requester: TEXT NOT NULL
    - receiver: TEXT NOT NULL

    Groupchats table columns:

    - id: TEXT NOT NULL
    - password: TEXT NOT NULL

    Members table columns:

    - id: TEXT NOT NULL
    - username: TEXT NOT NULL

    AF:

    - userA is said to "like" userB if a row (userA, userB, time,
      message) exists in the friends table. If both userA and userB like
      each other, they are friends. If userA likes userB but userB
      doesn't like userA, then there is a pending friend request from
      userA to userB.
    - If userA sends a message to a groupchat, then we store multiple
      messages in the messages table, one for each member of the
      groupchat, including userA themselves. The message sent from userA
      to userA is marked as retrieved, and each message has gcid set to
      the id of the groupchat.
    - userA is in groupchat gcB if and only if a row (gcB, userA) exists
      in the members table.

    RI:

    - No two users have the same username
    - No two groupchats have the same id
    - In the messages table, gcid is NULL if the message is a direct
      message *)

open Sqlite3
open Util

exception MalformedTime
exception UnknownUser of string
exception UnknownGCID of string
exception NoAccess of string * string

let db_file =
  "data" ^ Filename.dir_sep ^ "database" ^ Filename.dir_sep
  ^ "server.db"

let server_db = db_open db_file

(******************** Helper Functions ********************)

(** [prerr_and_fail rc fail_msg] prints the error message of the
    database and fails with [fail_msg]. *)
let prerr_and_fail (rc : Rc.t) (fail_msg : string) =
  prerr_endline (Rc.to_string rc);
  prerr_endline (errmsg server_db);
  failwith fail_msg

(** [handle_rc ok_msg rc] prints [ok_msg] if the return code [rc] is
    [OK] or [DONE]. Otherwise, it prints helpful error messages. *)
let handle_rc ok_msg = function
  | Rc.OK
  | Rc.DONE ->
      print_endline ok_msg;
      print_newline ()
  | rc -> prerr_and_fail rc "Server.Database: Return code is not OK"

(** [assert_rc rc expected err_msg] asserts that [rc] is [expected]. *)
let assert_rc rc expected =
  if rc <> expected then
    let err_msg =
      "Server.Database: Return code is not " ^ Rc.to_string expected
    in
    prerr_and_fail rc err_msg

(** [assert_rc rc expected err_msg] asserts that [rc] is [OK]. *)
let assert_ok rc = assert_rc rc Rc.OK

(** [assert_rc rc expected err_msg] asserts that [rc] is [ROW]. *)
let assert_row rc = assert_rc rc Rc.ROW

(** [time_ok time] raises [MalformedTime] if [time] is not in the right
    format. Otherwise, it is the identity function. *)
let time_ok time =
  if Time.chk_time time then time else raise MalformedTime

(******************** Create Tables ********************)

(** [create_table sql name] creates a table with the given [sql] if it
    doesn't already exist. *)
let create_table (sql : string) (name : string) =
  exec server_db sql
  |> handle_rc
       (String.capitalize_ascii name
       ^ " table found or successfully created")

let create_users_sql =
  "CREATE TABLE IF NOT EXISTS users (username TEXT NOT NULL, password \
   TEXT NOT NULL, public_key TEXT NOT NULL, time_registered TEXT NOT \
   NULL);"

let create_users_table () = create_table create_users_sql "users"

let create_messages_sql =
  "CREATE TABLE IF NOT EXISTS messages (sender TEXT NOT NULL, receiver \
   TEXT NOT NULL, content TEXT NOT NULL, time TEXT NOT NULL, type TEXT \
   NOT NULL, retrieved BOOLEAN NOT NULL, gcid TEXT);"

let create_messages_table () =
  create_table create_messages_sql "messages"

let create_friends_sql =
  "CREATE TABLE IF NOT EXISTS friends (requester TEXT NOT NULL, \
   receiver TEXT NOT NULL);"

let create_friends_table () = create_table create_friends_sql "friends"

let create_gcs_sql =
  "CREATE TABLE IF NOT EXISTS groupchats (id TEXT NOT NULL, password \
   TEXT NOT NULL);"

let create_gcs_table () = create_table create_gcs_sql "groupchats"

let create_members_sql =
  "CREATE TABLE IF NOT EXISTS members (id TEXT NOT NULL, username TEXT \
   NOT NULL);"

let create_members_table () = create_table create_members_sql "members"

let create_tables () =
  create_users_table ();
  create_messages_table ();
  create_friends_table ();
  create_gcs_table ();
  create_members_table ()

(******************** Print All (Debug) ********************)

(* This has no injection issues because [print_all] is only used
   internally. *)
let select_all_sql table = Printf.sprintf "SELECT * from %s" table

(** [print_option str] prints [s] followed by ["|"] if [str] is
    [Some s]; it prints ["|"] if [str] is [None]. *)
let print_option = function
  | Some s -> print_string (s ^ "|")
  | None -> print_string "|"

(** [print_all_cb] is the callback function for printing a table. *)
let print_all_cb row header =
  Array.iter print_option row;
  print_newline ()

(** [print_all table] prints all the data in [table]. For debugging
    purposes. *)
let print_all table =
  exec server_db ~cb:print_all_cb (select_all_sql table)
  |> handle_rc "All rows printed"

(******************** Add User ********************)

let insert_user_sql =
  "INSERT INTO users (username, password, public_key, time_registered) \
   VALUES (?, ?, ?, ?);"

(** [insert_user username pwd key time] inserts the user into the
    database with the given fields. Requires: [username] does not exist
    in the database. *)
let insert_user username pwd key time =
  let stmt = prepare server_db insert_user_sql in
  bind_values stmt [ TEXT username; TEXT pwd; TEXT key; TEXT time ]
  |> assert_ok;
  step stmt

let user_exists_sql =
  "SELECT EXISTS (SELECT 1 from users WHERE username = ?);"

let user_exists username =
  let stmt = prepare server_db user_exists_sql in
  bind_text stmt 1 username |> assert_ok;
  step stmt |> assert_row;
  column_bool stmt 0

(** [user_ok username] raises [UnknownUser] if [username] is not found
    in the database. Otherwise, it is the identity function. *)
let user_ok username =
  if user_exists username then username
  else raise (UnknownUser username)

let add_ok_str username =
  Printf.sprintf "User %s sucessfully added" username

let add_exists_str username =
  Printf.sprintf "User %s already exists" username

let add_user username pwd key time =
  let time = time_ok time in
  if user_exists username then (
    print_endline (add_exists_str username);
    print_newline ();
    false)
  else (
    insert_user username pwd key time |> handle_rc (add_ok_str username);
    true)

(******************** User key ********************)

let select_key_sql = "SELECT public_key FROM users WHERE username = ?"

let user_key username =
  let username = user_ok username in
  let stmt = prepare server_db select_key_sql in
  bind_text stmt 1 username |> assert_ok;
  step stmt |> assert_row;
  column_text stmt 0

(******************** Check password ********************)

let chk_pwd_sql =
  "SELECT EXISTS (SELECT 1 from users WHERE username = ? AND password \
   = ?);"

let chk_pwd username pwd =
  let username = user_ok username in
  let stmt = prepare server_db chk_pwd_sql in
  bind_values stmt [ TEXT username; TEXT pwd ] |> assert_ok;
  step stmt |> assert_row;
  column_bool stmt 0

(******************** Add message ********************)

let insert_msg_sql =
  "INSERT INTO messages (sender, receiver, content, time, type, \
   retrieved, gcid) VALUES (?, ?, ?, ?, ?, FALSE, ?);"

(** [add_msg_aux msg gcid] adds a message to the messages table.

    Requires:

    - If the message is a direct message, friend request, or friend
      request reply, then [gcid] is [None].
    - If the message is a group message, then [gcid] is [Some id].
    - The sender and receiver fields of [msg] must be existing users in
      the database.
    - [GCRequest] and [GCReqRep] should not be added to the database. *)
let add_msg_aux (msg : Msg.t) (gcid : string option) =
  let msg_type_str =
    match Msg.msg_type msg with
    | Message -> "Message"
    | FriendReq -> "FriendReq"
    | FriendReqRep (bo, key) -> "FriendReqRep"
    | GCMessage -> "GCMessage"
    | GCRequest -> failwith "add_msg_aux: GCRequest shouldn't be added"
    | GCReqRep _ -> failwith "add_msg_aux: GCReqRep shouldn't be added"
  in
  let sender = Msg.sender msg |> user_ok in
  let receiver = Msg.receiver msg |> user_ok in
  let content = Msg.content msg in
  let time = Msg.time msg |> time_ok in
  let maybe_gc =
    match gcid with
    | None -> ""
    | Some id -> " in groupchat " ^ id
  in
  let stmt = prepare server_db insert_msg_sql in
  bind_values stmt
    [
      TEXT sender;
      TEXT receiver;
      TEXT content;
      TEXT time;
      TEXT msg_type_str;
      (match gcid with
      | Some id -> TEXT id
      | None -> NULL);
    ]
  |> assert_ok;
  step stmt
  |> handle_rc
       (Printf.sprintf "%s sent a %s to %s%s at %s: %s" sender
          msg_type_str receiver maybe_gc time content);
  true

let add_msg (msg : Msg.t) =
  match Msg.msg_type msg with
  | Message
  | FriendReq
  | FriendReqRep _ ->
      add_msg_aux msg None
  | GCMessage
  | GCRequest
  | GCReqRep _ ->
      failwith "Precondition violated"

(******************** Get message ********************)

let select_msg_sql =
  "SELECT * FROM messages WHERE receiver = ? AND time > ? ORDER BY \
   time ASC;"

let select_new_msg_sql =
  "SELECT * FROM messages WHERE receiver = ? AND time > ? AND \
   retrieved = FALSE ORDER BY time ASC;"

let mark_retrieved_sql =
  "UPDATE messages SET retrieved = TRUE WHERE receiver = ? AND time > ?"

(** [mark_retrieved receiver time] marks all messages sent to [receiver]
    after [time] as retrieved. Requires: [receiver] is found in the user
    table and [time] is in the correct format. *)
let mark_retrieved receiver time =
  let stmt = prepare server_db mark_retrieved_sql in
  bind_values stmt [ TEXT receiver; TEXT time ] |> assert_ok;
  step stmt
  |> handle_rc
       (if time = Time.earliest_time then
        Printf.sprintf "All messages sent to %s are marked as retrieved"
          receiver
       else
         Printf.sprintf
           "Messages sent to %s after %s are marked as retrieved"
           receiver time)

(** [cons_one_msg lst row] adds [row] to [lst]. Requires: [row]
    represents a valid message. *)
let cons_one_msg lst (row : Data.t array) =
  match (row.(0), row.(1), row.(2), row.(3), row.(4), row.(6)) with
  | ( TEXT sender,
      TEXT receiver,
      TEXT content,
      TEXT time,
      TEXT msg_type_str,
      NULL ) ->
      let msg_type =
        match msg_type_str with
        | "FriendReq" -> Msg.FriendReq
        | "Message" -> Message
        | "FriendReqRep" ->
            if String.get content 0 = 'T' then FriendReqRep (true, "key")
            else FriendReqRep (false, "")
        | _ -> failwith "Error"
      in
      Msg.make_msg sender receiver time msg_type content :: lst
  | ( TEXT sender,
      TEXT receiver,
      TEXT content,
      TEXT time,
      TEXT "GCMessage",
      TEXT gcid ) ->
      Msg.make_msg sender gcid time GCMessage content :: lst
  | _ ->
      failwith
        "Server.Database.cons_one_msg: row is in the wrong format"

(** [print_msg_list lst] prints a list of messages. *)
let print_msg_list (lst : Msg.t list) =
  lst |> List.map Msg.string_of_msg |> List.map print_endline |> ignore

let get_msg_aux receiver time ~new_only =
  let receiver = user_ok receiver in
  let time = time_ok time in
  let sql = if new_only then select_new_msg_sql else select_msg_sql in
  let stmt = prepare server_db sql in
  bind_values stmt [ TEXT receiver; TEXT time ] |> assert_ok;
  let res = Sqlite3.fold stmt ~f:cons_one_msg ~init:[] in
  match res with
  | Rc.DONE, lst ->
      mark_retrieved receiver time;
      if time = Time.earliest_time then
        Printf.printf "Retrieved all messages sent to %s\n\n" receiver
      else
        Printf.printf "Retrieved messages sent to %s after %s\n\n"
          receiver time;
      List.rev lst
  | rc, _ ->
      prerr_and_fail rc
        "Server.Database.get_msg_aux: Return code is not DONE"

let get_msg_since receiver time =
  get_msg_aux receiver time ~new_only:false

let get_msg receiver = get_msg_since receiver Time.earliest_time

let get_new_msg_since receiver time =
  get_msg_aux receiver time ~new_only:true

let get_new_msg receiver = get_new_msg_since receiver Time.earliest_time

(******************** Friend Requests ********************)

let insert_req_sql =
  "INSERT INTO friends (requester, receiver) VALUES (?, ?);"

let new_fr (req : Msg.t) =
  assert (Msg.msg_type req = Msg.FriendReq);
  let sender = Msg.sender req |> user_ok in
  let receiver = Msg.receiver req |> user_ok in
  let stmt = prepare server_db insert_req_sql in
  bind_values stmt [ TEXT sender; TEXT receiver ] |> assert_ok;
  step stmt
  |> handle_rc
       (Printf.sprintf "New friend request from %s to %s" sender
          receiver);
  true

let like_exists_sql =
  "SELECT EXISTS (SELECT 1 from friends WHERE requester = ? AND \
   receiver = ?);"

(** [likes userA userB] is [true] if userA "likes" userB (that is, there
    is a row (userA, userB, time, message) in friends), and [false]
    otherwise. Requires: [userA] and [userB] exist in the table of
    users. *)
let likes userA userB =
  let stmt = prepare server_db like_exists_sql in
  bind_values stmt [ TEXT userA; TEXT userB ] |> assert_ok;
  step stmt |> assert_row;
  column_bool stmt 0

let fr_exist sender receiver =
  let sender = user_ok sender in
  let receiver = user_ok receiver in
  likes sender receiver && not (likes receiver sender)

let is_friend sender receiver =
  let sender = user_ok sender in
  let receiver = user_ok receiver in
  likes sender receiver && likes receiver sender

let accept_req_sql =
  "INSERT INTO friends (requester, receiver) VALUES (?, ?);"

let fr_accept sender receiver =
  let sender = user_ok sender in
  let receiver = user_ok receiver in
  if not (fr_exist sender receiver) then raise Not_found;
  let stmt = prepare server_db accept_req_sql in
  bind_values stmt [ TEXT receiver; TEXT sender ] |> assert_ok;
  (* Note that the receiver and sender are reversed: We want to add a
     line that says receiver likes the sender. *)
  step stmt
  |> handle_rc
       (Printf.sprintf "The friend request from %s to %s is accepted"
          sender receiver);
  true

let reject_req_sql =
  "DELETE FROM friends WHERE requester = ? AND receiver = ?;"

let fr_reject sender receiver =
  let sender = user_ok sender in
  let receiver = user_ok receiver in
  if not (fr_exist sender receiver) then raise Not_found;
  let stmt = prepare server_db reject_req_sql in
  bind_values stmt [ TEXT sender; TEXT receiver ] |> assert_ok;
  (* Deletes the friend request from sender to receiver. *)
  step stmt
  |> handle_rc
       (Printf.sprintf "The friend request from %s to %s is rejected"
          sender receiver);
  true

let select_liked_sql =
  "SELECT receiver FROM friends WHERE requester = ?"

(** [cons_one_text lst row] adds [row] to [lst]. Requires: [row] has
    only one column, which is text. *)
let cons_one_text lst (row : Data.t array) =
  match row.(0) with
  | Data.TEXT username -> username :: lst
  | _ ->
      failwith
        "Server.Database.cons_one_text: row is in the wrong format"

(** [like_list user] is a list of all users that [user] likes. Requires:
    [user] is found in the table of users. *)
let like_list user =
  let stmt = prepare server_db select_liked_sql in
  bind_text stmt 1 user |> assert_ok;
  let res = Sqlite3.fold stmt ~f:cons_one_text ~init:[] in
  match res with
  | Rc.DONE, lst -> lst
  | rc, _ ->
      prerr_and_fail rc
        "Server.Database.like_list: Return code is not DONE"

let friends_of user =
  let user = user_ok user in
  List.filter (fun u -> likes u user) (like_list user)

(******************** Groupchat ********************)

let gc_exists_sql =
  "SELECT EXISTS (SELECT 1 from groupchats WHERE id = ?);"

let gc_exists id =
  let stmt = prepare server_db gc_exists_sql in
  bind_values stmt [ TEXT id ] |> assert_ok;
  step stmt |> assert_row;
  column_bool stmt 0

(** [gc_ok id] raises [UnknownGCID] if [id] is not found in the
    database. Otherwise, it is the identity function. *)
let gc_ok id = if gc_exists id then id else raise (UnknownGCID id)

let is_in_gc_sql =
  "SELECT EXISTS (SELECT 1 from members WHERE id = ? AND username = ?);"

let is_in_gc id username =
  let id = gc_ok id in
  let username = user_ok username in
  let stmt = prepare server_db is_in_gc_sql in
  bind_values stmt [ TEXT id; TEXT username ] |> assert_ok;
  step stmt |> assert_row;
  column_bool stmt 0

let insert_member_sql =
  "INSERT INTO members (id, username) VALUES (?, ?);"

let add_member_gc id new_member =
  let id = gc_ok id in
  let new_member = user_ok new_member in
  if is_in_gc id new_member then false
  else
    let stmt = prepare server_db insert_member_sql in
    bind_values stmt [ TEXT id; TEXT new_member ] |> assert_ok;
    step stmt
    |> handle_rc
         (Printf.sprintf "Added user %s to the groupchat %s" new_member
            id);
    true

let insert_gc_sql =
  "INSERT INTO groupchats (id, password) VALUES (?, ?);"

let create_groupchat id password username =
  let username = user_ok username in
  if gc_exists id then false
  else
    let stmt = prepare server_db insert_gc_sql in
    bind_values stmt [ TEXT id; TEXT password ] |> assert_ok;
    step stmt
    |> handle_rc
         (Printf.sprintf "%s created groupchat %s with password %s"
            username id password);
    add_member_gc id username

let check_gc_pwd_sql =
  "SELECT EXISTS (SELECT 1 from groupchats WHERE id = ? AND password = \
   ?);"

let check_gc_password id password =
  let id = gc_ok id in
  let stmt = prepare server_db check_gc_pwd_sql in
  bind_values stmt [ TEXT id; TEXT password ] |> assert_ok;
  step stmt |> assert_row;
  column_bool stmt 0

(** [access_ok id username] raises [NoAccess] if [username] is not a
    member of [id]. Otherwise, it returns [()]. *)
let access_ok id username =
  if is_in_gc id username then () else raise (NoAccess (id, username))

let select_gcs_sql =
  "SELECT id FROM members WHERE username = ? ORDER BY id ASC;"

let gc_of_user username =
  let username = user_ok username in
  let stmt = prepare server_db select_gcs_sql in
  bind_values stmt [ TEXT username ] |> assert_ok;
  let res = Sqlite3.fold stmt ~f:cons_one_text ~init:[] in
  match res with
  | Rc.DONE, lst ->
      Printf.printf "Retrieved groupchats that user %s is in\n\n"
        username;
      List.rev lst
  | rc, _ ->
      prerr_and_fail rc
        "Server.Database.gc_of_user: Return code is not DONE"

let select_members_sql =
  "SELECT username FROM members WHERE id = ? ORDER BY username ASC;"

let members_of_gc id =
  let id = gc_ok id in
  let stmt = prepare server_db select_members_sql in
  bind_values stmt [ TEXT id ] |> assert_ok;
  let res = Sqlite3.fold stmt ~f:cons_one_text ~init:[] in
  match res with
  | Rc.DONE, lst ->
      Printf.printf "Retrieved members in groupchat %s\n\n" id;
      List.rev lst
  | rc, _ ->
      prerr_and_fail rc
        "Server.Database.members_of_gc: Return code is not DONE"

let mark_self_retrieved_sql =
  "UPDATE messages SET retrieved = TRUE WHERE sender = ? and receiver \
   = ?"

(** [mark_self_retrieved sender] marks all messages from [sender] to
    [sender] (via a groupchat) as retrieved. Requires: [sender] is found
    in the user table. *)
let mark_self_retrieved sender =
  let stmt = prepare server_db mark_self_retrieved_sql in
  bind_values stmt [ TEXT sender; TEXT sender ] |> assert_ok;
  step stmt
  |> handle_rc
       (Printf.sprintf
          "All messages that %s sent to themselves via a groupchat are \
           marked as retrieved"
          sender)

let add_msg_to_gc msg =
  assert (Msg.msg_type msg = GCMessage);
  let sender = Msg.sender msg |> user_ok in
  let gcid = Msg.receiver msg |> gc_ok in
  access_ok gcid sender;
  let content = Msg.content msg in
  let time = Msg.time msg |> time_ok in
  let mems = members_of_gc gcid in
  let msg_list =
    List.map
      (fun member -> Msg.make_msg sender member time GCMessage content)
      mems
  in
  let status_list =
    List.map (fun msg -> add_msg_aux msg (Some gcid)) msg_list
  in
  mark_self_retrieved sender;
  List.fold_left ( && ) true status_list
