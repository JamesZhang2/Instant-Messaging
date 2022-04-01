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

    Friends table columns:

    - requester: TEXT NOT NULL
    - receiver: TEXT NOT NULL

    AF: userA is said to "like" userB if a row (userA, userB, time,
    message) exists. If both userA and userB like each other, they are
    friends. If userA likes userB but userB doesn't like userA, then
    there is a pending friend request from userA to userB.

    RI: No two users have the same username. *)

open Sqlite3
open Util

exception MalformedTime
exception UnknownUser of string

let test = false

let db_file =
  "data" ^ Filename.dir_sep ^ "database" ^ Filename.dir_sep
  ^ if test then "test.db" else "server.db"

let server_db = db_open db_file

(** [handle_rc ok_msg rc] prints [ok_msg] if the return code [rc] is
    [OK] or [DONE]. Otherwise, it prints helpful error messages. *)
let handle_rc ok_msg = function
  | Rc.OK
  | Rc.DONE ->
      print_endline ok_msg;
      print_newline ()
  | r ->
      prerr_endline (Rc.to_string r);
      prerr_endline (errmsg server_db);
      failwith "Server.Database: Return code is not OK"

(** [assert_rc rc expected err_msg] asserts that [rc] is [expected]. *)
let assert_rc rc expected =
  if rc <> expected then (
    prerr_endline (Rc.to_string rc);
    prerr_endline (errmsg server_db);
    let err_msg =
      "Server.Database: Return code is not " ^ Rc.to_string expected
    in
    failwith err_msg)

let assert_ok rc = assert_rc rc Rc.OK
let assert_row rc = assert_rc rc Rc.ROW

(** [time_ok time] raises [MalformedTime] if [time] is not in the right
    format. Otherwise, it is the identity function. *)
let time_ok time =
  if Time.chk_time time then time else raise MalformedTime

(******************** Create Tables ********************)

let create_users_sql =
  "CREATE TABLE IF NOT EXISTS users (username TEXT NOT NULL, password \
   TEXT NOT NULL, public_key TEXT NOT NULL, time_registered TEXT NOT \
   NULL);"

let create_users_table () =
  exec server_db create_users_sql
  |> handle_rc "Users table found or successfully created"

let create_messages_sql =
  "CREATE TABLE IF NOT EXISTS messages (sender TEXT NOT NULL, receiver \
   TEXT NOT NULL, content TEXT NOT NULL, time TEXT NOT NULL, type TEXT \
   NOT NULL, retrieved BOOLEAN NOT NULL);"

let create_messages_table () =
  exec server_db create_messages_sql
  |> handle_rc "Messages table found or successfully created"

let create_friends_sql =
  "CREATE TABLE IF NOT EXISTS friends (requester TEXT NOT NULL, \
   receiver TEXT NOT NULL);"

let create_friends_table () =
  exec server_db create_friends_sql
  |> handle_rc "Friends table found or successfully created"

let create_tables () =
  create_users_table ();
  create_messages_table ();
  create_friends_table ()

(******************** Print All (Debug) ********************)

(* This has no injection issues because [print_all] is only used
   internally. *)
let select_all_sql table = Printf.sprintf "SELECT * from %s" table

let print_option = function
  | Some s -> print_string (s ^ "|")
  | None -> print_string "|"

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
    (false, add_exists_str username))
  else (
    insert_user username pwd key time |> handle_rc (add_ok_str username);
    (true, add_ok_str username))

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
   retrieved) VALUES (?, ?, ?, ?, ?, FALSE);"

let add_msg (msg : Msg.t) =
  (* assert (Msg.msg_type msg = Msg.Message); *)
  let ty =
    match Msg.msg_type msg with
    | Message -> "Message"
    | FriendReq -> "FriendReq"
    | FriendReqRep (bo, key) -> "FriendReqRep"
  in
  let sender = Msg.sender msg |> user_ok in
  let receiver = Msg.receiver msg |> user_ok in
  let content = Msg.content msg in
  let time = Msg.time msg |> time_ok in
  let stmt = prepare server_db insert_msg_sql in
  bind_values stmt
    [ TEXT sender; TEXT receiver; TEXT content; TEXT time; TEXT ty ]
  |> assert_ok;
  step stmt
  |> handle_rc
       (Printf.sprintf "%s sent a %s to %s at %s: %s" sender ty receiver
          time content);
  true

(******************** Get message ********************)

let select_msg_sql =
  "SELECT * FROM messages WHERE receiver = ? AND time > ? ORDER BY \
   time ASC;"

let select_new_msg_sql =
  "SELECT * FROM messages WHERE receiver = ? AND time > ? AND \
   retrieved = FALSE ORDER BY time ASC;"

let mark_as_retrieved_sql =
  "UPDATE messages SET retrieved = TRUE WHERE receiver = ? AND time > ?"

(** [mark_as_retrieved receiver time] marks all messages sent to
    [receiver] after [time] as retrieved. Requires: [receiver] is found
    in the user table and [time] is in the correct format. *)
let mark_as_retrieved receiver time =
  let stmt = prepare server_db mark_as_retrieved_sql in
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
  match (row.(0), row.(1), row.(2), row.(3), row.(4)) with
  | ( Data.TEXT sender,
      Data.TEXT receiver,
      Data.TEXT content,
      Data.TEXT time,
      Data.TEXT msg_type ) ->
      let msg_t =
        match msg_type with
        | "FriendReq" -> Msg.FriendReq
        | "Message" -> Message
        | "FriendReqRep" ->
            (* let _ = print_endline content in *)
            let bo = String.get content 0 = 'T' in
            FriendReqRep (bo, "T")
        | _ -> failwith "Error"
      in
      Msg.make_msg sender receiver time msg_t content :: lst
  | _ ->
      failwith
        "Server.Database.cons_one_msg: row is in the wrong format"

(** Debug *)
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
      mark_as_retrieved receiver time;
      if time = Time.earliest_time then
        Printf.printf "Retrieved all messages sent to %s\n\n" receiver
      else
        Printf.printf "Retrieved messages sent to %s after %s\n\n"
          receiver time;
      (* if test then print_msg_list (List.rev lst); *)
      let temp = List.rev lst in
      temp
  | r, _ ->
      prerr_endline (Rc.to_string r);
      prerr_endline (errmsg server_db);
      failwith "Server.Database.get_msg_aux: Return code is not DONE"

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

let approve_req_sql =
  "INSERT INTO friends (requester, receiver) VALUES (?, ?);"

let fr_approve sender receiver =
  let sender = user_ok sender in
  let receiver = user_ok receiver in
  if not (fr_exist sender receiver) then raise Not_found;
  let stmt = prepare server_db approve_req_sql in
  bind_values stmt [ TEXT receiver; TEXT sender ] |> assert_ok;
  (* Note that the receiver and sender are reversed: We want to add a
     line that says receiver likes the sender. *)
  step stmt
  |> handle_rc
       (Printf.sprintf "The friend request from %s to %s is approved"
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

(** [cons_one_user lst row] adds [row] to [lst]. Requires: [row] is the
    username of a user. *)
let cons_one_user lst (row : Data.t array) =
  match row.(0) with
  | Data.TEXT username -> username :: lst
  | _ ->
      failwith
        "Server.Database.cons_one_user: row is in the wrong format"

(** [like_list user] is a list of all users that [user] likes. Requires:
    [user] is found in the table of users. *)
let like_list user =
  let stmt = prepare server_db select_liked_sql in
  bind_text stmt 1 user |> assert_ok;
  let res = Sqlite3.fold stmt ~f:cons_one_user ~init:[] in
  match res with
  | Rc.DONE, lst -> lst
  | r, _ ->
      prerr_endline (Rc.to_string r);
      prerr_endline (errmsg server_db);
      failwith "Server.Database.like_list: Return code is not DONE"

let friends_of user =
  let user = user_ok user in
  List.filter (fun u -> likes u user) (like_list user)
