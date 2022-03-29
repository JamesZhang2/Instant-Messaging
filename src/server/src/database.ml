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
    - retrieved: BOOLEAN NOT NULL

    Friends table columns:

    - userA: TEXT NOT NULL
    - userB: TEXT NOT NULL
    - time: TEXT NOT NULL
    - message: TEXT NULLABLE

    (AF: userA is said to "like" userB if a row (userA, userB, time,
    message) exists. If both userA and userB like each other, they are
    friends. If userA likes userB but userB doesn't like userA, then
    there is a pending friend request from userA to userB.) *)

open Sqlite3
open Util

exception MalformedTime
exception UnknownUser of string

let test = true

let db_file =
  if test then "data/database/test.db" else "data/database/server.db"

let server_db = db_open db_file

(** [handle_rc ok_msg rc] prints [ok_msg] if the return code [rc] is
    [OK]. Otherwise, it prints helpful error messages. *)
let handle_rc ok_msg = function
  | Rc.OK ->
      print_endline ok_msg;
      print_newline ()
  | r ->
      prerr_endline (Rc.to_string r);
      prerr_endline (errmsg server_db)

(** [assert_rc_row rc] asserts that the recurn code [rc] is [ROW]. *)
let assert_rc_row = function
  | Rc.ROW -> ()
  | r ->
      prerr_endline (Rc.to_string r);
      prerr_endline (errmsg server_db);
      failwith "A row is expected but none is available."

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
   TEXT NOT NULL, content TEXT NOT NULL, time TEXT NOT NULL, retrieved \
   BOOLEAN NOT NULL);"

let create_messages_table () =
  exec server_db create_messages_sql
  |> handle_rc "Messages table found or successfully created"

let create_friends_sql =
  "CREATE TABLE IF NOT EXISTS friends (userA TEXT NOT NULL, userB TEXT \
   NOT NULL, time TEXT NOT NULL, message TEXT);"

let create_friends_table () =
  exec server_db create_friends_sql
  |> handle_rc "Friends table found or successfully created"

let create_tables () =
  create_users_table ();
  create_messages_table ();
  create_friends_table ()

(******************** Print All ********************)

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

let insert_user_sql username pwd key time =
  Printf.sprintf "INSERT INTO users VALUES ('%s', '%s', '%s', '%s');"
    username pwd key time

let insert_user username pwd key time =
  exec server_db (insert_user_sql username pwd key time)

let user_exists_sql username =
  Printf.sprintf
    "SELECT EXISTS (SELECT 1 from users WHERE username = '%s');"
    username

let user_exists_stmt username =
  prepare server_db (user_exists_sql username)

let user_exists username =
  let stmt = user_exists_stmt username in
  step stmt |> assert_rc_row;
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

let select_key_sql username =
  Printf.sprintf "SELECT public_key FROM users WHERE username = '%s'"
    username

let select_key_stmt username =
  prepare server_db (select_key_sql username)

let user_key username =
  let username = user_ok username in
  let stmt = select_key_stmt username in
  step stmt |> assert_rc_row;
  column_text stmt 0

(******************** Check password ********************)

let chk_pwd_sql username pwd =
  Printf.sprintf
    "SELECT EXISTS (SELECT 1 from users WHERE username = '%s' AND \
     password = '%s');"
    username pwd

let chk_pwd_stmt username pwd =
  prepare server_db (chk_pwd_sql username pwd)

let chk_pwd username pwd =
  let username = user_ok username in
  let stmt = chk_pwd_stmt username pwd in
  step stmt |> assert_rc_row;
  column_bool stmt 0

(******************** Add message ********************)

let insert_msg_sql sender receiver content time =
  Printf.sprintf
    "INSERT INTO messages VALUES ('%s', '%s', '%s', '%s', FALSE);"
    sender receiver content time

let add_msg (msg : Msg.t) =
  assert (Msg.msg_type msg = Msg.Message);
  let sender = Msg.sender msg |> user_ok in
  let receiver = Msg.receiver msg |> user_ok in
  let content = Msg.content msg in
  let time = Msg.time msg |> time_ok in
  exec server_db (insert_msg_sql sender receiver content time)
  |> handle_rc
       (Printf.sprintf "%s sent a message to %s at %s: %s" sender
          receiver time content);
  true

let get_msg receiver = failwith "Unimplemented"
let get_msg_since receiver time = failwith "Unimplemented"
let new_fr (req : Msg.t) = failwith "Unimplemented"
let fr_exist sender receiver = failwith "Unimplemented"
let is_friend sender receiver = failwith "Unimplemented"
let fr_approve sender receiver = failwith "Unimplemented"
let fr_reject sender receiver = failwith "Unimplemented"
let friends_of user = failwith "Unimplemented"
