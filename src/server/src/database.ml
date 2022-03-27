(** Users table columns:

    - username: TEXT NOT NULL
    - password: TEXT NOT NULL
    - public_key: TEXT NOT NULL
    - date_registered: TEXT NOT NULL

    Messages table columns:

    - sender: TEXT NOT NULL
    - receiver: TEXT NOT NULL
    - content: TEXT NOT NULL
    - time: TEXT NOT NULL
    - retrieved: BOOLEAN NOT NULL

    Friends table columns:

    - user_A: TEXT NOT NULL
    - user_B: TEXT NOT NULL
    - A_likes_B: BOOLEAN NOT NULL
    - time: TEXT NOT NULL
    - message: TEXT NULLABLE *)

open Sqlite3
open Util

let test = true

let db_file =
  if test then "data/database/test.db" else "data/database/server.db"

let server_db = db_open db_file

(** [handle_rc ok_msg] prints [ok_msg] if the return code [rc] is [OK].
    Otherwise, it prints helpful error messages. *)
let handle_rc ok_msg = function
  | Rc.OK ->
      print_endline ok_msg;
      print_newline ()
  | r ->
      prerr_endline (Rc.to_string r);
      prerr_endline (errmsg server_db)

let create_users_sql =
  "CREATE TABLE IF NOT EXISTS users (username TEXT NOT NULL, password \
   TEXT NOT NULL, public_key TEXT NOT NULL, date_registered TEXT NOT \
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
  "CREATE TABLE IF NOT EXISTS friends (user_A TEXT NOT NULL, user_B \
   TEXT NOT NULL, A_likes_B BOOLEAN NOT NULL, time TEXT NOT NULL, \
   message TEXT);"

let create_friends_table () =
  exec server_db create_friends_sql
  |> handle_rc "Friends table found or successfully created"

let create_tables () =
  create_users_table ();
  create_messages_table ();
  create_friends_table ()

let insert_user_sql username pwd key time =
  Printf.sprintf "INSERT INTO users VALUES ('%s', '%s', '%s', '%s');"
    username pwd key time

let insert_user username pwd key time =
  exec server_db (insert_user_sql username pwd key time)

let user_exists_sql username =
  Printf.sprintf
    "SELECT EXISTS (SELECT 1 from users where username = '%s');"
    username

let user_exists_stmt username =
  prepare server_db (user_exists_sql username)

(** [user_exists username] is [true] if [username] exists in the users
    table, and [false] otherwise. *)
let user_exists username = column_bool (user_exists_stmt username) 0

let add_user username pwd key time =
  if user_exists username then (false, "User already exists")
  else (
    insert_user username pwd key time
    |> handle_rc "User successfully added";
    (true, "User successfully added"))

type chk_user =
  | UserOK
  | UnknownUser of string
  | WrongPwd of string
  | MalformedUserTime of string

let chk_pwd username pwd = failwith "Unimplemented"
let create_msg_table () = failwith "Unimplemented"
let add_msg (msg : Msg.t) = failwith "Unimplemented"
let get_msg_since receiver time = failwith "Unimplemented"
let new_fr (req : Msg.t) = failwith "Unimplemented"
let fr_exist sender receiver = failwith "Unimplemented"
let is_friend sender receiver = failwith "Unimplemented"
let fr_approve sender receiver = failwith "Unimplemented"
let fr_reject sender receiver = failwith "Unimplemented"
let friends_of user = failwith "Unimplemented"
