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

let server_db = db_open "data/database/server.db"

(** [handle_rc db ok_msg] prints [ok_msg] if the return code [rc] is
    [OK]. Otherwise, it prints helpful error messages. *)
let handle_rc db ok_msg = function
  | Rc.OK ->
      print_endline ok_msg;
      print_newline ()
  | r ->
      prerr_endline (Rc.to_string r);
      prerr_endline (errmsg db)

let create_users_sql =
  "CREATE TABLE IF NOT EXISTS users (username TEXT NOT NULL, password \
   TEXT NOT NULL, public_key TEXT NOT NULL, date_registered TEXT NOT \
   NULL);"

let create_users_table () =
  exec server_db create_users_sql
  |> handle_rc server_db "Users table found or successfully created"

let create_messages_sql =
  "CREATE TABLE IF NOT EXISTS messages (sender TEXT NOT NULL, receiver \
   TEXT NOT NULL, content TEXT NOT NULL, time TEXT NOT NULL, retrieved \
   BOOLEAN NOT NULL);"

let create_messages_table () =
  exec server_db create_messages_sql
  |> handle_rc server_db "Messages table found or successfully created"

let create_friends_sql =
  "CREATE TABLE IF NOT EXISTS friends (user_A TEXT NOT NULL, user_B \
   TEXT NOT NULL, A_likes_B BOOLEAN NOT NULL, time TEXT NOT NULL, \
   message TEXT);"

let create_friends_table () =
  exec server_db create_friends_sql
  |> handle_rc server_db "Friends table found or successfully created"

let create_tables () =
  create_users_table ();
  create_messages_table ();
  create_friends_table ()

let create_users_table () = exec server_db create_users_sql
let add_user username pwd time = failwith "Unimplemented"

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
