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

exception MalformedTime

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
      assert false

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

(** [user_exists username] is [true] if [username] exists in the users
    table, and [false] otherwise. *)
let user_exists username =
  let stmt = user_exists_stmt username in
  step stmt |> assert_rc_row;
  column_bool stmt 0

let add_ok_str username =
  Printf.sprintf "User %s sucessfully added" username

let add_exists_str username =
  Printf.sprintf "User %s already exists" username

let add_user username pwd key time =
  if user_exists username then (
    print_endline (add_exists_str username);
    print_newline ();
    (false, add_exists_str username))
  else (
    insert_user username pwd key time |> handle_rc (add_ok_str username);
    (true, add_ok_str username))

type chk_user =
  | UserOK
  | UnknownUser of string
  | WrongPwd of string

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
