(** Reference: https://github.com/cedlemo/ocaml-sqlite3-notes;
    https://github.com/cedlemo/ocaml-sqlite3-notes/blob/master/README_sqlite3_tutorial.md *)

(** This is a prototype for Sqlite experimentation. It will be deleted. *)

open Sqlite3

let user_db = db_open "data/database/user.db"

let create_table_users_sql =
  "CREATE TABLE IF NOT EXISTS users (username TEXT NOT NULL, password \
   TEXT NOT NULL);"

let insert_user_sql username password =
  Printf.sprintf "INSERT INTO users VALUES ('%s','%s');" username
    password
(* Note: This can be vulnerable to BOBBY DROP TABLES attacks! We'll
   worry about that later. *)

let insert_user username password =
  exec user_db (insert_user_sql username password)

(** [handle_rc ok_msg] prints [ok_msg] if the return code [rc] is [OK].
    Otherwise, it prints helpful error messages. *)
let handle_rc ok_msg = function
  | Rc.OK ->
      print_endline ok_msg;
      print_newline ()
  | r ->
      prerr_endline (Rc.to_string r);
      prerr_endline (errmsg user_db)

(** [is_row rc] is [true] if [rc] is [ROW], [false] if [rc] is [DONE],
    and raises an exception if [rc] is anything else. *)
let is_row = function
  | Rc.ROW -> true
  | Rc.DONE -> false
  | r ->
      prerr_endline (Rc.to_string r);
      prerr_endline (errmsg user_db);
      assert false

let create_table_users () =
  exec user_db create_table_users_sql |> handle_rc "Created table users"

let insert_alice () =
  insert_user "Alice" "apple"
  |> handle_rc "Inserted user Alice into users"

let insert_bob () =
  insert_user "Bob" "banana" |> handle_rc "Inserted user Bob into users"

let insert_charlie () =
  insert_user "Charlie" "cherry"
  |> handle_rc "Inserted user Charlie into users"

(** [name_pswd_cb row header] is the callback function for printing out
    username and password. Requires: [row] is a row with username and
    password. *)
let name_pswd_cb row header =
  match (row.(0), row.(1)) with
  | Some username, Some password ->
      print_endline
        (Printf.sprintf "Username: %s, password: %s" username password)
  | _ -> assert false

let select_all_users_sql = "SELECT * FROM users;"

let select_all_users () =
  exec user_db ~cb:name_pswd_cb select_all_users_sql
  |> handle_rc
       "Selected all users, printed their usernames and passwords"

(** [pswd_cb row header] is the callback function for printing out
    password. Requires: [row] is a row with username and password. *)
let pswd_cb row header =
  match row.(1) with
  | Some password ->
      print_endline (Printf.sprintf "Password: %s" password)
  | _ -> assert false

let select_alice_sql = "SELECT * FROM users WHERE username = 'Alice';"

let select_alice () =
  exec user_db ~cb:name_pswd_cb select_alice_sql
  |> handle_rc "Selected Alice, printed her password"

(* The following are two ways to load the output of a statement to a
   variable, where the output may contain several rows. *)

let print_alice () =
  let stmt = prepare user_db select_alice_sql in
  let has_row = ref (step stmt |> is_row) in
  while !has_row do
    print_endline (column_text stmt 0);
    has_row := step stmt |> is_row
  done

let print_all_users () =
  let stmt = prepare user_db select_all_users_sql in
  let has_row = ref (step stmt |> is_row) in
  while !has_row do
    print_endline (column_text stmt 0);
    has_row := step stmt |> is_row
  done

(** [cons_one_user lst row] adds [row] to [lst]. Requires: The first
    element of [row] represents the username and the second element of
    [row] represents the password. *)
let cons_one_user lst (row : Data.t array) =
  match (row.(0), row.(1)) with
  | Data.TEXT username, Data.TEXT pwd -> (username, pwd) :: lst
  | _ -> assert false

(** [get_users_pwd_list ()] is a list of (username, password) pairs for
    all the users. *)
let get_users_pwd_list () =
  let stmt = prepare user_db select_all_users_sql in
  let res = Sqlite3.fold stmt ~f:cons_one_user ~init:[] in
  match res with
  | Rc.DONE, lst -> List.rev lst
  | r, _ ->
      prerr_endline (Rc.to_string r);
      prerr_endline (errmsg user_db);
      assert false

let print_all_users_pwds () =
  List.map
    (fun (username, pwd) ->
      Printf.printf "Username: %s, password: %s\n" username pwd)
    (get_users_pwd_list ())
  |> ignore

let db_main () =
  create_table_users ();
  insert_alice ();
  insert_bob ();
  insert_charlie ();
  select_all_users ();
  select_alice ();
  print_alice ();
  print_all_users ();
  print_all_users_pwds ()
