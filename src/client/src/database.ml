(** Main table columns: tablename=client

    - id INTEGER PRIMARY KEY AUTOINCREMENT
    - username TEXT NOT NULL UNIQUE
    - key TEXT NOT NULL

    Client [x]'s friend request table columns: tablename=x_req

    - id INTEGER PRIMARY KEY AUTOINCREMENT
    - user TEXT NOT NULL
    - key TEXT NOT NULL
    - message TEXT
    - isSender BOOL NOT NULL
    - time TEXT NOT NULL
    - accepted BOOL

    Client [x]'s message table columns: tablename=x_msg

    - id INTEGER PRIMARY KEY AUTOINCREMENT
    - user TEXT NOT NULL
    - message TEXT NOT NULL
    - isSender BOOL NOT NULL
    - time TEXT NOT NULL *)

open Sqlite3
open Util.Msg
open Util.Time

exception MalformedTime

(******************** General Helper Functions ********************)

let db_dir_prefix =
  "data" ^ Filename.dir_sep ^ "database" ^ Filename.dir_sep

(** [open_db name] opens database named [name] in the designated
    directory. *)
let open_db name = db_open (db_dir_prefix ^ name ^ ".db")

(** [clt_req name] is the friend request table name for client [name]*)
let clt_req name = name ^ "_req"

(** [clt_msg name] is the message table name for client [name]. *)
let clt_msg name = name ^ "_msg"

(** [time_ok time] is [time] if its formatting is correct. Raise
    [MalformedTime] otherwise. *)
let time_ok time = if chk_time time then time else raise MalformedTime

(** [bool_op_to_str f] is ["1"] if [f] is [Some true], ["0"] if [f] is
    [Some false], ["NULL"] if [f] is [None]. *)
let bool_op_to_str = function
  | None -> "NULL"
  | Some f -> if f then "1" else "0"

(** [bool_to_str f] is ["1"] if [f] is [true], ["0"] if [f] is [false]. *)
let bool_to_str f = if f then "1" else "0"

let bool_to_t f : Data.t =
  if f then INT (Int64.of_int 1) else INT (Int64.of_int 0)

let bool_op_to_t = function
  | None -> Data.NULL
  | Some f -> bool_to_t f

(** [str_op_to_str op] is [s] if [op] is [Some s], [""] if [op] is
    [None]. *)
let str_op_to_str = function
  | None -> ""
  | Some s -> s

(** [handle_rc ok_msg rc] is a pair [(f, msg)] where [f] is if the
    return code indicates successful, and [msg] is the helpful message
    attached. Side Effects: prints [ok_msg] if the return code [rc] is
    successful, prints error message otherwise. *)
let handle_rc ok_msg = function
  | Rc.OK
  | Rc.DONE ->
      (* print_endline ok_msg; *)
      print_newline ();
      (true, ok_msg)
  | r ->
      print_endline (Rc.to_string r);
      print_endline ("NOT " ^ ok_msg);
      print_newline ();
      (false, "NOT " ^ ok_msg)

(** [dir_handle h] is whether [h] indicates success. *)
let dir_handle (h : bool * header) =
  match h with
  | f, _ -> f

let assert_ok rc =
  if Rc.is_success rc then ()
  else print_endline "RC failed to assert ok.";
  ()

let rec print_list = function
  | [] -> ()
  | h :: t ->
      print_string (h ^ " ");
      print_list t

(** [data_to_string data] converts [data] to a string. *)
let data_to_string (data : Data.t) =
  match data with
  | NONE
  | NULL ->
      ""
  | FLOAT fl -> string_of_float fl
  | TEXT str -> str
  | BLOB str -> str
  | INT num -> Int64.to_string num

(** [put] is an alias for [Printf.sprintf]. *)
let put = Printf.sprintf

(** [cmb_rc lst] combines the response list to one response. *)
let cmb_rc (lst : (bool * header) list) =
  let rec inner acc = function
    | [] -> acc
    | h :: t -> (
        match acc with
        | f1, msg1 -> (
            match h with
            | f2, msg2 ->
                inner
                  ( f1 && f2,
                    if msg1 <> "" then msg1 ^ "\n" ^ msg2 else msg2 )
                  t))
  in
  inner (true, "") lst

(******************** Main Table ********************)

let init_dbs () =
  exec (open_db "client")
    "CREATE TABLE IF NOT EXISTS client (id INTEGER PRIMARY KEY \
     AUTOINCREMENT,username TEXT NOT NULL UNIQUE,key TEXT NOT NULL);"
  |> handle_rc "Client main table created or already existed. "

(** [add_client name key] adds [client] and [key] to main table.
    Requires: name has not been added. *)
let add_client name key =
  let st =
    prepare (open_db "client")
      "INSERT INTO client (username, key) VALUES (?, ?);"
  in
  bind_values st [ TEXT name; TEXT key ] |> assert_ok;
  step st |> handle_rc (put "Client %s inserted to client table. " name)

let is_client name =
  let st =
    prepare (open_db "client")
      "SELECT username FROM client WHERE username=?"
  in
  bind_text st 1 name |> assert_ok;
  let res =
    fold st
      ~f:(fun x ar -> Array.map data_to_string ar |> Array.append x)
      ~init:(Array.make 0 "")
  in
  match res with
  | _, ar -> Array.length ar <> 0

(******************** Client Tables Creation ********************)

(** [create_msg_table username] creates the message table for [username]
    if not exists. *)
let create_msg_table username =
  exec
    (open_db (clt_msg username))
    (put
       "CREATE TABLE IF NOT EXISTS %s (id INTEGER PRIMARY KEY \
        AUTOINCREMENT, user TEXT NOT NULL,message TEXT NOT NULL, \
        isSender BOOL NOT NULL, time TEXT NOT NULL);"
       (clt_msg username))
  |> handle_rc
       (put "%s's message table created or already existed. " username)

(** [create_req_table username] creates the friend request table for
    [username] if not exists. *)
let create_req_table username =
  exec
    (open_db (clt_req username))
    (put
       "CREATE TABLE IF NOT EXISTS %s (id INTEGER PRIMARY KEY \
        AUTOINCREMENT, user TEXT NOT NULL, key TEXT NOT NULL, message \
        TEXT, isSender BOOL NOT NULL, time TEXT NOT NULL, accepted \
        BOOL);"
       (clt_req username))
  |> handle_rc
       (put "%s's friend request table created or already existed. "
          username)

let create_dbs username key =
  if is_client username then
    (false, put "Client %s already exists. " username)
  else
    let a = add_client username key in
    let b = create_msg_table username in
    let c = create_req_table username in
    cmb_rc [ a; b; c ]

(******************** Friend Request Table ********************)

let is_in_req client user =
  let st =
    prepare
      (open_db (clt_req client))
      (put "SELECT id FROM %s WHERE user=?" (clt_req client))
  in
  bind_text st 1 user |> assert_ok;
  let res =
    fold st
      ~f:(fun x ar -> Array.map data_to_string ar |> Array.append x)
      ~init:(Array.make 0 "")
  in
  match res with
  | _, ar -> Array.length ar <> 0

let is_frd client user =
  let st =
    prepare
      (open_db (clt_req client))
      (put "SELECT accepted FROM %s WHERE user=?" (clt_req client))
  in
  bind_text st 1 user |> assert_ok;
  let res =
    fold st
      ~f:(fun x ar -> Array.map data_to_string ar |> Array.append x)
      ~init:(Array.make 0 "")
  in
  match res with
  | _, ar -> Array.length ar <> 0 && ar.(0) = "1"

let add_request client req key req_state =
  if is_client client then
    if create_req_table client |> dir_handle then
      if
        req |> sender <> (req |> receiver)
        && (req |> sender = client || req |> receiver = client)
      then
        let user =
          if sender req = client then receiver req else sender req
        in
        if is_in_req client user |> not then (
          let st =
            prepare
              (open_db (clt_req client))
              (put
                 "INSERT INTO %s (user, key, message, time, accepted, \
                  isSender) VALUES (?, ?, ?, ?, ?, ?);"
                 (clt_req client))
          in
          bind_values st
            [
              TEXT user;
              TEXT (str_op_to_str key);
              TEXT (content req);
              TEXT (time req);
              bool_op_to_t req_state;
              bool_to_t (user = receiver req);
            ]
          |> assert_ok;
          step st
          |> handle_rc
               (put
                  "A new friend request inserted to client %s' table. "
                  client))
        else
          ( false,
            put "Client %s's friend request already in record. " client
          )
      else (false, put "Client %s's friend request is invalid. " client)
    else
      ( false,
        put
          "Client %s's friend request table does not exist and cannot \
           be created. "
          client )
  else (false, put "Client %s does not exist. " client)

let update_request client username req_state =
  if is_client client then
    if create_req_table client |> dir_handle then
      if is_in_req client username then (
        let st =
          prepare
            (open_db (clt_req client))
            (put "UPDATE %s SET accepted=? WHERE user=?;"
               (clt_req client))
        in
        bind_values st [ bool_to_t req_state; TEXT username ]
        |> assert_ok;
        step st
        |> handle_rc
             (put "The friend state of %s (client) and %s updated. "
                client username))
      else
        ( false,
          put
            "This friend request of %s (client) and %s has not been \
             added to the table. "
            client username )
    else
      ( false,
        put
          "Client %s's friend request table does not exist and cannot \
           be created. "
          client )
  else (false, put "Client %s does not exist. " client)

(** [form_req_lsts client lst] group [lst] by 4 to form a new list of
    requests. *)
let rec form_req_lsts client = function
  | u :: i :: m :: t :: tail ->
      if i = "1" then
        make_msg client u t FriendReq m :: form_req_lsts client tail
      else if i = "0" then
        make_msg u client t FriendReq m :: form_req_lsts client tail
      else failwith "Friend request list has issues with partitioning. "
  | [] -> []
  | _ -> failwith "Friend request list has issues with partitioning. "

let get_all_reqs client =
  if is_client client then
    if create_req_table client |> dir_handle then
      let res =
        fold
          (prepare
             (open_db (clt_req client))
             (put "SELECT user, isSender, message, time FROM %s"
                (clt_req client)))
          ~f:(fun x ar ->
            (Array.map data_to_string ar |> Array.to_list) @ x)
          ~init:[]
      in
      match res with
      | _, lst -> form_req_lsts client lst
    else
      failwith
        (put
           "Client %s's friend request table does not exist and cannot \
            be created. "
           client)
  else failwith (put "Client %s does not exist. " client)

(* let rec form_frd_lsts client = function | u :: i :: m :: t :: a ::
   tail -> if a = "1" then if i = "1" then make_msg client u t FriendReq
   m :: form_frd_lsts client tail else if i = "0" then make_msg u client
   t FriendReq m :: form_frd_lsts client tail else failwith "Friend list
   has issues with partitioning. " else form_frd_lsts client tail | []
   -> [] | _ -> failwith "Friend list has issues with partitioning. " *)

(* should return t list or string list?*)
let get_all_frds client =
  if is_client client then
    if create_req_table client |> dir_handle then
      let res =
        fold
          (prepare
             (open_db (clt_req client))
             (put "SELECT user FROM %s WHERE accepted=true"
                (clt_req client)))
          ~f:(fun x ar ->
            (Array.map data_to_string ar |> Array.to_list) @ x)
          ~init:[]
      in
      match res with
      | _, lst -> lst
    else
      failwith
        (put
           "Client %s's friend request table does not exist and cannot \
            be created. "
           client)
  else failwith (put "Client %s does not exist. " client)

let get_req_by_name client username =
  if is_client client then
    if create_req_table client |> dir_handle then (
      let st =
        prepare
          (open_db (clt_req client))
          (put
             "SELECT user, isSender, message, time FROM %s WHERE user=?"
             (clt_req client))
      in
      bind_text st 1 username |> assert_ok;
      let res =
        fold st
          ~f:(fun x ar ->
            (Array.map data_to_string ar |> Array.to_list) @ x)
          ~init:[]
      in
      match res with
      | _, lst -> (
          match lst with
          | [ u; i; m; t ] ->
              if i = "1" then Some (make_msg client u t FriendReq m)
              else if i = "0" then
                Some (make_msg u client t FriendReq m)
              else None
          | _ -> None))
    else
      failwith
        (put
           "Client %s's friend request table does not exist and cannot \
            be created. "
           client)
  else failwith (put "Client %s does not exist. " client)

(******************** Message Table ********************)

let add_msg client msg =
  if is_client client then
    if create_msg_table client |> dir_handle then
      if
        msg |> sender <> (msg |> receiver)
        && (msg |> sender = client || msg |> receiver = client)
      then
        let user =
          if sender msg = client then receiver msg else sender msg
        in
        if is_frd client user then (
          let st =
            prepare
              (open_db (clt_msg client))
              (put
                 "INSERT INTO %s (user, message, time, isSender) \
                  VALUES (?, ?, ?, ?);"
                 (clt_msg client))
          in
          bind_values st
            [
              TEXT user;
              TEXT (content msg);
              TEXT (time msg);
              bool_to_t (user = receiver msg);
            ]
          |> assert_ok;
          step st
          |> handle_rc
               (put
                  "A new message from %s to %s inserted to client %s' \
                   table. "
                  (sender msg) (receiver msg) client))
        else
          (false, put "Client %s and %s are not friends. " client user)
      else (false, put "Client %s's message is invalid. " client)
    else
      ( false,
        put
          "Client %s's message table does not exist and cannot be \
           created. "
          client )
  else (false, put "Client %s does not exist. " client)

(** [form_msg_lsts client lst] group [lst] by 4 to form a new list of
    messages. *)
let rec form_msg_lsts client = function
  | u :: m :: t :: i :: tail ->
      if i = "1" then
        make_msg client u t Message m :: form_msg_lsts client tail
      else if i = "0" then
        make_msg u client t Message m :: form_msg_lsts client tail
      else failwith "Message list has issues with partitioning. "
  | [] -> []
  | _ -> failwith "Message list has issues with partitioning. "

let get_all_msgs_since client time =
  if is_client client then
    if create_msg_table client |> dir_handle then (
      let st =
        prepare
          (open_db (clt_msg client))
          (put
             "SELECT user, message, time, isSender accepted FROM %s \
              WHERE time > ? ORDER BY time ASC"
             (clt_msg client))
      in
      bind_text st 1 (time_ok time) |> assert_ok;
      let res =
        fold st
          ~f:(fun x ar ->
            (Array.map data_to_string ar |> Array.to_list) @ x)
          ~init:[]
      in
      match res with
      | _, lst ->
          (* print_int (List.length lst); *)
          form_msg_lsts client lst)
    else
      failwith
        (put
           "Client %s's message table does not exist and cannot be \
            created. "
           client)
  else failwith (put "Client %s does not exist. " client)

let get_msgs_by_frd client frd =
  if is_client client then
    if create_msg_table client |> dir_handle then
      if is_frd client frd then (
        let st =
          prepare
            (open_db (clt_msg client))
            (put
               "SELECT user, message, time, isSender accepted FROM %s \
                WHERE user=? ORDER BY time ASC"
               (clt_msg client))
        in
        bind_text st 1 frd |> assert_ok;
        let res =
          fold st
            ~f:(fun x ar ->
              (Array.map data_to_string ar |> Array.to_list) @ x)
            ~init:[]
        in
        match res with
        | _, lst -> form_msg_lsts client lst)
      else []
    else
      failwith
        (put
           "Client %s's message table does not exist and cannot be \
            created. "
           client)
  else failwith (put "Client %s does not exist. " client)

let get_msgs_by_frd_since client frd time =
  if is_client client then
    if create_msg_table client |> dir_handle then
      if is_frd client frd then (
        let st =
          prepare
            (open_db (clt_msg client))
            (put
               "SELECT user, message, time, isSender accepted FROM %s \
                WHERE user=? AND time>? ORDER BY time ASC"
               (clt_msg client))
        in
        bind_text st 1 frd |> assert_ok;
        bind_text st 2 (time_ok time) |> assert_ok;
        let res =
          fold st
            ~f:(fun x ar ->
              (Array.map data_to_string ar |> Array.to_list) @ x)
            ~init:[]
        in
        match res with
        | _, lst -> form_msg_lsts client lst)
      else []
    else
      failwith
        (put
           "Client %s's message table does not exist and cannot be \
            created. "
           client)
  else failwith (put "Client %s does not exist. " client)

(* let get_all_msgs = failwith "Unimplemented" *)

let request_test () =
  (match
     let a = init_dbs () in
     let b = create_dbs "alice" "666" in
     let c =
       add_request "alice"
         (make_msg "alice" "bob" "2000-01-01 08:00:00" FriendReq "hello")
         (Some "123") None
     in
     let d = update_request "alice" "bob" true in
     cmb_rc [ a; b; c; d ]
   with
  | f, m ->
      if f then print_endline ("TRUE: " ^ m)
      else print_endline ("FALSE: " ^ m));
  if is_in_req "alice" "bob" then print_endline "Bob is in Alice's list"
  else print_endline "Bob is NOT in Alice's list";
  let lst = get_all_reqs "alice" in
  List.iter
    (fun x -> print_endline (sender x ^ " AND " ^ receiver x))
    lst;
  let lst = get_all_frds "alice" in
  List.iter (fun x -> print_endline x) lst

let msg_test () =
  (let a = init_dbs () in
   let b = create_dbs "alice" "666" in
   let c =
     add_request "alice"
       (make_msg "alice" "bob" "2000-01-01 08:00:00" FriendReq "hello")
       (Some "123") None
   in
   let d = update_request "alice" "bob" true in
   let e =
     add_msg "alice"
       (make_msg "alice" "bob" "2000-01-01 09:00:00" Message
          "hello at 9")
   in
   let f =
     add_msg "alice"
       (make_msg "alice" "bob" "2000-01-02 11:00:00" Message
          "hello at 11")
   in
   let g =
     add_msg "alice"
       (make_msg "bob" "alice" "2000-01-02 08:00:00" Message
          "hello on second day")
   in
   match cmb_rc [ a; b; c; d; e; f; g ] with
   | f, m ->
       if f then print_endline ("TRUE: " ^ m)
       else print_endline ("FALSE: " ^ m));
  let lst = get_all_msgs_since "alice" "2000-01-01 00:00:00" in
  List.iter
    (fun x ->
      print_endline (sender x ^ " TO " ^ receiver x ^ " AT " ^ time x))
    lst;
  let lst = get_msgs_by_frd_since "alice" "bob" "2000-01-01 00:00:00" in
  List.iter
    (fun x ->
      print_endline (sender x ^ " TO " ^ receiver x ^ " AT " ^ time x))
    lst

let add_groupchat = failwith "Unimplemented"
let create_groupchat = failwith "Unimplemented"
let add_member_gc = failwith "Unimplemented"
let is_in_gc = failwith "Unimplemented"
let add_msg_to_gc = failwith "Unimplemented"
let get_msg_gc_since = failwith "Unimplemented"
let gc_of_user = failwith "Unimplemented"
let members_of_gc = failwith "Unimplemented"
