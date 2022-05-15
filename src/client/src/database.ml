(** Main table columns: tablename=client

    - id INTEGER PRIMARY KEY AUTOINCREMENT
    - username TEXT NOT NULL UNIQUE
    - key TEXT NOT NULL

    Client [x]'s friend request table columns: tablename=x_req

    - id INTEGER PRIMARY KEY AUTOINCREMENT
    - user TEXT NOT NULL
    - key TEXT NOT NULL
    - message TEXT
    - isSender BOOL NOT NULL // if [x] is sender
    - time TEXT NOT NULL
    - accepted BOOL

    Client [x]'s message table columns: tablename=x_msg

    - id INTEGER PRIMARY KEY AUTOINCREMENT
    - user TEXT NOT NULL
    - message TEXT NOT NULL
    - isSender BOOL NOT NULL
    - time TEXT NOT NULL

    Client [x]'s groupchat table columns: tablename=x_gc

    - id INTEGER PRIMARY KEY AUTOINCREMENT
    - name TEXT NOT NULL

    Client [x]'s groupchat member table columns: tablename=x_gcm

    - id INTEGER PRIMARY KEY AUTOINCREMENT
    - gcId INTEGER NOT NULL
    - mem TEXT NOT NULL

    Client [x]'s groupchat message table columns: tablename=x_gcmsg

    - id INTEGER PRIMARY KEY AUTOINCREMENT
    - gcId INTEGER NOT NULL
    - sender TEXT NOT NULL
    - message TEXT NOT NULL
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

(** [clt_gc name] is the groupchat table name for client [name]. *)
let clt_gc name = name ^ "_gc"

(** [clt_gcm name] is the groupchat member table name for client [name]. *)
let clt_gcm name = name ^ "_gcm"

(** [clt_gcmsg name] is the groupchat message table name for client
    [name]. *)
let clt_gcmsg name = name ^ "_gcmsg"

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

(** [handle_rc ok_msg rc] is a pair [(f, msg)] where [f] indicates if
    it's successful, and [msg] is the helpful message attached. Side
    Effects: prints [ok_msg] if the return code [rc] is successful,
    prints error message otherwise. *)
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
  List.fold_left
    (fun (f1, msg1) (f2, msg2) ->
      (f1 && f2, if msg1 <> "" then msg1 ^ "\n" ^ msg2 else msg2))
    (true, "") lst

(** [is_success feedback] is [true] if [feedback] indicates success,
    [false] otherwise. *)
let is_success (feedback : bool * string) =
  match feedback with
  | f, _ -> f

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
  let st =
    prepare
      (username |> clt_msg |> open_db)
      "CREATE TABLE IF NOT EXISTS ? (id INTEGER PRIMARY KEY \
       AUTOINCREMENT, user TEXT NOT NULL,message TEXT NOT NULL, \
       isSender BOOL NOT NULL, time TEXT NOT NULL);"
  in
  username |> clt_msg |> bind_text st 1 |> assert_ok;
  step st
  |> handle_rc
       (put "%s's message table created or already existed. " username)

(** [create_req_table username] creates the friend request table for
    [username] if not exists. *)
let create_req_table username =
  let st =
    prepare
      (username |> clt_req |> open_db)
      "CREATE TABLE IF NOT EXISTS ? (id INTEGER PRIMARY KEY \
       AUTOINCREMENT, user TEXT NOT NULL, key TEXT NOT NULL, message \
       TEXT, isSender BOOL NOT NULL, time TEXT NOT NULL, accepted \
       BOOL);"
  in
  username |> clt_msg |> bind_text st 1 |> assert_ok;
  step st
  |> handle_rc
       (put "%s's friend request table created or already existed. "
          username)

(** [create_gc_table username] creates the groupchat table for
    [username] if not exists. *)
let create_gc_table username =
  let st =
    prepare
      (username |> clt_gc |> open_db)
      "CREATE TABLE IF NOT EXISTS ? (id INTEGER PRIMARY KEY \
       AUTOINCREMENT, name TEXT NOT NULL);"
  in
  username |> clt_gc |> bind_text st 1 |> assert_ok;
  step st
  |> handle_rc
       (put "%s's groupchat table created or already existed. " username)

(** [create_gcm_table username] creates the groupchat member table for
    [username] if not exists. *)
let create_gcm_table username =
  let st =
    prepare
      (username |> clt_gcm |> open_db)
      "CREATE TABLE IF NOT EXISTS ? (id INTEGER PRIMARY KEY \
       AUTOINCREMENT, gcId INTEGER NOT NULL, mem TEXT NOT NULL);"
  in
  username |> clt_gcm |> bind_text st 1 |> assert_ok;
  step st
  |> handle_rc
       (put "%s's groupchat member table created or already existed. "
          username)

(** [create_gcmsg_table username] creates the groupchat message table
    for [username] if not exists. *)
let create_gcmsg_table username =
  let st =
    prepare
      (username |> clt_gcmsg |> open_db)
      "CREATE TABLE IF NOT EXISTS ? (id INTEGER PRIMARY KEY \
       AUTOINCREMENT, gcId INTEGER NOT NULL, sender TEXT NOT NULL, \
       receiver TEXT NOT NULL, message TEXT NOT NULL, time TEXT NOT \
       NULL);"
  in
  username |> clt_gcmsg |> bind_text st 1 |> assert_ok;
  step st
  |> handle_rc
       (put "%s's groupchat message table created or already existed. "
          username)

let create_dbs username key =
  if is_client username then
    (false, put "Client %s already exists. " username)
  else
    let a = add_client username key in
    let b = create_msg_table username in
    let c = create_req_table username in
    let d = create_gc_table username in
    let e = create_gcm_table username in
    let f = create_gcmsg_table username in
    cmb_rc [ a; b; c; d; e; f ]

(******************** Friend Request Table ********************)

let is_in_req client user =
  let st =
    prepare
      (client |> clt_req |> open_db)
      (client |> clt_req |> put "SELECT id FROM %s WHERE user=?")
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
      (client |> clt_req |> open_db)
      (client |> clt_req |> put "SELECT accepted FROM %s WHERE user=?")
  in
  bind_text st 1 user |> assert_ok;
  let res =
    fold st
      ~f:(fun x ar -> Array.map data_to_string ar |> Array.append x)
      ~init:(Array.make 0 "")
  in
  match res with
  | _, ar -> Array.length ar <> 0 && ar.(0) = "1"

(** [add_req client req key req_state user] adds a friend request
    related to [client] with current state [req_state] and [key].
    Requires: [req] and [client] are valid. *)
let add_req client req key req_state user =
  let st =
    prepare
      (client |> clt_req |> open_db)
      (client |> clt_req
      |> put
           "INSERT INTO %s (user, key, message, time, accepted, \
            isSender) VALUES (?, ?, ?, ?, ?, ?);")
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
       (put "A new friend request inserted to client %s' table. " client)

let add_request client req key req_state =
  if is_client client then
    if create_req_table client |> dir_handle then
      if
        req |> sender <> receiver req
        && (sender req = client || receiver req = client)
      then
        let user =
          if sender req = client then receiver req else sender req
        in
        if is_in_req client user |> not then
          add_req client req key req_state user
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
            (client |> clt_req |> open_db)
            (client |> clt_req
            |> put "UPDATE %s SET accepted=? WHERE user=?;")
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
             (client |> clt_req |> open_db)
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
             "SELECT user, message, time, isSender FROM %s WHERE time \
              > ? ORDER BY time ASC"
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
               "SELECT user, message, time, isSender FROM %s WHERE \
                user=? ORDER BY time ASC"
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
               "SELECT user, message, time, isSender FROM %s WHERE \
                user=? AND time>? ORDER BY time ASC"
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

(******************** Groupchat Table ********************)

(** [get_gc_id client id] is the [gcid] in [client]'s groupchat table
    whose corresponding [name] is [id], otherwise -1 (when [id] is not a
    valid groupchat id). *)
let get_gc_id client id =
  let st =
    prepare
      (client |> clt_gc |> open_db)
      (client |> clt_gc |> put "SELECT id FROM %s WHERE name=?")
  in
  bind_text st 1 id |> assert_ok;
  let res =
    fold st
      ~f:(fun x ar -> Array.map data_to_string ar |> Array.append x)
      ~init:(Array.make 0 "")
  in
  match res with
  | _, ar -> if Array.length ar = 0 then -1 else int_of_string ar.(0)

let is_gc client id = get_gc_id client id >= 0

(** [create_gc client id] creates a groupchat [id] having no member
    inside. *)
let create_gc client id =
  if is_client client then
    if create_gc_table client |> dir_handle then
      if is_gc client id |> not then (
        let st =
          prepare
            (client |> clt_gc |> open_db)
            (client |> clt_gc |> put "INSERT INTO %s (name) VALUES (?);")
        in
        bind_text st 1 id |> assert_ok;
        step st
        |> handle_rc
             (put "A new groupchat %s is created in client %s' table. "
                id client)
        |> is_success)
      else
        ( false,
          put "Client %s's groupchat %s already in record. " client id
        )
        |> is_success
    else
      ( false,
        put
          "Client %s's groupchat table does not exist and cannot be \
           created. "
          client )
      |> is_success
  else (false, put "Client %s does not exist. " client) |> is_success

(** [add_mem_gc client id mem] adds [mem] to groupchat [id]. *)
let add_mem_gc client id mem =
  if is_client client then
    if create_gcm_table client |> dir_handle then
      let gcid = get_gc_id client id in
      if gcid >= 0 then (
        let st =
          prepare
            (client |> clt_gcm |> open_db)
            (client |> clt_gcm
            |> put "INSERT INTO %s (gcId, mem) VALUES (?, ?);")
        in
        bind_int st 1 gcid |> assert_ok;
        bind_text st 2 mem |> assert_ok;
        step st
        |> handle_rc
             (put "Member %s is added to client %s's groupchat %s. " mem
                client id)
        |> is_success)
      else
        ( false,
          put "Client %s's groupchat %s has not been created. " client
            id )
        |> is_success
    else
      ( false,
        put
          "Client %s's groupchat member table does not exist and \
           cannot be created. "
          client )
      |> is_success
  else (false, put "Client %s does not exist. " client) |> is_success

let add_member_gc client id mem_list =
  List.fold_left
    (fun f mem -> f && add_mem_gc client id mem)
    true mem_list

let create_groupchat client id =
  let a = create_gc client id in
  let b = add_mem_gc client id client in
  a && b

let add_groupchat client id mem_list =
  let a = create_gc client id in
  List.map (fun mem -> add_mem_gc client id mem) mem_list
  |> List.cons a
  |> List.fold_left ( && ) true

let is_in_gc client id username =
  let gcid = get_gc_id client id in
  if gcid >= 0 then (
    let st =
      prepare
        (client |> clt_gcm |> open_db)
        (client |> clt_gcm
        |> put "SELECT id FROM %s WHERE gcId=? AND mem=?")
    in
    bind_int st 1 gcid |> assert_ok;
    bind_text st 2 username |> assert_ok;
    let res =
      fold st
        ~f:(fun x ar -> Array.map data_to_string ar |> Array.append x)
        ~init:(Array.make 0 "")
    in
    match res with
    | _, ar -> Array.length ar <> 0)
  else failwith (put "Groupchat %s is not valid. " id)

let add_msg_to_gc client msg =
  if is_client client then
    if create_gcmsg_table client |> dir_handle then (
      let gcid = receiver msg |> get_gc_id client in
      if gcid = -1 then
        (false, put "Groupchat %s does not exist. " (receiver msg))
        |> is_success
      else
        let st =
          prepare
            (open_db (clt_gcmsg client))
            (put
               "INSERT INTO %s (gcId, sender, message, time) VALUES \
                (?, ?, ?, ?);"
               (clt_gcmsg client))
        in
        bind_values st
          [
            INT (Int64.of_int gcid);
            TEXT (sender msg);
            TEXT (content msg);
            TEXT (time msg);
          ]
        |> assert_ok;
        step st
        |> handle_rc
             (put
                "A new message from %s to groupchat %s inserted to \
                 client %s' table. "
                (sender msg) (receiver msg) client)
        |> is_success)
    else
      ( false,
        put
          "Client %s's groupchat message table does not exist and \
           cannot be created. "
          client )
      |> is_success
  else (false, put "Client %s does not exist. " client) |> is_success

(** [form_gc_msg_lsts client id lst] group [lst] by 3 to form a new list
    of messages. *)
let rec form_gc_msg_lsts client id = function
  | s :: m :: t :: tail ->
      make_msg s id t GCMessage m :: form_gc_msg_lsts client id tail
  | [] -> []
  | _ -> failwith "Message list has issues with partitioning. "

let get_msg_gc_since client id time =
  if is_client client then
    if create_gcmsg_table client |> dir_handle then
      let gcid = get_gc_id client id in
      if gcid <> -1 then (
        let st =
          prepare
            (open_db (clt_gcmsg client))
            (put
               "SELECT sender, message, time FROM %s WHERE gcId=? AND \
                time>? ORDER BY time ASC"
               (clt_gcmsg client))
        in
        bind_int st 1 gcid |> assert_ok;
        bind_text st 2 (time_ok time) |> assert_ok;
        let res =
          fold st
            ~f:(fun x ar ->
              (Array.map data_to_string ar |> Array.to_list) @ x)
            ~init:[]
        in
        match res with
        | _, lst -> form_gc_msg_lsts client id lst)
      else failwith (put "Groupchat %s does not exist. " id)
    else
      failwith
        (put
           "Client %s's groupchat message table does not exist and \
            cannot be created. "
           client)
  else failwith (put "Client %s does not exist. " client)

let gc_of_user client =
  if is_client client then
    if create_gc_table client |> dir_handle then
      let res =
        fold
          (prepare
             (open_db (clt_gc client))
             (put "SELECT name FROM %s" (clt_gc client)))
          ~f:(fun x ar ->
            (Array.map data_to_string ar |> Array.to_list) @ x)
          ~init:[]
      in
      match res with
      | _, lst -> lst
    else
      failwith
        (put
           "Client %s's groupchat table does not exist and cannot be \
            created. "
           client)
  else failwith (put "Client %s does not exist. " client)

let members_of_gc client id =
  if is_client client then
    if create_gcm_table client |> dir_handle then
      let gcid = get_gc_id client id in
      if gcid <> -1 then
        let res =
          fold
            (prepare
               (open_db (clt_gcm client))
               (put "SELECT mem FROM %s WHERE gcId=%d" (clt_gcm client)
                  gcid))
            ~f:(fun x ar ->
              (Array.map data_to_string ar |> Array.to_list) @ x)
            ~init:[]
        in
        match res with
        | _, lst -> lst
      else failwith (put "Groupchat %s does not exist. " id)
    else
      failwith
        (put
           "Client %s's groupchat table does not exist and cannot be \
            created. "
           client)
  else failwith (put "Client %s does not exist. " client)

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