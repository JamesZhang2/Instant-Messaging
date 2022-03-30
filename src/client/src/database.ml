open Sqlite3
open Util.Msg

type msg_dir =
  | Sent
  | Received

exception MalformedTime
exception IncorrectUser
exception DBNotExist

let db_dir_prefix =
  "data" ^ Filename.dir_sep ^ "database" ^ Filename.dir_sep

let open_db name = db_open (db_dir_prefix ^ name ^ ".db")
let clt_req name = name ^ "_req"
let clt_msg name = name ^ "_msg"
let time_ok time = time
(* if Time.chk_time time then time else raise MalformedTime *)

let bool_op_to_str = function
  | None -> "NULL"
  | Some f -> if f then "1" else "0"

let bool_to_str f = if f then "1" else "0"

let str_op_to_str = function
  | None -> ""
  | Some s -> s

let handle_rc ok_msg = function
  | Rc.OK ->
      print_endline ok_msg;
      print_newline ();
      (true, ok_msg)
  | r ->
      print_endline (Rc.to_string r);
      print_endline ("NOT " ^ ok_msg);
      print_newline ();
      (false, "NOT " ^ ok_msg)

let dir_handle (h : bool * header) =
  match h with
  | f, _ -> f

let rec print_list = function
  | [] -> ()
  | h :: t ->
      print_string (h ^ " ");
      print_list t

let data_to_string (data : Data.t) =
  match data with
  | NONE
  | NULL ->
      ""
  | FLOAT fl -> string_of_float fl
  | TEXT str -> str
  | BLOB str -> str
  | INT num -> Int64.to_string num

let put = Printf.sprintf

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

(** Requires: name has not been added. *)
let add_client name key =
  exec (open_db "client")
    (put "INSERT INTO client (username, key) VALUES ('%s', '%s');" name
       key)
  |> handle_rc (put "Client %s inserted to client table. " name)

let is_client name =
  let res =
    fold
      (prepare (open_db "client")
         (put "SELECT username FROM client WHERE username='%s'" name))
      ~f:(fun x ar -> Array.map data_to_string ar |> Array.append x)
      ~init:(Array.make 0 "")
  in
  match res with
  | _, ar -> Array.length ar <> 0

(******************** Client Tables Creation ********************)

(** Requires: username is new *)
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

(** Requires: username is new *)
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

let in_req_tbl client user =
  let res =
    fold
      (prepare
         (open_db (clt_req client))
         (put "SELECT id FROM %s WHERE user='%s'" (clt_req client) user))
      ~f:(fun x ar -> Array.map data_to_string ar |> Array.append x)
      ~init:(Array.make 0 "")
  in
  match res with
  | _, ar -> Array.length ar <> 0

let isFriend client user =
  let res =
    fold
      (prepare
         (open_db (clt_req client))
         (put "SELECT accepted FROM %s WHERE user='%s'" (clt_req client)
            user))
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
        if in_req_tbl client user |> not then
          exec
            (open_db (clt_req client))
            (put
               "INSERT INTO %s (user, key, message, time, accepted, \
                isSender) VALUES ('%s', '%s', '%s', '%s', %s, %s);"
               (clt_req client) user (str_op_to_str key) (content req)
               (time req)
               (bool_op_to_str req_state)
               (bool_to_str (user = receiver req)))
          |> handle_rc
               (put
                  "A new friend request inserted to client %s' table. "
                  client)
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
      if in_req_tbl client username then
        exec
          (open_db (clt_req client))
          (put "UPDATE %s SET accepted=%s WHERE user='%s';"
             (clt_req client) (bool_to_str req_state) username)
        |> handle_rc
             (put "The friend state of %s (client) and %s updated. "
                client username)
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

let rec form_frd_lsts client = function
  | u :: i :: m :: t :: a :: tail ->
      if a = "1" then
        if i = "1" then
          make_msg client u t FriendReq m :: form_frd_lsts client tail
        else if i = "0" then
          make_msg u client t FriendReq m :: form_frd_lsts client tail
        else failwith "Friend list has issues with partitioning. "
      else form_frd_lsts client tail
  | [] -> []
  | _ -> failwith "Friend list has issues with partitioning. "

(* should return t list or string list?*)
let get_all_frds client =
  if is_client client then
    if create_req_table client |> dir_handle then
      let res =
        fold
          (prepare
             (open_db (clt_req client))
             (put
                "SELECT user, isSender, message, time, accepted FROM %s"
                (clt_req client)))
          ~f:(fun x ar ->
            (Array.map data_to_string ar |> Array.to_list) @ x)
          ~init:[]
      in
      match res with
      | _, lst -> form_frd_lsts client lst
    else
      failwith
        (put
           "Client %s's friend request table does not exist and cannot \
            be created. "
           client)
  else failwith (put "Client %s does not exist. " client)

let isInRequest client username = in_req_tbl client username

let get_req_by_name client username =
  if is_client client then
    if create_req_table client |> dir_handle then
      let res =
        fold
          (prepare
             (open_db (clt_req client))
             (put
                "SELECT user, isSender, message, time FROM %s WHERE \
                 user='%s'"
                (clt_req client) username))
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
          | _ -> None)
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
        if isFriend client user then
          exec
            (open_db (clt_msg client))
            (put
               "INSERT INTO %s (user, message, time, isSender) VALUES \
                ('%s', '%s', '%s', %s);"
               (clt_msg client) user (content msg) (time msg)
               (bool_to_str (user = receiver msg)))
          |> handle_rc
               (put
                  "A new message from %s to %s inserted to client %s' \
                   table. "
                  (sender msg) (receiver msg) client)
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
      let res =
        fold
          (prepare
             (open_db (clt_msg client))
             (put
                "SELECT user, message, time, isSender accepted FROM %s \
                 WHERE time > '%s' ORDER BY time ASC"
                (clt_msg client) (time_ok time)))
          ~f:(fun x ar ->
            (Array.map data_to_string ar |> Array.to_list) @ x)
          ~init:[]
      in
      match res with
      | _, lst ->
          print_int (List.length lst);
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
      if isFriend client frd then
        let res =
          fold
            (prepare
               (open_db (clt_msg client))
               (put
                  "SELECT user, message, time, isSender accepted FROM \
                   %s WHERE user='%s' ORDER BY time ASC"
                  (clt_msg client) frd))
            ~f:(fun x ar ->
              (Array.map data_to_string ar |> Array.to_list) @ x)
            ~init:[]
        in
        match res with
        | _, lst -> form_msg_lsts client lst
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
      if isFriend client frd then
        let res =
          fold
            (prepare
               (open_db (clt_msg client))
               (put
                  "SELECT user, message, time, isSender accepted FROM \
                   %s WHERE user='%s' AND time>'%s' ORDER BY time ASC"
                  (clt_msg client) frd (time_ok time)))
            ~f:(fun x ar ->
              (Array.map data_to_string ar |> Array.to_list) @ x)
            ~init:[]
        in
        match res with
        | _, lst -> form_msg_lsts client lst
      else []
    else
      failwith
        (put
           "Client %s's message table does not exist and cannot be \
            created. "
           client)
  else failwith (put "Client %s does not exist. " client)

let get_all_msgs = failwith "Unimplemented"

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
  if isInRequest "alice" "bob" then
    print_endline "Bob is in Alice's list"
  else print_endline "Bob is NOT in Alice's list";
  let lst = get_all_reqs "alice" in
  List.iter
    (fun x -> print_endline (sender x ^ " AND " ^ receiver x))
    lst;
  let lst = get_all_frds "alice" in
  List.iter
    (fun x -> print_endline (sender x ^ " AND " ^ receiver x))
    lst

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
