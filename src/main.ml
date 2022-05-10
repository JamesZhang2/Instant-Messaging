(* Adapted from
   https://github.com/ocsigen/js_of_ocaml/blob/master/examples/minesweeper *)

open Js_of_ocaml
module Html = Dom_html

let js = Js.string
let document = Html.window##.document
let log (txt : string) : unit = Firebug.console##log (js txt)
let show_elt elt = elt##.style##.display := js "block"
let hide_elt elt = elt##.style##.display := js "none"
let login_window = Html.getElementById "login-window"
let main_window = Html.getElementById "main-window"

let alter_text elt_id txt =
  let head = Html.getElementById elt_id in
  head##.innerHTML := js txt

let get_username () : string =
  let username_box = Html.getElementById "username-box" in
  Js.to_string (Js.Unsafe.coerce username_box)##.value

let login () =
  let login_btn = Html.getElementById "login-btn" in
  login_btn##.onclick :=
    Html.handler (fun ev ->
        alter_text "current_user"
          ("You are currently logged in as " ^ get_username ());
        (* hide_elt login_window; show_elt main_window; *)
        (* TODO: showing the main window doesn't work: destroys the
           styles*)
        Js._false)

type tab =
  | TMsg
  | TFriends
  | TFriendReq

let switch_tab_aux (tab : tab) : unit =
  let msg_tab = Html.getElementById "sidebar-msg" in
  let friends_tab = Html.getElementById "sidebar-friends" in
  let fr_tab = Html.getElementById "sidebar-fr" in
  match tab with
  | TMsg ->
      show_elt msg_tab;
      hide_elt friends_tab;
      hide_elt fr_tab
  | TFriends ->
      hide_elt msg_tab;
      show_elt friends_tab;
      hide_elt fr_tab
  | TFriendReq ->
      hide_elt msg_tab;
      hide_elt friends_tab;
      show_elt fr_tab

let tab_btn_event btn tab =
  btn##.onclick :=
    Html.handler (fun ev ->
        switch_tab_aux tab;
        Js._true)

let switch_tab () =
  let msg_btn = Html.getElementById "tab-msg-btn" in
  let friends_btn = Html.getElementById "tab-friends-btn" in
  let fr_btn = Html.getElementById "tab-fr-btn" in
  tab_btn_event msg_btn TMsg;
  tab_btn_event friends_btn TFriends;
  tab_btn_event fr_btn TFriendReq

let onload _ =
  login ();
  switch_tab ();
  Js._false

let () = Html.window##.onload := Html.handler onload
