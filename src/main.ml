(* Adapted from
   https://github.com/ocsigen/js_of_ocaml/blob/master/examples/minesweeper *)

open Js_of_ocaml
module Html = Dom_html

let js = Js.string
let document = Html.window##.document
let log (txt : string) : unit = Firebug.console##log (js txt)

(**[set_display elt value] sets the display property of [elt] to
   [value]. Requires: [value] is a valid property value of display, such
   as none, block, grid, flex, etc. *)
let set_display elt (value : string) = elt##.style##.display := js value

let login_window = Html.getElementById "login-window"
let main_window = Html.getElementById "main-window"

(* TODO: Should we define some elements here that are used
   frequently? *)

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
        set_display login_window "none";
        set_display main_window "grid";
        Js._false)

let logout () =
  let logout_btn = Html.getElementById "logout-btn" in
  logout_btn##.onclick :=
    Html.handler (fun ev ->
        set_display login_window "grid";
        set_display main_window "none";
        let username_box = Html.getElementById "username-box" in
        (Js.Unsafe.coerce username_box)##.value := js "";
        let password_box = Html.getElementById "password-box" in
        (Js.Unsafe.coerce password_box)##.value := js "";
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
      set_display msg_tab "block";
      set_display friends_tab "none";
      set_display fr_tab "none"
  | TFriends ->
      set_display msg_tab "none";
      set_display friends_tab "block";
      set_display fr_tab "none"
  | TFriendReq ->
      set_display msg_tab "none";
      set_display friends_tab "none";
      set_display fr_tab "block"

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
  logout ();
  Js._false

let () = Html.window##.onload := Html.handler onload
