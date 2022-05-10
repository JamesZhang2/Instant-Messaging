(* Adapted from
   https://github.com/ocsigen/js_of_ocaml/blob/master/examples/minesweeper *)

open Js_of_ocaml
module Html = Dom_html

let js = Js.string
let document = Html.window##.document
let log (txt : string) : unit = Firebug.console##log (js txt)

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
        log (get_username ());
        Js._true)

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
      msg_tab##.style##.display := js "block";
      friends_tab##.style##.display := js "none";
      fr_tab##.style##.display := js "none"
  | TFriends ->
      msg_tab##.style##.display := js "none";
      friends_tab##.style##.display := js "block";
      fr_tab##.style##.display := js "none"
  | TFriendReq ->
      msg_tab##.style##.display := js "none";
      friends_tab##.style##.display := js "none";
      fr_tab##.style##.display := js "block"

let switch_tab () =
  let msg_btn = Html.getElementById "tab-msg-btn" in
  let friends_btn = Html.getElementById "tab-friends-btn" in
  let fr_btn = Html.getElementById "tab-fr-btn" in
  msg_btn##.onclick :=
    Html.handler (fun ev ->
        switch_tab_aux TMsg;
        Js._true);
  friends_btn##.onclick :=
    Html.handler (fun ev ->
        switch_tab_aux TFriends;
        Js._true);
  fr_btn##.onclick :=
    Html.handler (fun ev ->
        switch_tab_aux TFriendReq;
        Js._true)

let onload _ =
  login ();
  switch_tab ();
  Js._false

let () = Html.window##.onload := Html.handler onload
