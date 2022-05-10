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

let onload _ =
  login ();
  Js._false

let () = Html.window##.onload := Html.handler onload
