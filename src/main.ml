open Js_of_ocaml
module Html = Dom_html

let js = Js.string
let document = Html.window##.document

let alter_text txt =
  let head = document##getElementById (js "something") in
  let main = Js.Opt.get head (fun () -> assert false) in
  main##.innerHTML := js txt

let onload _ =
  alter_text "Hi";
  Js._false

let () = Html.window##.onload := Html.handler onload
