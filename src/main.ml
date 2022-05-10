open Js_of_ocaml
module Html = Dom_html

let js = Js.string
let document = Html.window##.document

let alter_text txt =
  let main =
    Js.Opt.get
      (document##getElementById (js "something"))
      (fun () -> assert false)
  in
  ()