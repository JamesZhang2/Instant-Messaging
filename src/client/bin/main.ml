(* Adapted from
   https://github.com/ocsigen/js_of_ocaml/blob/master/examples/minesweeper *)

open Js_of_ocaml
module Html = Dom_html

let onload _ = Js._false
let () = Html.window##.onload := Html.handler onload
