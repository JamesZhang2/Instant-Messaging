(* Adapted from https://rgrinberg.github.io/opium/opium/index.html and
   https://github.com/rgrinberg/opium *)

open Opium

let hello _req = Response.of_plain_text "Hello World" |> Lwt.return

let greet req =
  let _ = print_endline "called" in
  let name = Router.param req "name" in
  Printf.sprintf "Hello, %s" name
  |> Response.of_plain_text |> Lwt.return

let more req =
  let msg = Router.param req "message" in
  Printf.sprintf "Message received: %s" msg
  |> Response.of_plain_text |> Lwt.return

let handle_post req =
  let content = Body.to_stream req.Request.body in
  let content' =
    Lwt_stream.map (fun s -> "Message received: " ^ s) content
    (* We can process the input using Lwt_stream.map *)
  in
  Response.make ~body:(Body.of_stream content') () |> Lwt.return

let () =
  (* let open App in *)
  App.empty |> App.get "/" hello
  |> App.get "/greet/:name" greet
  |> App.post "/message" handle_post
  |> App.run_command |> ignore
