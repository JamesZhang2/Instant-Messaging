(* Adapted from https://rgrinberg.github.io/opium/opium/index.html and
   https://github.com/rgrinberg/opium *)

open Opium
open Yojson.Basic.Util

type t = {
  user : string;
  msg : string;
}

let t_of_json j =
  {
    user = j |> member "user" |> to_string;
    msg = j |> member "message" |> to_string;
  }

let from_json json =
  try t_of_json json with
  | Type_error (s, _) -> failwith ("Parsing error: " ^ s)

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

let json_to_msg str =
  "Message received: "
  ^ (str |> Yojson.Basic.from_string |> from_json).msg

let handle_json req =
  let content = Body.to_stream req.Request.body in
  let content' = Lwt_stream.map json_to_msg content in
  Response.make ~body:(Body.of_stream content') () |> Lwt.return

let () =
  (* let open App in *)
  App.empty |> App.get "/" hello
  |> App.get "/greet/:name" greet
  |> App.post "/message" handle_post
  |> App.post "/json" handle_json
  |> App.run_command |> ignore
