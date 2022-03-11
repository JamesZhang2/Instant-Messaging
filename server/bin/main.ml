(** Accepts Requests from clients, hands it to the processor which
    produces Responses, and sends back Responses to clients. *)

(* Adapted from https://rgrinberg.github.io/opium/opium/index.html and
   https://github.com/rgrinberg/opium *)

open Opium
open Server.Processor

let get_body req =
  match req.Request.body |> Body.to_string |> Lwt.state with
  | Sleep -> ""
  | Return s -> s
  | Fail exn -> raise exn

let get_headers req = req.Request.headers |> Headers.to_list
let get_meth req = req.Request.meth |> Method.to_string

let process (req : Request.t) =
  let res = handle (get_meth req) (get_headers req) (get_body req) in
  Response.make
    ~status:(res |> status |> Status.of_string)
    ~headers:(res |> response_headers |> Headers.of_list)
    ~body:(res |> response_body |> Body.of_string)
    ()
  |> Lwt.return

(* let greet req = let _ = print_endline "called" in let name =
   Router.param req "name" in Printf.sprintf "Hello, %s" name |>
   Response.of_plain_text |> Lwt.return

   let more req = let msg = Router.param req "message" in Printf.sprintf
   "Message received: %s" msg |> Response.of_plain_text |> Lwt.return

   (** [handle_post req] handles the post request [req]. *) let
   handle_post req = let content = Body.to_stream req.Request.body in
   let content' = Lwt_stream.map (fun s -> "Message received: " ^ s)
   content (* We can process the input using Lwt_stream.map *) in
   Response.make ~body:(Body.of_stream content') () |> Lwt.return *)

(** [json_to_msg json] converts [json] to a message. *)
(* let json_to_msg json = "Message from " ^ (json |>
   Yojson.Basic.from_string |> from_json).user ^ ": " ^ (json |>
   Yojson.Basic.from_string |> from_json).msg *)

(** [handle_json req] handles the post request [req]. Requires: [json]
    is the string representation of a json file.*)
(* let handle_json req = let content_json = Body.to_stream
   req.Request.body in let content_str = Lwt_stream.map json_to_msg
   content_json in Response.make ~body:(Body.of_stream content_str) ()
   |> Lwt.return *)

let () =
  (* let open App in *)
  App.empty |> App.get "/get/" process
  |> App.post "/post/" process
  |> App.run_command |> ignore
