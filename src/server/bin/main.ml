(** Accepts Requests from clients, hands it to the processor which
    produces Responses, and sends back Responses to clients. *)

(* Adapted from https://rgrinberg.github.io/opium/opium/index.html and
   https://github.com/rgrinberg/opium *)

open Opium
open Server.Processor
open Server.Database

(** [get_headers req] is the headers in [req]. *)
let get_headers req = req.Request.headers |> Headers.to_list

(** [get_meth req] is the method of [req]. *)
let get_meth req = req.Request.meth |> Method.to_string

(** [response_maker (status, body)] makes a Response Lwt using [status]
    and [body]*)
let response_maker (status, body) =
  let made =
    Response.make
      ~status:(status |> Status.of_string)
        (* ~headers:(res |> response_headers |> Headers.of_list) *)
      ~body:(body |> Body.of_string)
      ()
  in
  Lwt.return made

(** [process req] processes the request [req] got from the clients. *)
let process (req : Request.t) =
  let res =
    handle (get_meth req) (get_headers req)
      (req.Request.body |> Body.to_string)
  in
  let status_body = Lwt.bind res status_body in
  Lwt.bind status_body response_maker

let () =
  create_tables ();
  App.empty
  |> App.post "/post/" process
  |> App.get "/get/" process |> App.run_command |> ignore
