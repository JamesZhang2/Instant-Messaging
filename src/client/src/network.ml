open Yojson.Basic.Util
open Lwt
open Cohttp
open Cohttp_lwt
open Cohttp_lwt_unix

type t = Cohttp_lwt.Response.t * Cohttp_lwt.Body.t

exception RequestFailed of string
exception IncorrectMeth

let url = "http://localhost:3000/"

(** [get_body res] is the body of [res] with type [string]. *)
(* let get_body (response : Response.t) = let open Lwt_result.Syntax in
   let body = Lwt_main.run (Body.to_string response.body) in match body
   with | Ok b -> b | Error e -> raise (RequestFailed (Error.to_string
   e)) (** [get_response res] is the response [res] in type [t]. *) let
   get_response (response : (Response.t, Error.t) Lwt_result.t) = let
   res = Lwt_main.run response in match res with | Ok res -> { body =
   Some (res |> get_body); status = Status.to_code res.status; header =
   Some (Headers.to_list res.headers); } | Error e -> raise
   (RequestFailed (Error.to_string e)) *)

let request ?(header = [ ("", "") ]) ?(body = "") (meth : string) : t =
  match meth with
  | "POST" ->
      let body =
        Cohttp_lwt_unix.Client.post ~body:(Body.of_string body)
          (Uri.of_string (url ^ "post/"))
      in
      Lwt_main.run body
      (* let res = if header = [ ("", "") ] then
         Piaf.Client.Oneshot.post ~body:(Body.of_string body)
         (Uri.of_string (url ^ "post/")) else Piaf.Client.Oneshot.post
         ~headers:header ~body:(Body.of_string body) (Uri.of_string (url
         ^ "post/")) in get_response res *)
  | "GET" ->
      let body =
        Cohttp_lwt_unix.Client.get (Uri.of_string (url ^ "get/"))
      in
      Lwt_main.run body
  | _ -> raise IncorrectMeth

let status (res : t) =
  match res with
  | resp, _ -> resp |> Response.status |> Code.code_of_status

let response_body (res : t) =
  Some
    (match res with
    | _, body -> body |> Cohttp_lwt.Body.to_string |> Lwt_main.run)

let response_header (res : t) =
  Some
    (match res with
    | resp, _ -> resp |> Response.headers |> Header.to_list)