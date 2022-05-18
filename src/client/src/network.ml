open Yojson.Basic.Util
open Lwt
open Cohttp
open Cohttp_lwt
open Cohttp_lwt_unix

type t = Cohttp_lwt.Response.t * Cohttp_lwt.Body.t

exception RequestFailed of string
exception IncorrectMeth

let url = "http://localhost:3000/"

let request ?(header = [ ("", "") ]) ?(body = "") (meth : string) : t =
  match meth with
  | "POST" ->
      let body =
        Cohttp_lwt_unix.Client.post ~body:(Body.of_string body)
          (Uri.of_string (url ^ "post/"))
      in
      Lwt_main.run body
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