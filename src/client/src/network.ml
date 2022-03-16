open Piaf
open Yojson.Basic.Util

type t = {
  status : int;
  body : string option;
  header : (string * string) list option;
}

exception RequestFailed of string
exception IncorrectMeth

let url = "http://localhost:3000/"

let get_body (response : Response.t) =
  let open Lwt_result.Syntax in
  let body = Lwt_main.run (Body.to_string response.body) in
  match body with
  | Ok b -> b
  | Error e -> raise (RequestFailed (Error.to_string e))

let get_response (response : (Response.t, Error.t) Lwt_result.t) =
  let res = Lwt_main.run response in
  match res with
  | Ok res ->
      {
        body = Some (res |> get_body);
        status = Status.to_code res.status;
        header = Some (Headers.to_list res.headers);
      }
  | Error e -> raise (RequestFailed (Error.to_string e))

let request ?(header = [ ("", "") ]) ?(body = "") (meth : string) : t =
  match meth with
  | "POST" ->
      let res =
        if header = [ ("", "") ] then
          Piaf.Client.Oneshot.post ~body:(Body.of_string body)
            (Uri.of_string (url ^ "post/"))
        else
          Piaf.Client.Oneshot.post ~headers:header
            ~body:(Body.of_string body)
            (Uri.of_string (url ^ "post/"))
      in
      get_response res
  | "GET" ->
      let res =
        Piaf.Client.Oneshot.get (Uri.of_string (url ^ "get/"))
      in
      get_response res
  | _ -> raise IncorrectMeth

let status res = res.status
let response_body res = res.body
let response_header res = res.header