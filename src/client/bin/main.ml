(* Adapted from https://github.com/anmonteiro/piaf *)

(* open Piaf open Yojson.Basic.Util

   let string_of_json file_name = Yojson.Basic.from_file file_name |>
   Yojson.Basic.to_string

   (** [get_result response] is the body of the response if the
   connection is successful, or the error returned if the connection is
   unsuccessful. *) let get_result (response : Response.t) = let open
   Lwt_result.Syntax in if Status.is_successful response.status then
   Body.to_string response.body else let message = Status.to_string
   response.status in Lwt.return (Error (`Msg message))

   (** [get_sync url] attempts to send a get request to [url]. *) let
   get_sync url = let open Lwt_result.Syntax in Lwt_main.run begin
   print_endline "Sending request..."; let* response =
   Client.Oneshot.get (Uri.of_string url) in get_result response end

   (** [post_message url message] attempts to send a post request to
   [url] with [message] as its body. *) let post_message url (message :
   string) = let open Lwt_result.Syntax in Lwt_main.run begin
   print_endline "Sending request..."; let* post_req =
   Client.Oneshot.post ~body:(Body.of_string message) (Uri.of_string
   url) in get_result post_req end

   (** [post_json url filename] sends a post request to [url], with body
   being the string converted from [filename]. Requires: [filename] is a
   valid json file.*) let post_json url (filename : string) =
   post_message url (filename |> string_of_json) *)
open Client.Network

let rec main () =
  begin
    let x = request ~body:"hello" "POST" in
    match response_body x with
    | None -> print_endline "nothing"
    | Some str -> print_endline str
  end;
  main ()
(* print_string "Enter message, then press enter to send: "; let msg =
   read_line () in (* TODO: put the message into a json file *) match
   post_message "http://localhost:3000/post/" msg with | Ok body ->
   print_endline body; main () | Error error -> let message =
   Error.to_string error in prerr_endline ("Error: " ^ message) *)

let () = main ()
