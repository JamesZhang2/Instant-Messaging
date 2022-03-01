(* Adapted from https://github.com/anmonteiro/piaf *)

open Piaf

let get_sync url =
  let open Lwt_result.Syntax in
  Lwt_main.run
    begin
      print_endline "Sending request...";

      let* response = Client.Oneshot.get (Uri.of_string url) in

      if Status.is_successful response.status then
        Body.to_string response.body
      else
        let message = Status.to_string response.status in
        Lwt.return (Error (`Msg message))
    end

let post_message url (message : string) =
  let open Lwt_result.Syntax in
  Lwt_main.run
    begin
      let* post_req =
        Client.Oneshot.post
          ~body:(Body.of_string message)
          (Uri.of_string url)
      in

      if Status.is_successful post_req.status then
        Body.to_string post_req.body
      else
        let message = Status.to_string post_req.status in
        Lwt.return (Error (`Msg message))
    end

let () =
  print_string "Enter a message: ";
  let msg = read_line () in
  match post_message "http://localhost:3000/message" msg with
  | Ok body -> print_endline body
  | Error error ->
      let message = Error.to_string error in
      prerr_endline ("Error: " ^ message)
