open Packager
open Parser
open Network

type msg = {
  sender : string;
  receiver : string;
  time : string;
  body : string;
}

exception EmptyBody

let send_msg sender receiver msg =
  let msg = Packager.pack_send_msg sender receiver msg in
  let raw_response = Network.request "Post" ~body:msg ~header:[] in
  let raw_body = Network.response_body raw_response in
  (* let body = *)
  match raw_body with
  | None -> false
  | Some str -> true
(* in let response = body |> Parser.parse |> Parser.get_plain in
   response *)

let get_msg = failwith "Unimplemented"
let register = failwith "Unimplemented"
let login = failwith "Unimplemented"
let friend_req = failwith "Unimplemented"
let friend_req_reply = failwith "Unimplemented"
