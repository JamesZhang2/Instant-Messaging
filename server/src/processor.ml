type t = {
  status : string;
  body : string;
  headers : (string * string) list;
}

(* For debugging *)
let print_headers headers =
  List.map (fun (a, b) -> print_endline (a ^ ": " ^ b)) headers
  |> ignore

let handle meth headers body =
  let body' = "Message Received: " ^ body in
  let headers' =
    [
      ("Host", "localhost");
      ("content-length", body' |> String.length |> string_of_int);
    ]
  in
  { status = "201"; headers = headers'; body = body' }

let status res = res.status
let response_body res = res.body
let response_headers res = res.headers
