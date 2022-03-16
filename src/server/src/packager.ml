type msg = {
  sender : string;
  receiver : string;
  time : string;
  message : string;
}

type obj =
  | List of string * obj list
  | Item of string * string

(** A recursive json string writer*)
let rec convert_object objs =
  let inner =
    match objs with
    | Item (name, message) ->
        "\"" ^ name ^ "\" : " ^ "\"" ^ message ^ "\", "
    | List (name, objects) ->
        let rec parse_list lst =
          match lst with
          | [] -> ""
          | h :: t -> convert_object h ^ convert_object (List (name, t))
        in
        "\"" ^ name ^ "\" : " ^ "[" ^ parse_list objects ^ "\", "
  in
  "{" ^ inner ^ "}"

let message_to_obj msg =
  let lst =
    [
      ("sender", msg.sender);
      ("receiver", msg.receiver);
      ("time", msg.time);
      ("message", msg.message);
    ]
  in
  let new_list = List.map (fun (a, b) -> Item (a, b)) lst in
  List ("message", new_list)

let error_parse = Item ("type", "Error")

let post_method_response text =
  if text = "Error" then convert_object error_parse
  else
    let lst = [ (Item ("type", "Post"), Item ("message", text)) ] in
    failwith "Unimplemented"

let get_method_response = failwith "Unimplemented"
let error_response = failwith "Unimplemented"