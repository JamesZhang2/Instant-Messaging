type msg = {
  sender : string;
  receiver : string;
  time : string;
  message : string;
}

type obj =
  | NestList of string * obj list
  | ItemList of (string * string) list

let ( ^.^ ) a b = a ^ ", \n" ^ b

(** [parse_item] item parses a string item in json. *)
let parse_item item =
  match item with
  | name, message -> "\t\"" ^ name ^ "\" : \"" ^ message ^ "\""

let rec parse_item_list lst =
  match lst with
  | [] -> ""
  | [ h ] -> parse_item h
  | h :: t -> parse_item h ^.^ parse_item_list t

(** A recursive json string writer*)
let rec convert_object objs =
  (* let inner = *)
  match objs with
  | ItemList lst ->
      parse_item_list lst
      (* let mapped = List.map parse_item lst in List.fold_left ( ^ ) ""
         mapped *)
  | NestList (name, objects) ->
      let header = "\"" ^ name ^ "\" : [\n" in
      let json_obj_lst = List.map (fun x -> [ x ]) objects in
      let str_list = List.map json_convert json_obj_lst in
      let json = String.concat ", \n" str_list in
      header ^ json ^ "\n]"

and json_convert lst =
  let str_lst = List.map convert_object lst in
  let concat = String.concat ", \n\t" str_lst in
  "{\n" ^ concat ^ "\n}"

let message_to_obj msg =
  let lst =
    [
      ("sender", msg.sender);
      ("receiver", msg.receiver);
      ("time", msg.time);
      ("message", msg.message);
    ]
  in
  ItemList lst

let error_parse message =
  ItemList [ ("type", "Error"); ("message", message) ]

let post_method_response ?(error_msg = "") text =
  if text = "Error" then json_convert [ error_parse error_msg ]
  else
    let lst = [ ItemList [ ("type", "Post"); ("message", text) ] ] in
    json_convert lst

let get_method_response msg_lst =
  let meth = ItemList [ ("type", "Get") ] in
  let msg_obj_lst = List.map message_to_obj msg_lst in
  let body = NestList ("message", msg_obj_lst) in
  json_convert [ meth; body ]

let error_response msg = ""