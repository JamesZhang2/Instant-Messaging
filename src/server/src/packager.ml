open Util

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

(** [convert_object obs] converts an [object obj] to a json string
    recursively*)
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
(** Converts a list of [object] to json string*)
and json_convert lst =
  let str_lst = List.map convert_object lst in
  let concat = String.concat ", \n\t" str_lst in
  "{\n" ^ concat ^ "\n}"

let message_to_obj msg =
  let msg_t =
    if Msg.msg_type msg = Msg.Message then "Message" else "FriendReq"
  in
  let lst =
    [
      ("sender", Msg.sender msg);
      ("receiver", Msg.receiver msg);
      ("time", Msg.time msg);
      ("msg_type", msg_t);
      ("message", Msg.content msg);
    ]
  in
  ItemList lst

let error_parse message =
  ItemList
    [
      ("type", "Error");
      ("time", Util.Time.string_of_now true);
      ("message", message);
    ]

let post_method_response text =
  let lst =
    [
      ItemList
        [
          ("type", "Post");
          ("time", Util.Time.string_of_now true);
          ("message", text);
        ];
    ]
  in
  json_convert lst

let get_method_response msg_lst =
  let meth =
    ItemList
      [ ("type", "GetMsg"); ("time", Util.Time.string_of_now true) ]
  in
  let msg_obj_lst = List.map message_to_obj msg_lst in
  let body = NestList ("message", msg_obj_lst) in
  json_convert [ meth; body ]

let error_response msg = json_convert [ error_parse msg ]
