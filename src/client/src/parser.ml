type response_type =
  | GetMethResponse of string
  | PostMethResponse of {
      sender : string;
      time : string;
      message : string;
    }

type t

let parse = failwith "Unimplemented"
let get_type = failwith "Unimplemented"