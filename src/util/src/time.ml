(** [pad_2_digits num] is the string with two characters that represents
    [num], padding 0 in the front as necessary. Requires:
    [0 <= num <= 99]. *)
let pad_2_digits num =
  if num >= 10 then string_of_int num else "0" ^ string_of_int num

let string_of_now ~local =
  let convert = if local then Unix.localtime else Unix.gmtime in
  let now = convert (Unix.time ()) in
  let hr = now.tm_hour in
  let min = pad_2_digits now.tm_min in
  let sec = pad_2_digits now.tm_sec in
  let month = now.tm_mon + 1 (* [tm_mon] = [0..11] *) in
  let day = now.tm_mday in
  let year = 1900 + now.tm_year in
  Printf.sprintf "%i:%s:%s %i/%i/%i" hr min sec month day year
