(** [pad_2_digits num] is the string with two characters that represents
    [num], padding 0 in the front as necessary. Requires:
    [0 <= num <= 99]. *)
let pad_2_digits num =
  if num >= 10 then string_of_int num else "0" ^ string_of_int num

let string_of_now ~local =
  let convert = if local then Unix.localtime else Unix.gmtime in
  let now = convert (Unix.time ()) in
  let year = 1900 + now.tm_year in
  let month = pad_2_digits (now.tm_mon + 1) (* [tm_mon] = [0..11] *) in
  let day = now.tm_mday in
  let hr = pad_2_digits now.tm_hour in
  let min = pad_2_digits now.tm_min in
  let sec = pad_2_digits now.tm_sec in
  Printf.sprintf "%i-%s-%i %s:%s:%s" year month day hr min sec

let time_regex =
  let year = "\\([0-9][0-9][0-9][0-9]\\)" in
  let month = "\\(0[1-9]\\|1[0-2]\\)" in
  let day = "\\([0-2][0-9]\\|3[0-1]\\)" in
  let hr = "\\([0-1][0-9]\\|2[0-3]\\)" in
  let min = "\\([0-5][0-9]\\)" in
  let sec = min in
  Printf.sprintf "%s-%s-%s %s:%s:%s" year month day hr min sec

let chk_time time =
  (* TODO: This does not check invalid dates like 2/31/2022. *)
  Str.string_match ("^" ^ time_regex ^ "$" |> Str.regexp) time 0

let earliest_time = "0000-01-01 00:00:00"
let latest_time = "9999-12-31 23:59:59"
