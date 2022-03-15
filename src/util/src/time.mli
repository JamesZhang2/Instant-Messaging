(** A useful package for formatting time. *)

val string_of_now : local:bool -> string
(** [string_of_now ~local] is the string representation of the current
    time. If [~local] is true, then the local time is used. Otherwise,
    GMT is used. *)

val time_regex : string
(** The regular expression for correctly formatted time. *)

val chk_time : string -> bool
(** [chk_time time] is true if [time] is in the right time format and
    false otherwise. The right time format is (h)h:mm:ss (m)m/(d)d/yyyy,
    where the hour, month and day fields do not start with a 0 (unless
    hour is 0) while the minute and second fields must have exactly two
    digits. The hour field must be between 0 and 23, inclusive. *)
