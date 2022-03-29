(** A useful package for formatting time. *)

val string_of_now : local:bool -> string
(** [string_of_now ~local] is the string representation of the current
    time. If [~local] is true, then the local time is used. Otherwise,
    GMT is used. *)

val time_regex : string
(** The regular expression for correctly formatted time. *)

val chk_time : string -> bool
(** [chk_time time] is [true] if [time] is in the right time format and
    [false] otherwise. The right time format is YYYY-MM-DD HH:MM:SS. The
    hour field must be between 00 and 23, inclusive. *)

val earliest_time : string
(** [earliest_time] is the earliest time that can be represented by this
    module. *)

val latest_time : string
(** [latest_time] is the latest time that can be represented by this
    module. *)
