(** A useful package for formatting time. *)

val string_of_now : local:bool -> string
(** [string_of_now ~local] is the string representation of the current
    time. If [~local] is true, then the local time is used. Otherwise,
    GMT is used. *)
