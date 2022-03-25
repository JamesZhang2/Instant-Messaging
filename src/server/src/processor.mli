(** A module that processes tasks handed by the server and gives a
    proper response back to the server. *)

type t
(** The abstract type for the response to the client. *)

val handle : string -> (string * string) list -> string Lwt.t -> t Lwt.t
(** [handle meth headers body] is the response to the client by sending
    [meth] as http method, [body] as http request body, and [header] as
    http header, as a [t Lwt.t]. Effect: sends a specified request to
    the server. *)

val status : t -> string Lwt.t
(** [status res] is the status code Lwt.t of the response [res] to the
    client. *)

val response_body : t -> string Lwt.t
(** [response_body res] is the body Lwt.t of the response [res] to the
    client. *)

val status_body : t -> (string * string) Lwt.t
(** [status_body res] is a tuple of status and body in Lwt*)

val response_headers : t -> (string * string) list
(** [response_header res] is the header of this response [res]. *)
