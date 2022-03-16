(** Networking processes library.

    This module includes functions that will be used to connect to the
    server as a client. *)

type t
(** The abstract type of response from the server. *)

type req_method =
  | Post
  | Get  (** Represents the methods sent to the server*)

val request :
  req_method -> ?header:(string * string) list -> ?body:string -> t
(** [request meth ?header ?body] is the response from the server by
    sending [meth] as http method, [body] as http request body, and
    [header] as http header. Effect: sends a specified request to the
    server. *)

val status : t -> int
(** [status res] is the status code of the response [res] from the
    server. *)

val response_body : t -> string option
(** [response_body res] is the body of the response [res] from the
    server. *)

val response_header : t -> string option
(** [response_header res] is the header of this response [res]. *)