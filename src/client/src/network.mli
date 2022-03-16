(** Networking processes library. This module includes functions that
    will be used to connect to the server as a client. *)

exception RequestFailed of string
(** Raised when the request is failed to send or its response has error. *)

exception IncorrectMeth
(** Raised when the specified http method is incorrect. *)

type t
(** The abstract type of response from the server. *)

val request :
  ?header:(string * string) list -> ?body:string -> string -> t
(** [request ?header ?body meth] is the response from the server by
    sending [meth] as http method, [body] as http request body, and
    [header] as http header. Effect: sends a specified request to the
    server. *)

val status : t -> int
(** [status res] is the status code of the response [res] from the
    server. *)

val response_body : t -> string option
(** [response_body res] is the body of the response [res] from the
    server. *)

val response_header : t -> (string * string) list option
(** [response_header res] is the header of this response [res]. *)