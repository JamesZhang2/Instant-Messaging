(** A module to process tasks and give a proper response. *)

type t
(** The abstract type of response for the response to client. *)

val handle : string -> ?header : (string * string) list
  -> ?body : string -> t
(** [handle meth ?header ?body] is the response to the client by sending 
[meth] as http method, [body] as http request body, and [header] as http header. 
Effect: sends a specified request to the server. *)

val status : t -> int 
(** [status res] is the status code of the response [res] to the client. *)

val response_body : t -> string option
(** [response_body res] is the body of the response [res] to the client. *)

val response_header : t -> string option 
(** [response_header res] is the header of this response [res]. *)