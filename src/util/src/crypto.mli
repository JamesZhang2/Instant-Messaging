(** A crypto library specifically for Instant Messaging. *)

type k
(** The abstract type of the symmetric key. *)

val sym_gen : unit -> k
(** [sym_gen] is a newly generated symmetric key. *)

val sym_enc : k -> string -> string
(** [sym_enc key ptext] is the ciphertext of [ptext] encrypted using
    [key]. *)

val sym_dec : k -> string -> string
(** [sym_dec key ctext] is the plaintext of [ctext] decrypted using
    [key]*)

type t
(** The abstract type of the public key-secret key pair. *)

type pk
(** The abstract type of the public key. *)

type sk
(** The abstract type of the secret key. *)

val pke_gen : unit -> t
(** [pke_gen] is a newly generated asymmetric key pair. *)

val get_pub : t -> pk
(** [get_pub key] is the public key component of [key]. *)

val get_priv : t -> sk
(** [get_priv key] is the secret key component of [key]. *)

val pke_enc : pk -> string -> string * string
(** [pke_enc pub ptext] is the ciphertext of [ptext] encrypted using
    [pub] key. *)

val pke_dec : sk -> string * string -> string
(** [pke_dec priv ctext] is the plaintext of [ctext] decrypted using
    [priv] key. *)

val get_pub_str : k -> string
(** [get_pub_str pub_key] is a storable string representation of the
    prublic key [pub_key]*)

val pub_from_str : string -> k
(** [pub_from_str str_key] is the abstract representation of public key
    given by string [str_key]*)
