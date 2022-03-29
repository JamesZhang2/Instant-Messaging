open Cryptokit

type k = string

let sym_gen () = "1234123412341234"
(* Random.(string secure_rng 16) *)
(* 16/24/32*)

let sym_enc key msg =
  let aes =
    Cipher.(aes ~mode:CBC ~pad:Cryptokit.Padding.length key Encrypt)
  in
  transform_string aes msg

let sym_dec key msg =
  let aes =
    Cipher.(aes ~mode:CBC ~pad:Cryptokit.Padding.length key Decrypt)
  in
  transform_string aes msg

type t = RSA.key

type pk = {
  size : int;
  n : string;
  e : string;
}

type sk = RSA.key

let pke_gen () = RSA.new_key 1024
let get_pub (key : t) = { size = key.size; n = key.n; e = key.e }
let get_priv key = key

(** [pub_enc pub msg] is the RSA-encrypted message of [msg] using [pub]
    key. *)
let pub_enc pub msg =
  let key = { (RSA.new_key pub.size) with n = pub.n; e = pub.e } in
  RSA.encrypt key msg

(** [pub_dec priv msg] is the RSA-decrpted message of [msg] using [priv]
    key. *)
let pub_dec key msg = RSA.decrypt key msg

let pke_enc pub msg =
  let symk = sym_gen () in
  (pub_enc pub symk, sym_enc symk msg)

let pke_dec key = function
  | str1, str2 ->
      let symk = pub_dec key str1 in
      sym_dec (String.sub symk (String.length symk - 16) 16) str2
(* let test str = let key = pke_gen () in let ctxt = pke_enc (get_pub
   key) str in pke_dec key ctxt *)

let get_pub_str key = failwith "Unimplemented"
let pub_from_str str = failwith "Unimplemented"