type t = {
  status : string;
  body : string;
  headers : (string * string) list;
}

let handle meth headers body = { status = "201"; headers; body }
let status res = res.status
let response_body res = res.body
let response_headers res = res.headers
