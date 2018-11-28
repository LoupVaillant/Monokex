module P = Proto

let (//) l p = List.filter p l
let (/@) l f = List.map    f l
let error s  = raise (Invalid_argument s)

type key      = IS | IE | RS | RE
type exchange = key * key
type action   = K of key
              | E of exchange

let client_key = function S -> IS | E -> IE
let server_key = function S -> RS | E -> RE
let exchange (ik, rk) = (client_key ik, server_key rk)
let action key = function
  | Key k      -> K (key      k)
  | Exchange e -> E (exchange e)
let message = function
  | Client m -> List.map (action client_key) m
  | Server m -> List.map (action server_key) m
let protocol (pre, post) =
  (List.map message pre,
   List.map message post)
let flat_pre  p = p |> protocol |> fst |> List.concat
let flat_post p = p |> protocol |> snd |> List.concat
let flat_all  p = flat_pre p @ flat_post p

let is_key          = function K _ -> true | E _ -> false
let is_exchange     = function E _ -> true | K _ -> false
let to_key          = function K k -> k    | E _ -> error "to_key"
let to_exchange     = function E e -> e    | K _ -> error "to_exchange"
let all_keys      p = flat_all p // is_key      /@ to_key
let all_exchanges p = flat_all p // is_exchange /@ to_exchange

let string_of_exchange = function
  | (IS, RS) -> "ss"
  | (IS, RE) -> "se"
  | (IE, RS) -> "es"
  | (IE, RE) -> "ee"
  | _        -> error "string_of_exchange"

let keys p =
  let actions  = all_keys p                             in
  let kk k txt = if List.mem k actions then txt else "" in
  ""
  ^ kk IS "- __(si, Is)__ The initiator's static key.\n"
  ^ kk IE "- __(ie, IE)__ The initiator's ephemeral key.\n"
  ^ kk RS "- __(rs, RS)__ The recipient's static key.\n"
  ^ kk RE "- __(re, RE)__ The recipient's ephemeral key.\n"

let secrets p =
  all_exchanges p
  |@ (function
      | (IS, RS) -> "- __ss__ = X25519(is, RS) = X25519(rs, IS)\n"
      | (IS, RE) -> "- __se__ = X25519(is, RE) = X25519(re, IS)\n"
      | (IE, RS) -> "- __es__ = X25519(ie, RS) = X25519(rs, IE)\n"
      | (IE, RE) -> "- __ee__ = X25519(ie, RE) = X25519(re, IE)\n"
      | _          -> error "secrets")
  |> String.concat ""

let rec mapi i f = function
  | x::l -> f i x :: mapi (i+1) f l
  | _    -> []
let rec map3 f l1 l2 l3 = match l1, l2, l3 with
  | x::l1, y::l2, z::l3 -> f x y z :: map3 f l1 l2 l3
  | _                   -> []

let chaining_keys p =
  let exchanges = all_exchanges p                                         in
  let e         = exchanges |@ string_of_exchange                         in
  let ck        = exchanges |> mapi 1 (fun i _ -> "CK" ^ string_of_int i) in
  let ck0       = "zero" :: List.map (fun c -> c ^ " ") ck                in
  map3 (fun ck ck0 e -> "- __" ^ ck ^ ":__ Blake2b-256("^ ck0 ^", "^ e^")\n")
    ck ck0 e
  |> String.concat ""

let rec sums acc = function
  | []      -> []
  | x :: xs -> let sum = acc + x in
               sum :: sums sum xs

let auth_keys p =
  p
  |> protocol
  |> snd
  |> List.map (fun m -> m // is_exchange |> List.length)
  |> List.filter ((<>) 0)
  |> sums 0
  |> List.map (fun i -> let s = string_of_int i in
                        "- __AK" ^ s ^ ":__ Blake2b-512(CK" ^ s ^ ")[0:31]\n")
  |> String.concat ""

let encryption_keys p =
  let actions   = flat_post p                                        in
  let shifted   = List.tl actions @ [K IS]                           in
  let zip       = List.map2 (fun a b -> (a, b)) actions shifted      in
  zip // (function E _, _ -> true | _ -> false)
  |> mapi 1 (fun i -> function
         | E _, E _ -> ""
         | E _, K _ -> let s = string_of_int i in
                       "- __EK"^ s ^":__ Blake2b-512(CK"^ s ^")[32:63]\n"
         | _        -> error "encryption_key")
  |> String.concat ""

let pe = print_endline
let ps = print_string

let print_protocol p =
  pe "Sender and recipient have the following X25519 key pairs (private half";
  pe "in lower case, public half in upper case):";
  pe "";
  ps (keys p);
  pe "";
  pe "Those key pairs are used to derive the following shared secrets:";
  pe "";
  ps (secrets p);
  pe "";
  pe "Those shared secrets are hashed to derive the following keys:";
  pe "";
  ps (chaining_keys p);
  ps (auth_keys p);
  ps (encryption_keys p);
  pe "";
  pe "_(\"[x:y]\" denotes a range; Blake2b-256 is used in keyed mode, with the";
  pe "key on the left.)_";
  pe "";
  pe "The message contain the following (`||` denotes concatenation):";
  pe "";
  ()

let proto str =
  str
  |> Lexing.from_string
  |> Scan.tokens
  |> Parsec.parse

let _ = print_protocol (proto "<- s ... -> e    <- e, ee, es  -> s, se")
