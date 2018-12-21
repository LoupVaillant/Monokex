(* Utils *)
let (//) l p = List.filter p l
let (/@) l f = List.map    f l
let error s  = raise (Invalid_argument s)

let rec mapi i f = function
  | x::l -> f i x :: mapi (i+1) f l
  | _    -> []
let rec map2 f l1 l2 = match l1, l2 with
  | x::l1, y::l2 -> f x y :: map2 f l1 l2
  | _            -> []
let rec map3 f l1 l2 l3 = match l1, l2, l3 with
  | x::l1, y::l2, z::l3 -> f x y z :: map3 f l1 l2 l3
  | _                   -> []
let rec sums acc = function
  | []      -> []
  | x :: xs -> let sum = acc + x in
               sum :: sums sum xs
let rec remove_last = function
  | []    -> error "remove_last"
  | [_]   -> []
  | x::xs -> x :: remove_last xs
let pad_right strings =
  let width = List.fold_left (fun w s -> max w (String.length s)) 0 strings in
  strings /@ (fun s -> s ^ String.make (width - String.length s) ' ')

(* Convert protocol to a less implicit type *)
type key      = IS | IE | RS | RE
type exchange = key * key
type action   = K of key
              | E of exchange

let client_key = function Proto.S -> IS | Proto.E -> IE
let server_key = function Proto.S -> RS | Proto.E -> RE
let exchange (ik, rk) = (client_key ik, server_key rk)
let action key = function
  | Proto.Key k      -> K (key      k)
  | Proto.Exchange e -> E (exchange e)
let message = function
  | Proto.Client m -> List.map (action client_key) m
  | Proto.Server m -> List.map (action server_key) m
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

(* Convert protocol to specs *)
let string_of_key : key -> string = function
  | IS -> "IS"
  | IE -> "IE"
  | RS -> "RS"
  | RE -> "RE"

let string_of_exchange : exchange -> string = function
  | (IS, RS) -> "ss"
  | (IS, RE) -> "se"
  | (IE, RS) -> "es"
  | (IE, RE) -> "ee"
  | _        -> error "string_of_exchange"

let keys : Proto.protocol -> string = fun p ->
  let actions  = all_keys p                             in
  let kk k txt = if List.mem k actions then txt else "" in
  ""
  ^ kk IS "- __(is, IS)__ The initiator's static key.\n"
  ^ kk IE "- __(ie, IE)__ The initiator's ephemeral key.\n"
  ^ kk RS "- __(rs, RS)__ The recipient's static key.\n"
  ^ kk RE "- __(re, RE)__ The recipient's ephemeral key.\n"

let secrets p =
  all_exchanges p
  /@ (function
      | (IS, RS) -> "- __ss__ = X25519(is, RS) = X25519(rs, IS)\n"
      | (IS, RE) -> "- __se__ = X25519(is, RE) = X25519(re, IS)\n"
      | (IE, RS) -> "- __es__ = X25519(ie, RS) = X25519(rs, IE)\n"
      | (IE, RE) -> "- __ee__ = X25519(ie, RE) = X25519(re, IE)\n"
      | _          -> error "secrets")
  |> String.concat ""

let chaining_keys : Proto.protocol -> string = fun p ->
  let exchanges = all_exchanges p                                         in
  let e         = exchanges /@ string_of_exchange                         in
  let ck        = exchanges |> mapi 1 (fun i _ -> "CK" ^ string_of_int i) in
  let ck0       = "zero" :: List.map (fun c -> c ^ " ") ck                in
  map3 (fun ck ck0 e -> "- __"                     ^ ck
                        ^ ":__ HChacha20("         ^ e
                        ^ ", zero) XOR HChacha20(" ^ ck0
                        ^ ", one)\n")
    ck ck0 e
  |> String.concat ""

let auth_numbers : Proto.protocol -> int list = fun p ->
  protocol p
  |> snd
  |> List.map (fun m -> m // is_exchange |> List.length)
  |> List.filter ((<>) 0)
  |> sums 0

let auth_keys : Proto.protocol -> string = fun p ->
  auth_numbers p
  |> List.map (fun i -> let s = string_of_int i in
                        "- __AK"^ s ^":__ Chacha20(CK"^ s ^", one)[ 0:31]\n")
  |> String.concat ""

let encryption_keys : Proto.protocol -> string = fun p ->
  let actions   = flat_post p                              in
  let shifted   = List.tl actions @ [K IS]                 in
  let zip       = map2 (fun a b -> (a, b)) actions shifted in
  zip // (function E _, _ -> true | _ -> false)
  |> mapi 1 (fun i -> function
         | E _, E _ -> ""
         | E _, K _ -> let s = string_of_int i in
                       "- __EK" ^ s ^ ":__ Chacha20(CK" ^ s ^ ", one)[32:63]\n"
         | _        -> error "encryption_key")
  |> String.concat ""

let payload_keys : Proto.protocol -> string = fun p ->
  auth_numbers p
  |> remove_last
  |> List.map (fun i -> let s = string_of_int i in
                        "- __PK"^ s ^":__ Chacha20(CK"^ s ^", two)[ 0:31]\n")
  |> String.concat ""

let encrypted_keys : Proto.protocol -> string = fun p ->
  let actions   = flat_post p                                    in
  let shifted   = List.tl actions                                in
  let zip       = map2 (fun a b -> (a, b)) actions shifted       in
  let keys      = zip // (function E _, _ -> true | _ -> false)
                  |> mapi 1 (fun i -> function
                         | E _, E _ -> ""
                         | E _, K k -> let s = string_of_int i in
                                       let x = string_of_key k in
                                       "    X" ^ x ^ "  = "
                                       ^ x ^ " XOR EK" ^ s ^ "\n"
                         | _        -> error "encryption_key")   in
  if keys = []
  then ""
  else String.concat "" keys ^ "\n"

let messages : Proto.protocol -> string = fun p ->
  let rec key ex = function
    | []             -> []
    | E e :: keys -> key (ex + 1) keys
    | K k :: keys -> let x = if ex > 0 then "X" else "" in
                        (x ^ string_of_key k) :: key ex keys                 in
  let rec msg ex = function
    | []            -> []
    | m :: messages -> let nex = ex + (m // is_exchange |> List.length) in
                       (String.concat " || " (key ex m)) :: msg nex messages in
  let rec auth ex tr = function
    | []            -> []
    | m :: messages ->
       let nex = ex + (m // is_exchange |> List.length) in
       let ntr = tr @ key ex m                          in
       (if nex = 0 || ntr = []
        then ""
        else (if (m // is_key |> (=) [])
              then "    Poly1305(AK"
              else " || Poly1305(AK")
             ^ string_of_int nex ^ ", "
             ^ String.concat " || " ntr ^ ")"
       ) :: auth nex ntr messages                                            in
  let m  = protocol p |> snd
           |> msg 0
           |> mapi 1 (fun i m -> "msg" ^ string_of_int i ^ " = " ^ m)
           |> pad_right                                                      in
  let a  = protocol p |> snd
           |> (auth 0 (flat_pre p // is_key /@ to_key /@ string_of_key))     in
  map2 (fun m a -> "    " ^ (if a = "" then String.trim m else m^a) ^ "\n") m a
  |> String.concat ""

let pre_shared : Proto.protocol -> string = fun p ->
  match (flat_pre p // is_key /@ to_key /@ string_of_key) with
  | []       -> ""
  | [k1]     -> "Note that " ^ k1 ^                " is shared in advance.\n"
  | [k1; k2] -> "Note that " ^ k1 ^ " and " ^ k2 ^ "are shared in advance.\n"
  | _        -> error "pre_shared"

let handshake : Proto.protocol -> string = fun p ->
  let send sender receiver msg_num =
    "- The "          ^ sender
    ^ " sends msg"    ^ string_of_int msg_num
    ^ " to the "      ^ receiver
    ^ ".\n"                                      in
  let payload sender receiver payload_num =
    "- The "        ^ sender
    ^ " may use PK" ^ string_of_int payload_num
    ^ " to send an encrypted payload.\n"         in
  let receive ex receiver msg_num =
    if ex
    then "- The "          ^ receiver
         ^ " verifies msg" ^ string_of_int msg_num
         ^ ", and aborts if it fails.\n"
    else "- The "          ^ receiver
         ^ " receives msg" ^ string_of_int msg_num
         ^ ".\n"                                 in
  let transmit sender receiver =
    "- The "          ^ receiver
    ^ " checks the "  ^ sender
    ^ "'s static key, and aborts if it fails.\n" in
  let rec messages sender receiver msg_num ex = function
    | []      -> ""
    | m :: ms -> let nex    = ex + (m // is_exchange |> List.length) in
                 let did_ex = nex > 0                                in
                 send sender receiver msg_num
                 ^ (if did_ex && ms <> []
                    then payload sender receiver nex
                    else "")
                 ^ receive did_ex receiver msg_num
                 ^ (if List.exists (fun e -> e = K IS || e = K RS) m
                    then transmit sender receiver
                    else "")
                 ^ messages receiver sender (msg_num + 1) nex ms in
  (protocol p
   |> snd
   |> messages "initiator" "respondent" 1 0)
  ^ "- The protocol is complete.  The session key is EK"
  ^ (all_exchanges p
     |> List.length
     |> string_of_int)
  ^ ".\n"

let print : out_channel -> Proto.protocol -> unit = fun channel p ->
  let ps s = output_string channel s in
  let pe s = ps s; ps "\n"           in
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
  ps (chaining_keys   p);
  ps (auth_keys       p);
  ps (encryption_keys p);
  ps (payload_keys    p);
  pe "";
  pe "_(\"[x:y]\" denotes a range; zero, one, and two are encoded in little";
  pe "endian format)_";
  pe "";
  pe "The messages contain the following (`||` denotes concatenation):";
  pe "";
  ps (encrypted_keys p);
  ps (messages p);
  pe "";
  ps (pre_shared p);
  pe "The handshake proceeds as follows:";
  pe "";
  ps (handshake p);
  pe "";
  pe "";
  ()
