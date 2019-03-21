open Utils

let rec mapi i f = function
  | x::l -> f i x :: mapi (i+1) f l
  | _    -> []
let rec zip_with f l1 l2 = match l1, l2 with
  | x::l1, y::l2 -> f x y :: zip_with f l1 l2
  | _            -> []
let zip l1 l2 = zip_with pair l1 l2
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

module P = Proto

(* Convert protocol to specs *)
let string_of_key : P.cs_key -> string = function
  | P.IS -> "IS"
  | P.IE -> "IE"
  | P.RS -> "RS"
  | P.RE -> "RE"

let string_of_exchange : P.exchange -> string = function
  | (P.S, P.S) -> "ss"
  | (P.S, P.E) -> "se"
  | (P.E, P.S) -> "es"
  | (P.E, P.E) -> "ee"

let keys : P.protocol -> string = fun p ->
  let actions  = P.all_keys p                           in
  let kk k txt = if List.mem k actions then txt else "" in
  ""
  ^ kk P.IS "- __(is, IS)__ The initiator's static key.\n"
  ^ kk P.IE "- __(ie, IE)__ The initiator's ephemeral key.\n"
  ^ kk P.RS "- __(rs, RS)__ The respondent's static key.\n"
  ^ kk P.RE "- __(re, RE)__ The respondent's ephemeral key.\n"

let secrets p =
  P.all_exchanges p
  /@ (function
      | (P.S, P.S) -> "- __ss__ = X25519(is, RS) = X25519(rs, IS)\n"
      | (P.S, P.E) -> "- __se__ = X25519(is, RE) = X25519(re, IS)\n"
      | (P.E, P.S) -> "- __es__ = X25519(ie, RS) = X25519(rs, IE)\n"
      | (P.E, P.E) -> "- __ee__ = X25519(ie, RE) = X25519(re, IE)\n")
  |> String.concat ""

let chaining_keys : P.protocol -> string = fun p ->
  let exchanges = P.all_exchanges p                                       in
  let e         = exchanges /@ string_of_exchange                         in
  let ck        = exchanges |> mapi 1 (fun i _ -> "CK" ^ string_of_int i) in
  let ck0       = "       " :: (ck  /@  ((^) "XOR " ))                    in
  map3 (fun ck ck0 e -> "- __"                       ^ ck
                        ^ ":__ HChacha20(HChacha20(" ^ e
                        ^ ", zero) "                 ^ ck0
                        ^ ", pid)\n")
    ck ck0 e
  |> String.concat ""

let auth_numbers : P.protocol -> int list = fun p ->
  snd p
  |> List.map    P.to_actions
  |> List.map    (fun m -> m // P.is_exchange |> List.length)
  |> List.filter ((<>) 0)
  |> sums 0

let auth_keys : P.protocol -> string = fun p ->
  auth_numbers p
  |> List.map (fun i -> let s = string_of_int i in
                        "- __AK"^ s ^":__ Chacha20(CK"^ s ^", one)[ 0:31]\n")
  |> String.concat ""

let encryption_keys : P.protocol -> string = fun p ->
  let actions   = (snd p) /@ P.to_actions |> List.concat in
  let shifted   = List.tl actions @ [P.Key P.S]          in
  zip actions shifted
  // (fst |- P.is_exchange)
  |> mapi 1 (fun i -> function
         | _, P.Exchange _ -> ""
         | _, P.Key      _ ->
            let s = string_of_int i in
            "- __EK" ^ s ^ ":__ Chacha20(CK" ^ s ^ ", one)[32:63]\n")
  |> String.concat ""

let payload_keys : P.protocol -> string = fun p ->
  auth_numbers p
  |> remove_last
  |> List.map (fun i -> let s = string_of_int i in
                        "- __PK"^ s ^":__ Chacha20(CK"^ s ^", two)[ 0:31]\n")
  |> String.concat ""

let encrypted_keys : P.protocol -> string = fun p ->
  let actions   = snd (P.cs_protocol p) |> List.concat in
  let shifted   = List.tl actions                      in
  let keys      = zip actions shifted
                  // (fst |- P.is_cs_exchange)
                  |> mapi 1 (fun i -> function
                         | _, P.CS_exchange _ -> ""
                         | _, P.CS_key      k -> let s = string_of_int i in
                                                 let x = string_of_key k in
                                                 "    X" ^ x ^ "  = "
                                                 ^ x ^ " XOR EK" ^ s ^ "\n") in
  if keys = []
  then ""
  else String.concat "" keys ^ "\n"

let messages : P.protocol -> string = fun p ->
  let rec key ex = function
    | []             -> []
    | P.CS_exchange e :: keys -> key (ex + 1) keys
    | P.CS_key      k :: keys -> let x = if ex > 0 then "X" else "" in
                                 (x ^ string_of_key k) :: key ex keys         in
  let rec msg ex = function
    | []            -> []
    | m :: messages -> let nex = ex + (m // P.is_cs_exchange |> List.length) in
                       (String.concat " || " (key ex m)) :: msg nex messages  in
  let rec auth ex tr = function
    | []            -> []
    | m :: messages ->
       let nex = ex + (m // P.is_cs_exchange |> List.length) in
       let ntr = tr @ key ex m                               in
       (if nex = 0 || ntr = []
        then ""
        else (if (m // P.is_cs_key |> (=) [])
              then "    Poly1305(AK"
              else " || Poly1305(AK")
             ^ string_of_int nex ^ ", "
             ^ String.concat " || " ntr ^ ")"
       ) :: auth nex ntr messages                                             in
  let m  = P.cs_protocol p |> snd
           |> msg 0
           |> mapi 1 (fun i m -> "msg" ^ string_of_int i ^ " = " ^ m)
           |> pad_right                                                       in
  let a  = P.cs_protocol p |> snd
           |> (auth 0 ((P.cs_protocol p |> fst |> List.concat)
                       // P.is_cs_key /@ P.to_cs_key /@ string_of_key))       in
  zip_with
    (fun m a -> "    "
                ^ (if a = "" then String.trim m else m ^ a)
                ^ "\n")
    m a
  |> String.concat ""

let pre_shared : P.protocol -> string = fun p ->
  match ((P.cs_protocol p |> fst |> List.concat)
         // P.is_cs_key
         /@ P.to_cs_key
         /@ string_of_key) with
  | []       -> ""
  | [k1]     -> "Note that " ^ k1 ^                 " is shared in advance.\n"
  | [k1; k2] -> "Note that " ^ k1 ^ " and " ^ k2 ^ " are shared in advance.\n"
  | _        -> error "pre_shared"

let handshake : P.protocol -> string = fun p ->
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
    | m :: ms -> let nex    = ex + (m // P.is_cs_exchange |> List.length) in
                 let did_ex = nex > 0                                     in
                 send sender receiver msg_num
                 ^ (if did_ex && ms <> []
                    then payload sender receiver nex
                    else "")
                 ^ receive did_ex receiver msg_num
                 ^ (if List.exists (swap List.mem [P.CS_key P.IS;
                                                   P.CS_key P.RS]) m
                    then transmit sender receiver
                    else "")
                 ^ messages receiver sender (msg_num + 1) nex ms in
  (P.cs_protocol p
   |> snd
   |> messages "initiator" "respondent" 1 0)
  ^ "- The protocol is complete.  The session key is EK"
  ^ (P.all_exchanges p
     |> List.length
     |> string_of_int)
  ^ ".\n"

let title t = t ^ "\n" ^ String.make (String.length t) '=' ^ "\n"

let print : out_channel -> string -> P.protocol -> unit =
  fun channel pattern p ->
  output_string  channel (title pattern);
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
  pe ("- __pid:__ \"Monokex " ^ pattern ^ "\"  (ASCII, 16 bytes, zero padded)");
  ps (chaining_keys   p);
  ps (auth_keys       p);
  ps (encryption_keys p);
  ps (payload_keys    p);
  pe "";
  pe "_(\"[x:y]\" denotes a range; one and two are encoded in little endian.)_";
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
