open Utils
module P = Proto
module L = Proto_log

(* Convert protocol to specs *)
let keys : P.protocol -> string = fun p ->
  let actions  = P.all_keys p                           in
  let kk k txt = if List.mem k actions then txt else "" in
  ""
  ^ kk P.IS "- __(is, IS)__ The initiator's static key.\n"
  ^ kk P.IE "- __(ie, IE)__ The initiator's ephemeral key.\n"
  ^ kk P.RS "- __(rs, RS)__ The responder's static key.\n"
  ^ kk P.RE "- __(re, RE)__ The responder's ephemeral key.\n"

let secrets p =
  P.all_exchanges p
  /@ (function
      | (P.S, P.S) -> "- __ss__ = X25519(is, RS) = X25519(rs, IS)\n"
      | (P.S, P.E) -> "- __se__ = X25519(is, RE) = X25519(re, IS)\n"
      | (P.E, P.S) -> "- __es__ = X25519(ie, RS) = X25519(rs, IE)\n"
      | (P.E, P.E) -> "- __ee__ = X25519(ie, RE) = X25519(re, IE)\n")
  |> String.concat ""

let string_of_hash = function
  | L.Hash i -> "H" ^ string_of_int i
  | L.Htag i -> "T" ^ string_of_int i
  | L.Hkey i -> "K" ^ string_of_int i

let string_of_plain = function
  | L.Payload i -> "p" ^ string_of_int i
  | L.Key k     -> P.string_of_key k

let string_of_crypt = function
  | L.Plain p      -> string_of_plain p
  | L.Crypt (p, i) -> "Chacha20(K" ^ string_of_int i
                      ^ ", " ^ string_of_plain p ^ ")"

let string_of_mix_input = function
  | L.Hcrypt c   -> string_of_crypt c
  | L.Exchange e -> P.string_of_exchange e
  | L.Prelude    -> "prelude"
  | L.Zero       -> "zero"
  | L.One        -> "one"

let string_of_msg_part = function
  | L.Mcrypt c -> string_of_crypt c
  | L.Mtag   i -> "T" ^ string_of_int i

let grid_left_of_mix mix =
  "- __" ^ string_of_hash mix.L.next ^ "__"

let grid_right_of_mix mix =
  let prefix = " = Blake2b(H" ^ string_of_int mix.L.prev      in
  let middle = " || " ^ string_of_mix_input mix.L.input ^ ")" in
  let suffix =
    (match mix.L.fallback with
     | None   -> ""
     | Some f ->
        let finish    = ", H" ^ string_of_int f ^ " otherwise"         in
        let prelude   = " if there is a prelude"                       in
        let payload i = " if msg" ^ string_of_int i ^ " has a payload" in
        (match mix.L.input with
         | L.Prelude                           -> prelude   ^ finish
         | L.Hcrypt L.Plain  (L.Payload i)     -> payload i ^ finish
         | L.Hcrypt L.Crypt ((L.Payload i), _) -> payload i ^ finish
         | _                                   -> error "grid_right_of_mix"))
  in [prefix; middle ^ suffix ^ "\n"]

let string_of_hashes pattern hashes =
  let left  = "- __H0__" :: (hashes /@ grid_left_of_mix) in
  let right = (" = \"Monokex " ^ pattern ^ "\""
               ^ " (ASCII, 64 bytes, zero padded)\n")
              :: grid (hashes /@ grid_right_of_mix)      in
  map2 (fun a b -> [a; b]) left right
  |> grid
  |> String.concat ""

let string_of_message msg_nb message =
  "- __msg" ^ string_of_int msg_nb ^ "__ = "
  ^ String.concat " || " (message /@ string_of_msg_part)
  ^ "\n"

let string_of_messages messages = mapi 1 string_of_message messages
                                  |> String.concat ""

let get_last_hash      st = "H" ^ string_of_int      st.L.last_hash
let get_hashes pattern st = string_of_hashes pattern st.L.hashes
let get_messages       st = string_of_messages      (st.L.messages /@ fst)

let pre_shared : P.protocol -> string = fun p ->
  match ((P.cs_protocol p |> fst |> List.concat)
         // P.is_cs_key
         /@ P.to_cs_key
         /@ P.string_of_key) with
  | []       -> ""
  | [k1]     -> "Note that " ^ k1 ^                 " is shared in advance.\n\n"
  | [k1; k2] -> "Note that " ^ k1 ^ " and " ^ k2 ^ " are shared in advance.\n\n"
  | _        -> error "pre_shared"

let amplified_messages p =
  let requests = range 1 (P.nb_messages p) // P.is_amplified p              in
  let to_str m = "msg" ^ string_of_int m                                    in
  let warnings = requests /@
                   (fun msg -> "the network packet containing " ^ to_str msg
                               ^ " should be as big as the network packet "
                               ^ "containing " ^ to_str (msg+1)
                               ^ (if Proto.first_server_payload p > (msg + 1)
                                  then ""
                                  else " (and its payload, if any)"))       in
  let messages = String.concat " and " (requests /@ to_str)                 in
  match warnings with
  | [] -> "" (* No amplified message at all *)
  | l  -> "To avoid network amplification attacks: "
          ^ String.concat "; " l
          ^ ". Pad " ^ messages ^ " with zeroes as necessary\n\n"
          |> paragraph

let handshake : P.protocol -> string = fun p ->
  let send sender receiver msg_num =
    "- The "          ^ sender
    ^ " sends msg"    ^ string_of_int msg_num
    ^ " to the "      ^ receiver
    ^ ".\n"                                      in
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
                 ^ receive did_ex receiver msg_num
                 ^ (if List.exists (swap List.mem [P.CS_key P.IS;
                                                   P.CS_key P.RS]) m
                    then transmit sender receiver
                    else "")
                 ^ messages
                     receiver sender
                     (msg_num + 1) nex ms        in
  let msgs = snd (P.cs_protocol p)               in
  (messages "initiator" "responder" 1 0 msgs)
  ^ "- The protocol is complete.  The session keys are the two halves of "
  ^ get_last_hash (L.log_of_protocol p)
  ^ ".\n"

let spec1 : string -> P.protocol -> string =
  fun pattern p ->
  String.concat "\n"
    [ pattern
    ; (String.make (max 3 (String.length pattern)) '=')
    ; ""
    ; "Sender and recipient have the following X25519 key pairs (private half"
    ; "in lower case, public half in upper case):"
    ; ""
    ; (keys p)
    ; "Those key pairs are used to derive the following shared secrets:"
    ; ""
    ; (secrets p)
    ; "Those shared secrets are hashed to derive the following keys"
    ; "(`||`denotes concatenation, zero and one are one byte numbers):"
    ; ""
    ; (get_hashes pattern (L.log_of_protocol p))
    ; "The messages contain the following (the payloads \"p*\" are optional):"
    ; ""
    ; (get_messages (L.log_of_protocol p))
    ; (pre_shared p)
      ^ (amplified_messages p)
      ^ "The handshake proceeds as follows:"
    ; ""
    ; (handshake p)
    ]

let spec protocols = String.concat "\n\n" (map_pair spec1 protocols)
