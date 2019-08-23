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

let string_of_hash = function
  | L.Hash i -> "H" ^ string_of_int i
  | L.Htag i -> "T" ^ string_of_int i
  | L.Hkey i -> "K" ^ string_of_int i

let string_of_raw_input = function
  | L.Prelude    -> "prelude"
  | L.Payload  i -> "p" ^ string_of_int i
  | L.Exchange e -> P.string_of_exchange e
  | L.Key      k -> P.string_of_key      k

let string_of_input = function
  | L.Iraw     raw  -> string_of_raw_input raw
  | L.Ienc (i, raw) -> "Chacha20(K" ^ string_of_int i
                       ^ ", " ^ string_of_raw_input raw ^ ")"
  | L.Itag  i       -> "T" ^ string_of_int i
  | L.Zero          -> "zero"
  | L.One           -> "one"

let grid_left_of_mix ((hash, _, _) : L.mix) =
  "- __" ^ string_of_hash hash ^ "__"

let grid_right_of_mix ((_, prev, input) : L.mix) =
  let finish     = ", H" ^ string_of_int prev ^ " otherwise"               in
  let prelude    = " if there is a prelude" ^ finish                       in
  let payload i  = " if msg" ^ string_of_int i ^ " has a payload" ^ finish in
  let opt_string = match input with
    | L.Iraw L.Prelude        -> prelude
    | L.Ienc (k, L.Prelude  ) -> prelude
    | L.Iraw (   L.Payload i) -> payload i
    | L.Ienc (k, L.Payload i) -> payload i
    | _                       -> ""
  in
  [ " = Blake2b(H" ^ string_of_int prev
  ; " || " ^ string_of_input input ^ ")"
    ^ opt_string
    ^ "\n"
  ]

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
  ^ String.concat " || " (message /@ string_of_input)
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
  (messages "initiator" "respondent" 1 0 msgs)
  ^ "- The protocol is complete.  The session keys are the two halves of "
  ^ get_last_hash (L.log_of_protocol p)
  ^ ".\n"

let print : out_channel -> string -> P.protocol -> unit =
  fun channel pattern p ->
  let ps s  = output_string channel s in
  let pe s  = ps s; ps "\n"           in
  pe pattern;
  pe (String.make (max 3 (String.length pattern)) '=');
  pe "";
  pe "Sender and recipient have the following X25519 key pairs (private half";
  pe "in lower case, public half in upper case):";
  pe "";
  ps (keys p);
  pe "";
  pe "Those key pairs are used to derive the following shared secrets:";
  pe "";
  ps (secrets p);
  pe "";
  pe "Those shared secrets are hashed to derive the following keys";
  pe "(`||`denotes concatenation, zero and one are one byte numbers):";
  pe "";
  ps (get_hashes pattern (L.log_of_protocol p));
  pe "";
  pe "The messages contain the following (the payloads \"p*\" are optional):";
  pe "";
  ps (get_messages (L.log_of_protocol p));
  pe "";
  ps (pre_shared p);
  ps (amplified_messages p);
  pe "The handshake proceeds as follows:";
  pe "";
  ps (handshake p);
  pe "";
  pe "";
  ()
