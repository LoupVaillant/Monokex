open Utils
module P = Proto

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

type state = { hash_nb  : int  (* last    hash    number *)
             ; tag_nb   : int  (* last    tag     number *)
             ; key_nb   : int  (* last    key     number *)
             ; msg_nb   : int  (* current message number *)
             ; hashes   : string
             ; messages : string list list
             ; curr_msg : string list
             ; has_key  : bool
             }

let prev_hash st = "H" ^ string_of_int (st.hash_nb - 1)
let curr_hash st = "H" ^ string_of_int st.hash_nb
let curr_tag  st = "T" ^ string_of_int st.tag_nb
let curr_key  st = "K" ^ string_of_int st.key_nb
let curr_msg  st =       string_of_int st.msg_nb

let inc_hash  st = { st with hash_nb = st.hash_nb + 1 }
let inc_tag   st = { st with tag_nb  = st.tag_nb  + 1 }
let inc_key   st = { st with key_nb  = st.key_nb  + 1 }
let inc_msg   st = { st with msg_nb  = st.msg_nb  + 1 }

let hash_expr prev input = "Blake2b(" ^ prev ^ " || " ^ input ^ ")"

let message_bit input st = { st with curr_msg = st.curr_msg @ [input] }

let mix_hash prev curr input st =
  { st with hashes = st.hashes^"- __"^curr^"__ = "^hash_expr prev input^"\n" }

let add_hash input =
  inc_hash |- fun st -> mix_hash (prev_hash st) (curr_hash st) input st

let if_keyed f st = if not st.has_key then st else f st

let add_tag =
  if_keyed (add_hash "zero"
            |- inc_tag
            |- (fun st -> mix_hash (prev_hash st) (curr_tag st) "one" st))
let add_key =
  if_keyed (add_hash "zero"
            |- inc_key
            |- (fun st -> mix_hash (prev_hash st) (curr_key st) "one" st))

let maybe_hash condition input =
  inc_hash
  |- (fun st -> let line =  "- __"    ^ curr_hash st
                            ^ "__ = " ^ hash_expr (prev_hash st) input
                            ^ " if "  ^ condition
                            ^ ", "    ^ prev_hash st ^ " otherwise\n"
                in { st with hashes = st.hashes ^ line })

let next_key st k =
  let k_str = P.string_of_key k in
  if P.is_ephemeral k || not st.has_key
  then st |> message_bit k_str |> add_hash k_str
  else let st1 = add_key st                                      in
       let enc_key = "Chacha20("^ curr_key st1 ^", "^ k_str ^")" in
       st1
       |> add_hash enc_key |>            message_bit enc_key
       |> add_tag          |> (fun st -> message_bit (curr_tag st) st)

let next_exchange st e =
  add_hash (P.string_of_exchange e) { st with has_key = true }

let add_message st msg =
  List.fold_left
    (fun st action -> P.map_cs_action (next_key st) (next_exchange st) action)
    (inc_msg st)
    msg
  |> maybe_hash ("msg"^ curr_msg st ^" has a payload") ("p" ^ curr_msg st)
  |> add_tag
  |> (fun st -> { st with curr_msg = st.curr_msg @ ["p" ^ curr_msg st] })
  |> (fun st -> if_keyed (message_bit (curr_tag st)) st)
  |> (fun st -> { st with messages = st.messages @ [st.curr_msg]
                        ; curr_msg = [] })

let add_pre_message st msg =
  List.fold_left
    (fun st action -> P.map_cs_action
                        (next_key st) (* static, not encrypted *)
                        (f_error "add_pre_message")
                        action)
    st msg

let state_of_protocol p =
  let pre_messages = p |> P.cs_protocol |> fst in
  let messages     = p |> P.cs_protocol |> snd in
  { hash_nb  = 0
  ; tag_nb   = 0
  ; key_nb   = 0
  ; msg_nb   = 0
  ; hashes   = ""
  ; messages = []
  ; curr_msg = []
  ; has_key  = false
  }
  |> swap (List.fold_left add_pre_message) pre_messages
  |> maybe_hash "there is a prelude" "prelude"
  |> swap (List.fold_left add_message) messages

let all_keys : P.protocol -> string = fun p -> (state_of_protocol p).hashes

let messages : P.protocol -> string = fun p ->
  mapi 1
    (fun n msg ->
      let msg_nb = string_of_int n in
      "- __msg" ^ msg_nb ^ "__ = " ^ String.concat " || " msg ^ "\n")
    (state_of_protocol p).messages
  |> String.concat ""

let pre_shared : P.protocol -> string = fun p ->
  match ((P.cs_protocol p |> fst |> List.concat)
         // P.is_cs_key
         /@ P.to_cs_key
         /@ P.string_of_key) with
  | []       -> ""
  | [k1]     -> "Note that " ^ k1 ^                 " is shared in advance.\n"
  | [k1; k2] -> "Note that " ^ k1 ^ " and " ^ k2 ^ " are shared in advance.\n"
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
  ^ curr_hash (state_of_protocol p)
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
  pe ("- __H0__ = \"Monokex " ^ pattern ^ "\"  (ASCII, 64 bytes, zero padded)");
  ps (all_keys p);
  pe "";
  pe "The messages contain the following (the payloads \"p*\" are optional):";
  pe "";
  ps (messages p);
  pe "";
  ps (pre_shared p);
  pe "";
  ps (amplified_messages p);
  pe "The handshake proceeds as follows:";
  pe "";
  ps (handshake p);
  pe "";
  pe "";
  ()
