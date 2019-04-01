open Utils
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

let all_keys : P.protocol -> string = fun p ->
  let exchanges = Proto.all_exchanges p /@ string_of_exchange in
  let currents  = range 1 (List.length exchanges)             in
  map2 (fun e c -> let current  = string_of_int c       in
                   let previous = string_of_int (c - 1) in
                   "- __CK"       ^ current
                   ^ ", AK"       ^ current
                   ^ ", EK"       ^ current
                   ^ ", PK"       ^ current
                   ^ ":__ XCKDF(" ^ e
                   ^ ", "         ^ (if c = 1
                                     then "zero"
                                     else "CK" ^ previous ^ " ")
                   ^ ", pid)\n"
    ) exchanges currents
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
  ^ "- The protocol is complete.  The session key is PK"
  ^ (P.all_exchanges p
     |> List.length
     |> string_of_int)
  ^ ".\n"

let print_xckdf : out_channel -> unit =
  fun channel ->
  let ps s = output_string channel s in
  let pe s = ps s; ps "\n"           in
  pe "XCKDF";
  pe "=====";
  pe "";
  pe "This is a Chacha20 based key derivation for X25519 shared secrets.";
  pe "";
  pe "The inputs are:";
  pe "";
  pe "- __DH:__   X25519 key exchange        (32 bytes)";
  pe "- __IK:__   Input key material         (32 bytes)";
  pe "- __salt:__ Protocol specific constant (16 bytes)";
  pe "";
  pe "The output of __XCKDF(DH, IK, salt)__ is a 128 byte buffer, defined as";
  pe "follows:";
  pe "";
  pe "- __H:__   HChacha20(DH, zero)";
  pe "- __X:__   H XOR prev";
  pe "- __I:__   HChacha20(X, salt)";
  pe "- __out:__ Chacha20(I, one)[0:127]";
  pe "";
  pe "_(\"[x:y]\" denotes a range; one is encoded in little endian.)_";
  pe "";
  pe "That output is divided among four 32-byte keys:";
  pe "";
  pe "- __K1:__ out[0 : 31]";
  pe "- __K2:__ out[32: 63]";
  pe "- __K3:__ out[64: 95]";
  pe "- __K4:__ out[96:127]";
  pe "";
  pe "We note: __K1, K2, K3, K4 = XCKDF(DH, IK, salt)__";
  pe "";
  pe "";
  ()

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
  pe "Those shared secrets are hashed to derive the following keys:";
  pe "";
  pe ("- __pid:__ \"Monokex " ^ pattern ^ "\"  (ASCII, 16 bytes, zero padded)");
  ps (all_keys          p);
  pe "";
  pe "_(The constant \"one\" is encoded in little endian.)_";
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
