open Utils
module P = Proto
module L = Proto_log

let keys : P.protocol -> string = fun p ->
  let actions  = P.all_keys p                           in
  let kk k txt = if List.mem k actions then txt else "" in
  ""
  ^ kk P.IS "    crypto_x25519_public_key(out->IS, in->is);\n"
  ^ kk P.IE "    crypto_x25519_public_key(out->IE, in->ie);\n"
  ^ kk P.RS "    crypto_x25519_public_key(out->RS, in->rs);\n"
  ^ kk P.RE "    crypto_x25519_public_key(out->RE, in->re);\n"

let secrets p =
  P.all_exchanges p
  /@ (function
      | (P.S, P.S) -> "    crypto_x25519(out->ss, in->is, out->RS)\n"
      | (P.S, P.E) -> "    crypto_x25519(out->se, in->is, out->RE)\n"
      | (P.E, P.S) -> "    crypto_x25519(out->es, in->ie, out->RS)\n"
      | (P.E, P.E) -> "    crypto_x25519(out->ee, in->ie, out->RE)\n")
  |> String.concat ""

let buffer_of_hash = function
  | L.Hash i -> "H" ^ string_of_int i
  | L.Htag i -> "T" ^ string_of_int i
  | L.Hkey i -> "K" ^ string_of_int i

let buffer_of_plain = function
  | L.Payload i -> "in->payloads[" ^ string_of_int i ^ "]"
  | L.Key k     -> P.string_of_key k
let size_of_plain = function
  | L.Payload i -> "in->payload_sizes[" ^ string_of_int i ^ "]"
  | L.Key k     -> "32"

let buffer_of_crypt = function
  | L.Plain p      -> buffer_of_plain p
  | L.Crypt (p, i) -> "chacha20(K" ^ string_of_int i
                      ^ ", " ^ buffer_of_plain p
                      ^ ", " ^ size_of_plain   p
                      ^ ")"
let size_of_crypt = function
  | L.Plain  p     -> size_of_plain p
  | L.Crypt (p, _) -> size_of_plain p

let buffer_of_mix_input = function
  | L.Hcrypt c   -> buffer_of_crypt c
  | L.Exchange e -> "in->" ^ P.string_of_exchange e
  | L.Prelude    -> "in->prelude"
  | L.Zero       -> "zero"
  | L.One        -> "one"
let size_of_mix_input = function
  | L.Hcrypt c   -> size_of_crypt c
  | L.Exchange e -> "32"
  | L.Prelude    -> "in->prelude_size"
  | L.Zero       -> "1"
  | L.One        -> "1"

let buffer_of_msg_part = function
  | L.Mcrypt c -> buffer_of_crypt c
  | L.Mtag   i -> "T" ^ string_of_int i
let size_of_msg_part = function
  | L.Mcrypt c -> size_of_crypt c
  | L.Mtag   i -> "16"

let has_prelude   = "in->has_prelude"
let has_payload i = "in->has_payload[" ^ string_of_int i ^ "]"

let statement_of_mix mix =
  let next_hash   = buffer_of_hash mix.L.next                         in
  let hash_pad    = String.make (3 - String.length next_hash) ' '     in
  let declaration = "    u8 " ^ next_hash ^ hash_pad ^ "[64];"        in
  let assignment  = "    blake2(" ^ next_hash ^ hash_pad
                    ^ ", H"       ^ string_of_int       mix.L.prev
                    ^ ", "        ^ buffer_of_mix_input mix.L.input
                    ^ ", "        ^ size_of_mix_input   mix.L.input
                    ^ ");\n"                                          in
  let pld_cond i  = "in->has_payload[" ^ string_of_int i ^ "]"        in
  let cond input  = (match input with
                     | L.Prelude                           -> "in->has_prelude"
                     | L.Hcrypt L.Plain  (L.Payload i)     -> pld_cond i
                     | L.Hcrypt L.Crypt ((L.Payload i), _) -> pld_cond i
                     | _ -> error "statement_of_mix")                 in
  let fallback f  = "\n    if("            ^ cond mix.L.input
                    ^ ") {\n    "          ^ assignment
                    ^ "    } else {\n"
                    ^ "        memcpy("    ^ next_hash
                    ^ ", H"                ^ string_of_int f
                    ^ ", 64);\n    }\n"                               in
  match mix.L.fallback with
  | None   -> declaration ^ assignment
  | Some f -> declaration ^ fallback f

let code_of_hashes hashes = hashes /@ statement_of_mix |> String.concat ""

(* TODO: use grid to align code*)
let statement_of_msg_part msg_num msg_part =
  "    memcpy(out->messages[" ^ string_of_int      msg_num
  ^ "] + offset, "            ^ buffer_of_msg_part msg_part
  ^ ");  offset += "          ^ size_of_msg_part   msg_part
  ^ ";\n"

let statements_of_msg msg_num (msg, hash_num) =
  let part_statements = msg /@ statement_of_msg_part msg_num
                        |> String.concat ""
  in ""
     ^ "    offset = 0;\n"               ^ part_statements
     ^ "    memcpy(out->message_hashes[" ^ string_of_int msg_num
     ^ "], H"                            ^ string_of_int hash_num
     ^ ");\n"

let code_of_messages msgs =
  msgs
  |> mapi 0 statements_of_msg
  |> String.concat "\n"


let code_of_log log =
  code_of_hashes log.L.hashes
  ^ code_of_messages log.L.messages

(* TODO: probably belongs to utils *)
let print_lines channel lines =
  List.iter (fun line -> output_string channel (line ^ "\n")) lines

let print_source channel pattern protocol =
  print_lines channel
    [ "// " ^ pattern
    ; protocol |> L.log_of_protocol |> code_of_log
    ; ""
    ]
