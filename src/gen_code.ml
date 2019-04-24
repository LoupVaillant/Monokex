open Utils
module P = Proto

type cs = Client | Server
type lr = Local  | Remote

let cs_of_int i = if i mod 2 == 1 then Client else Server

let map_cs a b cs = match cs with Client -> a | Server -> b
let map_lr a b lr = match lr with Local  -> a | Remote -> b

let local    cs = map_cs "client" "server" cs
let remote   cs = map_cs "server" "client" cs
let ctx_type cs = prefix ^ local cs ^ "_ctx "

(* init_header and init_source helpers *)
let is_authenticated protocol =
  map_cs
    (P.client_keys protocol <> [])
    (P.server_keys protocol <> [])

let uses_ephemeral protocol =
  map_cs
    (P.client_keys protocol // ((=) P.E) <> [])
    (P.server_keys protocol // ((=) P.E) <> [])

let lr_of_protocol (cs : cs) (protocol : P.protocol) =
  fst protocol
  /@ (fun pre_shared -> match pre_shared, cs with
                        | P.Client _, Client -> Local
                        | P.Server _, Server -> Local
                        | P.Client _, Server -> Remote
                        | P.Server _, Client -> Remote)

let init_proto pattern cs protocol =
  let lr       = lr_of_protocol cs protocol                          in
  let css      = map_cs "client" "server" cs                         in
  let ctx      = [ ctx_type cs    ; "*"; local cs ^ "_ctx" ]         in
  let seed     = ["uint8_t "      ; "" ; "random_seed"    ; "[32]" ] in
  let sk       = ["const uint8_t "; "" ; local  cs ^ "_sk"; "[32]" ] in
  let pk       = ["const uint8_t "; "" ; local  cs ^ "_pk"; "[32]" ] in
  let r        = ["const uint8_t "; "" ; remote cs ^ "_pk"; "[32]" ] in
  prototype
    "void"  (prefix ^ pattern ^ "_init_" ^  css)
    [ ctx
    ; if uses_ephemeral   protocol cs then seed else []
    ; if is_authenticated protocol cs then sk   else []
    ; if is_authenticated protocol cs then pk   else []
    ; if List.mem Remote lr           then r    else []
    ]

let init_body pattern cs protocol =
  let lr     = lr_of_protocol   cs protocol                                in
  let init   = "    kex_init   (ctx, pid_" ^ pattern ^ ");\n"              in
  let seed   = "    kex_seed   (ctx, random_seed);\n"                      in
  let sk_pk  = "    kex_locals (ctx, " ^ local cs ^ "_sk, "
               ^                         local cs ^ "_pk);\n"              in
  let r      = "    kex_receive(ctx, ctx->remote_pk, "^remote cs^"_pk);\n" in
  let l      = "    kex_receive(ctx, ctx->local_pk, ctx->local_pk);\n"     in
  "\n{\n"
  ^ "    " ^ prefix ^ "ctx *ctx = &(" ^ local cs ^ "_ctx->ctx);\n"
  ^ init
  ^ (if uses_ephemeral   protocol cs then seed  else "")
  ^ (if is_authenticated protocol cs then sk_pk else "")
  ^ (lr /@ (function Local -> l | Remote -> r) |> String.concat "")
  ^ "}\n"

(* Generate source code for the init functions *)
let init_header pattern cs protocol =
  init_proto pattern cs protocol ^ ";\n"

let init_source pattern cs protocol =
  let lower_pattern = String.lowercase_ascii pattern in
  init_proto lower_pattern cs protocol ^ init_body lower_pattern cs protocol

(* message_proto & message_body helpers *)
(* nb reffers to the function number, and starts at 1 *)
let receives p nb = nb > 1
let sends    p nb = nb <= List.length (snd p)

let sends_payload    p nb =
  map_cs
    (nb >= P.first_client_payload p)
    (nb >= P.first_server_payload p)
let receives_payload p nb =
  map_cs
    (nb > P.first_server_payload p)
    (nb > P.first_client_payload p)

let auths    p nb = P.first_auth p <= nb
let verifies p nb = P.first_auth p <  nb

let gets_remote p nb = receives p nb &&
                         List.mem P.S (P.nth_message p (nb - 1)
                                       |> P.to_actions
                                       |> P.get_keys)

let message_proto pattern nb protocol =
  let cs          = cs_of_int nb                                         in
  let session_key = nb >= List.length (snd protocol)                     in
  let current     = string_of_int nb                                     in
  let previous    = string_of_int (nb - 1)                               in
  let ctx         = [ctx_type cs; "*"; local cs ^ "_ctx"        ]        in
  let sk          = ["uint8_t " ; "" ; "session_key"    ; "[32]"]        in
  let rk          = ["uint8_t " ; "" ; remote cs ^ "_pk"; "[32]"]        in
  prototype
    (if verifies protocol nb then "int" else "void")
    (prefix ^ pattern ^ "_" ^ current)
    [ ctx
    ; if session_key             then sk else []
    ; if gets_remote protocol nb then rk else []
    ; if sends protocol nb
      then let size = string_of_int (P.nth_message_size protocol nb) in
           ["uint8_t "      ; ""; "msg" ^ current ; "[" ^ size ^ "]"]
      else []
    ; if receives protocol nb
      then let size = string_of_int (P.nth_message_size protocol (nb-1)) in
           ["const uint8_t "; ""; "msg" ^ previous; "[" ^ size ^ "]"]
      else []
    ]

let str_msg nb  = "msg" ^ string_of_int nb
let key_comment key nb =
  (map_cs "-> I" "<- R" (cs_of_int nb)) ^ P.map_key "E" "S" key

let message_offset nb_keys =
  let message_offset = string_of_int (nb_keys * 32) in
  if nb_keys = 0
  then "     "
  else " + " ^ message_offset

let receive_key message_number nb_keys key =
  let ctx_key = P.map_key "ctx->remote_pke" "ctx->remote_pk " key in
  "    kex_receive   (ctx, " ^ ctx_key
  ^ ", "                     ^ str_msg message_number
  ^                            message_offset nb_keys
  ^ "      );  // "          ^ key_comment key message_number
  ^ "\n"

let send_key message_number nb_keys key =
  let ctx_key = P.map_key "ctx->local_pke" "ctx->local_pk " key in
  "    kex_send      (ctx, " ^ str_msg message_number
  ^                            message_offset nb_keys
  ^ "      , "               ^ ctx_key
  ^ " );  // "               ^ key_comment key message_number
  ^ "\n"

let exchange cs exchange =
  let update s = "    kex_update_key" ^ s ^ "\n"                        in
  let ee  = update "(ctx, ctx->local_ske , ctx->remote_pke);  //    ee" in
  let ss  = update "(ctx, ctx->local_sk  , ctx->remote_pk );  //    ss" in
  let ces = update "(ctx, ctx->local_ske , ctx->remote_pk );  //    es" in
  let ses = update "(ctx, ctx->local_sk  , ctx->remote_pke);  //    es" in
  let cse = update "(ctx, ctx->local_sk  , ctx->remote_pke);  //    se" in
  let sse = update "(ctx, ctx->local_ske , ctx->remote_pk );  //    se" in
  match exchange with
  | P.E, P.E -> ee
  | P.S, P.S -> ss
  | P.E, P.S -> map_cs ces ses cs
  | P.S, P.E -> map_cs cse sse cs

let auth message_number nb_keys =
  "    kex_auth      (ctx, " ^ str_msg message_number
  ^ (if nb_keys = 0
     then ");                              // auth\n"
     else message_offset nb_keys
          ^ ");                         // auth\n"
    )

let verify message_number nb_keys =
  "    if (kex_verify(ctx, " ^ str_msg message_number
  ^ (if nb_keys = 0
     then ")) { return -1; }               // verify\n"
     else message_offset nb_keys
          ^ ")) { return -1; }          // verify\n"
    )

let key_counts =
  let rec counts p start = function
    | []      -> []
    | x :: xs -> let new_start = (if p x then 1 else 0) + start in
                 new_start :: counts p new_start xs
  in
  counts P.is_key (-1)

let process_message do_key do_av protocol nb =
  let cs      = cs_of_int nb                             in
  let message = P.to_actions (P.nth_message protocol nb) in
  let keys    = zip message (key_counts message)
                /@ (fun (action, key_count)
                    -> P.map_action
                         (do_key nb key_count)
                         (exchange cs)
                         action)
                |> String.concat ""                      in
  let av      = if P.first_auth protocol <= nb
                then do_av nb (List.length (P.get_keys message))
                else ""                                  in
  keys ^ av

let receive_message = process_message receive_key verify
let send_message    = process_message send_key    auth

let message_body nb protocol =
  let messages    = snd protocol               in
  let cs          = cs_of_int nb               in
  let session_key = nb >= List.length messages in
  "\n{\n"
  ^ "    " ^ prefix ^ "ctx *ctx = &(" ^ local cs ^ "_ctx->ctx);\n"
  ^ (if receives protocol nb then receive_message protocol (nb-1) else "")
  ^ (if sends    protocol nb then send_message    protocol  nb    else "")
  ^ (if gets_remote protocol nb
     then "    copy32(" ^ remote cs ^ "_pk  , ctx->remote_pk);\n"
     else "")
  ^ (if session_key
     then "    copy32(session_key, ctx->keys + 96);\n"
          ^ "    WIPE_CTX(ctx);\n"
     else "";)
  ^ (if verifies protocol nb
     then "    return 0;\n"
     else "")
  ^ "}\n"

(* Generate source code for the message functions *)
let message_header pattern nb protocol =
  message_proto pattern nb protocol ^ ";\n"

let message_source pattern nb protocol =
  message_proto pattern nb protocol
  ^ message_body nb protocol

let print_lines channel lines =
  List.iter (fun line -> output_string channel (line ^ "\n")) lines

(* Common source code *)
let print_header_prefix channel =
  print_lines channel
    [ "#include <inttypes.h>"
    ; "#include <stddef.h>"
    ; ""
    ; "typedef struct {"
    ; "    uint8_t transcript[128];"
    ; "    uint8_t keys      [128];"
    ; "    uint8_t local_sk   [32];"
    ; "    uint8_t local_pk   [32];"
    ; "    uint8_t local_ske  [32];"
    ; "    uint8_t local_pke  [32];"
    ; "    uint8_t remote_pk  [32];"
    ; "    uint8_t remote_pke [32];"
    ; "    uint8_t pid        [16];"
    ; "    size_t  transcript_size;"
    ; "} " ^ prefix ^ "ctx;"
    ; ""
    ; "typedef struct { " ^ prefix ^ "ctx ctx; } " ^ prefix ^ "client_ctx;"
    ; "typedef struct { " ^ prefix ^ "ctx ctx; } " ^ prefix ^ "server_ctx;"
    ; ""
    ]

let print_source_prefix channel =
  print_lines channel
    [ "#include <monocypher.h>"
    ; "#include \"monokex.h\""
    ; ""
    ; "#define WIPE_CTX(ctx)        crypto_wipe(ctx   , sizeof(*(ctx)))"
    ; "#define WIPE_BUFFER(buffer)  crypto_wipe(buffer, sizeof(buffer))"
    ; ""
    ; "static const uint8_t zero[32] = {0};"
    ; "static const uint8_t one [16] = {1};"
    ; ""
    ; "static void copy16(uint8_t out[16], const uint8_t in[16])"
    ; "{"
    ; "    for (size_t i = 0; i < 16; i++) { out[i]  = in[i]; }"
    ; "}"
    ; "static void copy32(uint8_t out[32], const uint8_t in[32])"
    ; "{"
    ; "    for (size_t i = 0; i < 32; i++) { out[i]  = in[i]; }"
    ; "}"
    ; "static void xor32 (uint8_t out[32], const uint8_t in[32])"
    ; "{"
    ; "    for (size_t i = 0; i < 32; i++) { out[i] ^= in[i]; }"
    ; "}"
    ; ""
    ; prototype
        "static void" "kex_update_key"
        [ [prefix ^ "ctx " ; "*"; "ctx"           ]
        ; ["const uint8_t "; "" ; "secret_key[32]"]
        ; ["const uint8_t "; "" ; "public_key[32]"]
        ]
    ; "{"
    ; "    // Extract"
    ; "    uint8_t tmp[32];"
    ; "    crypto_x25519(tmp, secret_key, public_key);"
    ; "    crypto_chacha20_H(tmp, tmp, zero);"
    ; "    xor32(tmp, ctx->keys);"
    ; "    crypto_chacha20_H(tmp, tmp, ctx->pid);"
    ; ""
    ; "    // Expand"
    ; "    crypto_chacha_ctx chacha_ctx;"
    ; "    crypto_chacha20_init  (&chacha_ctx, tmp, one);"
    ; "    crypto_chacha20_stream(&chacha_ctx, ctx->keys, 128);"
    ; ""
    ; "    // Clean up"
    ; "    WIPE_BUFFER(tmp);"
    ; "    WIPE_CTX(&chacha_ctx);"
    ; "}"
    ; ""
    ; "static void kex_auth(" ^ prefix ^ "ctx *ctx, uint8_t mac[16])"
    ; "{"
    ; "    crypto_poly1305(mac, ctx->transcript, ctx->transcript_size,"
    ; "                    ctx->keys + 32);"
    ; "}"
    ; ""
    ; "static int kex_verify(" ^ prefix ^ "ctx *ctx, const uint8_t mac[16])"
    ; "{"
    ; "    uint8_t real_mac[16];"
    ; "    kex_auth(ctx, real_mac);"
    ; "    int mismatch = crypto_verify16(real_mac, mac);"
    ; "    if (mismatch) {  WIPE_CTX(ctx); }"
    ; "    WIPE_BUFFER(real_mac);"
    ; "    return mismatch;"
    ; "}"
    ; ""
    ; "static void kex_send(" ^ prefix ^ "ctx *ctx,"
    ; "                     uint8_t msg[32], const uint8_t src[32])"
    ; "{"
    ; "    // Send message, encrypted if we have a key"
    ; "    copy32(msg, src);"
    ; "    xor32(msg, ctx->keys + 64);"
    ; "    // Record sent message"
    ; "    copy32(ctx->transcript + ctx->transcript_size, msg);"
    ; "    ctx->transcript_size += 32;"
    ; "}"
    ; ""
    ; "static void kex_receive(" ^ prefix ^ "ctx *ctx,"
    ; "                        uint8_t dest[32], const uint8_t msg[32])"
    ; "{"
    ; "    // Record incoming message"
    ; "    copy32(ctx->transcript + ctx->transcript_size, msg);"
    ; "    ctx->transcript_size += 32;"
    ; "    // Receive message, decrypted it if we have a key"
    ; "    copy32(dest, msg);"
    ; "    xor32(dest, ctx->keys + 64);"
    ; "}"
    ; ""
    ; "void kex_init(" ^ prefix ^ "ctx *ctx, const uint8_t pid[16])"
    ; "{"
    ; "    copy32(ctx->keys     , zero); // first chaining key"
    ; "    copy32(ctx->keys + 64, zero); // first encryption key"
    ; "    copy16(ctx->pid      , pid);  // protocol id"
    ; "    ctx->transcript_size = 0;     // transcript starts empty"
    ; "}"
    ; ""
    ; "static void kex_seed(" ^ prefix ^ "ctx *ctx, uint8_t random_seed[32])"
    ; "{"
    ; "    copy32(ctx->local_ske        , random_seed);"
    ; "    crypto_wipe(random_seed, 32); // auto wipe seed to avoid reuse"
    ; "    crypto_x25519_public_key(ctx->local_pke, ctx->local_ske);"
    ; "}"
    ; ""
    ; prototype
        "static void" "kex_locals"
        [ [prefix ^ "ctx " ; "*"; "ctx"         ]
        ; ["const uint8_t "; "" ; "local_sk[32]"]
        ; ["const uint8_t "; "" ; "local_pk[32]"]
        ]
    ; "{"
    ;"    if (local_pk == 0) crypto_x25519_public_key(ctx->local_pk, local_sk);"
    ;"    else               copy32                  (ctx->local_pk, local_pk);"
    ; "    copy32(ctx->local_sk         , local_sk   );"
    ; "}"
    ; ""
    ]

(* Specific source code *)
let block_comment comment =
  let slashes = "////" ^ String.make (String.length comment) '/' ^ "////" in
  slashes ^ "\n/// " ^ comment ^ " ///\n" ^ slashes

let print_header_pattern channel pattern protocol =
  let lower_pattern = String.lowercase_ascii pattern in
  print_lines channel
    [ block_comment pattern
    ; init_header lower_pattern Client protocol
    ; init_header lower_pattern Server protocol
    ];
  let messages    = snd protocol         in
  let nb_messages = List.length messages in
  for i = 1 to nb_messages + 1 do
    output_string channel (message_header lower_pattern i protocol ^ "\n")
  done

let print_source_pattern channel pattern protocol =
  let lower_pattern = String.lowercase_ascii pattern in
  print_lines channel
    [ block_comment pattern
    ; "static const uint8_t pid_" ^ lower_pattern
      ^ "[16] = \"Monokex "       ^ pattern ^ "\";"
    ; ""
    ; init_source pattern Client protocol
    ; init_source pattern Server protocol
    ];
  let messages    = snd protocol         in
  let nb_messages = List.length messages in
  for i = 1 to nb_messages + 1 do
    output_string channel (message_source lower_pattern i protocol ^ "\n")
  done
