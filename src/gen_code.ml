open Utils
module P = Proto

let prefix = "monokex_"

let keys         message = message  // P.is_key      /@ P.to_key
let exchanges    message = message  // P.is_exchange /@ P.to_exchange
let has_exchange message = message |> P.to_actions |> exchanges |> (<>) []

let rec first_auth = function
  | []        -> error "first_auth: protocol makes no key exchange"
  | msg::msgs -> if has_exchange msg
                 then 1
                 else 1 + first_auth msgs

type cs = Client | Server
type lr = Local  | Remote

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
    (P.client_keys protocol // ((=) Proto.E) <> [])
    (P.server_keys protocol // ((=) Proto.E) <> [])

let cs_of_keys messages =
  messages /@ (function Proto.Client _ -> Client
                      | Proto.Server _ -> Server)

let lr_of_cs csa csb = match csa, csb with
  | Client, Client -> Local
  | Server, Server -> Local
  | Server, Client -> Remote
  | Client, Server -> Remote

let lr_of_protocol (cs : cs) (protocol : Proto.protocol) =
  protocol
  |> fst
  |> cs_of_keys
  |> List.map (lr_of_cs cs)

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
let receives nb =
  check (nb >= 0) "receives";
  nb <> 1

let sends nb (messages : Proto.message list) =
  check (nb >= 0) "sends";
  nb <= List.length messages

let verifies nb messages = first_auth messages <  nb
let auths    nb messages = first_auth messages <= nb

let r_actions nb messages = if not (receives nb) then []
                            else P.to_actions (List.nth messages (nb - 2))
let s_actions nb messages = if not (sends nb messages) then []
                            else P.to_actions (List.nth messages (nb - 1))

let r_size nb messages =
  check (receives nb) "r_size";
  let acts   = r_actions nb messages                  in
  let t_size = if verifies nb messages then 16 else 0 in
  let k_size = List.length (keys acts) * 32           in
  k_size + t_size

let s_size nb messages =
  check (sends nb messages) "s_size";
  let acts   = s_actions nb messages               in
  let t_size = if auths nb messages then 16 else 0 in
  let k_size = List.length (keys acts) * 32        in
  k_size + t_size

let message_proto pattern nb messages =
  let cs          = if nb mod 2 = 1 then Client else Server              in
  let gets_remote = List.mem (Proto.Key Proto.S) (r_actions nb messages) in
  let session_key = nb >= List.length messages                           in
  let current     = string_of_int nb                                     in
  let previous    = string_of_int (nb - 1)                               in
  let ctx         = [ctx_type cs; "*"; local cs ^ "_ctx"        ]        in
  let sk          = ["uint8_t " ; "" ; "session_key"    ; "[32]"]        in
  let rk          = ["uint8_t " ; "" ; remote cs ^ "_pk"; "[32]"]        in
  prototype
    (if verifies nb messages then "int" else "void")
    (prefix ^ pattern ^ "_" ^ current)
    [ ctx
    ; if session_key then sk else []
    ; if gets_remote then rk else []
    ; if sends nb messages then
        let ss = string_of_int (s_size nb messages) in
        ["uint8_t "      ; ""; "msg" ^ current;  "[" ^ ss ^ "]"]
      else []
    ; if receives nb then
        let ss = string_of_int (r_size nb messages) in
        ["const uint8_t "; ""; "msg" ^ previous; "[" ^ ss ^ "]"]
      else []
    ]

let str_msg nb  = "msg" ^ string_of_int nb
let key_comment key nb =
  (if nb mod 2 = 0 then "<- R" else "-> I") ^ P.map_key "E" "S" key

let message_offset       nb_keys = string_of_int (nb_keys * 32)
let message_offset_space nb_keys =
  if nb_keys = 0
  then "     "
  else " + " ^ message_offset nb_keys

let receive_key message_number nb_keys key =
  let ctx_key = P.map_key "ctx->remote_pke" "ctx->remote_pk " key in
  "    kex_receive   (ctx, " ^ ctx_key
  ^ ", "                     ^ str_msg message_number
  ^                            message_offset_space nb_keys
  ^ "      );  // "          ^ key_comment key message_number
  ^ "\n"

let send_key message_number nb_keys key =
  let ctx_key = P.map_key "ctx->local_pke" "ctx->local_pk " key in
  "    kex_send      (ctx, " ^ str_msg message_number
  ^                            message_offset_space nb_keys
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
  | Proto.E, Proto.E -> ee
  | Proto.S, Proto.S -> ss
  | Proto.E, Proto.S -> map_cs ces ses cs
  | Proto.S, Proto.E -> map_cs cse sse cs

let auth message_number nb_keys =
  "    kex_auth      (ctx, " ^ str_msg message_number
  ^ (if nb_keys = 0
     then ");                              // auth\n"
     else message_offset_space nb_keys
          ^ ");                         // auth\n"
    )

let verify message_number nb_keys =
  "    if (kex_verify(ctx, " ^ str_msg message_number
  ^ (if nb_keys = 0
     then ")) { return -1; }               // verify\n"
     else message_offset_space nb_keys
          ^ ")) { return -1; }          // verify\n"
    )

(* counts elements of a list, one by one. *)
let rec counts p start = function
  | []      -> []
  | x :: xs -> let new_start = (if p x then 1 else 0) + start in
               new_start :: counts p new_start xs
let key_counts = counts P.is_key (-1)

let process_message process_key cs message_number message =
  List.map2
    (fun ke count ->
      P.map_action (process_key message_number count) (exchange cs) ke
    )
    message
    (key_counts message)
  |> String.concat ""

let receive_message = process_message receive_key
let send_message    = process_message send_key

let message_body nb messages =
  let cs          = if nb mod 2 == 1 then Client else Server             in
  let session_key = nb >= List.length messages                           in
  let gets_remote = List.mem (Proto.Key Proto.S) (r_actions nb messages) in
  "\n{\n"
  ^ "    " ^ prefix ^ "ctx *ctx = &(" ^ local cs ^ "_ctx->ctx);\n"
  ^ (if receives nb
     then
       let message = List.nth messages (nb - 2) |> P.to_actions in
       let nb_keys = List.length (keys message)                 in
       receive_message cs (nb - 1) message
       ^ (if verifies nb messages
          then verify (nb - 1) nb_keys
          else "")
     else ""
    )
  ^ (if sends nb messages
     then
       let message = List.nth messages (nb - 1) |> P.to_actions in
       let nb_keys = List.length (keys message)                 in
       send_message cs nb message
       ^ (if auths nb messages
          then auth nb nb_keys
          else "")
     else ""
    )
  ^ (if gets_remote
     then "    copy32(" ^ remote cs ^ "_pk  , ctx->remote_pk);\n"
     else "")
  ^ (if session_key
     then "    copy32(session_key, ctx->keys + 96);\n"
          ^ "    WIPE_CTX(ctx);\n"
     else "";)
  ^ (if verifies nb messages
     then "    return 0;\n"
     else "")
  ^ "}\n"

let message_header pattern nb messages =
  message_proto pattern nb messages ^ ";\n"

let message_source pattern nb messages =
  message_proto pattern nb messages
  ^ message_body nb messages

let print_lines channel lines =
  List.iter (fun line -> output_string channel (line ^ "\n")) lines

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
    output_string channel (message_header lower_pattern i messages ^ "\n")
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
    output_string channel (message_source lower_pattern i messages ^ "\n")
  done
