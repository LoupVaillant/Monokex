(* Utils *)
let (//) l p = List.filter p l
let (/@) l f = List.map    f l
let error s  = raise (Invalid_argument s)
let f_error s = fun _ -> error s
let check assertion s = if not assertion then error s
let const x y = x
let id    x   = x

(* Protocol manipulation *)
let map_cs c s = function
  | Proto.Client actions -> c actions
  | Proto.Server actions -> s actions

let map_ke k e = function
  | Proto.Key      key      -> k key
  | Proto.Exchange exchange -> e exchange

let map_es e s = function
  | Proto.E -> e
  | Proto.S -> s

let actions = function
  | Proto.Client acts -> acts
  | Proto.Server acts -> acts

let is_client_message = map_cs (const true ) (const false)
let is_server_message = map_cs (const false) (const true )
let is_key            = map_ke (const true ) (const false)
let is_exchange       = map_ke (const false) (const true )
let to_key            = map_ke id (f_error "to_key")
let to_exchange       = map_ke (f_error "to_exchange") id

let keys            message  = message  // is_key            /@ to_key
let exchanges       message  = message  // is_exchange       /@ to_exchange
let client_messages messages = messages // is_client_message /@ actions
let server_messages messages = messages // is_server_message /@ actions

let client_keys messages = client_messages messages |> List.concat |> keys
let server_keys messages = server_messages messages |> List.concat |> keys

let has_key      message = message |> actions |> keys      |> (<>) []
let has_exchange message = message |> actions |> exchanges |> (<>) []

let rec first_auth = function
  | []        -> error "first_auth: protocol makes no key exchange"
  | msg::msgs -> if has_exchange msg
                 then 1
                 else 1 + first_auth msgs


type cs = Client | Server
type lr = Local  | Remote

(* init_header and init_source helpers *)
let is_authenticated (cs : cs) (protocol : Proto.protocol) = match cs with
  | Client -> client_keys (fst protocol @ snd protocol) <> []
  | Server -> server_keys (fst protocol @ snd protocol) <> []

let uses_ephemeral cs protocol = match cs with
  | Client -> client_keys (snd protocol) // ((=) Proto.E) <> []
  | Server -> server_keys (snd protocol) // ((=) Proto.E) <> []

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

let indent n s = String.make n ' ' ^ s

let init_proto pattern cs protocol =
  let lr    = lr_of_protocol   cs protocol                          in
  let css   = match cs with Client -> "client" | Server -> "server" in
  let fn    = "void crypto_kex_" ^ pattern ^ "_init_" ^  css ^ "("  in
  let arg a = ",\n" ^ indent (String.length fn) a                   in
  let seed  = arg "uint8_t         random_seed[32]"                 in
  let sk    = arg "const uint8_t   local_sk   [32]"                 in
  let pk    = arg "const uint8_t   local_pk   [32]"                 in
  let r     = arg "const uint8_t   remote_pk  [32]"                 in
  fn ^ "crypto_kex_ctx *ctx"
  ^ (if uses_ephemeral   cs protocol then seed    else "")
  ^ (if is_authenticated cs protocol then sk ^ pk else "")
  ^ (if List.mem Remote lr           then r       else "")
  ^ ")"

let init_body cs protocol =
  let lr     = lr_of_protocol   cs protocol                            in
  let init   = "    kex_init   (ctx);\n"                               in
  let seed   = "    kex_seed   (ctx, random_seed);\n"                  in
  let sk_pk  = "    kex_locals (ctx, local_sk, local_pk);\n"           in
  let r      = "    kex_receive(ctx, ctx->remote_pk, remote_pk);\n"    in
  let l      = "    kex_receive(ctx, ctx->local_pk, ctx->local_pk);\n" in
  "\n{\n"
  ^ init
  ^ (if uses_ephemeral   cs protocol then seed  else "")
  ^ (if is_authenticated cs protocol then sk_pk else "")
  ^ (lr /@ (function Local -> l | Remote -> r) |> String.concat "")
  ^ "}\n"

(* Generate source code for the init functions *)
let init_header pattern cs protocol =
  init_proto pattern cs protocol ^ ";\n"

let init_source pattern cs protocol =
  init_proto pattern cs protocol ^ init_body cs protocol

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
                            else actions (List.nth messages (nb - 2))
let s_actions nb messages = if not (sends nb messages) then []
                            else actions (List.nth messages (nb - 1))

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
  let remote      = List.mem (Proto.Key Proto.S) (r_actions nb messages)      in
  let session_key = nb >= List.length messages                                in
  let return_type = if verifies nb messages then "int" else "void"            in
  let current     = string_of_int nb                                          in
  let previous    = string_of_int (nb - 1)                                    in
  let fn          = return_type ^ " crypto_kex_" ^ pattern^"_"^current ^ "("  in
  let arg a       = ",\n" ^ indent (String.length fn) a                       in
  let sk          = arg  "uint8_t         session_key[32]"                    in
  let rk          = arg  "uint8_t         remote_pk[32]"                      in
  fn ^ "crypto_kex_ctx *ctx"
  ^ (if session_key then sk   else "")
  ^ (if remote      then rk   else "")
  ^ (if sends nb messages then
       let ss = string_of_int (s_size nb messages) in
       arg ("uint8_t         msg" ^ current  ^ "[" ^ ss ^ "]")
     else "")
  ^ (if receives nb then
       let rs = string_of_int (r_size nb messages) in
       arg ("const uint8_t   msg" ^ previous ^ "[" ^ rs ^ "]")
     else "")
  ^ ")"

(* let line l = "    " ^  l ^ "\n" *)

let str_msg nb  = "msg" ^ string_of_int nb
let key_comment key nb =
  (if nb mod 2 = 0 then "<- R" else "-> I") ^ map_es "E" "S" key

let message_offset       nb_keys = string_of_int (nb_keys * 32)
let message_offset_space nb_keys =
  if nb_keys = 0
  then "     "
  else " + " ^ message_offset nb_keys

let receive_key message_number nb_keys key =
  let ctx_key = map_es "ctx->remote_pke" "ctx->remote_pk " key    in
  "    kex_receive   (ctx, " ^ ctx_key
  ^ ", "                     ^ str_msg message_number
  ^                            message_offset_space nb_keys
  ^ "      );  // "          ^ key_comment key message_number
  ^ key_comment key message_number
  ^ "\n"

let send_key message_number nb_keys key =
  let ctx_key = map_es "ctx->local_pke" "ctx->local_pk " key        in
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
  | Proto.E, Proto.S -> (match cs with Client -> ces | Server -> ses)
  | Proto.S, Proto.E -> (match cs with Client -> cse | Server -> sse)

let message_offset nb_keys = string_of_int (nb_keys * 32)

let auth message_number nb_keys =
  "    kex_auth      (ctx, " ^ str_msg message_number
  ^ (if nb_keys = 0
     then ");                              // auth\n"
     else " + " ^ message_offset nb_keys
          ^ ");                         // auth\n"
    )

let verify message_number nb_keys =
  "    if (kex_verify(ctx, " ^ str_msg message_number
  ^ (if nb_keys = 0
     then ")) { return -1; }               // verify\n"
     else " + " ^ message_offset nb_keys
          ^ ")) { return -1; }          // verify\n"
    )

(* counts elements of a list, one by one. *)
let rec counts p start = function
  | []      -> []
  | x :: xs -> let new_start = (if p x then 1 else 0) + start in
               new_start :: counts p new_start xs
let key_counts = counts is_key (-1)

let process_message process_key cs message_number message =
  List.map2
    (fun ke count ->
      map_ke (process_key message_number count) (exchange cs) ke
    )
    message
    (key_counts message)
  |> String.concat ""

let receive_message = process_message receive_key
let send_message    = process_message send_key

let message_body nb messages =
  let cs          = if nb mod 2 == 1 then Client else Server             in
  let session_key = nb >= List.length messages                           in
  let remote      = List.mem (Proto.Key Proto.S) (r_actions nb messages) in
  "\n{\n"
  ^ (if receives nb
     then
       let message = List.nth messages (nb - 2) |> actions in
       let nb_keys = List.length (keys message)            in
       receive_message cs (nb - 1) message
       ^ (if verifies nb messages
          then verify (nb - 1) nb_keys
          else "")
     else ""
    )
  ^ (if sends nb messages
     then
       let message = List.nth messages (nb - 1) |> actions in
       let nb_keys = List.length (keys message)            in
       send_message cs nb message
       ^ (if auths nb messages
          then auth nb nb_keys
          else "")
     else ""
    )
  ^ (if remote
     then "    copy32(remote_pk  , ctx->remote_pk);\n"
     else "")
  ^ (if session_key
     then "    copy32(session_key, ctx->derived_keys + 32);\n"
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
    ; "    uint8_t transcript [128];"
    ; "    uint8_t chaining_key[32];"
    ; "    uint8_t derived_keys[64];"
    ; "    uint8_t local_sk    [32];"
    ; "    uint8_t local_pk    [32];"
    ; "    uint8_t local_ske   [32];"
    ; "    uint8_t local_pke   [32];"
    ; "    uint8_t remote_pk   [32];"
    ; "    uint8_t remote_pke  [32];"
    ; "    size_t  transcript_size;"
    ; "} crypto_kex_ctx;"
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
    ; "static void copy32(uint8_t out[32], const uint8_t in[32])"
    ; "{"
    ; "    for (size_t i = 0; i < 32; i++) { out[i]  = in[i]; }"
    ; "}"
    ; "static void xor32 (uint8_t out[32], const uint8_t in[32])"
    ; "{"
    ; "    for (size_t i = 0; i < 32; i++) { out[i] ^= in[i]; }"
    ; "}"
    ; ""
    ; "static void kex_update_key(crypto_kex_ctx *ctx,"
    ; "                           const uint8_t   secret_key[32],"
    ; "                           const uint8_t   public_key[32])"
    ; "{"
    ; "    // Extract"
    ; "    uint8_t shared_secret[32];"
    ; "    crypto_x25519(shared_secret, secret_key, public_key);"
    ; "    crypto_chacha20_H(shared_secret    , shared_secret    , zero);"
    ; "    crypto_chacha20_H(ctx->chaining_key, ctx->chaining_key, one );"
    ; "    xor32(ctx->chaining_key, shared_secret);"
    ; ""
    ; "    // Expand (directly from chaining key)"
    ; "    crypto_chacha_ctx chacha_ctx;"
    ; "    crypto_chacha20_init  (&chacha_ctx, ctx->chaining_key, one);"
    ; "    crypto_chacha20_stream(&chacha_ctx, ctx->derived_keys, 64);"
    ; ""
    ; "    // Clean up"
    ; "    WIPE_BUFFER(shared_secret);"
    ; "    WIPE_CTX(&chacha_ctx);"
    ; "}"
    ; ""
    ; "static void kex_auth(crypto_kex_ctx *ctx, uint8_t mac[16])"
    ; "{"
    ; "    crypto_poly1305(mac, ctx->transcript, ctx->transcript_size,"
    ; "                    ctx->derived_keys);"
    ; "}"
    ; ""
    ; "static int kex_verify(crypto_kex_ctx *ctx, const uint8_t mac[16])"
    ; "{"
    ; "    uint8_t real_mac[16];"
    ; "    kex_auth(ctx, real_mac);"
    ; "    int mismatch = crypto_verify16(real_mac, mac);"
    ; "    if (mismatch) {  WIPE_CTX(ctx); }"
    ; "    WIPE_BUFFER(real_mac);"
    ; "    return mismatch;"
    ; "}"
    ; ""
    ; "static void kex_send(crypto_kex_ctx *ctx,"
    ; "                     uint8_t msg[32], const uint8_t src[32])"
    ; "{"
    ; "    // Send message, encrypted if we have a key"
    ; "    copy32(msg, src);"
    ; "    xor32(msg, ctx->derived_keys + 32);"
    ; "    // Record sent message"
    ; "    copy32(ctx->transcript + ctx->transcript_size, msg);"
    ; "    ctx->transcript_size += 32;"
    ; "}"
    ; ""
    ; "static void kex_receive(crypto_kex_ctx *ctx,"
    ; "                        uint8_t dest[32], const uint8_t msg[32])"
    ; "{"
    ; "    // Record incoming message"
    ; "    copy32(ctx->transcript + ctx->transcript_size, msg);"
    ; "    ctx->transcript_size += 32;"
    ; "    // Receive message, decrypted it if we have a key"
    ; "    copy32(dest, msg);"
    ; "    xor32(dest, ctx->derived_keys + 32);"
    ; "}"
    ; ""
    ; "static void kex_init(crypto_kex_ctx *ctx)"
    ; "{"
    ; "    copy32(ctx->chaining_key     , zero);"
    ; "    copy32(ctx->derived_keys + 32, zero);"
      ^ "  // first encryption key is zero"
    ; "    ctx->transcript_size = 0;"
    ; "}"
    ; ""
    ; "static void kex_seed(crypto_kex_ctx *ctx, uint8_t random_seed[32])"
    ; "{"
    ; "    copy32(ctx->local_ske        , random_seed);"
    ; "    crypto_wipe(random_seed, 32); // auto wipe seed to avoid reuse"
    ; "    crypto_x25519_public_key(ctx->local_pke, ctx->local_ske);"
    ; "}"
    ; ""
    ; "static void kex_locals(crypto_kex_ctx *ctx,"
    ; "                       const uint8_t   local_sk   [32],"
    ; "                       const uint8_t   local_pk   [32])"
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
    ; init_source lower_pattern Client protocol
    ; init_source lower_pattern Server protocol
    ];
  let messages    = snd protocol         in
  let nb_messages = List.length messages in
  for i = 1 to nb_messages + 1 do
    output_string channel (message_source lower_pattern i messages ^ "\n")
  done
