open Utils
module P = Proto

type cs = Client | Server
type lr = Local  | Remote

let cs_of_int i = if i mod 2 == 1 then Client else Server

let map_cs a b cs = match cs with Client -> a | Server -> b
let map_lr a b lr = match lr with Local  -> a | Remote -> b

let local    cs = map_cs "client" "server" cs
let remote   cs = map_cs "server" "client" cs

(* init_header and init_source helpers *)
let is_authenticated protocol =
  map_cs
    (List.mem P.S (P.client_keys protocol))
    (List.mem P.S (P.server_keys protocol))

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

let init_proto u8 pattern cs protocol =
  let lr       = lr_of_protocol cs protocol                              in
  let css      = map_cs "client" "server" cs                             in
  let ctx      = [ prefix ^ "ctx "    ; "*"; "ctx" ]                     in
  let seed     = [ u8 ^ " "           ; "" ; "random_seed"    ; "[32]" ] in
  let sk       = [ "const " ^ u8 ^ " "; "" ; local  cs ^ "_sk"; "[32]" ] in
  let pk       = [ "const " ^ u8 ^ " "; "" ; local  cs ^ "_pk"; "[32]" ] in
  let r        = [ "const " ^ u8 ^ " "; "" ; remote cs ^ "_pk"; "[32]" ] in
  prototype
    "void"  (prefix ^ pattern ^ "_" ^ css ^ "_init")
    [ ctx
    ; if uses_ephemeral   protocol cs then seed else []
    ; if is_authenticated protocol cs then sk   else []
    ; if is_authenticated protocol cs then pk   else []
    ; if List.mem Remote lr           then r    else []
    ]

let knows_remote protocol cs = List.mem Remote (lr_of_protocol cs protocol)

let flip_exchange = function
  | (P.E, P.S) -> (P.S, P.E)
  | (P.S, P.E) -> (P.E, P.S)
  | (P.E, P.E) -> (P.E, P.E)
  | (P.S, P.S) -> (P.S, P.S)
let flip_action = function
  | P.Key      k -> P.Key k
  | P.Exchange e -> P.Exchange (flip_exchange e)
let flip_actions actions = List.map flip_action actions
let local_protocol protocol = function
  | Client -> snd protocol /@ P.map_message id           id
  | Server -> snd protocol /@ P.map_message flip_actions flip_actions

let code_of_action i =
  let wrap s = if i == 0 then s else "(" ^ s ^ " << " ^ string_of_int i ^ ")"
  in function
  | P.Key P.E             -> wrap "E"
  | P.Key P.S             -> wrap "S"
  | P.Exchange (P.E, P.E) -> wrap "EE"
  | P.Exchange (P.E, P.S) -> wrap "ES"
  | P.Exchange (P.S, P.E) -> wrap "SE"
  | P.Exchange (P.S, P.S) -> wrap "SS"
let code_of_actions actions =
  let shifts = range 0 (List.length actions - 1) /@ (fun i -> i * 3) in
  let exprs  = map2 code_of_action shifts actions                    in
  String.concat " + " exprs ^ ";\n"
let code_of_protocol protocol cs =
  let prefixes = range 0 3 /@
                   (fun i -> "    ctx->messages[" ^ string_of_int i ^ "] = ") in
  let messages = local_protocol protocol cs /@ code_of_actions                in
  let filler   = range 1 4 /@ (fun _ -> "0;\n")                               in
  map2 (fun prefix message -> prefix ^ message) prefixes (messages @ filler)
  |> String.concat ""

let contains_remote proto_half cs =
  let keys = proto_half /@ P.get_cs_keys |> List.concat in
  map_cs (List.mem P.RS keys) (List.mem P.IS keys) cs
let has_remote  proto cs = contains_remote (proto |> P.cs_protocol |> fst) cs
let gets_remote proto cs = contains_remote (proto |> P.cs_protocol |> snd) cs
let should_send cs       = map_cs true false cs
let flags_of_protocol protocol cs =
  let flags = []
              @ (if has_remote  protocol cs then ["HAS_REMOTE" ] else [])
              @ (if gets_remote protocol cs then ["GETS_REMOTE"] else [])
              @ (if should_send          cs then ["SHOULD_SEND"] else []) in
  if flags = []
  then ""
  else "    ctx->flags |= " ^ String.concat " | " flags ^ ";\n"

let init_body pattern cs protocol =
  let init   = "    kex_init    (ctx, pid_" ^ pattern ^ ");\n"  in
  let seed   = "    kex_seed    (ctx, random_seed);\n"          in
  let sk_pk  = "    kex_locals  (ctx, " ^ local cs ^ "_sk, "
               ^                          local cs ^ "_pk);\n"  in
  let cp_r   = "    copy(ctx->sr, " ^ remote cs ^ "_pk, 32);\n" in
  let r      = "    kex_mix_hash(ctx, ctx->sr, 32);\n"          in
  let l      = "    kex_mix_hash(ctx, ctx->sp, 32);\n"          in
  "\n{\n"
  ^ init
  ^ (if uses_ephemeral   protocol cs then seed  else "")
  ^ (if is_authenticated protocol cs then sk_pk else "")
  ^ (if knows_remote     protocol cs then cp_r  else "")
  ^ (flags_of_protocol protocol cs)
  ^ (code_of_protocol  protocol cs)
  ^ (lr_of_protocol   cs protocol
     /@ (function Local -> l | Remote -> r) |> String.concat "")
  ^ "}\n"

(* Generate source code for the init functions *)
let action_comment r protocol msg_nb =
  let msg_size = string_of_int (P.nth_message_size protocol msg_nb)  in
  let rw       = if r then "read " else "write"                      in
  let actions  = P.map_message id id (P.nth_message protocol msg_nb) in
  let remote   = r && List.mem (P.Key P.S) actions                   in
  "// - " ^ rw ^ " (" ^ msg_size ^ " bytes)\n"
  ^ if remote then "// - remote key\n" else ""

let action_comments cs protocol =
  range 1 (P.nb_messages protocol)
  /@ (fun msg_nb -> let r = map_cs (is_even msg_nb) (is_odd msg_nb) cs in
                    action_comment r protocol msg_nb)
  |> String.concat ""

let init_header pattern cs protocol =
  ""
  ^ "// Initialises a handshake context for the " ^ local cs ^ ".\n"
  ^ "// Actions happen in the following order:\n"
  ^ "//\n"
  ^ action_comments cs protocol
  ^ "// - final\n"
  ^ init_proto "uint8_t" pattern cs protocol ^ ";\n"

let init_source pattern cs protocol =
  let lower_pattern = String.lowercase_ascii pattern in
  init_proto "u8" lower_pattern cs protocol
  ^ init_body lower_pattern cs protocol

(* Common source code *)
let header_prefix =
  [ "#ifndef MONOKEX_H" (* we use the header name here, not the prefix *)
  ; "#define MONOKEX_H" (* we use the header name here, not the prefix *)
  ; ""
  ; "#include <inttypes.h>"
  ; "#include <stddef.h>"
  ; ""
  ; "typedef struct {"
  ; "    uint8_t  pool[64];    // random pool"
  ; "    uint8_t  hash[64];    // chaining hash"
  ; "    uint8_t  s [32];      // static    secret key"
  ; "    uint8_t  sp[32];      // static    public key"
  ; "    uint8_t  e [32];      // ephemeral secret key"
  ; "    uint8_t  ep[32];      // ephemeral public key"
  ; "    uint8_t  sr[32];      // static    remote key"
  ; "    uint8_t  er[32];      // ephemeral remote key"
  ; "    uint16_t messages[4]; // Message tokens"
  ; "    unsigned short flags; // Status flags"
  ; "} " ^ prefix ^ "ctx;"
  ; ""
  ; "typedef enum {"
  ; "    "^prefix_caps^"READ,  "^prefix_caps^"WRITE, "^prefix_caps^"REMOTE_KEY,"
  ; "    "^prefix_caps^"FINAL, "^prefix_caps^"NONE"
  ; "} " ^ prefix ^ "action;"
  ; ""
  ; "// Basic read & write functions"
  ; "// Maximum message size is 96 bytes"
  ; "//"
  ; "// If message_size is bigger than the actual message, the message will"
  ; "// be padded with random data."
  ; "//"
  ; "// If message_size is smaller than the actual message, the behaviour is"
  ; "// undefined.  (The implementation tries to fail loudly, though)"
  ; "//"
  ; "// Padding bytes are ignored by " ^ prefix ^ "read()."
  ; "int  "^prefix^"read ("^prefix^"ctx *ctx, const uint8_t *msg, size_t size);"
  ; "void "^prefix^"write("^prefix^"ctx *ctx, uint8_t       *msg, size_t size);"
  ; ""
  ; "// Advanced read & write functions (with payload)"
  ; "// Maximum message size is 96 bytes, plus the size of the payload."
  ; "//"
  ; "// If payload is NULL, no payload is sent. Payload_size must be zero."
  ; "// If payload_size is zero, but payload is not NULL, an empty payload is"
  ; "// sent."
  ; prototype "int" (prefix ^ "read_p")
      [ [ prefix ^ "ctx " ;  "*"; "ctx"                          ]
      ; [ "uint8_t "      ;  "*"; "payload, size_t payload_size" ]
      ; [ "const uint8_t ";  "*"; "message, size_t message_size" ]
      ] ^ ";"
  ; prototype "void" (prefix ^ "write_p")
      [ [ prefix ^ "ctx " ; "*"; "ctx"                          ]
      ; [ "uint8_t "      ; "*"; "message, size_t message_size" ]
      ; [ "const uint8_t "; "*"; "payload, size_t payload_size" ]
      ] ^ ";"
  ; ""
  ; "// Adds a prelude to the transcript hash."
  ; "// Call once, just after " ^ prefix ^ "*_init()."
  ; prototype "void" (prefix ^ "add_prelude")
      [ [ prefix ^ "ctx *ctx"                           ]
      ; [ "const uint8_t *prelude, size_t prelude_size" ]
      ] ^ ";"
  ; ""
  ; "// Gets the remote key."
  ; "// MUST be called as soon as the remote key has been transmitted."
  ; "// (Sometimes the key is known in advance, and is never transmitted.)"
  ; "void " ^ prefix ^ "remote_key(" ^ prefix ^ "ctx *ctx, uint8_t key[32]);"
  ; ""
  ; "// Gets the session key and wipes the context."
  ; "// The extra key can be used as a second session key, or as a hash for"
  ; "// channel binding."
  ; prototype "void" (prefix ^ "final")
      [ [ prefix ^ "ctx *ctx"       ]
      ; [ "uint8_t session_key[32]" ]
      ; [ "uint8_t extra_key  [32]" ]
      ] ^ ";"
  ; ""
  ; "// Next action to perform. Can be used instead of hard coding everything."
  ; "//"
  ; "// "^prefix_caps^"READ        call "^prefix^"read()"
  ; "// "^prefix_caps^"WRITE       call "^prefix^"write()"
  ; "// "^prefix_caps^"REMOTE_KEY  call "^prefix^"remote_key()"
  ; "// "^prefix_caps^"FINAL       call "^prefix^"final()"
  ; "// "^prefix_caps^"NONE        " ^
      "The context has been wiped, don't call anything."
  ; "//"
  ; "// If next_message_size is not NULL, the minimum size of the next"
  ; "// message (without payload) will be written in it."
  ; prototype (prefix ^ "action") (prefix ^ "next_action")
      [ [ "const " ^ prefix ^ "ctx *ctx" ]
      ; [ "size_t *next_message_size"    ]
      ] ^ ";"
  ]

let header_suffix = [ "#endif // MONOKEX_H" ]

let source_prefix =
  [ "#include \"monocypher.h\""
  ; "#include \"monokex.h\""
  ; ""
  ; "/////////////////"
  ; "/// Utilities ///"
  ; "/////////////////"
  ; "#define FOR(i, start, end)  for (size_t i = (start); (i) < (end); (i)++)"
  ; "#define WIPE_CTX(ctx)       crypto_wipe(ctx   , sizeof(*(ctx)))"
  ; "#define WIPE_BUFFER(buffer) crypto_wipe(buffer, sizeof(buffer))"
  ; ""
  ; "typedef uint8_t   u8;"
  ; "typedef uint16_t u16;"
  ; ""
  ; "// Message token bytecode"
  ; "typedef enum { E=1, S=2, EE=3, ES=4, SE=5, SS=6 } action;"
  ; "static int is_key     (unsigned i) { return i <= S;  }"
  ; "static int is_exchange(unsigned i) { return i >= EE; }"
  ; ""
  ; "// Context status flags"
  ; "static const u16 IS_OK       =  1; // Allways 1 (becomes zero when wiped)"
  ; "static const u16 HAS_KEY     =  2; // True if we have a symmetric key"
  ; "static const u16 HAS_REMOTE  =  4; // True if we have the remote DH key"
  ; "static const u16 GETS_REMOTE =  8; // True if the remote key is wanted"
  ; "static const u16 SHOULD_SEND = 16; // Send/receive toggle"
  ; ""
  ; "// memcpy clone"
  ; "static void copy(u8 *out, const u8 *in, size_t nb)"
  ; "{"
  ; "    FOR(i, 0, nb) {"
  ; "        out[i] = in[i];"
  ; "    }"
  ; "}"
  ; ""
  ; "static const u8 zero[8] = {0};"
  ; ""
  ; "/////////////////////"
  ; "/// State machine ///"
  ; "/////////////////////"
  ; "#define kex_mix_hash "^prefix^"add_prelude // it's the same thing"
  ; ""
  ; "void kex_mix_hash("^prefix^"ctx *ctx, const u8 *input, size_t input_size)"
  ; "{"
  ; "    crypto_blake2b_general("
    ^       "ctx->hash, 64, ctx->hash, 64, input, input_size);"
  ; "}"
  ; ""
  ; "static void kex_extra_hash(" ^ prefix ^ "ctx *ctx, u8 out[64])"
  ; "{"
  ; "    u8 one [1] = {1};"
  ; "    crypto_blake2b_general(ctx->hash, 64, ctx->hash, 64, zero, 1);"
  ; "    crypto_blake2b_general(out      , 64, ctx->hash, 64,  one, 1);"
  ; "}"
  ; ""
  ; "static void kex_update_key(" ^ prefix ^ "ctx *ctx,"
  ; "                           const u8 secret_key[32],"
  ; "                           const u8 public_key[32])"
  ; "{"
  ; "    u8 tmp[32];"
  ; "    crypto_x25519(tmp, secret_key, public_key);"
  ; "    kex_mix_hash(ctx, tmp, 32);"
  ; "    ctx->flags |= HAS_KEY;"
  ; "    WIPE_BUFFER(tmp);"
  ; "}"
  ; ""
  ; "static void kex_auth(" ^ prefix ^ "ctx *ctx, u8 tag[16])"
  ; "{"
  ; "    if (!(ctx->flags & HAS_KEY)) { return; }"
  ; "    u8 tmp[64];"
  ; "    kex_extra_hash(ctx, tmp);"
  ; "    copy(tag, tmp, 16);"
  ; "    WIPE_BUFFER(tmp);"
  ; "}"
  ; ""
  ; "static int kex_verify(" ^ prefix ^ "ctx *ctx, const u8 tag[16])"
  ; "{"
  ; "    if (!(ctx->flags & HAS_KEY)) { return 0; }"
  ; "    u8 real_tag[64]; // actually 16 useful bytes"
  ; "    kex_extra_hash(ctx, real_tag);"
  ; "    if (crypto_verify16(tag, real_tag)) {"
  ; "        WIPE_CTX(ctx);"
  ; "        WIPE_BUFFER(real_tag);"
  ; "        return -1;"
  ; "    }"
  ; "    WIPE_BUFFER(real_tag);"
  ; "    return 0;"
  ; "}"
  ; ""
  ; "static void kex_write_raw(" ^ prefix ^ "ctx *ctx, u8 *msg,"
  ; "                          const u8 *src, size_t size)"
  ; "{"
  ; "    copy(msg, src, size);"
  ; "    kex_mix_hash(ctx, msg, size);"
  ; "}"
  ; ""
  ; "static void kex_read_raw(" ^ prefix ^ "ctx *ctx, u8 *dest,"
  ; "                         const u8 *msg, size_t size)"
  ; "{"
  ; "    kex_mix_hash(ctx, msg, size);"
  ; "    copy(dest, msg, size);"
  ; "}"
  ; ""
  ; "static void kex_write(" ^
      prefix ^ "ctx *ctx, u8 *msg, const u8 *src, size_t size)"
  ; "{"
  ; "    if (!(ctx->flags & HAS_KEY)) {"
  ; "        kex_write_raw(ctx, msg, src, size);"
  ; "        return;"
  ; "    }"
  ; "    // we have a key, we encrypt"
  ; "    u8 key[64]; // actually 32 useful bytes"
  ; "    kex_extra_hash(ctx, key);"
  ; "    crypto_chacha20(msg, src, size, key, zero);"
  ; "    kex_mix_hash(ctx, msg, size);"
  ; "    kex_auth(ctx, msg + size);"
  ; "    WIPE_BUFFER(key);"
  ; "}"
  ; ""
  ; "static int kex_read(" ^
      prefix ^ "ctx *ctx, u8 *dest, const u8 *msg, size_t size)"
  ; "{"
  ; "    if (!(ctx->flags & HAS_KEY)) {"
  ; "        kex_read_raw(ctx, dest, msg, size);"
  ; "        return 0;"
  ; "    }"
  ; "    // we have a key, we decrypt"
  ; "    u8 key[64]; // actually 32 useful bytes"
  ; "    kex_extra_hash(ctx, key);"
  ; "    kex_mix_hash(ctx, msg, size);"
  ; "    if (kex_verify(ctx, msg + size)) {"
  ; "        WIPE_BUFFER(key);"
  ; "        return -1;"
  ; "    }"
  ; "    crypto_chacha20(dest, msg, size, key, zero);"
  ; "    WIPE_BUFFER(key);"
  ; "    return 0;"
  ; "}"
  ; ""
  ; "static unsigned kex_next_token(" ^ prefix ^ "ctx *ctx)"
  ; "{"
  ; "    unsigned token = ctx->messages[0] & 7;"
  ; "    ctx->messages[0] >>= 3;"
  ; "    return token;"
  ; "}"
  ; ""
  ; "static void kex_next_message(" ^ prefix ^ "ctx *ctx)"
  ; "{"
  ; "    FOR (i, 0, 3) {"
  ; "        ctx->messages[i] = ctx->messages[i+1];"
  ; "    }"
  ; "    ctx->messages[3] = 0;"
  ; "}"
  ; ""
  ; "//////////////////////"
  ; "/// Initialisation ///"
  ; "//////////////////////"
  ; "static void kex_init(" ^ prefix ^ "ctx *ctx, const u8 pid[32])"
  ; "{"
  ; "    copy(ctx->hash, pid, 64);"
  ; "    ctx->flags = IS_OK;"
  ; "}"
  ; ""
  ; "static void kex_seed(" ^ prefix ^ "ctx *ctx, u8 random_seed[32])"
  ; "{"
  ; "    // Note we only use the second half of the pool for now."
  ; "    // The first half will be used later to re-generate the pool."
  ; "    crypto_chacha20(ctx->pool, 0, 64, random_seed, zero);"
  ; "    crypto_wipe(random_seed, 32); // auto wipe seed to avoid reuse"
  ; "#ifndef DISABLE_ELLIGATOR"
  ; "    crypto_hidden_key_pair(ctx->ep, ctx->e, ctx->pool + 32);"
  ; "#else"
  ; "    copy(ctx->e, ctx->pool + 32, 32);"
  ; "    crypto_x25519_public_key(ctx->ep, ctx->e);"
  ; "#endif"
  ; "}"
  ; ""
  ; "static void kex_locals(" ^
      prefix ^ "ctx *ctx, const u8 s[32], const u8 sp[32])"
  ; "{"
  ; "    if (sp == 0) { crypto_x25519_public_key(ctx->sp, s);      }"
  ; "    else         { copy                    (ctx->sp, sp, 32); }"
  ; "    copy(ctx->s, s, 32);"
  ; "}"
  ; ""
  ; "//////////////////////"
  ; "/// Send & receive ///"
  ; "//////////////////////"
  ; "int "^prefix^"read ("^prefix^"ctx *ctx, const u8 *m, size_t m_size)"
  ; "{"
  ; "    return "^prefix^"read_p(ctx, 0, 0, m, m_size);"
  ; "}"
  ; ""
  ; "void "^prefix^"write("^prefix^"ctx *ctx, u8 *m, size_t m_size)"
  ; "{"
  ; "    "^prefix^"write_p(ctx, m, m_size, 0, 0);"
  ; "}"
  ; ""
  ; prototype "int" (prefix ^ "read_p")
      [ [ prefix ^ "ctx "; "*"; "ctx"              ]
      ; [ "u8 "          ; "*"; "p, size_t p_size" ]
      ; [ "const u8 "    ; "*"; "m, size_t m_size" ]
      ]
  ; "{"
  ; "    // Do nothing & fail if we should not receive"
  ; "    size_t min_size;"
  ; "    if ("^prefix^"next_action(ctx, &min_size) != "^prefix_caps^"READ ||"
  ; "        m_size < min_size + p_size"
    ^            prefix_space ^ prefix_space ^ "          ||"
  ; "        (p == 0 && p_size != 0)) {"
  ; "        WIPE_CTX(ctx);"
  ; "        return -1;"
  ; "    }"
  ; "    // Next time, we'll send"
  ; "    ctx->flags |= SHOULD_SEND;"
  ; ""
  ; "    // receive core message"
  ; "    while (ctx->messages[0] != 0) { // message not yet empty"
  ; "        size_t tag_size = ctx->flags & HAS_KEY ? 16 : 0;"
  ; "        switch (kex_next_token(ctx)) {"
  ; "        case E : kex_read_raw(ctx, ctx->er, m, 32);"
  ; "                 m += 32;"
  ; "#ifndef DISABLE_ELLIGATOR"
  ; "                 crypto_hidden_to_curve(ctx->er, ctx->er);"
  ; "#endif"
  ; "                 break;"
  ; "        case S : if (kex_read(ctx, ctx->sr, m, 32)) { return -1; }"
  ; "                 m += 32 + tag_size;"
  ; "                 ctx->flags |= HAS_REMOTE;                     break;"
  ; "        case EE: kex_update_key(ctx, ctx->e, ctx->er);         break;"
  ; "        case ES: kex_update_key(ctx, ctx->e, ctx->sr);         break;"
  ; "        case SE: kex_update_key(ctx, ctx->s, ctx->er);         break;"
  ; "        case SS: kex_update_key(ctx, ctx->s, ctx->sr);         break;"
  ; "        default:; // never happens"
  ; "        }"
  ; "    }"
  ; "    kex_next_message(ctx);"
  ; ""
  ; "    // Read payload, if any"
  ; "    if (p != 0) { if (kex_read(ctx, p, m, p_size)) { return -1; } }"
  ; "    else        { if (kex_verify(ctx, m)         ) { return -1; } }"
  ; "    return 0;"
  ; "}"
  ; ""
  ; prototype "void" (prefix ^ "write_p")
      [ [ prefix ^ "ctx "; "*"; "ctx"              ]
      ; [ "u8 "          ; "*"; "m, size_t m_size" ]
      ; [ "const u8 "    ; "*"; "p, size_t p_size" ]
      ]
  ; "{"
  ; "    // Fail if we should not send (the failure is alas delayed)"
  ; "    size_t min_size;"
  ; "    if ("^prefix^"next_action(ctx, &min_size) != "^prefix_caps^"WRITE ||"
  ; "        m_size < min_size + p_size"
    ^            prefix_space ^ prefix_space ^ "           ||"
  ; "        (p == 0 && p_size != 0)) {"
  ; "        WIPE_CTX(ctx);"
  ; "        return;"
  ; "    }"
  ; "    // Next time, we'll receive"
  ; "    ctx->flags &= ~SHOULD_SEND;"
  ; ""
  ; "    // Send core message"
  ; "    while (ctx->messages[0] != 0) { // message not yet empty"
  ; "        size_t tag_size = ctx->flags & HAS_KEY ? 16 : 0;"
  ; "        switch (kex_next_token(ctx)) {"
  ; "        case E : kex_write_raw (ctx, m, ctx->ep, 32); m += 32;            "
    ^                                                                   "break;"
  ; "        case S : kex_write     (ctx, m, ctx->sp, 32); m += 32 + tag_size; "
    ^                                                                   "break;"
  ; "        case EE: kex_update_key(ctx, ctx->e, ctx->er);                    "
    ^                                                                   "break;"
  ; "        case ES: kex_update_key(ctx, ctx->e, ctx->sr);                    "
    ^                                                                   "break;"
  ; "        case SE: kex_update_key(ctx, ctx->s, ctx->er);                    "
    ^                                                                   "break;"
  ; "        case SS: kex_update_key(ctx, ctx->s, ctx->sr);                    "
    ^                                                                   "break;"
  ; "        default:; // never happens"
  ; "        }"
  ; "    }"
  ; "    kex_next_message(ctx);"
  ; ""
  ; "    // Write payload, if any"
  ; "    size_t tag_size = ctx->flags & HAS_KEY ? 16 : 0;"
  ; "    if (p != 0) { kex_write(ctx, m, p, p_size); m += tag_size + p_size; }"
  ; "    else        { kex_auth (ctx, m);            m += tag_size;          }"
  ; ""
  ; "    // Pad"
  ; "    size_t pad_size = m_size - min_size - p_size;"
  ; "    if (pad_size != 0) {"
  ; "        // Regenerate the pool with its first half,"
  ; "        // then use the second half for padding."
  ; "        // That way we keep the first half of the pool fresh."
  ; "        crypto_chacha20(ctx->pool, 0, 64, ctx->pool, zero);"
  ; "        crypto_chacha20(m, 0, pad_size, ctx->pool + 32, zero);"
  ; "    }"
  ; "}"
  ; ""
  ; "///////////////"
  ; "/// Outputs ///"
  ; "///////////////"
  ; "void " ^ prefix ^ "remote_key(" ^ prefix ^ "ctx *ctx, u8 key[32])"
  ; "{"
  ; "    if (!(ctx->flags & HAS_REMOTE)) {"
  ; "        WIPE_CTX(ctx);"
  ; "        return;"
  ; "    }"
  ; "    copy(key, ctx->sr, 32);"
  ; "    ctx->flags &= ~GETS_REMOTE;"
  ; "}"
  ; ""
  ; "void " ^ prefix ^ "final(" ^ prefix ^ "ctx *ctx, u8 key[32], u8 extra[32])"
  ; "{"
  ; "    if (" ^ prefix ^ "next_action(ctx, 0) == " ^ prefix_caps ^ "FINAL) {"
  ; "        copy(key, ctx->hash, 32);"
  ; "        if (extra != 0) {"
  ; "            copy(extra, ctx->hash + 32, 32);"
  ; "        }"
  ; "    }"
  ; "    WIPE_CTX(ctx);"
  ; "}"
  ; ""
  ; "///////////////////"
  ; "/// Next action ///"
  ; "///////////////////"
  ; prototype (prefix ^ "action") (prefix ^ "next_action")
      [ [ "const " ^ prefix ^ "ctx *ctx" ]
      ; [ "size_t *next_message_size"    ]
      ]
  ; "{"
  ; "    // Next message size (if any)"
  ; "    if (next_message_size) {"
  ; "        unsigned has_key = ctx->flags & HAS_KEY ? 16 : 0;"
  ; "        uint16_t message = ctx->messages[0];"
  ; "        size_t   size    = 0;"
  ; "        while (message != 0) {"
  ; "            if (is_exchange(message & 7)) { has_key = 16;         }"
  ; "            if (is_key     (message & 7)) { size += 32 + has_key; }"
  ; "            message >>= 3;"
  ; "        }"
  ; "        *next_message_size = size + has_key;"
  ; "    }"
  ; "    // Next action"
  ; "    int should_get_remote ="
  ; "        (ctx->flags & HAS_REMOTE) &&"
  ; "        (ctx->flags & GETS_REMOTE);"
  ; "    return !(ctx->flags & IS_OK)    ? " ^ prefix_caps ^ "NONE"
  ; "        :  should_get_remote        ? " ^ prefix_caps ^ "REMOTE_KEY"
  ; "        :  ctx->messages[0] == 0    ? " ^ prefix_caps ^ "FINAL"
  ; "        :  ctx->flags & SHOULD_SEND ? " ^ prefix_caps ^ "WRITE"
  ; "        :                             " ^ prefix_caps ^ "READ;"
  ; "}"
  ; ""
  ]

let print_lines channel lines =
  List.iter (fun line -> output_string channel (line ^ "\n")) lines

let print_header_prefix channel = print_lines channel header_prefix
let print_header_suffix channel = print_lines channel header_suffix
let print_source_prefix channel = print_lines channel source_prefix

(* Specific source code *)
let block_comment comment =
  let slashes = "////" ^ String.make (String.length comment) '/' ^ "////" in
  slashes ^ "\n/// " ^ comment ^ " ///\n" ^ slashes

let print_header_pattern channel pattern p =
  let lower_pattern = String.lowercase_ascii pattern in
  print_lines channel
    [ block_comment pattern
    ; ""
    ; init_header lower_pattern Client p
    ; init_header lower_pattern Server p
    ]

let print_source_pattern channel pattern p =
  let lower_pattern = String.lowercase_ascii pattern in
  print_lines channel
    [ block_comment pattern
    ; "static const u8 pid_" ^ lower_pattern
      ^ "[64] = \"Monokex "       ^ pattern ^ "\";"
    ; ""
    ; init_source pattern Client p
    ; init_source pattern Server p
    ]
