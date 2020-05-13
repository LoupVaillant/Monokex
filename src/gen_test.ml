open Utils
module P = Proto

let print_prefix : out_channel -> unit = fun c ->
  List.iter (fun line -> output_string c (line ^ "\n"))
    [ "#include \"test_core.h\""
    ; "#include \"monokex.h\""
    ; "#include \"monocypher.h\""
    ; ""
    ; "#define RANDOM_INPUT(name, size) u8 name[size]; p_random(name, size)"
    ; ""
    ; "typedef uint8_t u8;"
    ]

let test_function : string -> P.protocol -> string = fun pattern p ->
  let lower_pattern = String.lowercase_ascii pattern             in
  let c_seed        = "    RANDOM_INPUT(client_seed, 32);\n"     in
  let s_seed        = "    RANDOM_INPUT(server_seed, 32);\n"     in
  let c_public      = "    RANDOM_INPUT(css, 32);  u8 cps[32];  "
                      ^ "crypto_x25519_public_key(cps, css);\n"  in
  let s_public      = "    RANDOM_INPUT(sss, 32);  u8 sps[32];  "
                      ^ "crypto_x25519_public_key(sps, sss);\n"  in
  let uses   key    = List.mem key (P.all_keys    p)             in
  let shares key    = List.mem key (P.shared_keys p)             in
  "static void test_" ^ lower_pattern ^"()\n"
  ^ "{\n"
  ^ (if uses P.IE then c_seed   else "")
  ^ (if uses P.RE then s_seed   else "")
  ^ (if uses P.IS then c_public else "")
  ^ (if uses P.RS then s_public else "")
  ^ "    crypto_kex_ctx client, server;\n"
  ^ ("    crypto_kex_" ^ lower_pattern ^ "_client_init(&client"
     ^ (if uses   P.IE then ", client_seed" else "")
     ^ (if uses   P.IS then ", css, cps"    else "")
     ^ (if shares P.RS then ", sps"         else "")
     ^ ");\n")
  ^ ("    crypto_kex_" ^ lower_pattern ^ "_server_init(&server"
     ^ (if uses   P.RE then ", server_seed" else "")
     ^ (if uses   P.RS then ", sss, sps"    else "")
     ^ (if shares P.IS then ", cps"         else "")
     ^ ");\n")
  ^ "    u8 pid[64] = \"Monokex " ^ pattern ^ "\";\n"
  ^ "    test_pattern(&client, &server, pid);\n"
  ^ "}\n"

let print_pattern : out_channel -> string -> Proto.protocol -> unit =
  fun c pattern p ->
  output_string c ("\n" ^ test_function pattern p)
