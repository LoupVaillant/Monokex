open Utils
module P = Proto

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

let pattern pattern p = "\n" ^ test_function pattern p



let test protocols =
  "#include \"test_core.h\"\n"
  ^ "#include \"monokex.h\"\n"
  ^ "#include \"monocypher.h\"\n"
  ^ "\n"
  ^ "#define RANDOM_INPUT(name, size) u8 name[size]; p_random(name, size)\n"
  ^ "\n"
  ^ "typedef uint8_t u8;\n"
  ^ "\n"
  ^ String.concat "\n" (map_pair pattern protocols)
  ^ "\n\nint main()\n{\n"
  ^ String.concat "\n"
      (protocols /@ (fst
                     |- String.lowercase_ascii
                     |- (fun pattern -> "    test_" ^  pattern ^ "();")))
  ^ "\n    return 0;\n}\n"

