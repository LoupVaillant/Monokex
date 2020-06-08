open Utils

(* Utils *)
let map_pair f l =
  let l1, l2 = List.split l in
  String.concat "\n" (List.map2 f l1 l2) ^ "\n"

let panic error =
  prerr_endline error;
  exit 1

let protocol_errors (name, p) =
  match Validate.v p with
  | []     -> []
  | errors -> ["Bad protocol: " ^ name ^ ":\n"
               ^ String.concat "" (errors /@ (fun e -> "- " ^ e ^ "\n"))]


(* main code *)
let protocols = Lexing.from_channel stdin |> Scan.tokens |> Parsec.protocols
let errors    = protocols /@ protocol_errors |> List.concat

let _ =
  if protocols = [] then panic "There are no protocol to generate!";
  if errors   <> [] then panic ("\n" ^ String.concat "\n" errors ^ "\n");

  write "gen/spec.md"
    (map_pair (Gen_spec.spec) protocols);

  write "gen/monokex.h"
    (Gen_code.header_prefix
     ^ map_pair (Gen_code.header_pattern) protocols
     ^ Gen_code.header_suffix);
  write "gen/monokex.c"
    (Gen_code.source_prefix
     ^ map_pair (Gen_code.source_pattern) protocols);

  write "gen/test.c"
    (Gen_test.prefix
     ^ map_pair (Gen_test.pattern) protocols
     ^ "\nint main()\n{\n"
     ^ String.concat "\n"
         (protocols /@ (fst
                        |- String.lowercase_ascii
                        |- (fun pattern -> "    test_" ^  pattern ^ "();")))
     ^ "\n    return 0;\n}\n")
