open Utils

let panic error =
  prerr_endline error;
  exit 1

let protocol_errors (name, p) =
  match Validate.v p with
  | []     -> []
  | errors -> ["Bad protocol: " ^ name ^ ":\n"
               ^ String.concat "" (errors /@ (fun e -> "- " ^ e ^ "\n"))]

let _ =
  let protocols = Lexing.from_channel stdin |>Scan.tokens |>Parsec.protocols in
  let errors    = protocols /@ protocol_errors |> List.concat                in
  if protocols = [] then panic "There are no protocol to generate!";
  if errors   <> [] then panic ("\n" ^ String.concat "\n" errors ^ "\n");

  write "gen/spec.md"   (Gen_spec.spec   protocols);
  write "gen/monokex.h" (Gen_code.header protocols);
  write "gen/monokex.c" (Gen_code.source protocols);
  write "gen/test.c"    (Gen_test.test   protocols);
