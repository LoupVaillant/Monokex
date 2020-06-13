open Utils

let panic error =
  prerr_endline error;
  exit 1

let protocol_errors (name, p) =
  match Validate.v p with
  | []     -> []
  | errors -> ["Bad protocol: " ^ name ^ ":\n"
               ^ String.concat "" (errors /@ (fun e -> "- " ^ e ^ "\n"))]

let mkdir dir =
  try Unix.mkdir "gen" 0o755
  with Unix.Unix_error(Unix.EEXIST, _, _) -> ()

let _ =
  let proto  = Lexing.from_channel stdin |>Scan.tokens |>Parsec.protocols in
  let errors = proto /@ protocol_errors |> List.concat                    in
  if proto  =  [] then panic "There are no protocol to generate!";
  if errors <> [] then panic ("\n" ^ String.concat "\n" errors ^ "\n");

  mkdir "gen";
  write "gen/spec.md"   (Gen_spec.spec   proto);
  write "gen/monokex.h" (Gen_code.header proto);
  write "gen/monokex.c" (Gen_code.source proto (read "src/deps-elligator.c"));
  write "gen/test.c"    (Gen_test.test   proto);
  write "gen/test_core.c" (read "src/test_core.c");
  write "gen/test_core.h" (read "src/test_core.h");
  write "gen/makefile"    (read "src/makefile"   );
  write "gen/monokex.pc"  (read "src/monokex.pc" );
