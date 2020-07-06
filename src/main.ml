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
  try Unix.mkdir dir 0o755
  with Unix.Unix_error(Unix.EEXIST, _, _) -> ()

let _ =
  let proto  = Lexing.from_channel stdin |>Scan.tokens |>Parsec.protocols in
  let errors = proto /@ protocol_errors |> List.concat                    in
  if proto  =  [] then panic "There are no protocol to generate!";
  if errors <> [] then panic ("\n" ^ String.concat "\n" errors ^ "\n");

  mkdir "elligator";
  write "elligator/spec.md"   (Gen_spec.spec   proto);
  write "elligator/monokex.h" (Gen_code.header proto);
  write "elligator/monokex.c" (Gen_code.source proto
                                 (read "src/deps-elligator.c"));
  write "elligator/test.c"    (Gen_test.test   proto);
  write "elligator/test_core.c" (read "src/test_core.c");
  write "elligator/test_core.h" (read "src/test_core.h");
  write "elligator/makefile"    (read "src/makefile"   );
  write "elligator/monokex.pc"  (read "src/monokex.pc" );

  mkdir "classic";
  write "classic/spec.md"   (Gen_spec.spec   proto);
  write "classic/monokex.h" (Gen_code.header proto);
  write "classic/monokex.c" (Gen_code.source proto
                               (read "src/deps-classic.c"));
  write "classic/test.c"    (Gen_test.test   proto);
  write "classic/test_core.c" (read "src/test_core.c");
  write "classic/test_core.h" (read "src/test_core.h");
  write "classic/makefile"    (read "src/makefile"   );
  write "classic/monokex.pc"  (read "src/monokex.pc" );
