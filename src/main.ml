open Utils

(* Utils *)
let iter_pair f l =
  let l1, l2 = List.split l in
  List.iter2 f l1 l2

let panic error =
  prerr_endline error;
  exit 1

let protocol_errors (name, p) =
  match Validate.v p with
  | []     -> []
  | errors -> ["Bad protocol: " ^ name ^ ":\n"
               ^ String.concat "" (errors /@ (fun e -> "- " ^ e ^ "\n"))]

let parse channel =
  Lexing.from_channel channel
  |> Scan.tokens
  |> Parsec.protocols

let _ =
  let folder    = Sys.argv.(1)                                in
  let spec      = open_out (folder ^ "/spec.md"  )            in
  let header    = open_out (folder ^ "/monokex.h")            in
  let source    = open_out (folder ^ "/monokex.c")            in
  let test      = open_out (folder ^ "/test.c")               in
  let protocols = parse stdin                                 in
  let errors    = protocols /@ protocol_errors |> List.concat in

  if protocols = [] then panic "There is no protocol to generate!";
  if errors   <> [] then panic ("\n" ^ String.concat "\n" errors ^ "\n");

  iter_pair (Gen_spec.print spec) protocols;

  Gen_code.print_header_prefix header;
  iter_pair (Gen_code.print_header_pattern header) protocols;
  Gen_code.print_header_suffix header;

  Gen_code.print_source_prefix source;
  iter_pair (Gen_code.print_source_pattern source) protocols;

  Gen_test.print_prefix test;
  iter_pair (Gen_test.print_pattern test) protocols;
  output_string test "\nint main()\n{\n";
  List.iter (fst
             |- String.lowercase_ascii
             |- (fun pattern -> "    test_" ^  pattern ^ "();\n")
             |- output_string test)
    protocols;
  output_string test "    return 0;\n}\n";

  close_out test;
  close_out source;
  close_out header;
  close_out spec;
  ()
