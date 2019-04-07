(* Utils *)
let iter_pair f l =
  let l1, l2 = List.split l in
  List.iter2 f l1 l2

let panic error =
  prerr_endline error;
  exit 1

let validate (name, p) =
  match Validate.v p with
  | Validate.Valid        -> ()
  | Validate.Broken error -> panic ("Bad protocol: " ^ name ^ ": " ^ error)

let parse channel =
  Lexing.from_channel channel
  |> Scan.tokens
  |> Parsec.protocols

let _ =
  let folder    = Sys.argv.(1) in
  let spec      = open_out (folder ^ "/spec.md"  ) in
  let header    = open_out (folder ^ "/monokex.h") in
  let source    = open_out (folder ^ "/monokex.c") in
  let protocols = parse stdin                      in

  if protocols = [] then panic "There is no protocol to generate!";

  List.iter validate protocols;

  Gen_spec.print_xckdf spec;
  iter_pair (Gen_spec.print spec) protocols;

  Gen_code.print_header_prefix header;
  iter_pair (Gen_code.print_header_pattern header) protocols;

  Gen_code.print_source_prefix source;
  iter_pair (Gen_code.print_source_pattern source) protocols;

  close_out source;
  close_out header;
  close_out spec;
  ()
