open Proto

let validate (name, p) =
  match Validate.v p with
  | Validate.Valid        -> ()
  | Validate.Broken error -> prerr_endline ("Bad protocol: " ^ name
                                            ^ ": "           ^ error);
                             exit 1

let parse channel =
  Lexing.from_channel channel
  |> Scan.tokens
  |> Parsec.protocols

let _ =
  let input     = stdin       in
  let output    = stdout      in
  let protocols = parse input in
  List.iter validate protocols;
  List.iter (fun (name, p) ->
      output_string  output (name                                 ^ "\n");
      output_string  output (String.make (String.length name) '=' ^ "\n");
      output_string  output "\n";
      Gen_spec.print output p
    ) protocols;
  ()
