let protocol_of_string str =
  str
  |> Lexing.from_string
  |> Scan.tokens
  |> Parsec.protocol

let validate str =
  Validate.v (protocol_of_string str)

let _ = List.map validate
          [ "<- s ... -> e    <- e, ee, es  -> s, se"      (* XK1 *)
          ; "         -> e    <- e, ee, s   -> s, es, se"
          ; "         -> e, e"
          ; "         -> s, s"
          ; "         -> e    <- e, e"
          ; "         -> e    <- s, s"
          ; "         -> e    <- e, ee, es  -> s, se"
          ; "         -> e    <- e, ee, ee  -> s, se"
          ; "         -> e    <- e          -> ee"         (* valid!! *)
          ; "         -> e    -> e, ee"
          ; "<- e ... -> e, ee"
          ; "<- s -> s, ss ... -> e, es"
          ; ""
          ; "<- s ... -> e <- e, ee, es -> s"
          ]

