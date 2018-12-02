open Proto

let validate str =
  str
  |> Lexing.from_string
  |> Scan.tokens
  |> Parsec.protocol
  |> Validate.v

let _ = List.map validate
          [ "<- s ... -> e    <- e, ee, es  -> s, se"
          ; "         -> e    <- e, ee, s   -> s, es, se"
          ; "         -> e, e"
          ; "         -> s, s"
          ; "         -> e    <- e, e"
          ; "         -> e    <- s, s"
          ; "         -> e    <- e, ee, es  -> s, se"
          ; "         -> e    <- e, ee, ee  -> s, se"
          ; "         -> e    <- e          -> ee"
          ; "         -> e    -> e, ee"
          ; "<- e ... -> e, ee"
          ; "<- s -> s, ss ... -> e, es"
          ; ""
          ; "<- s ... -> e <- e, ee, es -> s"
          ]
