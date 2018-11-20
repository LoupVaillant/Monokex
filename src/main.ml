open Utils
open Proto

let validate str =
  str
  |> Lexing.from_string
  |> Scan.tokens
  |> Parsec.parse
  |> Validate.v

let a = validate
          "<- s
           ...
           -> e
           <- e, ee, es
           -> s, se"

let b = validate
          "-> e
           <- e, ee, es
           -> s, se"

let a = validate
          "<- s
           ...
           -> e
           -> e
           <- e, ee, es
           -> s, se"

let a = validate
          "<- s
           -> s, ss
           ...
           -> e
           <- e, ee
           -> s, se"
