type token =
    Key        of Proto.key
  | Exchange   of Proto.exchange
  | Name       of string
  | Scan_error of char
  | Dots
  | Comma
  | Left
  | Right
  | Eof
val tokens : Lexing.lexbuf -> (int * token) Stream.t
