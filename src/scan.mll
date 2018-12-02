{
  open Lexing
  open Proto

  type token = Key        of key
             | Exchange   of exchange
             | Name       of string
             | Scan_error of char
             | Dots | Comma | Left | Right | Eof

  let p lexbuf = lexbuf.lex_curr_p.pos_lnum
}

let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let letter  = ['a'-'z' 'A'-'Z' '_']
let number  = ['0'-'9']
let id      = letter (letter | number)*

rule next_token =
  parse
  | white   {                  next_token lexbuf } (* Skip whitespace *)
  | newline { new_line lexbuf; next_token lexbuf } (* Count lines     *)
  | "ee"    {(p lexbuf, Exchange (E, E))}
  | "es"    {(p lexbuf, Exchange (E, S))}
  | "se"    {(p lexbuf, Exchange (S, E))}
  | "ss"    {(p lexbuf, Exchange (S, S))}
  | "s"     {(p lexbuf, Key S          )}
  | "e"     {(p lexbuf, Key E          )}
  | ','     {(p lexbuf, Comma          )}
  | "<-"    {(p lexbuf, Left           )}
  | "->"    {(p lexbuf, Right          )}
  | "..."   {(p lexbuf, Dots           )}
  | id as i {(p lexbuf, Name i         )}
  | eof     {(p lexbuf, Eof            )}
  | _ as c  {(p lexbuf, Scan_error c   )}

{
  let tokens lexbuf =
    Stream.from (fun _ -> let (pos, token) = next_token lexbuf in
                          match token with
                          | Eof -> None
                          | t   -> Some (pos, t))
}
