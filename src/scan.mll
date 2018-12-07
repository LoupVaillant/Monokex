{
  open Lexing

  type token = Key        of Proto.key
             | Exchange   of Proto.exchange
             | Name       of string
             | Scan_error of char
             | Column | Dots | Comma | Left | Right | Eof

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
  | "ee"    {(p lexbuf, Exchange (Proto.E, Proto.E))}
  | "es"    {(p lexbuf, Exchange (Proto.E, Proto.S))}
  | "se"    {(p lexbuf, Exchange (Proto.S, Proto.E))}
  | "ss"    {(p lexbuf, Exchange (Proto.S, Proto.S))}
  | "s"     {(p lexbuf, Key Proto.S                )}
  | "e"     {(p lexbuf, Key Proto.E                )}
  | ':'     {(p lexbuf, Column                     )}
  | "..."   {(p lexbuf, Dots                       )}
  | ','     {(p lexbuf, Comma                      )}
  | "<-"    {(p lexbuf, Left                       )}
  | "->"    {(p lexbuf, Right                      )}
  | id as i {(p lexbuf, Name i                     )}
  | eof     {(p lexbuf, Eof                        )}
  | _ as c  {(p lexbuf, Scan_error c               )}

{
  let tokens lexbuf =
    Stream.from (fun _ -> let (pos, token) = next_token lexbuf in
                          match token with
                          | Eof -> None
                          | t   -> Some (pos, t))
}
