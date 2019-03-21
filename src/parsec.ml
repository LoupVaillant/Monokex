open Utils
open Scan

type 'a psr = (int * token) Stream.t -> 'a

let current_line : (int * token) Stream.t -> string = fun str ->
  match Stream.peek str with
  | None           -> "end of input"
  | Some (line, _) -> "line " ^ string_of_int line

(* General combinators *)
let (<$>) : ('a -> 'b)     -> 'a psr -> 'b psr = fun f psr str -> f (psr str)
let (<*>) : ('a -> 'b) psr -> 'a psr -> 'b psr = fun f x str ->
  let f = f str in (* Order of evaluation matters, *)
  let r = x str in (* because of the stream.       *)
  f r

let (<* ) : 'a psr -> 'b psr -> 'a psr = fun x y ->      const <$> x <*> y
let ( *>) : 'a psr -> 'b psr -> 'b psr = fun x y -> swap const <$> x <*> y
let (<$ ) : 'a     -> 'b psr -> 'a psr = fun x p ->          const x <$> p

let pure : 'a -> 'a psr = fun x str -> x

let (<|>) : 'a psr -> 'a psr -> 'a psr = fun primary backup str ->
  try primary str with
    Stream.Failure -> backup str

let (<?>) : 'a psr -> string -> 'a psr = fun psr expectation str ->
  try psr str with Stream.Failure ->
    raise (Stream.Error ("Parse error, " ^ current_line str
                         ^ ": expected " ^ expectation))

let rec many : 'a psr -> 'a list psr = fun psr ->
  fun str -> str |> (* eta expansion works around eager evaluation *)
               ((cons <$> psr <*> many psr) <|> pure [])

let sep_by item sep error = cons <$> item <*> many (sep *> (item <?> error))

(* Base parsers *)
let token_if : (token -> bool) -> token psr = fun p str ->
  match Stream.peek str with
  | None               -> raise Stream.Failure
  | Some (line, token) ->
     match token with
     | Scan_error c -> raise (Stream.Error ("Parse error, " ^ current_line str
                                            ^ ": illegal character : '"
                                            ^ String.make 1 c ^ "'"))
     | token        -> if p token
                       then (Stream.junk str; token)
                       else raise Stream.Failure

let token : token -> unit psr = fun t -> () <$ token_if ((=) t)

(* Grammar *)
let is_key      = function Scan.Key      _ -> true | _ -> false
let is_exchange = function Scan.Exchange _ -> true | _ -> false
let is_name     = function Scan.Name     _ -> true | _ -> false
let to_name     = function Scan.Name n -> n | _-> error "to_name"
let to_action   = function Scan.Key      k -> Proto.Key      k
                         | Scan.Exchange e -> Proto.Exchange e
                         | _               -> error "to_action"

let key            = to_action <$> token_if is_key
let exchange       = to_action <$> token_if is_exchange
let name           = to_name   <$> token_if is_name
let action         = key <|> exchange
let message        = sep_by action (token Comma) "comma separated actions"
let client_message = (fun m -> Proto.Client m) <$> token Right *> message
let server_message = (fun m -> Proto.Server m) <$> token Left  *> message
let any_message    = client_message <|> server_message
let protocol_name  = name <* (token Column <?> "column")
let protocol       = (fun pre -> function
                       | []   -> ([] , pre )
                       | post -> (pre, post))
                     <$> many any_message
                     <*> (token Dots *> (many any_message)
                          <|> pure [])
let protocols      = many (pair <$> protocol_name <*> protocol)
