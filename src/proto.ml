open Utils

type key      = E | S
type exchange = key * key
type action   = Key         of key
              | Exchange    of exchange
type message  = Client      of action list
              | Server      of action list
type protocol = message list * message list

(* Alternate representation: Specify client/server at the key level *)
type cs_key      = IE | IS | RE | RS
type cs_action   = CS_key      of cs_key
                 | CS_exchange of exchange
type cs_message  = cs_action list
type cs_protocol = cs_message list * cs_message list

let string_of_key = function
  | IS -> "IS"
  | IE -> "IE"
  | RS -> "RS"
  | RE -> "RE"

let string_of_exchange = function
  | (S, S) -> "ss"
  | (S, E) -> "se"
  | (E, S) -> "es"
  | (E, E) -> "ee"

let map_message c s = function
  | Client actions -> c actions
  | Server actions -> s actions

let map_action k e = function
  | Key      key      -> k key
  | Exchange exchange -> e exchange

let map_cs_action k e = function
  | CS_key      key      -> k key
  | CS_exchange exchange -> e exchange

let map_key e s = function
  | E -> e
  | S -> s

let is_client      = map_message   (const true ) (const false)
let is_server      = map_message   (const false) (const true )
let is_key         = map_action    (const true ) (const false)
let is_exchange    = map_action    (const false) (const true )
let is_cs_key      = map_cs_action (const true ) (const false)
let is_cs_exchange = map_cs_action (const false) (const true )
let is_ephemeral   = map_key       true  false
let is_static      = map_key       false true

let to_messages p  = fst p @ snd p
let to_actions     = map_message   id id
let to_key         = map_action    id (f_error "to_key")
let to_exchange    = map_action    (f_error "to_exchange") id
let to_cs_key      = map_cs_action id (f_error "to_key")
let to_cs_exchange = map_cs_action (f_error "to_exchange") id

let get_keys            message  = message  // is_key         /@ to_key
let get_exchanges       message  = message  // is_exchange    /@ to_exchange
let get_cs_keys         message  = message  // is_cs_key      /@ to_cs_key
let get_cs_exchanges    message  = message  // is_cs_exchange /@ to_cs_exchange
let get_client_messages messages = messages // is_client      /@ to_actions
let get_server_messages messages = messages // is_server      /@ to_actions

let cs_message =
  let to_cs_key e s    = map_key (CS_key e) (CS_key s) in
  let to_cs_exchange e = CS_exchange e                 in
  map_message
    (fun a -> a /@ map_action (to_cs_key IE IS) to_cs_exchange)
    (fun a -> a /@ map_action (to_cs_key RE RS) to_cs_exchange)

let cs_protocol p =
  (fst p /@ cs_message,
   snd p /@ cs_message)

let all_keys p =
  to_messages p
  |> List.map (map_message
                 (fun a -> (get_keys a) /@ map_key IE IS)
                 (fun a -> (get_keys a) /@ map_key RE RS))
  |> List.concat

let all_exchanges p =
  to_messages p
  |> List.map    to_actions
  |> List.concat
  |> List.filter is_exchange
  |> List.map    to_exchange

let client_keys p = to_messages p |>get_client_messages |>List.concat |>get_keys
let server_keys p = to_messages p |>get_server_messages |>List.concat |>get_keys


(* By the numbers *)
let rec first_like f = function
  | []     -> 1
  | x :: l -> if f x then 1 else 1 + first_like f l

let fst_exch_like f p = first_like
                          (List.exists f)
                          (snd p /@ to_actions /@ get_exchanges)

let first_exchange = fst_exch_like (const true)
let first_es       = fst_exch_like ((=) (E, S))
let first_se       = fst_exch_like ((=) (S, E))
let first_ee       = fst_exch_like ((=) (E, E))
let first_ss       = fst_exch_like ((=) (S, S))

let to_odd  n = if is_odd  n then n else n + 1
let to_even n = if is_even n then n else n + 1

let first_client_payload = fst_exch_like (fun (e, _) -> e = E) |- to_odd
let first_server_payload = fst_exch_like (fun (_, e) -> e = E) |- to_even
let first_client_auth    = first_se |- to_odd
let first_server_auth    = first_es |- to_even

let nth_message      p n = let messages = snd p in
                           if n > List.length messages || n <= 0
                           then error "nth_message"
                           else List.nth messages (n - 1)
let nth_cs_message   p n = cs_message (nth_message p n)
let nth_message_size p n =
  let nb_keys = List.length (get_keys (to_actions(nth_message p n)))  in
  let nb_tags = if first_exchange p <= n then 16 else 0               in
  nb_keys * 32 + nb_tags
