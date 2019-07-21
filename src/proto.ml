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
let is_ephemeral   = function IE | RE -> true  | IS | RS -> false
let is_static      = function IE | RE -> false | IS | RS -> true

let to_messages p  = fst p @ snd p
let to_actions     = map_message   id id
let to_key         = map_action    id (f_error "to_key")
let to_exchange    = map_action    (f_error "to_exchange") id
let to_cs_key      = map_cs_action id (f_error "to_cs_key")
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

let contains k e = match k with
  | IE -> fst e = E
  | IS -> fst e = S
  | RE -> snd e = E
  | RS -> snd e = S

let first_auth     p     = fst_exch_like (const true) p
let first_exchange p e   = fst_exch_like ((=) e     ) p
let first_used_key p k   = fst_exch_like (contains k) p
let uses_exchange  p e n = first_exchange p e <= n
let uses_key       p k n = first_used_key p k <= n

let to_odd  n = if is_odd  n then n else n + 1
let to_even n = if is_even n then n else n + 1

let first_client_payload p = first_used_key p IE |> to_odd
let first_server_payload p = first_used_key p RE |> to_even
let first_payload        p = min
                               (first_client_payload p)
                               (first_server_payload p)

let nb_messages      p   = List.length (snd p)
let nth_message      p n = if  n <= 0 || n > nb_messages p
                           then error "nth_message"
                           else List.nth (snd p) (n - 1)
let nth_cs_message   p n = cs_message (nth_message p n)
let nth_message_size p n =
  let actions   = to_actions (nth_message p n)                            in
  let nb_before = actions |> take_while is_key |> get_keys |> List.length in
  let nb_after  = actions |> drop_while is_key |> get_keys |> List.length in
  let nb_keys   = nb_before + nb_after                                    in
  let nb_tags   = if      first_auth p < n then 1 + nb_keys
                  else if first_auth p = n then 1 + nb_after
                  else                          0                         in
  nb_keys*32 + nb_tags*16

let is_amplified p n =
  let is_client_amplified = is_odd  n && not (uses_key p RE n) in
  let is_server_amplified = is_even n && not (uses_key p IE n) in
  let is_not_last         = n < nb_messages p                  in
  is_not_last && (is_client_amplified || is_server_amplified)
