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
