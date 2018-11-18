open Utils
open Proto

type error    = Ok
              | Impossible
              | Dup_key
              | Dup_exchange
              | Dup_encrypt
              | Shared_ephemeral

(* Report the first error that occurs *)
let (&&&) a b = match a with
  | Ok  -> Lazy.force b
  | err -> err

(* Actions that happen during a protocol *)
let send : state -> key -> state = fun (shared, actions) ->
  function
  | E -> ({ shared with client_e = true; }, Key E :: actions)
  | S -> ({ shared with client_s = true; }, Key S :: actions)

let reply : state -> key -> state = fun (shared, actions) ->
  function
  | E -> ({ shared with server_e = true; }, Key E :: actions)
  | S -> ({ shared with server_s = true; }, Key S :: actions)

let exchange : state -> exchange -> state
  = fun (shared, actions) exchange-> (shared, Exchange exchange :: actions)

(* Validity of the actions *)
let check : bool -> error -> error = fun b e -> if b then e else Ok

let can_encrypt : state -> bool = fun (shared, actions) ->
  List.for_all (function Exchange _ -> false | Key _ -> true) actions
  || match actions with
     | []              -> true
     | Key      _ :: _ -> false
     | Exchange _ :: _ -> true

let client_keys (shared, _) = (shared.client_e, shared.client_s)
let server_keys (shared, _) = (shared.server_e, shared.server_s)

let can_transmit : (state -> bool * bool) -> state -> key -> error
  = fun get_keys state key ->
  ( match key with
    | E -> check (fst (get_keys state)) Dup_key
    | S -> check (snd (get_keys state)) Dup_key
  )
  &&& lazy (check (can_encrypt state) Dup_encrypt)

let can_send  : state -> key -> error = can_transmit client_keys
let can_reply : state -> key -> error = can_transmit server_keys

let can_exchange : state -> exchange -> error
  = fun (shared, actions) exchange ->
  if (List.mem (Exchange exchange) actions)
  then Dup_exchange
  else match exchange with
       | (E, E) -> check (shared.client_e && shared.server_e) Impossible
       | (E, S) -> check (shared.client_e && shared.server_s) Impossible
       | (S, E) -> check (shared.client_s && shared.server_e) Impossible
       | (S, S) -> check (shared.client_s && shared.server_s) Impossible

let valid_message : (state -> key -> error) ->
                    (state -> key -> state) ->
                    state -> message -> error
  = fun can_message message ->
  let rec aux st =
  function
  | []                -> Ok
  | Key      k :: msg -> can_message  st k &&& lazy (aux (message  st k) msg)
  | Exchange x :: msg -> can_exchange st x &&& lazy (aux (exchange st x) msg)
  in aux

let valid_send  : state -> message -> error = valid_message can_send  send
let valid_reply : state -> message -> error = valid_message can_reply reply

let rec valid_sends : state -> message list -> error
  = fun state -> function
              | []                  -> Ok
              | message :: messages -> valid_send state message &&&
                                         lazy (valid_replies state messages)
and valid_replies : state -> message list -> error
  = fun state -> function
              | []                  -> Ok
              | message :: messages -> valid_reply state message &&&
                                         lazy (valid_sends state messages)

let valid_protocol : shared -> message list -> error
  = fun shared messages ->
  check (not shared.client_e && not shared.server_e) Shared_ephemeral &&&
    lazy (valid_sends (shared, []) messages)



