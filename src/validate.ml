open Proto

type shared = { client_s: bool
              ; client_e: bool
              ; server_s: bool
              ; server_e: bool }
type state  = { shared     : shared
              ; exchanges  : exchange list
              ; last_action: action }

exception Error of string
let error      e = raise (Error e)
let error_if b e = if b then error e

let u_exchange    ex st = { st with exchanges   = ex }
let u_last_action  a st = { st with last_action = a  }
let u_shared      sh st = { st with shared      = sh }

let a_exchange ex = fun st -> u_exchange (ex :: st.exchanges) st
let a_client_s st = u_shared {st.shared with client_s = true} st
let a_client_e st = u_shared {st.shared with client_e = true} st
let a_server_s st = u_shared {st.shared with server_s = true} st
let a_server_e st = u_shared {st.shared with server_e = true} st

let add_exchange ex st = st |> a_exchange ex |> u_last_action (Exchange ex)
let add_client_s    st = st |> a_client_s    |> u_last_action (Key S      )
let add_client_e    st = st |> a_client_e    |> u_last_action (Key E      )
let add_server_s    st = st |> a_server_s    |> u_last_action (Key S      )
let add_server_e    st = st |> a_server_e    |> u_last_action (Key E      )

let action_is_key = function
  | Exchange _ -> false
  | Key      _ -> true

let last_action_was_key st = action_is_key st.last_action

let last_action_was_encrypted_key st =
  last_action_was_key st && st.exchanges <> []

let check_can_send_key st = match st.last_action with
  | Exchange _ -> ()
  | Key      _ -> error_if (st.exchanges <> [])
                    ("Two encrypted keys in a row.  "
                     ^ "Interleave an exchange first.")

let string_of_exchange : exchange -> string = function
  | (S, S) -> "ss"
  | (S, E) -> "se"
  | (E, S) -> "es"
  | (E, E) -> "ee"

let can_exchange : exchange -> state -> bool = fun ex st ->
  match ex with
  | (S, S) -> st.shared.client_s && st.shared.server_s
  | (S, E) -> st.shared.client_s && st.shared.server_e
  | (E, S) -> st.shared.client_e && st.shared.server_s
  | (E, E) -> st.shared.client_e && st.shared.server_e

let send : key -> state -> state = fun key st ->
  check_can_send_key st;
  match key with
  | S -> error_if st.shared.client_s "Initiator static key is sent twice.";
         add_client_s st
  | E -> error_if st.shared.client_e "Initiator ephemeral key is sent twice.";
         add_client_e st

let reply : key -> state -> state = fun key st ->
  check_can_send_key st;
  match key with
  | S -> error_if st.shared.server_s "Respondent static key is sent twice.";
         add_server_s st
  | E -> error_if st.shared.server_e "Respondent ephemeral key is sent twice.";
         add_server_e st

let exchange : exchange -> state -> state = fun ex st ->
  error_if (List.mem ex st.exchanges) ("Exchange '" ^ string_of_exchange ex
                                       ^ "' is performed twice.");
  error_if (not (can_exchange ex st)) ("Exchange '" ^ string_of_exchange ex
                                       ^ "' is impossible.");
  add_exchange ex st

let message : state -> message -> state = fun st message ->
  let (send_or_reply, actions) = match message with
    | Client actions -> (send , actions)
    | Server actions -> (reply, actions) in
  let act st = function
    | Key      k -> send_or_reply k st
    | Exchange e -> exchange      e st   in
  error_if (not (List.exists action_is_key actions)) "Message sends no key.";
  List.fold_left act st actions

let messages : message list -> state -> state = fun messages st ->
  List.fold_left message st messages

let check_alternation : message list -> unit =
  let alternation msg1 msg2 = match (msg1, msg2) with
    | Client _, Server m -> Server m
    | Server _, Client m -> Client m
    | _                  -> error ("Messages must alternate directions, " ^
                                     "initiator first")
  in fun messages ->
     ignore (List.fold_left alternation (Server []) messages)

let run_protocol : protocol -> unit = fun (pre, run) ->
  let init = { shared      = { client_s = false
                             ; client_e = false
                             ; server_s = false
                             ; server_e = false }
             ; exchanges   = []
             ; last_action = Exchange (E, E) } in (* dummy value *)
  let st = messages pre init                   in
  error_if (st.shared.client_e ||
              st.shared.server_e)      "Pre-shared ephemeral key";
  error_if (st.exchanges <> [])        "Key exchange performed in advance.";
  error_if (run = [])                  "Protocol has no message!";
  check_alternation run;
  let final = messages run st                in
  error_if (last_action_was_key final) "Protocol doesn't end by a key exchange."

(* TODO: verify that every key is eventually used. Note that it would
   mask the "must end by a key exchange" error.
   TODO: decide what to do with respect to ephemeral keys:
   - Must the initiator always send an ephemeral?
   - Must the respondent include an ephemeral in its replies?
   - More generally, does sending messages means we have to produce
     an ephemeral?
   - While we're at it, must the ephemeral be sent before anything else?
 *)

type t = Valid
       | Broken of string

let v : protocol -> t = fun protocol ->
  try run_protocol protocol; Valid with
    Error e -> Broken e
