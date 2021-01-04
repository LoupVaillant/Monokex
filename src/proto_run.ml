open Utils
module P = Proto

(* TODO: maybe those two belong to Proto? *)
let shares_key key p = List.mem key (P.shared_keys p)
let has_key    key p = List.mem key (P.all_keys p)
let shares_client_key p = shares_key P.IS p
let shares_server_key p = shares_key P.RS p
let has_client_key    p = has_key    P.IS p
let has_server_key    p = has_key    P.RS p

type action = E   (* Ephemeral         *)
            | S   (* Static            *)
            | EE  (* ee exchange       *)
            | ES  (* es exchange       *)
            | SE  (* se exchange       *)
            | SS  (* ss exchange       *)
            | Pa  (* payload           *)
            | H0  (* first hash (init) *)
            | IS  (* client key (init) *)
            | RS  (* server key (init) *)
            | Pr  (* prelude    (init) *)

let convert_action = function
  | P.Key P.E             -> E
  | P.Key P.S             -> S
  | P.Exchange (P.E, P.E) -> EE
  | P.Exchange (P.E, P.S) -> ES
  | P.Exchange (P.S, P.E) -> SE
  | P.Exchange (P.S, P.S) -> SS

let convert_message = function
  | P.Client actions -> (actions /@ convert_action) @ [Pa]
  | P.Server actions -> (actions /@ convert_action) @ [Pa]

let convert_init_action = function
  | P.Client [P.Key P.S] -> IS
  | P.Server [P.Key P.S] -> RS
  | _                    -> error "Invalid init action"

let convert_protocol p =
  let init_actions = fst p /@ convert_init_action in
  let msg_actions  = snd p /@ convert_message     in
  match msg_actions with
  | msg :: msgs -> ((H0 :: init_actions) @ (Pr :: msg)) :: msgs
  | []          -> error "No message"

let log_actions log actions =
  swap (List.fold_left (swap log)) actions

let act_protocol cr cw sr sw =
  let flip message =
    message /@ (function
                | EE     -> EE
                | ES     -> SE
                | SE     -> ES
                | SS     -> SS
                | action -> action)
  in
  let rec act_client protocol st = match protocol with
    | []          -> st
    | msg :: msgs -> st |> cw msg |> sr (flip msg) |> act_server msgs
  and act_server protocol st = match protocol with
    | []          -> st
    | msg :: msgs -> st |> sw (flip msg) |> cr msg |> act_client msgs
  in
  fun protocol st -> act_client (convert_protocol protocol) st

let log_protocol client server protocol state =
  act_protocol
    client (fun _ -> id)
    server (fun _ -> id)
    protocol state
