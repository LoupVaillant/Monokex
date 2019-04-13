open Utils
module P = Proto

(* Utilities *)
let rec duplicates = function
  | []     -> []
  | x :: l -> if List.mem x l
              then x :: duplicates (l // (<>) x)
              else duplicates l

let rec remove_duplicates = function
  | []     -> []
  | x :: l -> let no_dups = remove_duplicates l in
              if List.mem x no_dups
              then no_dups
              else x :: no_dups

let rec take_until f = function
  | []     -> []
  | x :: l -> if f x then x :: take_until f l else []

let rec drop_until f = function
  | []     -> []
  | x :: l -> if f x then l else drop_until f l

let rec runs_of f = function
  | [] -> []
  | l  -> let next = drop_until (f |- not) l in
          let run  = take_until f next       in
          let rest = drop_until f next       in
          run :: runs_of f rest

let pre_actions  p = List.concat (fst (P.cs_protocol p))
let post_actions p = List.concat (snd (P.cs_protocol p))
let all_actions  p = pre_actions p @ post_actions p

(* Verifications *)
let message_order p =
  let rec aux = function
    | []                        -> []
    | [_]                       -> []
    | P.Client _::P.Server s::l -> aux (P.Server s :: l)
    | P.Server _::P.Client c::l -> aux (P.Client c :: l)
    | P.Client _::P.Client _::l -> "Two initiator messages in a row."  :: aux l
    | P.Server _::P.Server _::l -> "Two respondent messages in a row." :: aux l
  in
  match snd p with
  | []              -> ["There is no message!"]
  | P.Server _ :: l -> "First message come from the respondent." :: aux l
  | l               -> aux l

let pre_shared_ephemeral p =
  let keys = P.get_cs_keys (pre_actions p) in
  let is   = if List.mem P.IE keys then ["Pre-shared initiator ephemeral key."]
             else []                       in
  let rs   = if List.mem P.RE keys then ["Pre-shared respondent ephemeral key."]
             else []                       in
  is @ rs

let pre_done_key_exchange p =
  P.get_cs_exchanges (pre_actions p)
  /@ P.string_of_exchange
  /@ (fun e -> "Exchange " ^ e ^ " happens before protocol start.")

let impossible_exchanges p =
  all_actions p
  |> List.fold_left
       (fun (keys, pairs) -> function
         | P.CS_key      k -> ((k :: keys), pairs)
         | P.CS_exchange e -> (keys, (keys, e) :: pairs))
       ([], [])
  |> snd
  |> List.filter
       (fun (keys, (ik, rk)) ->
         let i = match ik with
           | P.E -> List.mem P.IE keys
           | P.S -> List.mem P.IS keys in
         let r = match rk with
           | P.E -> List.mem P.RE keys
           | P.S -> List.mem P.RS keys in
         not (i && r))
  |> List.map snd
  |> List.map (fun e -> "The exchange " ^ P.string_of_exchange e
                        ^ " cannot be performed.")

let duplicate_exchanges p =
  duplicates (P.all_exchanges p)
  /@ (fun x -> "The exchange " ^ P.string_of_exchange x
               ^ " appears more than once.")

let duplicate_keys p =
  duplicates (P.all_keys p)
  /@ (fun x -> "The key " ^ P.string_of_key x ^ " appears more than once.")

let two_keys_in_a_row p =
  all_actions p
  |> drop_until P.is_cs_exchange
  |> runs_of    P.is_cs_key
  |> List.filter (fun run -> List.length run > 1)
  |> (function
      |[] -> []
      | _  -> [paragraph "Unimplemented feature: two encrypted keys in a row."])

let unused_key p =
  let keys      = P.all_keys      p in
  let exchanges = P.all_exchanges p in
  keys // ((function
            | P.IE -> List.exists (fun e -> fst e = P.E) exchanges
            | P.IS -> List.exists (fun e -> fst e = P.S) exchanges
            | P.RE -> List.exists (fun e -> snd e = P.E) exchanges
            | P.RS -> List.exists (fun e -> snd e = P.S) exchanges)
           |- not)
  /@ P.string_of_key
  /@ (fun k -> "Key " ^ k ^ " is unused.")

let must_use_ephemeral p =
  let kci n used unused =
    let sender = if is_odd n then "Initiator" else "Respondent" in
    if P.uses_exchange p used n && not (P.uses_exchange p unused n)
    then [sender ^ " sends payload with " ^ P.string_of_exchange used
          ^ " and without "               ^ P.string_of_exchange unused ^ "."]
    else []                                                      in
  let iss n = kci n (P.S, P.S) (P.E, P.S)                        in
  let ise n = kci n (P.S, P.E) (P.E, P.E)                        in
  let rss n = kci n (P.S, P.S) (P.S, P.E)                        in
  let res n = kci n (P.E, P.S) (P.E, P.E)                        in
  let client_messages = range 1 (List.length (snd p)) // is_odd  in
  let server_messages = range 1 (List.length (snd p)) // is_even in
  List.concat (client_messages   /@ iss
               @ client_messages /@ ise
               @ server_messages /@ rss
               @ server_messages /@ res)

let v p =
  let simple_errors =
            (message_order           p
             @ pre_shared_ephemeral  p
             @ pre_done_key_exchange p
             @ impossible_exchanges  p
             @ duplicate_exchanges   p
             @ duplicate_keys        p
             @ two_keys_in_a_row     p
             @ unused_key            p)    in
  let subtle_errors = must_use_ephemeral p in
  remove_duplicates (if simple_errors = []
                     then subtle_errors
                     else simple_errors)
