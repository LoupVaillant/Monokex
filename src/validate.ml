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

let rec take_while f = function
  | []     -> []
  | x :: l -> if f x then x :: take_while f l else []

let rec drop_while f = function
  | []     -> []
  | x :: l -> if f x then drop_while f l else x :: l

let rec runs_of f l =
  match drop_while (f |- not) l with
  | []   -> []
  | next -> let run  = take_while f next in
            let rest = drop_while f next in
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
  | P.Server _ :: l -> "First message comes from the respondent." :: aux l
  | l               -> aux l

let duplicate_exchanges p =
  duplicates (P.all_exchanges p)
  /@ (fun x -> "The exchange " ^ P.string_of_exchange x
               ^ " appears more than once.")

let duplicate_keys p =
  duplicates (P.all_keys p)
  /@ (fun x -> "The key " ^ P.string_of_key x ^ " appears more than once.")

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

let pre_shared_ephemeral p =
  let keys = P.get_cs_keys (pre_actions p) in
  if List.mem P.IE keys || List.mem P.RE keys
  then ["Monokex does not support pre-shared ephemeral keys."]
  else []

let pre_done_key_exchange p =
  P.get_cs_exchanges (pre_actions p)
  /@ P.string_of_exchange
  /@ (fun e -> "Exchange " ^ e ^ " happens before protocol start.")

let sender_key_first p =
  match fst (P.cs_protocol p) /@ P.get_cs_keys |> List.concat with
  | [P.RS; P.IS] ->["Monokex does not support pre-sharing respondent key first"]
  | _            -> []

(* TODO: tell the user exactly why the exchange cannot happen:
 * We should tell which key is either missing or happens too late.
 *)
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

let must_use_ephemeral p =
  let kci n used unused =
    let sender = if is_odd n then "Initiator" else "Respondent" in
    if P.uses_exchange p used n && not (P.uses_exchange p unused n)
    then [sender ^ " sends payload with " ^ P.string_of_exchange used
          ^ " and without "               ^ P.string_of_exchange unused ^ "."]
    else []                                                  in
  let iss n = kci n (P.S, P.S) (P.E, P.S)                    in
  let ise n = kci n (P.S, P.E) (P.E, P.E)                    in
  let rss n = kci n (P.S, P.S) (P.S, P.E)                    in
  let res n = kci n (P.E, P.S) (P.E, P.E)                    in
  let client_messages = range 1 (P.nb_messages p) // is_odd  in
  let server_messages = range 1 (P.nb_messages p) // is_even in
  List.concat (client_messages   /@ iss
               @ client_messages /@ ise
               @ server_messages /@ rss
               @ server_messages /@ res)

(* Aggregate all verifications         *)
(* Several error messages may be given *)
let v p =
  (* Select the first batch of errors, if any *)
  let first_errors e =
    match e /@ List.concat |> drop_while ((=) []) with
    | []          -> []
    | errors :: _ -> errors
  in
  remove_duplicates
    (first_errors
       [ [ message_order         p
         ; duplicate_exchanges   p
         ; duplicate_keys        p
         ; unused_key            p
         ]
       ; [ pre_shared_ephemeral  p
         ; pre_done_key_exchange p
         ]
       ; [ sender_key_first      p
         ; impossible_exchanges  p
         ; must_use_ephemeral    p
         ]
       ]
    )
