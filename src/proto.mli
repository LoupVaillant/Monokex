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

val string_of_key      : cs_key   -> string
val string_of_exchange : exchange -> string

val map_message  : (action list -> 'a) -> (action list -> 'a) -> message   -> 'a
val map_action   : (key         -> 'a) -> (exchange    -> 'a) -> action    -> 'a
val map_cs_action: (cs_key      -> 'a) -> (exchange    -> 'a) -> cs_action -> 'a
val map_key      : 'a                  -> 'a                  -> key       -> 'a

val is_client     : message   -> bool
val is_server     : message   -> bool
val is_key        : action    -> bool
val is_exchange   : action    -> bool
val is_cs_key     : cs_action -> bool
val is_cs_exchange: cs_action -> bool
val is_ephemeral  : key       -> bool
val is_static     : key       -> bool

val to_messages   : protocol  -> message list
val to_actions    : message   -> action list
val to_key        : action    -> key      (* Fails if action is an exchange *)
val to_exchange   : action    -> exchange (* Fails if action is a key       *)
val to_cs_key     : cs_action -> cs_key   (* Fails if action is an exchange *)
val to_cs_exchange: cs_action -> exchange (* Fails if action is a key       *)

val get_keys            : action    list -> key      list
val get_exchanges       : action    list -> exchange list
val get_cs_keys         : cs_action list -> cs_key   list
val get_cs_exchanges    : cs_action list -> exchange list
val get_client_messages : message   list -> action list list
val get_server_messages : message   list -> action list list

val cs_message : message  -> cs_message
val cs_protocol: protocol -> cs_protocol

val all_keys     : protocol -> cs_key   list
val all_exchanges: protocol -> exchange list

val client_keys  : protocol -> key list
val server_keys  : protocol -> key list

(* By the numbers *)
val first_auth     : protocol -> int
val first_used_key : protocol -> cs_key -> int
val first_exchange : protocol -> exchange -> int

val uses_exchange : protocol -> exchange -> int -> bool
val uses_key      : protocol -> cs_key   -> int -> bool

val first_client_payload : protocol -> int
val first_server_payload : protocol -> int
val first_client_auth    : protocol -> int
val first_server_auth    : protocol -> int

val nb_messages      : protocol -> int
val nth_message      : protocol -> int -> message
val nth_cs_message   : protocol -> int -> cs_message
val nth_message_size : protocol -> int -> int
