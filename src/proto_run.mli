(* TODO: maybe those belong to Proto? *)
val shares_key        :  Proto.cs_key -> Proto.protocol -> bool
val has_key           :  Proto.cs_key -> Proto.protocol -> bool
val shares_client_key :  Proto.protocol -> bool
val shares_server_key :  Proto.protocol -> bool
val has_client_key    :  Proto.protocol -> bool
val has_server_key    :  Proto.protocol -> bool

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

val log_actions :
  ('action -> 'a -> 'a) -> (* for each action *)
  'action list -> 'a -> 'a

val act_protocol :
  (action list -> 'a -> 'b) ->  (* Client read  *)
  (action list -> 'b -> 'c) ->  (* Client write *)
  (action list -> 'c -> 'b) ->  (* Server read  *)
  (action list -> 'b -> 'a) ->  (* Server write *)
  Proto.protocol -> 'b -> 'b

val log_protocol :
  (action list -> 'a -> 'a) -> (* client *)
  (action list -> 'a -> 'a) -> (* server *)
  Proto.protocol -> 'a -> 'a
