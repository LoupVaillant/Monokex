type hash = Hash of int       (* lone intermediate hash    *)
          | Htag of int * int (* hash + authentication tag *)
          | Hkey of int * int (* hash + encryption key     *)

type plain = Payload of int (* message number *)
           | Key     of Proto.cs_key
type crypt = Plain of plain
           | Crypt of plain * int (* encryption key number *)

type mix_input = Hcrypt   of crypt
               | Exchange of Proto.exchange
               | Prelude
               | No_input

type mix = { next     : hash
           ; prev     : int  (* only H<nb> *)
           ; input    : mix_input
           }

type log = { initial_hash : int
           ; prelude_hash : int
           ; last_hash    : int
           ; hashes       : mix list
           ; messages     : (crypt list * int) list (* message + hash *)
           }

val log_of_protocol : Proto.protocol -> log
