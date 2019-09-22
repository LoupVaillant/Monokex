type hash = Hash of int (* intermediate hash  *)
          | Htag of int (* authentication tag *)
          | Hkey of int (* encryption key     *)

type plain = Payload of int (* message number *)
           | Key     of Proto.cs_key
type crypt = Plain of plain
           | Crypt of plain * int (* encryption key number *)

type mix_input = Hcrypt   of crypt
               | Exchange of Proto.exchange
               | Prelude
               | Zero
               | One

type mix = { next     : hash
           ; prev     : int        (* only H<nb> *)
           ; input    : mix_input
           ; fallback : int option (* only H<nb> *)
           }

type msg_part = Mcrypt of crypt
              | Mtag   of int

type log = { initial_hash : int
           ; prelude_hash : int
           ; last_hash    : int
           ; hashes       : mix list
           ; messages     : (msg_part list * int) list (* message + hash *)
           }

val log_of_protocol : Proto.protocol -> log
