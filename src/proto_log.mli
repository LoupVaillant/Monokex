type hash = Hash of int (* intermediate hash  *)
          | Htag of int (* authentication tag *)
          | Hkey of int (* encryption key     *)

type raw_input = Prelude
               | Payload  of int (* message number *)
               | Key      of Proto.cs_key
               | Exchange of Proto.exchange

type input = Iraw of       raw_input
           | Ienc of int * raw_input (* key number, input *)
           | Itag of int             (* tag number *)
           | Zero
           | One

type mix = hash * int * input

type log = { last_hash : int
           ; hashes    : mix list
           ; messages  : input list list
           }

val log_of_protocol : Proto.protocol -> log
