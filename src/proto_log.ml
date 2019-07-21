open Utils
module P = Proto

type hash = Hash of int (* intermediate hash  *)
          | Htag of int (* authentication tag *)
          | Hkey of int (* encryption key     *)

type raw_input = Prelude
               | Payload  of int (* message number *)
               | Key      of P.cs_key
               | Exchange of P.exchange

type input = Iraw of       raw_input
           | Ienc of int * raw_input (* key number, input *)
           | Itag of int             (* tag number *)
           | Zero
           | One

type mix = hash * int * input

type state = { hash_nb  : int  (* current hash    number *)
             ; tag_nb   : int  (* current tag     number *)
             ; key_nb   : int  (* current key     number *)
             ; msg_nb   : int  (* current message number *)
             ; hashes   : mix list
             ; messages : input list list
             ; curr_msg : input list
             ; has_key  : bool
             }

type log = { last_hash : int
           ; hashes    : mix list
           ; messages  : input list list
           }

let inc_hash  st = { st with hash_nb = st.hash_nb + 1 }
let inc_tag   st = { st with tag_nb  = st.tag_nb  + 1 }
let inc_key   st = { st with key_nb  = st.key_nb  + 1 }
let inc_msg   st = { st with msg_nb  = st.msg_nb  + 1 }

let input_of_raw st raw = if st.has_key
                          then Ienc (st.key_nb, raw)
                          else Iraw raw

let add_hash hash_line (st:state) = { st with hashes  = hash_line :: st.hashes }

let mix_hash input st =
  st
  |> add_hash (Hash (st.hash_nb + 1), st.hash_nb, input st)
  |> inc_hash

let prelude_input    st = Iraw Prelude
let payload_input    st = input_of_raw st (Payload st.msg_nb)
let exchange_input e st = Iraw (Exchange e)
let key_input      k st = if P.is_ephemeral k
                          then Iraw            (Key k)
                          else input_of_raw st (Key k)

let mix_tag st =
  if st.has_key
  then st
       |> add_hash (Hash (st.hash_nb + 1), st.hash_nb, Zero)
       |> add_hash (Htag (st.tag_nb  + 1), st.hash_nb, One)
       |> inc_tag |> inc_hash
  else st
let mix_ekey st =
  if st.has_key
  then st
       |> add_hash (Hash (st.hash_nb + 1), st.hash_nb, Zero)
       |> add_hash (Hkey (st.key_nb  + 1), st.hash_nb, One)
       |> inc_key |> inc_hash
  else st

let mix_prelude    st = st |> mix_ekey |> mix_hash (prelude_input   )
let mix_payload    st = st |> mix_ekey |> mix_hash (payload_input   )
let mix_key      k st = st |> mix_ekey |> mix_hash (key_input      k)
let mix_exchange e st = st             |> mix_hash (exchange_input e)

let msg_bit input st = { st with curr_msg = input st :: st.curr_msg }
let commit_msg    st = { st with messages = List.rev st.curr_msg :: st.messages
                               ; curr_msg = []
                               ; msg_nb   = st.msg_nb + 1 }

let add_tag        st = if st.has_key
                        then st |> mix_tag |> msg_bit (fun st -> Itag st.tag_nb)
                        else st

let add_prelude    st = st |> mix_prelude                             |> add_tag
let add_payload    st = st |> mix_payload    |> msg_bit payload_input |> add_tag
let add_key      k st = st |> mix_key k      |> msg_bit (key_input k) |> add_tag
let add_exchange e st = st |> mix_exchange e |> fun st->{st with has_key = true}

let add_pre_message st msg =
  List.fold_left (fun st action -> P.map_cs_action
                                     (swap mix_key st)
                                     (f_error "add_pre_message")
                                     action)
    st msg

let add_message st msg =
  List.fold_left (fun st action -> P.map_cs_action
                                     (swap add_key      st)
                                     (swap add_exchange st)
                                     action)
    st msg
  |> add_payload
  |> commit_msg

let log_of_protocol p =
  let pre_messages = p |> P.cs_protocol |> fst in
  let messages     = p |> P.cs_protocol |> snd in
  { hash_nb  = 0
  ; tag_nb   = 0
  ; key_nb   = 0
  ; msg_nb   = 1
  ; hashes   = []
  ; messages = []
  ; curr_msg = []
  ; has_key  = false
  }
  |> swap (List.fold_left add_pre_message) pre_messages
  |> add_prelude
  |> swap (List.fold_left add_message) messages
  |> fun st -> { last_hash = st.hash_nb
               ; hashes    = List.rev st.hashes
               ; messages  = List.rev st.messages
               }
