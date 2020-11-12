open Utils
module P = Proto

type hash = Hash of int       (* lone intermediate hash    *)
          | Htag of int * int (* hash + authentication tag *)
          | Hkey of int * int (* hash + encryption key     *)

type plain = Payload of int (* message number *)
           | Key     of P.cs_key
type crypt = Plain of plain
           | Crypt of plain * int (* encryption key number *)

type mix_input = Hcrypt   of crypt
               | Exchange of P.exchange
               | Prelude
               | No_input

type mix = { next     : hash
           ; prev     : int (* only H<nb> *)
           ; input    : mix_input
           }

type state = { hash_nb  : int  (* current hash    number *)
             ; tag_nb   : int  (* current tag     number *)
             ; key_nb   : int  (* current key     number *)
             ; msg_nb   : int  (* current message number *)
             ; hashes   : mix list
             ; messages : (crypt list * int) list (* message + hash *)
             ; curr_msg : crypt list
             ; has_key  : bool
             }

type log = { initial_hash : int
           ; prelude_hash : int
           ; last_hash    : int
           ; hashes       : mix list
           ; messages     : (crypt list * int) list (* message + hash *)
           }

let next_hash st = Hash (st.hash_nb + 1)
let next_tag  st = Htag (st.hash_nb + 1, st.tag_nb  + 1)
let next_key  st = Hkey (st.hash_nb + 1, st.key_nb  + 1)

let inc_hash  st = { st with hash_nb = st.hash_nb + 1 }
let inc_tag   st = { st with tag_nb  = st.tag_nb  + 1 }
let inc_key   st = { st with key_nb  = st.key_nb  + 1 }
let inc_msg   st = { st with msg_nb  = st.msg_nb  + 1 }

let mix_input_of_crypt st plain = Hcrypt (if st.has_key
                                          then Crypt (plain, st.key_nb)
                                          else Plain plain)

let payload_crypt st = if st.has_key
                       then Crypt (Payload st.msg_nb, st.key_nb)
                       else Plain (Payload st.msg_nb)
let key_crypt   k st = if st.has_key && P.is_static k
                       then Crypt (Key k, st.key_nb)
                       else Plain (Key k)

let payload_input st = Hcrypt (payload_crypt st)
let key_input   k st = Hcrypt (key_crypt   k st)

let hash_line st mix_input hash_output =
  { next  = hash_output
  ; prev  = st.hash_nb
  ; input = mix_input
  }

let add_hash_line line (st:state) = { st with hashes = line :: st.hashes }

let mix_hash input st =
  st
  |> add_hash_line (hash_line st (input st) (next_hash st))
  |> inc_hash

let mix_tag input st =
  if st.has_key
  then st
       |> add_hash_line (hash_line st (input st) (next_tag st))
       |> inc_tag |> inc_hash
  else mix_hash input st

let mix_ekey st =
  if st.has_key
  then st
       |> add_hash_line (hash_line st No_input (next_key st))
       |> inc_key |> inc_hash
  else st

let mix_exchange  e st = st             |> mix_hash (fun _ -> Exchange e)
let mix_prelude     st = st |> mix_ekey |> mix_tag  (fun _ -> Prelude)
let mix_payload     st = st |> mix_ekey |> mix_tag payload_input
let mix_ephemeral k st = st             |> mix_tag (key_input k)
let mix_static    k st = st |> mix_ekey |> mix_tag (key_input k)
let mix_key       k st = if P.is_static k
                         then mix_static    k st
                         else mix_ephemeral k st

let msg_part input st = { st with curr_msg = input st :: st.curr_msg }
let commit_msg     st = { st with messages = (List.rev st.curr_msg, st.hash_nb)
                                             :: st.messages
                                ; curr_msg = []
                                ; msg_nb   = st.msg_nb + 1 }

let add_prelude    st = st |> mix_prelude
let add_payload    st = st |> mix_payload    |> msg_part payload_crypt
let add_key      k st = st |> mix_key k      |> msg_part (key_crypt k)
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
  |> fun st -> let initial_hash = fst (P.cs_protocol p) /@ P.get_cs_keys
                                  |> List.concat |> List.length in
               { initial_hash = initial_hash
               ; prelude_hash = initial_hash + 1
               ; last_hash    = st.hash_nb
               ; hashes       = List.rev st.hashes
               ; messages     = List.rev st.messages
               }
