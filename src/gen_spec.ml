open Utils
module P = Proto
module L = Proto_log
module R = Proto_run

type fsm = { hash    : int
           ; has_key : bool
           ; key     : int
           ; tag     : int
           }
let fsm_0 = { hash    = 0
            ; has_key = false
            ; key     = 1
            ; tag     = 1
            }

let inc_hash i fsm = { fsm with hash    = fsm.hash + i }
let inc_key    fsm = { fsm with key     = fsm.key  + 1 }
let inc_tag    fsm = { fsm with tag     = fsm.tag  + 1 }
let get_key    fsm = { fsm with has_key = true         }
let up_kex fsm = fsm |> inc_hash 1 |> get_key
let up_raw fsm = fsm |> inc_hash 1
let up_enc fsm = fsm |> inc_hash 2 |> inc_key |> inc_tag
let up_pld fsm = if fsm.has_key then up_enc fsm else up_raw fsm

let fsm_hash fsm = "H" ^ string_of_int fsm.hash
let fsm_key  fsm = "K" ^ string_of_int fsm.key
let fsm_tag  fsm = "T" ^ string_of_int fsm.tag

let update action fsm = match action with
  | R.E  -> up_raw fsm
  | R.S  -> up_pld fsm
  | R.EE -> up_kex fsm
  | R.ES -> up_kex fsm
  | R.SE -> up_kex fsm
  | R.SS -> up_kex fsm
  | R.Pa -> up_pld fsm
  | R.H0 -> fsm
  | R.IS -> up_raw fsm
  | R.RS -> up_raw fsm
  | R.Pr -> up_raw fsm

type cs = Client | Server

let send_action pattern cs payload action (fsm, desc, msg) =
  let out_fsm   = update action fsm                                       in
  let ls        = match cs with Client -> "i" | Server -> "r"             in
  let lp        = match cs with Client -> "I" | Server -> "R"             in
  let rp        = match cs with Client -> "R" | Server -> "I"             in
  let h0        = "H" ^ lp ^ string_of_int  out_fsm.hash                  in
  let h1        = "H" ^ lp ^ string_of_int (out_fsm.hash - 1)             in
  let h2        = "H" ^ lp ^ string_of_int (out_fsm.hash - 2)             in
  let k         = "K" ^ lp ^ string_of_int out_fsm.key                    in
  let t         = "T" ^ lp ^ string_of_int out_fsm.tag                    in
  let ep        = "E" ^ lp                                                in
  let es        = "e" ^ ls                                                in
  let er        = "E" ^ rp                                                in
  let sp        = "S" ^ lp                                                in
  let ss        = "s" ^ ls                                                in
  let sr        = "S" ^ rp                                                in
  let rkdf    p = ["    "  ^ h0          ; " = KDF("^ h1 ^", "^   p ^")"] in
  let ekdf1     = ["    "  ^ h1  ^", "^ k; " = ENC("^ h2 ^", "^  "Zero)"] in
  let ekdf2   p = ["    E_"^ p           ; " = ENC("^ k  ^", "^   p ^")"] in
  let ekdf3   p = ["    "  ^ h0  ^", "^ t; " = KDF("^ h1 ^", E_"^ p ^")"] in
  let raw_kdf p = rkdf p :: desc                                          in
  let enc_kdf p = if out_fsm.has_key
                   then ekdf3 p :: ekdf2 p :: ekdf1 :: desc
                   else rkdf p                      :: desc               in
  let raw_pld  p = p :: msg                                               in
  let enc_pld  p = (if out_fsm.has_key
                    then "E_" ^ p ^ " || " ^ t
                    else p
                   ) :: msg                                               in
  match action with
  | R.H0 -> (out_fsm, ["    "^ h0; " = H0"] :: desc, msg)
  | R.IS -> (out_fsm, raw_kdf "SI"                 , msg)
  | R.RS -> (out_fsm, raw_kdf "SR"                 , msg)
  | R.Pr -> (out_fsm, raw_kdf "prelude"            , msg)
  | R.E  -> (out_fsm, raw_kdf ep     , raw_pld ep     )
  | R.S  -> (out_fsm, enc_kdf sp     , enc_pld sp     )
  | R.Pa -> (out_fsm, enc_kdf payload, enc_pld payload)
  | R.EE -> (out_fsm, raw_kdf ("DH("^ es ^", "^ er ^")"), msg)
  | R.ES -> (out_fsm, raw_kdf ("DH("^ es ^", "^ sr ^")"), msg)
  | R.SE -> (out_fsm, raw_kdf ("DH("^ ss ^", "^ er ^")"), msg)
  | R.SS -> (out_fsm, raw_kdf ("DH("^ ss ^", "^ sr ^")"), msg)

let read_action pattern cs payload action (fsm, desc) =
  let out_fsm   = update action fsm                                        in
  let ls        = match cs with Client -> "i" | Server -> "r"              in
  let lp        = match cs with Client -> "I" | Server -> "R"              in
  let rp        = match cs with Client -> "R" | Server -> "I"              in
  let h0        = "H" ^ lp ^ string_of_int  out_fsm.hash                   in
  let h1        = "H" ^ lp ^ string_of_int (out_fsm.hash - 1)              in
  let h2        = "H" ^ lp ^ string_of_int (out_fsm.hash - 2)              in
  let k         = "K" ^ lp ^ string_of_int out_fsm.key                     in
  let lt        = "T" ^ lp ^ string_of_int out_fsm.tag                     in
  let rt        = "T" ^ rp ^ string_of_int out_fsm.tag                     in
  let es        = "e" ^ ls                                                 in
  let er        = "E" ^ rp                                                 in
  let ss        = "s" ^ ls                                                 in
  let sr        = "S" ^ rp                                                 in
  let rkdf    p = ["    "  ^ h0           ; " = KDF("^ h1 ^", "^   p ^")"] in
  let ekdf1     = ["    "  ^ h1  ^", "^ k ; " = ENC("^ h2 ^", "^  "Zero)"] in
  let ekdf2   p = ["    "  ^ h0  ^", "^ lt; " = KDF("^ h1 ^", E_"^ p ^")"] in
  let ekdf3     = ["    "; "!!ASSERT("^ rt ^" == "^ lt ^")"]               in
  let ekdf4   p = ["    "  ^ p            ; " = DEC("^ k  ^", E_"^ p ^")"] in
  let raw_kdf p = rkdf p :: desc                                           in
  let enc_kdf p = if out_fsm.has_key
                   then ekdf4 p :: ekdf3 :: ekdf2 p :: ekdf1 :: desc
                   else rkdf p                               :: desc       in
  match action with
  | R.H0 -> (out_fsm, ["    "^ h0; " = H0"] :: desc)
  | R.IS -> (out_fsm, raw_kdf "SI")
  | R.RS -> (out_fsm, raw_kdf "SR")
  | R.Pr -> (out_fsm, raw_kdf "prelude")
  | R.E  -> (out_fsm, raw_kdf er)
  | R.S  -> (out_fsm, enc_kdf sr)
  | R.Pa -> (out_fsm, enc_kdf payload)
  | R.EE -> (out_fsm, raw_kdf ("DH("^ es ^", "^ er ^")"))
  | R.ES -> (out_fsm, raw_kdf ("DH("^ es ^", "^ sr ^")"))
  | R.SE -> (out_fsm, raw_kdf ("DH("^ ss ^", "^ er ^")"))
  | R.SS -> (out_fsm, raw_kdf ("DH("^ ss ^", "^ sr ^")"))

let send_message pattern cs payload actions fsm =
  (fsm, [], [])
  |> R.log_actions (send_action pattern cs payload) actions
  |> fun (fsm, description, message) ->
     ( fsm,
       ["    "; "!!-> " ^ (message |> List.rev |> String.concat " || ")]
       :: description)

let read_message pattern cs payload actions fsm =
  (fsm, [])
  |> R.log_actions (read_action pattern cs payload) actions

type state = { client : fsm
             ; server : fsm
             ; msg    : int
             ; out    : string list list list
             }

let run_protocol pattern =
  let payload st  = "PLD" ^ string_of_int st.msg                    in
  let client_read actions st =
    let out, str = read_message pattern Client (payload st) actions st.client
    in {st with
         client = out;
         msg    = st.msg + 1;
         out    = ([""; "<<Initiator:"] :: List.rev str) :: st.out} in
  let server_read actions st =
    let out, str = read_message pattern Server (payload st) actions st.server
    in {st with
         server = out;
         msg    = st.msg + 1;
         out    = ([""; "<<Responder:"] :: List.rev str) :: st.out} in
  let client_write actions st =
    let out, str = send_message pattern Client (payload st) actions st.client
    in {st with
         client = out;
         out    = List.rev str :: st.out}                           in
  let server_write actions st =
    let out, str = send_message pattern Server (payload st) actions st.server
    in {st with
         server = out;
         out    = List.rev str :: st.out}                           in
  let st0 = { client = fsm_0
            ; server = fsm_0
            ; msg    = 1
            ; out    = []
            } in
  fun p ->
  let st = R.act_protocol
             client_read client_write
             server_read server_write
             p st0
  in
  st.out
  |> cons [ []
           ;[""; "<<The session key is "
                 ^ "HI" ^ string_of_int st.client.hash ^ " == "
                 ^ "HR" ^ string_of_int st.server.hash]]
  |> List.rev
  |> List.concat
  |> cons [""; "<<Initiator:"]
  |> grid
  |> List.map (Str.global_replace (Str.regexp " *<<") "")
  |> List.map (Str.global_replace (Str.regexp " *!!") "    ")
  |> String.concat "\n"

let spec1 (pattern, p) =
  String.concat "\n"
    [ pattern
    ; (String.make (max 3 (String.length pattern)) '=')
    ; "- H0    : 32 byte domain separation string."
    ; "- KDF   : keyed Blake2b, 512 bits output, divided in 2 halves."
    ; "- ENC   : Chacha20, with nonce = 0."
    ; "- DEC   : Chacha20, with nonce = 0."
    ; "- DH    : X25519"
    ; "- ASSERT: Checks condition, aborts protocol if false."
    ; "- PLD.  : Arbitrary message payloads."
    ; "- H..   : 256-bit hashes"
    ; "- K..   : 256-bit keys"
    ; "- T..   : 128-bit tags"
    ; "- Zero  : 64 byte all-zero string"
    ; ""
    ; (let actions  = P.all_keys p                           in
       let kk k txt = if List.mem k actions then txt else [] in
         ([["- H0 "; " = \"Monokex "^ pattern ^"\" (ASCII, zero padded)"]]
          @ kk P.IS [["- si, SI"; " = Initiator's static key pair."   ]]
          @ kk P.IE [["- ei, EI"; " = Initiator's ephemeral key pair."]]
          @ kk P.RS [["- sr, SR"; " = Responder's static key pair."   ]]
          @ kk P.RE [["- er, ER"; " = Responder's ephemeral key pair."]])
         |> grid
         |> String.concat "\n"
      )
    ; ""
    ; run_protocol pattern p
    ; ""
    ]
let spec protocols =
    String.concat "\n" (protocols /@ spec1)
