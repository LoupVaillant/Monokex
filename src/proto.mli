type key      = E | S
type exchange = key * key
type action   = Key of key | Exchange of exchange
type message  = Client of action list
              | Server of action list
type shared   = { client_s: bool;
                  client_e: bool;
                  server_s: bool;
                  server_e: bool; }
type state    = shared * action list
type protocol = shared * message list
