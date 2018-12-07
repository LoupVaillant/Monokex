type key      = E | S
type exchange = key * key
type action   = Key      of key
              | Exchange of exchange
type message  = Client   of action list
              | Server   of action list
type protocol = message list * message list
