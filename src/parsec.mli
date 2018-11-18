type 'a psr = (int * Lex.token) Stream.t -> 'a

val parse : (Proto.message list * Proto.message list) psr
