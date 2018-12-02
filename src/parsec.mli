val protocol  : (int * Scan.token) Stream.t -> Proto.protocol
val protocols : (int * Scan.token) Stream.t -> (string * Proto.protocol) list
