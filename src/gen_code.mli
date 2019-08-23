
val print_header_prefix : out_channel -> unit
val print_header_suffix : out_channel -> unit
val print_source_prefix : out_channel -> unit

val print_header_pattern : out_channel -> string -> Proto.protocol -> unit
val print_source_pattern : out_channel -> string -> Proto.protocol -> unit
