val pair    : 'a -> 'b -> 'a * 'b
val cons    : 'a -> 'a list -> 'a list

val swap    : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val const   : 'a -> 'b -> 'a
val id      : 'a -> 'a
val (|-)    : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

val (//)    : 'a list -> ('a -> bool) -> 'a list
val (/@)    : 'a list -> ('a -> 'b  ) -> 'b list

val error   : string -> 'a
val f_error : string -> 'a -> 'b
val check   : bool -> string -> unit

val last : 'a list -> 'a
val init : 'a list -> 'a list

(* Pretty printing functions *)
val grid      : string list list -> string list
val prototype : string -> string -> string list list -> string
