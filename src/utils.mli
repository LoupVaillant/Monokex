(* Application wide parameters. *)
val prefix      : string
val prefix_caps : string
val prefix_space: string

(* Ordinary utilities *)
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

val is_even : int -> bool
val is_odd  : int -> bool

(* List handling *)
val last       : 'a list -> 'a
val init       : 'a list -> 'a list
val range      : int -> int -> int list
val mapi       : int -> (int -> 'a -> 'b) -> 'a list -> 'b list
val zip_with   : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val zip        : 'a list -> 'b list -> ('a * 'b) list
val map2       : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val map_pair   : ('a -> 'b -> 'c) -> ('a * 'b) list -> 'c list
val take_while : ('a -> bool) -> 'a list -> 'a list
val drop_while : ('a -> bool) -> 'a list -> 'a list

(* Pretty printing functions *)
val pad_right : string list      -> string list
val grid      : string list list -> string list
val prototype : string -> string -> string list list -> string
val paragraph : string -> string

(* Reading & writing files *)
val read : string -> string
val write : string -> string -> unit
