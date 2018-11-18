type t = Valid
       | Broken of string

val v : Proto.protocol -> t
