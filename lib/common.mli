val read_line : filename:string -> string list

val ( ||> ) : 'x * 'y -> ('x -> 'y -> 'z) -> 'z

val ignore : 'a -> unit
