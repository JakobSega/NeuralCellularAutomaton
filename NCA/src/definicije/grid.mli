type 'a t

val init : int -> int -> 'a -> 'a t
val get_cell : 'a t -> int -> int -> 'a
val set_cell : 'a t -> int -> int -> 'a -> unit
val get_neighbors : 'a t -> int -> int -> 'a list
val height : 'a t -> int
val width : 'a t -> int