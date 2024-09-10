type 'a t

val init : 'a Grid.t -> ('a -> 'a list -> 'a) -> 'a t

val get_grid : 'a t -> 'a Grid.t
val set_grid : 'a t -> 'a Grid.t -> 'a t

val get_update_rule : 'a t -> ('a -> 'a list -> 'a)
val set_update_rule : 'a t -> ('a -> 'a list -> 'a) -> 'a t

val empty_nca : int -> int -> 'a -> ('a -> 'a list -> 'a) -> 'a t