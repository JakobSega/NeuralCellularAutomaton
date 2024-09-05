(* Define the type 'a t which represents a cellular automaton with cells of type 'a *)
type 'a t

(* Initialize a cellular automaton with a grid and an update rule *)
val init : 'a Grid.t -> ('a -> 'a list -> 'a) -> 'a t

(* Accessor to get the grid from a cellular automaton *)
val get_grid : 'a t -> 'a Grid.t

(* Set a new grid for the cellular automaton *)
val set_grid : 'a t -> 'a Grid.t -> 'a t

(* Accessor to get the update rule from a cellular automaton *)
val get_update_rule : 'a t -> ('a -> 'a list -> 'a)

val empty_nca : int -> int -> 'a -> ('a -> 'a list -> 'a) -> 'a t