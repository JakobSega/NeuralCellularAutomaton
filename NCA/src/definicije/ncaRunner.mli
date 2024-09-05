type 'a t

val init : 'a CellularAutomaton.t -> 'a t
val step : 'a t -> 'a t
val run : 'a t -> int -> 'a t
val get_grid : 'a t -> 'a Grid.t
val get_nca : 'a t -> 'a CellularAutomaton.t
val set_nca : 'a t -> 'a CellularAutomaton.t -> 'a t