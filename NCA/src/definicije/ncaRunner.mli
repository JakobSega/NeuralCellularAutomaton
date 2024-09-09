type 'a t

val init : 'a CellularAutomaton.t -> 'a t
val step : Cell.t t -> Cell.t t
val run : Cell.t t -> int -> Cell.t t
val get_grid : 'a t -> 'a Grid.t
val get_nca : 'a t -> 'a CellularAutomaton.t
val set_nca : 'a t -> 'a CellularAutomaton.t -> 'a t
val iterations : 'a t -> int