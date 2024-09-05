type 'a t

val init : int -> int -> 'a -> 'a t
(* Initializes a grid with given width, height, and initial cell value *)

val get_cell : 'a t -> int -> int -> 'a
(* Retrieves the cell at position (x, y) *)

val set_cell : 'a t -> int -> int -> 'a -> unit
(* Sets the cell at position (x, y) to the specified value *)

val get_neighbors : 'a t -> int -> int -> 'a list
(* Returns a list of neighbors for the cell at position (x, y) *)

val height : 'a t -> int
(* Returns the height (number of rows) of the grid *)

val width : 'a t -> int
(* Returns the width (number of columns) of the grid *)
