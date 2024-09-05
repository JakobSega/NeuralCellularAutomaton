type 'a t = {
  grid : 'a Grid.t;
  update_rule : 'a -> 'a list -> 'a;
}

(* Initialize a cellular automaton with a grid and an update rule *)
let init grid update_rule = { grid; update_rule }

(* Accessor for the grid *)
let get_grid t = t.grid

(* Accessor for the update rule *)
let get_update_rule t = t.update_rule

let set_grid t new_grid = { t with grid = new_grid }

(* Optional: A helper function to initialize an empty NCA based on dimensions, initial state, and an update rule *)
let empty_nca width height init_state update_rule =
  let grid = Grid.init width height init_state in
  { grid; update_rule }
