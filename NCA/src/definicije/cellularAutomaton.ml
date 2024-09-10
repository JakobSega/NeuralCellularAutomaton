type 'a t = {
  grid : 'a Grid.t;
  update_rule : 'a -> 'a list -> 'a;
}

let init grid update_rule = { grid; update_rule }

let get_grid t = t.grid
let set_grid t new_grid = { t with grid = new_grid }

let get_update_rule t = t.update_rule
let set_update_rule t new_rule = { t with update_rule = new_rule }

let empty_nca width height init_state update_rule =
  let grid = Grid.init width height init_state in
  { grid; update_rule }
