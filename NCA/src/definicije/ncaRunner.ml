type 'a nca = 'a CellularAutomaton.t

type 'a t = {
  nca : 'a nca;
  iterations : int;
}

let init nca =
  { nca = nca ; iterations = 0 }

let step t =
  let grid = CellularAutomaton.get_grid t.nca in
  let height = Grid.height grid in
  let width = Grid.width grid in
  let rule = CellularAutomaton.get_update_rule t.nca in
  let new_grid = Grid.init width height (Grid.get_cell grid 0 0) in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let cell = Grid.get_cell grid x y in
      let neighbors = Grid.get_neighbors grid x y in
      (* Check if the cell or any neighbor meets the alpha condition *)
      let should_update = 
        Cell.get_alpha cell > 0.1 ||
        List.exists (fun neighbor -> Cell.get_alpha neighbor > 0.1) neighbors
      in
      let new_cell = if should_update then rule cell neighbors else cell in
      Grid.set_cell new_grid x y new_cell
    done
  done;
  let updated_nca = CellularAutomaton.set_grid t.nca new_grid in
  { nca = updated_nca; iterations = t.iterations + 1 }
  

let run t steps =
  let rec aux n t =
    if n <= 0 then t
    else aux (n - 1) (step t)
  in
  aux steps t

let get_grid t = CellularAutomaton.get_grid t.nca
let get_nca t = t.nca
let set_nca t new_nca =
  { t with nca = new_nca }
let iterations t = t.iterations