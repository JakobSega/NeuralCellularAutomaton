type 'a nca = 'a CellularAutomaton.t

type 'a t = {
  nca : 'a nca;
  iterations : int;
}

(* Initialize a new NCA runner with zero iterations *)
let init nca =
  { nca = nca ; iterations = 0 }

(* Perform one step in the cellular automaton and increment iterations *)
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
      let new_cell = rule cell neighbors in
      Grid.set_cell new_grid x y new_cell
    done
  done;
  let updated_nca = CellularAutomaton.set_grid t.nca new_grid in
  { nca = updated_nca; iterations = t.iterations + 1 }

(* Run the NCA for a given number of steps, updating iterations accordingly *)
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