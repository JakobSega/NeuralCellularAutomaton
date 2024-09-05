open Definicije

(* Message type to handle cell clicks *)
type msg = Klik of int * int | KeyPress of int

(* Model definition *)
type model = {
  avtomat : Cell.t NcaRunner.t;
}

(* Initialize a model with an empty grid *)
let init width height cell update_rule =
  let init_nca = NcaRunner.init (CellularAutomaton.init (Grid.init width height cell) update_rule) in
  { avtomat = init_nca }

(* Update function to handle cell clicks *)
let update model = function
  | Klik (x, y) ->
      let grid = NcaRunner.get_grid model.avtomat in
      let radius = 5 in (* Radius of the circular neighborhood *)
      let width = Grid.width grid in
      let height = Grid.height grid in
      
      (* Function to handle toroidal wrapping *)
      let wrap_index idx max =
        if idx < 0 then idx + max
        else if idx >= max then idx - max
        else idx
      in

      (* Update cells in the 7x7 circular neighborhood *)
      for dx = -radius to radius do
        for dy = -radius to radius do
          let distance = sqrt (float_of_int (dx * dx + dy * dy)) in
          if distance <= float_of_int radius then
            let nx = wrap_index (x + dx) width in
            let ny = wrap_index (y + dy) height in
            let cell = Grid.get_cell grid nx ny in
            let new_cell = Cell.set_alpha 0.0 (Cell.set_rgb (0.0, 0.0, 0.0) cell) in
            Grid.set_cell grid nx ny new_cell
        done
      done;
      
      let new_nca = CellularAutomaton.set_grid (NcaRunner.get_nca model.avtomat) grid in
      let updated_runner = NcaRunner.set_nca model.avtomat new_nca in
      { avtomat = updated_runner }

  | KeyPress n ->
    let steps = 2. ** (float_of_int n) in
    let updated_nca = NcaRunner.run model.avtomat (int_of_float steps) in
    { avtomat = updated_nca }