open Definicije
open Ai

(* Define the type for different pages *)
type page = 
  | InitialPage
  | PaintPage
  | GridPage

(* Message type to handle various events *)
type msg =
  | ClickErase of int * int
  | ClickPaint of int * int
  | DoubleClick of int*int
  | KeyPress of int
  | KeyPressLeft
  | KeyPressRight
  | KeyPressUp
  | KeyPressDown
  | SetColor of float * float * float * float
  | ButtonClick
  | Demonstration
  | BeginTraining

(* Model definition with optional avtomat and ai_model *)
type model = {
  current_page : page;
  avtomat : Cell.t NcaRunner.t option;
  color : (float * float * float * float) option;
}

(* Initialize the model with the initial page *)
let init () = {
  current_page = InitialPage;
  avtomat = None;
  color = None;
}

(* Function to initialize the grid and NCA runner *)
let init_grid width height cell update_rule =
  let grid = CellularAutomaton.init (Grid.init width height cell) update_rule in
  let nca = NcaRunner.init grid in
  Some nca

let init_painting_grid () =
  let do_nothing cell _neighbors = cell in
  let grid = CellularAutomaton.init (Grid.init 20 20 (Cell.init (1.0, 1.0, 1.0) 1.0 [||])) do_nothing in
  let nca = NcaRunner.init grid in
  Some nca

(* Update function to handle cell clicks, key presses, and page switches *)
let update model = function
  | ClickErase (x, y) ->
      (match model.avtomat with
        | Some avtomat ->
          let grid = NcaRunner.get_grid avtomat in
          let radius = 5 in
          let width = Grid.width grid in
          let height = Grid.height grid in
          let wrap_index idx max =
            if idx < 0 then idx + max
            else if idx >= max then idx - max
            else idx
          in
          (* Update cells in the circular neighborhood *)
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
          let new_nca = CellularAutomaton.set_grid (NcaRunner.get_nca avtomat) grid in
          let updated_runner = NcaRunner.set_nca avtomat new_nca in
          { model with avtomat = Some updated_runner }
        | None -> model)

  | ClickPaint (x, y) ->
    (match model.avtomat with
    | Some avtomat ->
       let grid = NcaRunner.get_grid avtomat in
       let cell = 
         match model.color with
         | Some (r, g, b, a) -> Cell.init (r, g, b) a [||]
         | None -> Cell.init (0.0, 0.0, 0.0) 0.0 [||]
       in
       Grid.set_cell grid x y cell;  (* Set the cell in the grid *)
       let new_nca = CellularAutomaton.set_grid (NcaRunner.get_nca avtomat) grid in
       let updated_runner = NcaRunner.set_nca avtomat new_nca in
       { model with avtomat = Some updated_runner }
    | None -> model)

  | DoubleClick (x, y) ->
    (match model.avtomat with
    | Some avtomat ->
        let grid = NcaRunner.get_grid avtomat in
        let cell = Grid.get_cell grid x y in
        let black_cell = Cell.set_alpha 1.0 (Cell.set_rgb (0.0, 0.0, 0.0) cell) in
        Grid.set_cell grid x y black_cell;
        let new_nca = CellularAutomaton.set_grid (NcaRunner.get_nca avtomat) grid in
        let updated_runner = NcaRunner.set_nca avtomat new_nca in
        { model with avtomat = Some updated_runner }
    | None -> model)
  
  | KeyPress n ->
    (match model.avtomat with
     | Some avtomat ->
         let steps = int_of_float (2. ** float_of_int n) in
         let updated_nca = NcaRunner.run avtomat steps in
         { model with avtomat = Some updated_nca }
     | None -> model)

  | KeyPressLeft ->
    (match model.avtomat with
     | Some avtomat -> 
      let new_update_rule = MoveRules.move_left in
      let nca = NcaRunner.get_nca avtomat in
      let updated_nca = CellularAutomaton.set_update_rule nca new_update_rule in
      { model with avtomat = Some (NcaRunner.set_nca avtomat updated_nca) }
     | None -> model)

  | KeyPressRight ->
    (match model.avtomat with
     | Some avtomat -> 
      let new_update_rule = MoveRules.move_right in
      let nca = NcaRunner.get_nca avtomat in
      let updated_nca = CellularAutomaton.set_update_rule nca new_update_rule in
      { model with avtomat = Some (NcaRunner.set_nca avtomat updated_nca) }
     | None -> model)

  | KeyPressUp ->
    (match model.avtomat with
     | Some avtomat -> 
      let new_update_rule = MoveRules.move_up in
      let nca = NcaRunner.get_nca avtomat in
      let updated_nca = CellularAutomaton.set_update_rule nca new_update_rule in
      { model with avtomat = Some (NcaRunner.set_nca avtomat updated_nca) }
     | None -> model)

  | KeyPressDown ->
    (match model.avtomat with
    | Some avtomat -> 
     let new_update_rule = MoveRules.move_down in
     let nca = NcaRunner.get_nca avtomat in
      let updated_nca = CellularAutomaton.set_update_rule nca new_update_rule in
     { model with avtomat = Some (NcaRunner.set_nca avtomat updated_nca) }
    | None -> model)

  | Demonstration ->
    (match model.avtomat with
    | Some avtomat -> 
      let new_update_rule = MoveRules.move_down in
      let nca = NcaRunner.get_nca avtomat in
       let updated_nca = CellularAutomaton.set_update_rule nca new_update_rule in
      { model with current_page = GridPage ; avtomat = Some (NcaRunner.set_nca avtomat updated_nca) }
    | None -> model)

  | SetColor (r, g, b, a) ->   Printf.printf "Test message\n" ;{ model with color = Some (r, g, b, a) }

  | ButtonClick ->
      { model with current_page = PaintPage; avtomat = init_painting_grid () }

  | BeginTraining ->
    Printf.printf "Test message\n" ;
    (* Retrieve the initial grid for training *)
    let initial_grid = NcaRunner.get_grid (Option.get model.avtomat) in
    
    (* Train the network using the initial grid *)
    let trained_update_rule = Network.train_network initial_grid in
    
    (* Initialize the grid with the trained update rule *)
    let grid = Grid.init 120 120 (Cell.init (1.0, 1.0, 1.0) 0.0 (Array.make 12 0.0 )) in
    Grid.set_cell grid 60 60 (Cell.init (0.0, 0.0, 0.0) 1.0 (Array.make 12 1.0 ));
    let updated_nca = Some (NcaRunner.init (CellularAutomaton.init grid trained_update_rule)) in
    (* Update the model *)
    { model with current_page = GridPage ; avtomat = updated_nca }
    