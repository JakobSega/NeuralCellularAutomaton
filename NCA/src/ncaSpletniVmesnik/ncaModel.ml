open Definicije
open Ai
open Definicije.TrainAi

(* Define the type for different pages *)
type page = 
  | InitialPage
  | FileDropPage
  | ImageProcessingPage
  | TrainingPage
  | GridPage

(* Define the type for different training modes *)
(* type training_mode = Growing | Persistent | Regenerating *)

(* Message type to handle cell clicks and key presses *)
type msg =
  | Klik of int * int
  | KeyPress of int
  | ButtonClick of training_mode
  | FileDropped of string           (* Path to the dropped image *)
  | ImageProcessed of Cell.t Grid.t        (* Once the image is processed, pass the grid *)
  | TrainingCompleted of AiModel.t  (* Once training is complete, pass the AI model *)

(* Model definition with optional avtomat and ai_model *)
type model = {
  current_page : page;
  avtomat : Cell.t NcaRunner.t option;
  ai_model : AiModel.t option;
  selected_mode : training_mode option;  (* Track the selected training mode *)
}

(* Initialize the model with the initial page *)
let init () = {
  current_page = InitialPage;
  avtomat = None;
  ai_model = None;
  selected_mode = None;
}

(* Function to initialize the grid and NCA runner *)
let init_grid width height cell update_rule =
  let grid = CellularAutomaton.init (Grid.init width height cell) update_rule in
  let nca = NcaRunner.init grid in
  Some nca

(* Define a no-op update rule *)
let _do_nothing cell _neighbors = cell

(* Update function to handle cell clicks, key presses, and page switches *)
let rec update model = function
  | Klik (x, y) ->
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

  | KeyPress n ->
      (match model.avtomat with
       | Some avtomat ->
           let steps = int_of_float (2. ** float_of_int n) in
           let updated_nca = NcaRunner.run avtomat steps in
           { model with avtomat = Some updated_nca }
       | None -> model)

  | ButtonClick mode ->
      (* Update the page to FileDropPage and store the selected training mode *)
      { model with current_page = FileDropPage; selected_mode = Some mode }

  | FileDropped file_path ->
      (* Transition to the ImageProcessingPage and start processing the image *)
      let updated_model = { model with current_page = ImageProcessingPage } in
      (* Process the image and update model *)
      let grid = ImageProcessor.process_image file_path in
      (* Trigger the ImageProcessed message after processing the image *)
      update updated_model (ImageProcessed grid)

  | ImageProcessed grid ->
      (* Initialize the NCA with the processed grid using _do_nothing rule *)
      let new_nca = CellularAutomaton.init grid _do_nothing in
      (* Automatically start training and transition to TrainingPage *)
      let updated_model = { model with current_page = TrainingPage; avtomat = Some (NcaRunner.init new_nca) } in
      (* Start the training process in the background and trigger TrainingCompleted when done *)
      let training_mode = match model.selected_mode with
        | Some mode -> mode
        | None -> Growing (* Default mode *)
      in
      let trained_model = AiModel.train_with_image grid training_mode in
      update updated_model (TrainingCompleted trained_model)

  | TrainingCompleted trained_model ->
      (* After training, switch to GridPage and apply the new update_rule *)
      let update_rule = AiModel.get_update_rule trained_model in
      let updated_nca = match model.avtomat with
        | _ ->
          init_grid 200 200 (Cell.init (0.0, 0.0, 0.0) 1.0 [||]) update_rule
      in
      { model with current_page = GridPage; ai_model = Some trained_model; avtomat = updated_nca }
