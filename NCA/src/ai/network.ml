open Definicije

(* Define a type for a vector *)
type vec = float array

(* Define a type for a matrix *)
type mat = float array array

(* Utility function to create a matrix *)
let create_matrix rows cols =
  Array.init rows (fun _ -> Array.make cols 0.0)

(* Utility function to create a vector *)
let create_vector size =
  Array.make size 0.0

(* Randomly initialize a matrix *)
let random_matrix rows cols =
  Array.init rows (fun _ -> Array.init cols (fun _ -> Random.float 1.0))

(* Randomly initialize a vector *)
let random_vector size =
  Array.init size (fun _ -> Random.float 1.0)

(* ReLU activation function *)
let relu x = if x > 0.0 then x else 0.0

(* Matrix-vector multiplication *)
let mat_vec_mult mat vec =
  let rows = Array.length mat in
  let cols = Array.length vec in
  let result = Array.make rows 0.0 in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      result.(i) <- result.(i) +. (mat.(i).(j) *. vec.(j))
    done
  done;
  result

(* Matrix addition of a matrix and a row vector *)
let mat_add_row_vec mat vec =
  let rows = Array.length mat in
  let cols = Array.length mat.(0) in
  let result = Array.copy mat in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      result.(i).(j) <- result.(i).(j) +. vec.(i)
    done
  done;
  result

(* Apply activation function element-wise *)
let mat_map f mat =
  let rows = Array.length mat in
  let cols = Array.length mat.(0) in
  Array.init rows (fun i ->
    Array.init cols (fun j ->
      f mat.(i).(j)))

(* Define a type for the neural network layers *)
type layer = {
  weights : mat;
  biases : vec;
  activation : (mat -> mat); (* Activation function *)
}

(* Initialize a layer *)
let initialize_layer input_size output_size =
  let weights = random_matrix output_size input_size in
  let biases = random_vector output_size in
  { weights; biases; activation = (fun x -> mat_map relu x) }

(* Define a simple neural network *)
type neural_network = {
  layer1 : layer;
  layer2 : layer;
  mutable loss : float option; (* Changed to mutable *)
}

(* Initialize a neural network with specified sizes *)
let initialize_network input_size hidden_size output_size =
  let layer1 = initialize_layer input_size hidden_size in  (* Input to hidden layer *)
  let layer2 = initialize_layer hidden_size output_size in (* Hidden to output layer *)
  { layer1; layer2; loss = None } (* Initialize loss as None *)

(* Forward pass for a single layer *)
let forward_layer layer input =
  let z = Array.map (fun v -> mat_vec_mult layer.weights v) input in
  let z_biased = mat_add_row_vec z layer.biases in
  layer.activation z_biased

(* Forward pass for the entire network *)
let forward_network network inputs =
  let layer1_output = forward_layer network.layer1 inputs in
  forward_layer network.layer2 layer1_output

(* Define the cell_to_input_vec function *)
let cell_to_input_vec cell =
  let rgb = Cell.get_rgb cell in
  let alpha = Cell.get_alpha cell in
  let hidden = Cell.get_hidden cell in
  let vec = Array.make 16 0.0 in
  let (r, g, b) = rgb in
  vec.(0) <- r;
  vec.(1) <- g;
  vec.(2) <- b;
  vec.(3) <- alpha;
  Array.blit hidden 0 vec 4 12;
  vec

(* Define the cell_and_neighbors_to_input_vec function *)
let cell_and_neighbors_to_input_vec cell neighbors =
  let cell_vec = cell_to_input_vec cell in
  let neighbors_vecs = List.map cell_to_input_vec neighbors in
  let all_vecs = cell_vec :: neighbors_vecs in
  let flatten_vecs = Array.concat all_vecs in (* Use Array.concat directly on the array list *)
  let input_vec = Array.make 144 0.0 in
  Array.blit flatten_vecs 0 input_vec 0 144;
  input_vec

(* Define the output_vec_to_cell function *)
let output_vec_to_cell output =
  let rgb = (output.(0), output.(1), output.(2)) in
  let alpha = output.(3) in
  let hidden = Array.init 12 (fun i -> output.(i + 4)) in
  Cell.init rgb alpha hidden

(* Define the update_rule function *)
let update_rule network =
  fun cell neighbors ->
    let input = cell_and_neighbors_to_input_vec cell neighbors in
    let output = forward_network network [|input|] in
    output_vec_to_cell output.(0)

(* Define the crossover function *)
let crossover parent1 parent2 =
  let crossover_layer layer1 layer2 =
    let rows = Array.length layer1.weights in
    let cols = Array.length layer1.weights.(0) in
    let new_weights = create_matrix rows cols in
    let new_biases = create_vector (Array.length layer1.biases) in
    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        if Random.float 1.0 < 0.5 then
          new_weights.(i).(j) <- layer1.weights.(i).(j)
        else
          new_weights.(i).(j) <- layer2.weights.(i).(j)
      done
    done;
    for i = 0 to Array.length new_biases - 1 do
      if Random.float 1.0 < 0.5 then
        new_biases.(i) <- layer1.biases.(i)
      else
        new_biases.(i) <- layer2.biases.(i)
    done;
    { weights = new_weights; biases = new_biases; activation = layer1.activation }
  in
  { layer1 = crossover_layer parent1.layer1 parent2.layer1;
    layer2 = crossover_layer parent1.layer2 parent2.layer2;
    loss = None }

(* Define the mutate function *)
let mutate network =
  let mutate_layer layer =
    let rows = Array.length layer.weights in
    let cols = Array.length layer.weights.(0) in
    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        if Random.float 1.0 < 0.1 then
          layer.weights.(i).(j) <- layer.weights.(i).(j) +. (Random.float 0.2 -. 0.1)
      done
    done;
    for i = 0 to Array.length layer.biases - 1 do
      if Random.float 1.0 < 0.1 then
        layer.biases.(i) <- layer.biases.(i) +. (Random.float 0.2 -. 0.1)
    done;
    layer
  in
  { layer1 = mutate_layer network.layer1;
    layer2 = mutate_layer network.layer2;
    loss = network.loss }

let target_grid grid =
  let large_grid = Grid.init 40 40 (Cell.init (0.0, 0.0, 0.0) 1.0 [||]) in
  let place_in_middle small_grid large_grid =
    let small_width = Grid.width small_grid in
    let small_height = Grid.height small_grid in
    let large_width = Grid.width large_grid in
    let large_height = Grid.height large_grid in

    if small_width <> 20 || small_height <> 20 then
      failwith "The small grid must be 20x20."
    else if large_width <> 40 || large_height <> 40 then
      failwith "The large grid must be 120x120."
    else
      let start_x = (large_width - small_width) / 2 in
      let start_y = (large_height - small_height) / 2 in

      for y = 0 to small_height - 1 do
        for x = 0 to small_width - 1 do
          let cell = Grid.get_cell small_grid x y in
          Grid.set_cell large_grid (start_x + x) (start_y + y) cell
        done;
      done;
      large_grid
  in
  place_in_middle grid large_grid
    
let starting_grid () =
  let grid = Grid.init 40 40 (Cell.init (1.0, 1.0, 1.0) 0.0 (Array.make 12 0.0)) in
  let hidden = Array.init 12 (fun _ -> (Random.float 2.0) -. 1.0) in
  Grid.set_cell grid 20 20 (Cell.init (0.0, 0.0, 0.0) 1.0 hidden);
  grid

let distance grid1 grid2 =
  let width = Grid.width grid1 in
  let height = Grid.height grid1 in
  let total_distance = ref 0.0 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let cell1 = Grid.get_cell grid1 x y in
      let cell2 = Grid.get_cell grid2 x y in
      let (r1, g1, b1) = Cell.get_rgb cell1 in
      let (r2, g2, b2) = Cell.get_rgb cell2 in
      let a1 = Cell.get_alpha cell1 in
      let a2 = Cell.get_alpha cell2 in
      let distance =
        (r1 -. r2) ** 2.0 +. (g1 -. g2) ** 2.0 +. (b1 -. b2) ** 2.0 +. (a1 -. a2) ** 2.0
      in
      total_distance := !total_distance +. distance
    done;
  done;
  !total_distance

(* Define the train_network function using genetic algorithm *)
let train_network grid =
  let target_grid = target_grid grid in
  let starting_grid = starting_grid () in
  let population_size = 1 in
  let mutation_rate = 0.1 in
  let crossover_rate = 0.5 in
  let max_generations = -1 in
  let desired_distance = 1.0 in (* Hardcoded desired distance *)

  let population = Array.init population_size (fun _ -> initialize_network 144 144 16) in

  let rec train population generation =
    if generation > max_generations then
      let best_network = Array.fold_left (fun best network ->
        match network.loss with
        | Some loss when loss < (match best.loss with Some l -> l | None -> max_float) -> network
        | _ -> best
      ) (Array.get population 0) population
      in
      (* Return the update_rule function based on the best network *)
      update_rule best_network
    else
      let evaluate network =
        let nca = CellularAutomaton.init starting_grid (update_rule network) in
        let runner = NcaRunner.init nca in
        let final_nca = NcaRunner.run runner 100 in
        let final_grid = NcaRunner.get_grid final_nca in
        let dist = distance final_grid target_grid in
        network.loss <- Some dist;
        dist
      in
      (* Apply the evaluation function to each network in the population *)
      Array.iter (fun network -> ignore (evaluate network)) population;
      (* Sort the population based on the evaluated fitness *)
      Array.sort (fun n1 n2 ->
        compare (match n1.loss with Some l -> l | None -> max_float)
                (match n2.loss with Some l -> l | None -> max_float)
      ) population;
      
      let sorted_population = population in

      let new_population = Array.init population_size (fun i ->
        if i < population_size / 2 then
          Array.get sorted_population i
        else if Random.float 1.0 < crossover_rate then
          let parent1 = Array.get sorted_population (Random.int (population_size / 2)) in
          let parent2 = Array.get sorted_population (Random.int (population_size / 2)) in
          crossover parent1 parent2
        else
          let parent = Array.get sorted_population (Random.int (population_size / 2)) in
          if Random.float 1.0 < mutation_rate then
            mutate parent
          else
            parent
      ) in
      let best_in_generation = Array.get sorted_population 0 in
      if (match best_in_generation.loss with Some dist -> dist | None -> max_float) <= desired_distance then
        update_rule best_in_generation
      else
        train new_population (generation + 1)
  in
  train population 0


(*open Definicije

(* Define a type for the neural network layers *)
type layer = {
  weights : Lacaml.D.Mat.t;
  biases : Lacaml.D.Vec.t;
  activation : (Lacaml.D.Mat.t -> Lacaml.D.Mat.t); (* Activation function *)
}

(* Initialize a layer *)
let initialize_layer input_size output_size =
  let weights = Lacaml.D.Mat.random ~rows:output_size ~cols:input_size in
  let biases = Lacaml.D.Vec.random ~size:output_size in
  { weights; biases; activation = (fun x -> Lacaml.D.Mat.map ~f:relu x) } (* Apply ReLU element-wise *)

(* Define a simple neural network *)
type neural_network = {
  layer1 : layer;
  layer2 : layer;
  mutable loss : float option; (* Changed to mutable *)
}

let relu x = if x > 0.0 then x else 0.0

(* Initialize a neural network with specified sizes *)
let initialize_network input_size hidden_size output_size =
  let layer1 = initialize_layer input_size hidden_size in
  let layer2 = initialize_layer hidden_size output_size in
  { layer1; layer2; loss = None } (* Initialize loss as None *)

(* Forward pass for a single layer *)
let forward_layer layer input =
  let z = Lacaml.D.Mat.(layer.weights *@ input) in
  let z_biased = Lacaml.D.Mat.add_row_vec z layer.biases in
  layer.activation z_biased

(* Forward pass for the entire network *)
let forward_network network inputs =
  let layer1_output = forward_layer network.layer1 inputs in
  forward_layer network.layer2 layer1_output

(* Define the update_rule function *)
let update_rule network =
  fun cell neighbors ->
    let input = cell_and_neighbors_to_input_vec cell neighbors in
    let output = forward_network network (Lacaml.D.Mat.of_array (Array.to_list input) ~rows:16 ~cols:1) in
    output_vec_to_cell (Lacaml.D.Mat.to_array output)

(* Define the cell_to_input_vec function *)
let cell_to_input_vec cell =
  let rgb = Cell.get_rgb cell in
  let alpha = Cell.get_alpha cell in
  let hidden = Cell.get_hidden cell in
  let vec = Array.make 16 0.0 in
  vec.(0) <- fst rgb;
  vec.(1) <- snd rgb;
  vec.(2) <- snd (snd rgb);
  vec.(3) <- alpha;
  Array.blit hidden 0 vec 4 12;
  vec

(* Define the cell_and_neighbors_to_input_vec function *)
let cell_and_neighbors_to_input_vec cell neighbors =
  let cell_vec = cell_to_input_vec cell in
  let neighbors_vecs = List.map cell_to_input_vec neighbors in
  let all_vecs = cell_vec :: neighbors_vecs in
  let input_vec = Array.make 144 0.0 in
  let flatten_vecs = Array.concat (List.map Array.to_list all_vecs) in
  Array.blit flatten_vecs 0 input_vec 0 144;
  input_vec

(* Define the output_vec_to_cell function *)
let output_vec_to_cell output =
  let rgb = (output.(0), output.(1), output.(2)) in
  let alpha = output.(3) in
  let hidden = Array.init 12 (fun i -> output.(i + 4)) in
  Cell.init rgb alpha hidden

(* Define the crossover function *)
let crossover parent1 parent2 =
  let crossover_layer layer1 layer2 =
    let weights1 = layer1.weights in
    let weights2 = layer2.weights in
    let biases1 = layer1.biases in
    let biases2 = layer2.biases in
    let rows = Lacaml.D.Mat.rows weights1 in
    let cols = Lacaml.D.Mat.cols weights1 in
    let new_weights = Lacaml.D.Mat.create ~rows ~cols in
    let new_biases = Lacaml.D.Vec.create ~size:(Lacaml.D.Vec.size biases1) in
    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        if Random.float 1.0 < 0.5 then
          Lacaml.D.Mat.set new_weights i j (Lacaml.D.Mat.get weights1 i j)
        else
          Lacaml.D.Mat.set new_weights i j (Lacaml.D.Mat.get weights2 i j)
      done
    done;
    for i = 0 to (Lacaml.D.Vec.size biases1) - 1 do
      if Random.float 1.0 < 0.5 then
        Lacaml.D.Vec.set new_biases i (Lacaml.D.Vec.get biases1 i)
      else
        Lacaml.D.Vec.set new_biases i (Lacaml.D.Vec.get biases2 i)
    done;
    { weights = new_weights; biases = new_biases; activation = layer1.activation }
  in
  { layer1 = crossover_layer parent1.layer1 parent2.layer1;
    layer2 = crossover_layer parent1.layer2 parent2.layer2;
    loss = None }

(* Define the mutate function *)
let mutate network =
  let mutate_layer layer =
    let mutate_matrix matrix =
      let rows = Lacaml.D.Mat.rows matrix in
      let cols = Lacaml.D.Mat.cols matrix in
      for i = 0 to rows - 1 do
        for j = 0 to cols - 1 do
          if Random.float 1.0 < 0.1 then
            let value = Lacaml.D.Mat.get matrix i j in
            Lacaml.D.Mat.set matrix i j (value +. (Random.float 0.2 -. 0.1))
        done
      done
    in
    let mutate_vector vector =
      let size = Lacaml.D.Vec.size vector in
      for i = 0 to size - 1 do
        if Random.float 1.0 < 0.1 then
          let value = Lacaml.D.Vec.get vector i in
          Lacaml.D.Vec.set vector i (value +. (Random.float 0.2 -. 0.1))
      done
    in
    mutate_matrix layer.weights;
    mutate_vector layer.biases;
    layer
  in
  { layer1 = mutate_layer network.layer1;
    layer2 = mutate_layer network.layer2;
    loss = network.loss }

(* Define the train_network function using genetic algorithm *)
let train_network grid =
  let target_grid = target_grid grid in
  let starting_grid = starting_grid () in
  let population_size = 100 in
  let mutation_rate = 0.1 in
  let crossover_rate = 0.5 in
  let max_generations = 100 in
  let desired_distance = 1.0 in (* Hardcoded desired distance *)

  let population = Array.init population_size (fun _ -> initialize_network 144 144 16) in

  let rec train population generation =
    if generation > max_generations then
      let best_network = Array.fold_left (fun best network ->
        match network.loss with
        | Some loss when loss < (match best.loss with Some l -> l | None -> max_float) -> network
        | _ -> best
      ) (Array.get population 0) population
      in
      (* Return the update_rule function based on the best network *)
      update_rule best_network
    else
      let evaluate network =
        let nca = CellularAutomaton.init starting_grid (update_rule network) in
        let runner = NcaRunner.init nca in
        let final_grid = NcaRunner.run runner 100 in
        let dist = distance final_grid target_grid in
        network.loss <- Some dist;
        dist
      in
      let sorted_population = Array.sort (fun n1 n2 ->
        compare (match n1.loss with Some l -> l | None -> max_float)
                (match n2.loss with Some l -> l | None -> max_float)
      ) population in

      let new_population = Array.init population_size (fun i ->
        if i < population_size / 2 then
          Array.get sorted_population i
        else if Random.float 1.0 < crossover_rate then
          let parent1 = Array.get sorted_population (Random.int (population_size / 2)) in
          let parent2 = Array.get sorted_population (Random.int (population_size / 2)) in
          crossover parent1 parent2
        else
          let parent = Array.get sorted_population (Random.int (population_size / 2)) in
          if Random.float 1.0 < mutation_rate then
            mutate parent
          else
            parent
      ) in
      let best_in_generation = Array.get sorted_population 0 in
      if (match best_in_generation.loss with Some dist -> dist | None -> max_float) <= desired_distance then
        update_rule best_in_generation
      else
        train new_population (generation + 1)
  in
  train population 0
*)