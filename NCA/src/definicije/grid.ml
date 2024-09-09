type 'a t = 'a array array

let init width height init_cell =
  Array.init height (fun _ -> Array.init width (fun _ -> init_cell))

let get_cell grid x y = grid.(y).(x)
let set_cell grid x y cell = grid.(y).(x) <- cell

let get_neighbors grid x y =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  let neighbors = ref [] in
  for dx = -1 to 1 do
    for dy = -1 to 1 do
      if dx <> 0 || dy <> 0 then  (* Exclude the cell itself *)
        let nx = (x + dx + width) mod width in
        let ny = (y + dy + height) mod height in
        neighbors := grid.(ny).(nx) :: !neighbors
    done;
  done;
  !neighbors

let height grid = Array.length grid
let width grid = Array.length grid.(0)
