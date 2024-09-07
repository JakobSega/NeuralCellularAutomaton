open Definicije

module Resize = struct
  let resize_image (img_data : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t)
                   ~original_width
                   ~original_height
                   ~new_width
                   ~new_height =
    let scale_x = float_of_int original_width /. float_of_int new_width in
    let scale_y = float_of_int original_height /. float_of_int new_height in
  
    (* Allocate a new Bigarray to store the resized image data *)
    let resized_image = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (new_width * new_height * 4) in
  
    (* Resample the image using nearest neighbor interpolation *)
    for y = 0 to new_height - 1 do
      for x = 0 to new_width - 1 do
        let orig_x = int_of_float (float_of_int x *. scale_x) in
        let orig_y = int_of_float (float_of_int y *. scale_y) in
        for c = 0 to 3 do (* R, G, B, A channels *)
          let orig_index = (orig_y * original_width + orig_x) * 4 + c in
          let new_index = (y * new_width + x) * 4 + c in
          Bigarray.Array1.set resized_image new_index (Bigarray.Array1.get img_data orig_index)
        done
      done
    done;
    resized_image
end

let process_image file_path =
  (* Load the image with 4 channels (RGBA) *)
  match Stb_image.loadf ~channels:4 file_path with
  | Error (`Msg err) -> failwith ("Failed to load image: " ^ err)
  | Ok image_data ->
    let original_width = Stb_image.width image_data in
    let original_height = Stb_image.height image_data in
    let width = 20 in
    let height = 20 in
    
    (* Resize the image data to 20x20 *)
    let resized_image = Resize.resize_image (Stb_image.data image_data)
                           ~original_width
                           ~original_height
                           ~new_width:width
                           ~new_height:height in
    
    (* Create a grid with an initial cell *)
    let initial_cell = Cell.init (0.0, 0.0, 0.0) 1.0 [||] in
    let grid = Grid.init width height initial_cell in
    
    (* Update each cell in the grid based on the resized image *)
    let update_cell x y =
      let index = (y * width + x) * 4 in (* Each pixel has 4 components: R, G, B, A *)
      let r = Bigarray.Array1.get resized_image index in
      let g = Bigarray.Array1.get resized_image (index + 1) in
      let b = Bigarray.Array1.get resized_image (index + 2) in
      let a = Bigarray.Array1.get resized_image (index + 3) in
      
      (* Normalize the RGB and alpha values to [0, 1] *)
      let r_norm = r /. 255.0 in
      let g_norm = g /. 255.0 in
      let b_norm = b /. 255.0 in
      let a_norm = a /. 255.0 in
      
      (* Create a cell with the extracted RGB and alpha values *)
      let cell = Cell.init (r_norm, g_norm, b_norm) a_norm [||] in
      
      (* Set the cell in the grid *)
      Grid.set_cell grid x y cell
    in

    (* Apply the update to each cell in the grid *)
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        update_cell x y
      done
    done;

    grid
