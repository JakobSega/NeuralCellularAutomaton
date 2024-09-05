open Definicije
open NcaModel

(* Convert RGB color to a string *)
let rgb_to_string (r, g, b) =
  Printf.sprintf "rgb(%d, %d, %d)" (int_of_float (r *. 255.)) (int_of_float (g *. 255.)) (int_of_float (b *. 255.))

(* Convert RGB and alpha to a string with conditional opacity handling *)
let color_with_opacity r g b a =
  let color = rgb_to_string (r, g, b) in
  let opacity = min (max a 0.0) 1.0 in (* Ensure alpha is within the valid range [0, 1] *)
  (color, opacity)

(* View function to render the grid *)
let view model =
  let grid = NcaRunner.get_grid model.avtomat in
  let width = Grid.width grid in
  let height = Grid.height grid in
  let cell_size = 5 in
  let rows =
    List.init height (fun y ->
      let cells = List.init width (fun x ->
        let cell = Grid.get_cell grid x y in
        let (r, g, b) = Cell.get_rgb cell in
        let a = Cell.get_alpha cell in
        let (color, opacity) = color_with_opacity r g b a in
        Vdom.elt "td"
          ~a:[ Vdom.style "background-color" color;
               Vdom.style "width" (Printf.sprintf "%dpx" cell_size);
               Vdom.style "height" (Printf.sprintf "%dpx" cell_size);
               Vdom.style "border" "1px solid #ccc";
               Vdom.style "padding" "0"; (* Remove padding *)
               Vdom.style "margin" "0";  (* Remove margin *)
               Vdom.style "box-sizing" "border-box"; (* Include border and padding in the element's total width and height *)
               Vdom.style "opacity" (Printf.sprintf "%.2f" opacity); (* Apply opacity *)
               Vdom.style "cursor" "pointer"; (* Change cursor to pointer on hover *)
               Vdom.onclick (fun _ -> Klik (x, y)) (* Handle click event *)
             ]
          []
      ) in
      Vdom.elt "tr"
        ~a:[ Vdom.style "margin" "0"; (* Remove margin *)
             Vdom.style "padding" "0" ] (* Remove padding *)
        cells
    )
  in
  Vdom.elt "table"
    ~a:[ Vdom.style "border-collapse" "collapse"; (* Ensure borders collapse *)
         Vdom.style "border-spacing" "0"; (* Remove space between cells *)
         Vdom.style "width" (Printf.sprintf "%dpx" (width * cell_size));
         Vdom.onkeydown (fun e ->
           let key_code = e.which in
           let key_number =
             if key_code >= 48 && key_code <= 57 then (* Check if the key is 0-9 *)
               key_code - 48  (* Convert key code to integer *)
             else
               0  (* Default to 0 for non-digit keys *)
           in
           KeyPress key_number  (* Send KeyPress message *)
         );
         Vdom.attr "tabindex" "0"; (* Make the table focusable *)
       ]
    [ Vdom.elt "tbody" rows ]