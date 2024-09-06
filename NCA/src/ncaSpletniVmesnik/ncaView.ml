open Definicije
open NcaModel

let rgb_to_string (r, g, b) =
  Printf.sprintf "rgb(%d, %d, %d)" (int_of_float (r *. 255.)) (int_of_float (g *. 255.)) (int_of_float (b *. 255.))

let color_with_opacity r g b a =
  let color = rgb_to_string (r, g, b) in
  let opacity = min (max a 0.0) 1.0 in
  (color, opacity)

(* View for non-interactive grid on the TrainingPage *)
let view_grid_non_interactive model =
  match model.avtomat with
  | Some avtomat ->
      let grid = NcaRunner.get_grid avtomat in
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
                   Vdom.style "opacity" (Printf.sprintf "%.2f" opacity);
                   Vdom.style "width" (Printf.sprintf "%dpx" cell_size);
                   Vdom.style "height" (Printf.sprintf "%dpx" cell_size);
                   Vdom.style "border" "1px solid #ccc";
                   ]
              []
          ) in
          Vdom.elt "tr" cells
        ) in
      Vdom.elt "div" [
        Vdom.text "Training in progress...";  (* Add this text *)
        Vdom.elt "table" [ Vdom.elt "tbody" rows ];

        (* Add a button to trigger BeginTraining message *)
        Vdom.elt "button" 
          ~a:[ Vdom.onclick (fun _ -> 
            match NcaRunner.get_grid avtomat with
            | grid -> BeginTraining grid
            (* | _ -> BeginTraining (Grid.init 20 20 (Cell.init (0.0, 0.0, 0.0) 1.0 [||])) *)
            ) 
          ] [ Vdom.text "Begin Training" ];
      ]
  | None -> Vdom.elt "div" [ Vdom.text "Grid is not available" ]

(* View for interactive grid on the GridPage *)
let view_grid model =
  match model.avtomat with
  | Some avtomat ->
    let grid = NcaRunner.get_grid avtomat in
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
  | None -> Vdom.elt "div" [ Vdom.text "Grid is not available" ]

let view model =
  match model.current_page with
  | InitialPage ->
      Vdom.elt "div" [
        Vdom.elt "h1" [ Vdom.text "Welcome to the Automaton App!" ];

        (* Button 1: Growing *)
        Vdom.elt "button" ~a:[ Vdom.onclick (fun _ -> ButtonClick Growing) ] [ Vdom.text "Growing" ];
        Vdom.elt "p" [ Vdom.text "In this mode, the automaton will exhibit a growing pattern over time." ];
  
        (* Button 2: Persistent *)
        Vdom.elt "button" ~a:[ Vdom.onclick (fun _ -> ButtonClick Persistent) ] [ Vdom.text "Persistent" ];
        Vdom.elt "p" [ Vdom.text "In this mode, the automaton will maintain a stable or persistent pattern." ];
  
        (* Button 3: Regenerating *)
        Vdom.elt "button" ~a:[ Vdom.onclick (fun _ -> ButtonClick Regenerating) ] [ Vdom.text "Regenerating" ];
        Vdom.elt "p" [ Vdom.text "In this mode, the automaton will exhibit a regenerating or cyclical behavior." ];
      ]
  
  | FileDropPage ->
      Vdom.elt "div" [
        Vdom.elt "h2" [ Vdom.text "Upload an image to process" ];
    
        (* Create a file input element to select a file *)
        Vdom.elt "input"
          ~a:[
            Vdom.attr "type" "file";
            Vdom.attr "id" "myFile";  (* ID to associate with the form *)
            Vdom.attr "name" "filename";
            Vdom.attr "accept" "image/*";  (* Restrict file types to images *)
            Vdom.onchange (fun ev ->
              let target = Js_of_ocaml.Js.Unsafe.get ev "target" in
              let files = Js_of_ocaml.Js.Unsafe.get target "files" in
              let file = Js_of_ocaml.Js.array_get files 0 in
              let file_name = Js_of_ocaml.Js.to_string (Js_of_ocaml.Js.Unsafe.get file "name") in
              (* Update the model with the selected file path *)
              FileDropped file_name  (* This triggers an update to store the file path in the model *)
            );
          ] [];
    
        (* Create a button to trigger the FileDropped message *)
        Vdom.elt "button"
          ~a:[
            Vdom.onclick (fun _ -> FileDropped "C:\\Users\\blin\\Documents\\Programiranje1\\Projekt_PROG2\\emoji.png") ] [ Vdom.text "Test FileDropped message" ];
      ]
     
  | ImageProcessingPage ->
      Vdom.elt "div" [
        (* Button 1: Growing *)
        Vdom.elt "button" ~a:[ Vdom.onclick (fun _ -> ButtonClick Regenerating
          (*match model.file_path with
            | Some file_name -> ProcessImage file_name;
            | None -> ProcessImage "C:\\Users\\blin\\Documents\\Programiranje1\\Projekt_PROG2\\emoji.png";*)
            ) ] [ Vdom.text "Click to process the image and start the training of the ai model." ];
      ]

  | TrainingPage ->
      view_grid_non_interactive model;
      
  | GridPage ->
      view_grid model
  