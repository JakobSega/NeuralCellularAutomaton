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
                  (* No click event to make it non-interactive *)
                   ]
              []
          ) in
          Vdom.elt "tr" cells
        ) in
      Vdom.elt "table" [ Vdom.elt "tbody" rows ]
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
                   Vdom.style "opacity" (Printf.sprintf "%.2f" opacity);
                   Vdom.style "width" (Printf.sprintf "%dpx" cell_size);
                   Vdom.style "height" (Printf.sprintf "%dpx" cell_size);
                   Vdom.style "border" "1px solid #ccc";
                   Vdom.onclick (fun _ -> Klik (x, y)) ]
              []
          ) in
          Vdom.elt "tr" cells
        ) in
      Vdom.elt "table" [ Vdom.elt "tbody" rows ]
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
      Vdom.elt "div" ~a:[
        Vdom.style "width" "100%";
        Vdom.style "height" "100%";
        Vdom.style "border" "1px dashed #ccc";
        Vdom.style "border-radius" "5px";
        Vdom.style "padding" "20px";
        Vdom.style "text-align" "center";
        Vdom.style "font-size" "24px";
        Vdom.style "font-weight" "bold";
        Vdom.attr "ondragover" "event.preventDefault(); event.stopPropagation();";
        Vdom.on "drop" (Vdom.Decoder.map (fun ev ->
          let file = Js_of_ocaml.Js.Unsafe.get ev "dataTransfer" |> Js_of_ocaml.Js.Unsafe.get "files" |> Js_of_ocaml.Js.Unsafe.get "0" in
          let file_path = Js_of_ocaml.Js.Unsafe.get file "path" in
          Some (FileDropped file_path)
        ) Vdom.Decoder.object_);
      ] [ Vdom.text "Drop an image here" ]
          
  | ImageProcessingPage ->
      Vdom.elt "div" [ Vdom.text "Image is being processed, please wait..." ]

  | TrainingPage ->
      Vdom.elt "div" [
        Vdom.text "Training in progress...";
        view_grid_non_interactive model;
      ]
      
  | GridPage ->
      view_grid model
  