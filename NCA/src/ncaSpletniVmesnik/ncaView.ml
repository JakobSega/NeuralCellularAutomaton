open Definicije
open NcaModel

let rgb_to_string (r, g, b) =
  Printf.sprintf "rgb(%d, %d, %d)" (int_of_float (r *. 255.)) (int_of_float (g *. 255.)) (int_of_float (b *. 255.))

let color_with_opacity r g b a =
  let color = rgb_to_string (r, g, b) in
  let opacity = min (max a 0.0) 1.0 in
  (color, opacity)

let view_initial_page () =
  Vdom.elt "div" [
    Vdom.elt "h1" [ Vdom.text "Welcome to the Automaton App!" ];
    
    (* Instructions *)
    Vdom.elt "p" [ Vdom.text "This is the home page. From here, you can navigate to other pages of the application." ];

    (* Button 1: Growing *)
    Vdom.elt "button" ~a:[ Vdom.onclick (fun _ -> ButtonClick) ] [ Vdom.text "Paint" ];
    Vdom.elt "p" [ Vdom.text "Click on this button to go to the Paint Page and draw the grid on which you want to train your Neural Cellular Automata." ];
  ]

(* View for interactive grid on the GridPage *)
let view_grid model =
  match model.avtomat with
  | Some avtomat ->
    let grid = NcaRunner.get_grid avtomat in
    let width = Grid.width grid in
    let height = Grid.height grid in
    let cell_size = 8 in
    
    (* Instructions for GridPage *)
    let instructions =
      Vdom.elt "p" [ Vdom.text "This is the Grid Page. Here, you can interact with the grid. Click on the grid to erase a part of the grid, doulbe click to set a cell bleck.
       By pressing the arrow keys you can set the update rule of the automaton to a predefined rule that will move all active cells in the direction of the arrow you clicked as the automaton runs.
       To run the automaton press any key while the table is active and the automaton will advance for one step, pres on a number key n and the automaton will advance for 2**n steps." ] in

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
                 Vdom.style "padding" "0"; 
                 Vdom.style "margin" "0";  
                 Vdom.style "box-sizing" "border-box"; 
                 Vdom.style "opacity" (Printf.sprintf "%.2f" opacity); 
                 Vdom.style "cursor" "pointer"; 
                 Vdom.onclick (fun _ -> ClickErase (x, y)); 
                 Vdom.ondblclick (fun _ -> DoubleClick (x, y)); 
               ]
            []
        ) in
        Vdom.elt "tr"
          ~a:[ Vdom.style "margin" "0"; 
               Vdom.style "padding" "0" ] 
          cells
      )
    in
    Vdom.elt "div" [
      instructions;
      Vdom.elt "table"
        ~a:[ Vdom.style "border-collapse" "collapse"; 
             Vdom.style "border-spacing" "0"; 
             Vdom.style "width" (Printf.sprintf "%dpx" (width * cell_size)) ]
        [ Vdom.elt "tbody" rows ]
    ]
  | None -> Vdom.elt "div" [ Vdom.text "Grid is not available." ]

(* View for the painting grid on the PaintPage *)
let view_painting_grid model =
  match model.avtomat with
  | Some avtomat ->
    let grid = NcaRunner.get_grid avtomat in
    let width = Grid.width grid in
    let height = Grid.height grid in
    let cell_size = 20 in
    
    (* Instructions for PaintPage *)
    let instructions =
      Vdom.elt "p" [ Vdom.text "This is the Paint Page. You can draw on the grid by clicking cells and adjusting the color.
      You can also start training or run a demonstration by clicking the respective buttons." ] in
    
    (* Color input section *)
    let color_selector =
      Vdom.elt "div" ~a:[ Vdom.style "margin-bottom" "10px" ] [
        Vdom.elt "label" [ Vdom.text "R: " ];
        Vdom.input ~a:[
          Vdom.value (match model.color with Some (r, _, _, _) -> string_of_float r | None -> "0.0");
          Vdom.oninput (fun s ->
            match model.color with
            | Some (_, g, b, a) -> SetColor (float_of_string s, g, b, a)
            | None -> SetColor (float_of_string s, 0.0, 0.0, 0.0)
          )
        ] [];
        Vdom.elt "br" [];
        
        Vdom.elt "label" [ Vdom.text "G: " ];
        Vdom.input ~a:[
          Vdom.value (match model.color with Some (_, g, _, _) -> string_of_float g | None -> "0.0");
          Vdom.oninput (fun s ->
            match model.color with
            | Some (r, _, b, a) -> SetColor (r, float_of_string s, b, a)
            | None -> SetColor (0.0, float_of_string s, 0.0, 0.0)
          )
        ] [];
        Vdom.elt "br" [];
        
        Vdom.elt "label" [ Vdom.text "B: " ];
        Vdom.input ~a:[
          Vdom.value (match model.color with Some (_, _, b, _) -> string_of_float b | None -> "0.0");
          Vdom.oninput (fun s ->
            match model.color with
            | Some (r, g, _, a) -> SetColor (r, g, float_of_string s, a)
            | None -> SetColor (0.0, 0.0, float_of_string s, 0.0)
          )
        ] [];
        Vdom.elt "br" [];
        
        Vdom.elt "label" [ Vdom.text "A: " ];
        Vdom.input ~a:[
          Vdom.value (match model.color with Some (_, _, _, a) -> string_of_float a | None -> "0.0");
          Vdom.oninput (fun s ->
            match model.color with
            | Some (r, g, b, _) -> SetColor (r, g, b, float_of_string s)
            | None -> SetColor (0.0, 0.0, 0.0, float_of_string s)
          )
        ] []
      ]
    in
    
    (* Color preview section *)
    let color_preview =
      Vdom.elt "div" ~a:[
        Vdom.style "background-color" (
          match model.color with
          | Some (r, g, b, a) -> Printf.sprintf "rgba(%.0f,%.0f,%.0f,%.2f)" (r *. 255.0) (g *. 255.0) (b *. 255.0) a
          | None -> "rgba(0,0,0,0)"
        );
        Vdom.style "width" "50px";
        Vdom.style "height" "50px";
        Vdom.style "border" "1px solid #ccc";
        Vdom.style "box-sizing" "border-box";
      ] []
    in
    
    (* Button for BeginTraining *)
    let begin_training_button =
      Vdom.elt "button" ~a:[ Vdom.onclick (fun _ -> BeginTraining) ] [ Vdom.text "Begin Training" ]
    in

    (* Button for Demonstration *)
    let begin_demonstration_button =
      Vdom.elt "button" ~a:[ Vdom.onclick (fun _ -> Demonstration) ] [ Vdom.text "Begin Demonstration" ]
    in

    (* Create the grid rows with square cells *)
    let rows =
      List.init height (fun y ->
        let cells = List.init width (fun x ->
          let cell = Grid.get_cell grid x y in
          let (r, g, b) = Cell.get_rgb cell in
          let a = Cell.get_alpha cell in
          let color = Printf.sprintf "rgba(%.0f,%.0f,%.0f,%.2f)" (r *. 255.0) (g *. 255.0) (b *. 255.0) a in
          Vdom.elt "td"
            ~a:[ Vdom.style "background-color" color;
                 Vdom.style "width" (Printf.sprintf "%dpx" cell_size);
                 Vdom.style "height" (Printf.sprintf "%dpx" cell_size);
                 Vdom.style "border" "1px solid #ccc";
                 Vdom.style "padding" "0";
                 Vdom.style "margin" "0";
                 Vdom.style "box-sizing" "border-box";
                 Vdom.style "cursor" "pointer";
                 Vdom.onclick (fun _ -> ClickPaint (x, y))
               ]
            []
        ) in
        Vdom.elt "tr"
          ~a:[ Vdom.style "margin" "0"; 
               Vdom.style "padding" "0" ]
          cells
      )
    in
    
    (* Wrap the entire interface in a single div element *)
    Vdom.elt "div" [
      instructions;
      Vdom.elt "table" ~a:[ Vdom.style "border-collapse" "collapse"; 
                            Vdom.style "border-spacing" "0"; 
                            Vdom.style "width" (Printf.sprintf "%dpx" (width * cell_size));
                            Vdom.style "height" (Printf.sprintf "%dpx" (height * cell_size)) ] 
      [ Vdom.elt "tbody" rows ];
      color_selector;
      color_preview;
      begin_training_button;
      begin_demonstration_button
    ]
  
  | None -> Vdom.elt "div" [ Vdom.text "PaintingGrid is not available." ]
  
  

(* Page view definitions *)
let view model =
  match model.current_page with
  | InitialPage -> view_initial_page ()

  | PaintPage -> view_painting_grid model

  | GridPage -> view_grid model
