open Definicije
open NcaModel

let rgb_to_string (r, g, b) =
  Printf.sprintf "rgb(%d, %d, %d)" (int_of_float (r *. 255.)) (int_of_float (g *. 255.)) (int_of_float (b *. 255.))

let color_with_opacity r g b a =
  let color = rgb_to_string (r, g, b) in
  let opacity = min (max a 0.0) 1.0 in
  (color, opacity)

(* View for interactive grid on the GridPage *)
let view_grid model =
  match model.avtomat with
  | Some avtomat ->
    let grid = NcaRunner.get_grid avtomat in
    let width = Grid.width grid in
    let height = Grid.height grid in
    let cell_size = 8 in
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
                 Vdom.onclick (fun _ -> ClickErase (x, y)); (* Handle single click event *)
                 Vdom.ondblclick (fun _ -> DoubleClick (x, y)); (* Handle double click event *)
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
              if key_code >= 48 && key_code <= 57 then (* Check if the key is 0-9 *)
                KeyPress (key_code - 48)  (* Convert key code to integer *)
              else
                match key_code with
                | 37 -> KeyPressLeft  (* Left arrow key *)
                | 39 -> KeyPressRight (* Right arrow key *)
                | 38 -> KeyPressUp    (* Up arrow key *)
                | 40 -> KeyPressDown  (* Down arrow key *)
                | _  -> KeyPress 0  (* Default to current rule *)
           );
           Vdom.attr "tabindex" "0"; (* Make the table focusable *)
         ]
      [ Vdom.elt "tbody" rows ]
  | None -> Vdom.elt "div" [ Vdom.text "Grid is not available." ]

let view_painting_grid model =
  match model.avtomat with
  | Some avtomat ->
    let grid = NcaRunner.get_grid avtomat in
    let width = Grid.width grid in
    let height = Grid.height grid in
    let cell_size = 20 in  (* Set a size that makes the grid visible *)
    
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
                 Vdom.style "height" (Printf.sprintf "%dpx" cell_size); (* Ensure square cells *)
                 Vdom.style "border" "1px solid #ccc";
                 Vdom.style "padding" "0"; (* Remove padding *)
                 Vdom.style "margin" "0";  (* Remove margin *)
                 Vdom.style "box-sizing" "border-box"; (* Include border and padding in the element's total width and height *)
                 Vdom.style "cursor" "pointer"; (* Change cursor to pointer on hover *)
                 Vdom.onclick (fun _ -> ClickPaint (x, y)) (* Handle click event *)
               ]
            []
        ) in
        Vdom.elt "tr"
          ~a:[ Vdom.style "margin" "0"; (* Remove margin *)
               Vdom.style "padding" "0" ] (* Remove padding *)
          cells
      )
    in
    
    (* Wrap the entire interface in a single div element *)
    Vdom.elt "div" [
      Vdom.elt "table" ~a:[ Vdom.style "border-collapse" "collapse"; (* Ensure borders collapse *)
                            Vdom.style "border-spacing" "0"; (* Remove space between cells *)
                            Vdom.style "width" (Printf.sprintf "%dpx" (width * cell_size)); (* Set width based on number of cells *)
                            Vdom.style "height" (Printf.sprintf "%dpx" (height * cell_size)) ] (* Set height based on number of cells *)
      [ Vdom.elt "tbody" rows ];
      color_selector;
      color_preview;
      begin_training_button;
      begin_demonstration_button
    ]
  
    | None -> Vdom.elt "div" [ Vdom.text "PaintingGrid is not available." ]
  
  
  
let view_initial_page () =
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

(* Page view definitions *)
let view model =
  match model.current_page with
  | InitialPage -> view_initial_page ()

  | PaintPage -> view_painting_grid model

  | GridPage -> view_grid model
