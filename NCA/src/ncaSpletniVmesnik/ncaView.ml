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
        Vdom.text "Training in progress...";
        Vdom.elt "table" [ Vdom.elt "tbody" rows ];

        (* Add a button to trigger BeginTraining message *)
        Vdom.elt "button" 
          ~a:[ Vdom.onclick (fun _ -> BeginTraining) ] [ Vdom.text "Begin Training" ];
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

let view_file_path_page model =
  match model.selected_mode with
  | Some _ ->
    Vdom.elt "div" [
      Vdom.elt "h2" [ Vdom.text "Enter the path to the image to process" ];

      (* Text input for image path *)
      Vdom.elt "input"
        ~a:[
          Vdom.attr "type" "text";
          Vdom.attr "id" "imagePath";
          Vdom.attr "placeholder" "Enter image path here";
          Vdom.onchange (fun ev ->
            let target_opt = Js_of_ocaml.Js.Opt.to_option (Js_of_ocaml.Js.Unsafe.get ev "target") in
            match target_opt with
            | Some target ->
              let path = Js_of_ocaml.Js.to_string (Js_of_ocaml.Js.Unsafe.get target "value") in
              FilePath path
            | None -> FilePath "C:\\Users\\blin\\Documents\\Programiranje1\\Projekt_PROG2\\emoji.png" (* Default case *)
          );
        ] [];
            
      (* Button to trigger ProcessImage message (optional test) *)
      Vdom.elt "button"
        ~a:[
          Vdom.onclick (fun _ -> FilePath "C:\\Users\\blin\\Documents\\Programiranje1\\Projekt_PROG2\\emoji.png") 
        ] [ Vdom.text "Test ProcessImage message" ];
    ]
  | None -> Vdom.elt "div" [ Vdom.text "No mode was selected" ]
  
  
  
  

(* Page view definitions *)
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

  | FilePathPage -> view_file_path_page model
    (*Vdom.elt "div" [
      Vdom.elt "h2" [ Vdom.text "Upload an image to process" ];
  
      (* Create a file input element to select a file *)
      Vdom.elt "input"
        ~a:[
          Vdom.attr "type" "file";
          Vdom.attr "accept" "image/*";  (* Restrict file types to images *)
          Vdom.onchange (fun ev ->
            let target_opt = Js_of_ocaml.Js.Opt.to_option ev##.target in
            match target_opt with
            | Some target ->
              let files = Js_of_ocaml.Js.Unsafe.get target "files" in
              let file = Js_of_ocaml.Js.array_get files 0 in
              if Js_of_ocaml.Js.Optdef.test file then
                let file_reader = Js_of_ocaml.File.fileReader () in
                file_reader##readAsArrayBuffer file;
                
                (* Once the file is read, process the result *)
                Js_of_ocaml.Js.Unsafe.set file_reader "onload" 
                  (Js_of_ocaml.Js.wrap_callback (fun () ->
                    let file_content = Js_of_ocaml.Typed_array.Bigstring.of_arrayBuffer (Js_of_ocaml.Js.Unsafe.coerce file_reader##.result) in
                    (* Send the file content as ArrayBuffer (binary) to OCaml *)
                    FileDropped file_content  (* Send the raw image data to OCaml for further processing *)
                  ))
              else
                FileDropped (Js_of_ocaml.Typed_array.Bigstring.create 0)  (* Handle no file selected *)
            | None -> 
              FileDropped (Js_of_ocaml.Typed_array.Bigstring.create 0)  (* Handle no target element *)
          );
        ] []
    ]*)

  | ImageProcessingPage ->
    Vdom.elt "div" [
      Vdom.text (Printf.sprintf "Image path: %s" (match model.path with
      | Some path -> path
      | _ -> "No path."));  (* Inform the user of the path to be used *)
      
      (* Add a button to manually trigger image processing *)
      (match model.path with
       | Some _ ->
         Vdom.elt "button" ~a:[ Vdom.onclick (fun _ -> ProcessImage) ] [ Vdom.text "Click to process Image." ]
       | None ->
         Vdom.elt "div" [ Vdom.text "No image data available." ]
      )
    ]

  | TrainingPage -> view_grid_non_interactive model

  | GridPage -> view_grid model
