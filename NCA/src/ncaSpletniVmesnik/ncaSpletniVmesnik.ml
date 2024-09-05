open Definicije

(* Define a no-op update rule *)
let _do_nothing cell _neighbors = cell

(* Define the update rule *)
let custom_update_rule cell neighbors =
  if List.exists (fun neighbor -> Cell.get_alpha neighbor < 0.1) neighbors then
    Cell.set_rgb (0.0, 0.0, 0.0) (Cell.set_alpha 0.0 cell)
  else
    cell


(* Initialize the model *)
let init_model =
  NcaModel.init 250 250 (Cell.init (1.0, 0.0, 1.0) 1.0 [||]) custom_update_rule

(* Helper function to get the first element of a list, safely *)
let hd_opt = function
  | [] -> None
  | x :: _ -> Some x

(* Define the app *)
let app =
  Vdom.simple_app
    ~init:init_model
    ~view:NcaView.view
    ~update:NcaModel.update
    ()

(* Run the application *)
let () =
  let open Js_browser in
  let run () =
    let container =
      Js_browser.Document.get_elements_by_class_name document "container"
      |> Array.to_list
      |> hd_opt
      |> Option.value ~default:(Js_browser.Document.create_element document "div")
    in
    Vdom_blit.run app
    |> Vdom_blit.dom
    |> Js_browser.Element.append_child container
  in
  Window.set_onload window run
