open Definicije

(* Define a no-op update rule *)
let _do_nothing cell _neighbors = cell

(* Define the update rule *)
let _custom_update_rule cell neighbors =
  if List.exists (fun neighbor -> Cell.get_alpha neighbor < 0.1) neighbors then
    Cell.set_rgb (0.0, 0.0, 0.0) (Cell.set_alpha 0.0 cell)
  else
    cell

let hd_opt = function
  | [] -> None
  | x :: _ -> Some x

let app =
  Vdom.simple_app
    ~init:(NcaModel.init ())
    ~view:NcaView.view
    ~update:NcaModel.update
    
let () =
  let open Js_browser in
  let run () =
    let container =
      Js_browser.Document.get_elements_by_class_name document "container"
      |> Array.to_list
      |> hd_opt
      |> Option.value ~default:(Js_browser.Document.create_element document "div")
    in
    Vdom_blit.run (app ())
    |> Vdom_blit.dom
    |> Js_browser.Element.append_child container
  in
  Window.set_onload window run
