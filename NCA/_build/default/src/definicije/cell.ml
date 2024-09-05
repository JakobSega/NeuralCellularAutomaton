type t = {
  rgb : float * float * float;
  alpha : float;
  hidden : float array;
}

let init rgb alpha hidden = { rgb; alpha; hidden }
let get_rgb cell = cell.rgb
let get_alpha cell = cell.alpha
let get_hidden cell = cell.hidden

let set_rgb rgb cell = { cell with rgb }
let set_alpha alpha cell = { cell with alpha }
let set_hidden hidden cell = { cell with hidden }
