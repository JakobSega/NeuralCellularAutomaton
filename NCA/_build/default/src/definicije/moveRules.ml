let move_left _cell neighbors =
  (* The cell to the right is at index 4 in the neighbors list *)
  List.nth neighbors 1

let move_right _cell neighbors =
  (* The cell to the left is at index 3 in the neighbors list *)
  List.nth neighbors 6
  
let move_up _cell neighbors =
  (* The cell below is at index 7 in the neighbors list *)
  List.nth neighbors 3
  
let move_down _cell neighbors =
  (* The cell above is at index 1 in the neighbors list *)
  List.nth neighbors 4
  