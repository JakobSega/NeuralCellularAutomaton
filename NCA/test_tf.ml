open Tensorflow;;
open Tensorflow.Ops;;

let a = const_float [1] 2.0;;
let b = const_float [1] 3.0;;
let c = add a b;;
let result = Session.run (Session.Output.scalar_float c);;
Printf.printf "TensorFlow test: 2.0 + 3.0 = %f\n" result;;
