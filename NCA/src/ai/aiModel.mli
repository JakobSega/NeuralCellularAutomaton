(* aiModel.mli 

(* Mock AI Model *)
type t = {
  name: string;
}

(* Initialize a mock AI model *)
val init : unit -> t

(* Simulate training the AI model *)
val train_with_image : Definicije.Cell.t Definicije.Grid.t -> Definicije.TrainAi.training_mode -> t

val get_update_rule : t -> ('a -> 'a list -> 'a)*)