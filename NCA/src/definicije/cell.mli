type t

val init : (float * float * float) -> float -> (float array) -> t
val get_rgb : t -> (float * float * float)
val get_alpha : t -> float
val get_hidden : t -> (float array)
val set_rgb : (float * float * float) -> t -> t
val set_alpha : float -> t -> t
val set_hidden : (float array) -> t -> t