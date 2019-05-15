type t

val make : measures:float array -> labels:Label.t array -> float -> t
val run : t -> float
val get_index : label:Label.t -> t -> int
val get : label:Label.t -> t -> float
val pp : t Fmt.t
val exists : label:Label.t -> t -> bool
