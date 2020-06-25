type t

val make : measures:float array -> labels:string array -> float -> t
val run : t -> float
val get_index : label:string -> t -> int
val get : label:string -> t -> float
val pp : t Fmt.t
val exists : label:string -> t -> bool
