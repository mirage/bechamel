type t

val make : measures:float array -> labels:Label.t array -> float -> t

module Map : Map.S with type key = string

module Json : sig
  val construct : t -> Json_repr.ezjsonm
  val deconstruct : Json_repr.ezjsonm -> (t, Rresult.R.msg) result
end

val run : t -> float
val get_index : label:Label.t -> t -> int
val get : label:Label.t -> t -> float
val pp : ?colors:Fmt.style Map.t -> t Fmt.t
