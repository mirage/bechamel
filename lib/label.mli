type t = private string

module type Safe = sig
  type t

  module Json : sig
    val witness : t Json_encoding.encoding
    val construct : t -> Json_repr.ezjsonm
    val deconstruct : Json_repr.ezjsonm -> (t, Rresult.R.msg) result
  end

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : t Fmt.t
  val to_string : t -> string

  module Map : Map.S with type key = t
end

module Json : sig
  val witness : t Json_encoding.encoding
  val construct : t -> Json_repr.ezjsonm
  val deconstruct : Json_repr.ezjsonm -> (t, Rresult.R.msg) result
end

val equal : t -> t -> bool
val compare : t -> t -> int
val pp : t Fmt.t
val to_string : t -> string

module Map : Map.S with type key = t

(** / **)

val of_string : string -> t
