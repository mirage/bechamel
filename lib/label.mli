type t = private string

module type Safe = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : t Fmt.t

  module Map : Map.S with type key = t
end

val equal : t -> t -> bool
val compare : t -> t -> int
val pp : t Fmt.t

module Map : Map.S with type key = t

(** / **)

val of_string : string -> t
