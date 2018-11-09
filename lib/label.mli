type t = private string

val equal : t -> t -> bool
val pp : t Fmt.t

module type Safe = sig
  type t

  val equal : t -> t -> bool
  val pp : t Fmt.t
end

(** / **)

val of_string : string -> t
