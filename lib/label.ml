type t = string

module type Safe = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : t Fmt.t
  val to_string : t -> string

  module Map : Map.S with type key = t

  val of_string : string -> t
end

let equal a b = String.equal a b
let compare a b = String.compare a b
let pp = Fmt.string

external to_string : t -> string = "%identity"

module Map : Map.S with type key = t = Map.Make (struct
  type nonrec t = t

  let compare = compare
end)

external of_string : string -> t = "%identity"
