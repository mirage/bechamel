type t = string

let equal a b = String.equal a b
let pp = Fmt.string

module type Safe = sig
  type t

  val equal : t -> t -> bool
  val pp : t Fmt.t
end

external of_string : string -> t = "%identity"
