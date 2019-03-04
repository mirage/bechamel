type t = string

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

module Json = struct
  let witness = Json_encoding.string

  let construct = Json_encoding.construct witness

  let deconstruct json = match Json_encoding.destruct witness json with
    | v -> Ok v
    | exception Invalid_argument msg -> Rresult.R.error_msg msg
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
