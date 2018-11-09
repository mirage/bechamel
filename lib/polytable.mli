module Make (Functor : S.FUNCTOR) : sig
  type 'a key

  module Key : sig
    type 'a info = 'a Functor.t

    val info : 'a key -> 'a info

    type t = V : 'a key -> t

    val create : 'a info -> 'a key
    val pack : 'a key -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val hash : t -> int
  end

  type t
  type binding = B : 'a key * 'a -> binding

  val create : len:int -> t
  val mem : t -> 'a key -> bool
  val add : t -> 'a key -> 'a -> unit
  val set : t -> 'a key -> ('a -> unit) -> unit
  val rem : t -> 'a key -> unit
  val find : t -> 'a key -> 'a option
end
