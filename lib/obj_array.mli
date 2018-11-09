type t

val create : len:int -> Obj.t -> t
val create_null : len:int -> t
val empty : t
val length : t -> int
val get : t -> int -> Obj.t
val set : t -> int -> Obj.t -> unit
val swap : t -> int -> int -> unit
