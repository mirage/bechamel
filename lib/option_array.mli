type 'a t

val empty : 'a t
val create : len:int -> 'a t
val length : 'a t -> int
val get : 'a t -> int -> 'a option
val get_some_exn : 'a t -> int -> 'a
val is_none : 'a t -> int -> bool
val is_some : 'a t -> int -> bool
val set : 'a t -> int -> 'a option -> unit
val set_some : 'a t -> int -> 'a -> unit
val set_none : 'a t -> int -> unit
