type 'a t

val empty : _ t
val create : len:int -> 'a -> 'a t
val length : 'a t -> int
val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> unit
val swap : _ t -> int -> int -> unit
val of_array : 'a array -> 'a t
val to_array : 'a t -> 'a array
