type 'a t

val stage : 'a -> 'a t
val unstage : 'a t -> 'a
