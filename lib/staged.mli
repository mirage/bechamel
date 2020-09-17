(** Staged value.

    Staged value is used by the module {!Test} to protect the underlying value
    about optimization (specially cross-module optimization). *)

type 'a t

val stage : 'a -> 'a t

val unstage : 'a t -> 'a
