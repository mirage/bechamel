module Safe (M : S.MEASURE with type label = string) :
  S.MEASURE
  with type witness = M.witness
   and type value = M.value
   and type label = Label.t

module type UNSAFE = S.MEASURE with type label = string
module type S = S.MEASURE with type label = Label.t

module Switch : sig
  type m = X
  type v = Y

  type ('k, 'a) t =
    | Measure : ('w, 'a) measure -> (m, ('w, 'a) bind) t
    | Value : 'a value -> (v, 'a) t

  and ('w, 'a) measure = (module S with type witness = 'w and type value = 'a)

  and 'a value = (module S with type value = 'a)

  and ('w, 'a) bind = 'w * 'a

  module Measure (X : S) : sig
    val x : (m, (X.witness, X.value) bind) t
  end

  module Value (X : S) : sig
    val x : (v, X.value) t
  end

  val measure : (m, ('w, 'a) bind) t -> ('w, 'a) measure
  val value : (v, 'a) t -> 'a value
  val to_value : (m, ('w, 'a) bind) t -> (v, 'a) t
  val blit : (m, ('w, 'a) bind) t -> 'w -> 'a -> unit
end

module Measure : S.FUNCTOR with type 'a t = (Switch.m, 'a) Switch.t
module Value : S.FUNCTOR with type 'a t = (Switch.v, 'a) Switch.t
module Extension : module type of Extension.Make (Measure)

val make :
     (module UNSAFE with type witness = 'w and type value = 'a)
  -> ('w, 'a) Switch.bind Extension.extension

val instance :
     (module UNSAFE with type witness = 'w and type value = 'a)
  -> ('w, 'a) Switch.bind Extension.extension
  -> Extension.t

(** Accessors *)

val label : Extension.t -> Label.t
val ( ++ ) : Extension.t -> Extension.t -> Label.t array
val with_run : Extension.t list -> Label.t array
