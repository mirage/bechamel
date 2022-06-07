type 'a impl = (module S.MEASURE with type witness = 'a)
(** Type of module implementation to record a measure. *)

type 'a measure
(** Type of measure. ['a] represents the {i witness} to record the measure. *)

type witness
(** Abstract type of a {i witness} to be able to record a {!measure}. *)

val register : 'w impl -> 'w measure
(** [register (module Measure)] registers a implementation to record a specific
    measure. The implementation will be globally accessible. *)

val instance : 'w impl -> 'w measure -> witness
(** [instance (module Measure) measure] returns a value which is able to
    introspect a measure [measure]. *)

val load : witness -> unit
(** [load w] signals to the operating-system to allocate resources needed to
    record the underlying measure. *)

val unload : witness -> unit
(** [unload w] releases the operating-system's resources used record the underlying
    measure. *)

val label : witness -> string
(** [label w] is the name of the underlying measure represented by [w]. *)

val unit : witness -> string

type value = V : 'w * 'w impl -> value

val prj : witness -> value
val run : string
