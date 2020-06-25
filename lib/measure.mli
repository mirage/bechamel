type 'a impl = (module S.MEASURE with type witness = 'a)
type 'a measure
type witness

val register : 'w impl -> 'w measure
val instance : 'w impl -> 'w measure -> witness

val load : witness -> unit
val unload : witness -> unit
val label : witness -> string

type value = V : 'w * 'w impl -> value

val prj : witness -> value

val run : string
