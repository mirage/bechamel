module One = struct
  type witness = unit
  type value = int ref
  type label = string

  let load () = ()
  let unload () = ()
  let make () = ()
  let float _ = 1.
  let label () = "one"

  let diff a b =
    assert (a = b) ;
    a

  let epsilon () = {contents= 1}
  let blit () v = v := 1

  let compare _ _ = 0
end

module Minor_allocated = struct
  type witness = unit
  type value = float ref
  type label = string

  let stat : Gc.stat ref = {contents= Gc.quick_stat ()}
  let load () = ()
  let unload () = ()
  let make () = ()
  let float x = !x
  let diff a b = {contents= !b -. !a}
  let epsilon () = {contents= 0.}
  let label () = "minor-allocated"

  let blit () v =
    stat := Gc.quick_stat () ;
    v := !stat.minor_words

  let compare a b = (compare : float -> float -> int) !a !b
end

module Major_allocated = struct
  type witness = unit
  type value = float ref
  type label = string

  let stat : Gc.stat ref = {contents= Gc.quick_stat ()}
  let load () = ()
  let unload () = ()
  let make () = ()
  let float x = !x
  let diff a b = {contents= !b -. !a}
  let epsilon () = {contents= 0.}
  let label () = "major-allocated"

  let blit () v =
    stat := Gc.quick_stat () ;
    v := !stat.major_words

  let compare a b = (compare : float -> float -> int) !a !b
end

module Promoted = struct
  type witness = unit
  type value = float ref
  type label = string

  let stat : Gc.stat ref = {contents= Gc.quick_stat ()}
  let load () = ()
  let unload () = ()
  let make () = ()
  let float x = !x
  let diff a b = {contents= !b -. !a}
  let epsilon () = {contents= 0.}
  let label () = "promoted"

  let blit () v =
    stat := Gc.quick_stat () ;
    v := !stat.promoted_words

  let compare a b = (compare : float -> float -> int) !a !b
end

module Compaction = struct
  type witness = unit
  type value = int ref
  type label = string

  let stat : Gc.stat ref = {contents= Gc.quick_stat ()}
  let load () = ()
  let unload () = ()
  let make () = ()
  let float x = float_of_int !x
  let diff a b = {contents= !b - !a}
  let epsilon () = {contents= 0}
  let label () = "compaction"

  let blit () v =
    stat := Gc.quick_stat () ;
    v := !stat.compactions

  let compare a b = (compare : int -> int -> int) !a !b
end

module Minor_collection = struct
  type witness = unit
  type value = int ref
  type label = string

  let stat : Gc.stat ref = {contents= Gc.quick_stat ()}
  let load () = ()
  let unload () = ()
  let make () = ()
  let float x = float_of_int !x
  let diff a b = {contents= !b - !a}
  let epsilon () = {contents= 0}
  let label () = "minor-collection"

  let blit () v =
    stat := Gc.quick_stat () ;
    v := !stat.minor_collections

  let compare a b = (compare : int -> int -> int) !a !b
end

module Major_collection = struct
  type witness = unit
  type value = int ref
  type label = string

  let stat : Gc.stat ref = {contents= Gc.quick_stat ()}
  let load () = ()
  let unload () = ()
  let make () = ()
  let float x = float_of_int !x
  let diff a b = {contents= !b - !a}
  let epsilon () = {contents= 0}
  let label () = "major-collection"

  let blit () v =
    stat := Gc.quick_stat () ;
    v := !stat.major_collections

  let compare a b = (compare : int -> int -> int) !a !b
end

module Monotonic_clock = struct
  type witness = unit
  type value = int64 ref
  type label = string

  let load _witness = ()
  let unload _witness = ()
  let make () = ()
  let float x = Int64.to_float !x
  let diff a b = {contents = Int64.sub !b !a}
  let epsilon () = {contents=0L}
  let label _witness = "monotonic-clock"
  let blit _witness v = v := Mtime_clock.now_ns ()

  let compare a b = Int64.compare !a !b
end

module Extension = struct
  type ('w, 'a) t = ('w, 'a) Measure.Switch.bind Measure.Extension.extension

  let one = Measure.make (module One)
  let minor_allocated = Measure.make (module Minor_allocated)
  let major_allocated = Measure.make (module Major_allocated)
  let promoted = Measure.make (module Promoted)
  let compaction = Measure.make (module Compaction)
  let minor_collection = Measure.make (module Minor_collection)
  let major_collection = Measure.make (module Major_collection)
  let monotonic_clock = Measure.make (module Monotonic_clock)
end

module Instance = struct
  let one = Measure.instance (module One) Extension.one
  let minor_allocated = Measure.instance (module Minor_allocated) Extension.minor_allocated
  let major_allocated = Measure.instance (module Major_allocated) Extension.major_allocated
  let promoted = Measure.instance (module Promoted) Extension.promoted
  let compaction = Measure.instance (module Compaction) Extension.compaction
  let major_collection = Measure.instance (module Major_collection) Extension.major_collection
  let minor_collection = Measure.instance (module Minor_collection) Extension.minor_collection
  let monotonic_clock = Measure.instance (module Monotonic_clock) Extension.monotonic_clock
end
