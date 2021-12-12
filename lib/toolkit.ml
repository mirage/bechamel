module One = struct
  type witness = unit

  let load () = ()
  let unload () = ()
  let make () = ()
  let get () = 1.
  let label () = "one"
  let unit () = "one"
end

module Minor_allocated = struct
  type witness = unit

  let load () = ()
  let unload () = ()
  let make () = ()
  let get () = (Gc.quick_stat ()).minor_words
  let label () = "minor-allocated"
  let unit () = "mnw"
end

module Major_allocated = struct
  type witness = unit

  let load () = ()
  let unload () = ()
  let make () = ()
  let get () = (Gc.quick_stat ()).major_words
  let label () = "major-allocated"
  let unit () = "mjw"
end

module Promoted = struct
  type witness = unit

  let load () = ()
  let unload () = ()
  let make () = ()
  let get () = (Gc.quick_stat ()).promoted_words
  let label () = "promoted"
  let unit () = "p"
end

module Compaction = struct
  type witness = unit

  let load () = ()
  let unload () = ()
  let make () = ()
  let get () = float_of_int (Gc.quick_stat ()).compactions
  let label () = "compaction"
  let unit () = "compact"
end

module Minor_collection = struct
  type witness = unit

  let load () = ()
  let unload () = ()
  let make () = ()
  let get () = float_of_int (Gc.quick_stat ()).minor_collections
  let label () = "minor-collection"
  let unit () = "mn-collect"
end

module Major_collection = struct
  type witness = unit

  let load () = ()
  let unload () = ()
  let make () = ()
  let get () = float_of_int (Gc.quick_stat ()).major_collections
  let label () = "major-collection"
  let unit () = "mj-collect"
end

module Monotonic_clock = struct
  type witness = unit

  let load () = ()
  let unload () = ()
  let make () = ()
  let get () = Int64.to_float (Monotonic_clock.now ())
  let label () = "monotonic-clock"
  let unit () = "ns"
end

module Extension = struct
  type 'w t = 'w Measure.measure

  let one = Measure.register (module One)
  let minor_allocated = Measure.register (module Minor_allocated)
  let major_allocated = Measure.register (module Major_allocated)
  let promoted = Measure.register (module Promoted)
  let compaction = Measure.register (module Compaction)
  let minor_collection = Measure.register (module Minor_collection)
  let major_collection = Measure.register (module Major_collection)
  let monotonic_clock = Measure.register (module Monotonic_clock)
end

module Instance = struct
  let one = Measure.instance (module One) Extension.one

  let minor_allocated =
    Measure.instance (module Minor_allocated) Extension.minor_allocated

  let major_allocated =
    Measure.instance (module Major_allocated) Extension.major_allocated

  let promoted = Measure.instance (module Promoted) Extension.promoted
  let compaction = Measure.instance (module Compaction) Extension.compaction

  let major_collection =
    Measure.instance (module Major_collection) Extension.major_collection

  let minor_collection =
    Measure.instance (module Minor_collection) Extension.minor_collection

  let monotonic_clock =
    Measure.instance (module Monotonic_clock) Extension.monotonic_clock
end
