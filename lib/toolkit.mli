module One : Measure.UNSAFE with type witness = unit and type value = int ref

module Minor_allocated :
  Measure.UNSAFE with type witness = unit and type value = float ref

module Major_allocated :
  Measure.UNSAFE with type witness = unit and type value = float ref

module Promoted :
  Measure.UNSAFE with type witness = unit and type value = float ref

module Compaction :
  Measure.UNSAFE with type witness = unit and type value = int ref

module Minor_collection :
  Measure.UNSAFE with type witness = unit and type value = int ref

module Major_collection :
  Measure.UNSAFE with type witness = unit and type value = int ref

module Monotonic_clock : Measure.UNSAFE with type witness = Clock.kind and type value = int64 ref
module Realtime_clock : Measure.UNSAFE with type witness = Clock.kind and type value = int64 ref
module Monotonic_coarse_clock : Measure.UNSAFE with type witness = Clock.kind and type value = int64 ref
module Realtime_coarse_clock : Measure.UNSAFE with type witness = Clock.kind and type value = int64 ref
module Boot_time_clock : Measure.UNSAFE with type witness = Clock.kind and type value = int64 ref

module Extension : sig
  type ('w, 'a) t = ('w, 'a) Measure.Switch.bind Measure.Extension.extension

  val one : (One.witness, One.value) t
  val minor_allocated : (Minor_allocated.witness, Minor_allocated.value) t
  val major_allocated : (Major_allocated.witness, Major_allocated.value) t
  val promoted : (Promoted.witness, Promoted.value) t
  val compaction : (Compaction.witness, Compaction.value) t
  val minor_collection : (Minor_collection.witness, Minor_collection.value) t
  val major_collection : (Major_collection.witness, Major_collection.value) t

  val monotonic_clock : (Monotonic_clock.witness, Monotonic_clock.value) t
  val realtime_clock : (Realtime_clock.witness, Realtime_clock.value) t
  val monotonic_coarse_clock : (Monotonic_coarse_clock.witness, Monotonic_coarse_clock.value) t
  val realtime_coarse_clock : (Realtime_coarse_clock.witness, Realtime_coarse_clock.value) t
  val boot_time_clock : (Boot_time_clock.witness, Boot_time_clock.value) t
end

module Instance : sig
  val one : Measure.Extension.t
  val minor_allocated : Measure.Extension.t
  val major_allocated : Measure.Extension.t
  val promoted : Measure.Extension.t
  val compaction : Measure.Extension.t
  val minor_collection : Measure.Extension.t
  val major_collection : Measure.Extension.t

  val monotonic_clock : Measure.Extension.t
  val realtime_clock : Measure.Extension.t
  val monotonic_coarse_clock : Measure.Extension.t
  val realtime_coarse_clock : Measure.Extension.t
  val boot_time_clock : Measure.Extension.t
end
