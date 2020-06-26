module One : S.MEASURE with type witness = unit
module Minor_allocated : S.MEASURE with type witness = unit
module Major_allocated : S.MEASURE with type witness = unit
module Promoted : S.MEASURE with type witness = unit
module Compaction : S.MEASURE with type witness = unit
module Minor_collection : S.MEASURE with type witness = unit
module Major_collection : S.MEASURE with type witness = unit
module Monotonic_clock : S.MEASURE with type witness = unit

module Extension : sig
  type 'w t = 'w Measure.measure

  val one : One.witness t
  val minor_allocated : Minor_allocated.witness t
  val major_allocated : Major_allocated.witness t
  val promoted : Promoted.witness t
  val compaction : Compaction.witness t
  val minor_collection : Minor_collection.witness t
  val major_collection : Major_collection.witness t
  val monotonic_clock : Monotonic_clock.witness t
end

module Instance : sig
  val one : Measure.witness
  val minor_allocated : Measure.witness
  val major_allocated : Measure.witness
  val promoted : Measure.witness
  val compaction : Measure.witness
  val minor_collection : Measure.witness
  val major_collection : Measure.witness
  val monotonic_clock : Measure.witness
end
