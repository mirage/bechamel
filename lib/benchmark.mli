val run :
     ?sampling:[`Linear of int | `Geometric of float]
  -> ?stabilize:bool
  -> ?quota:Mtime.span
  -> Measure.Extension.t list
  -> Test.Elt.t
  -> Measurement_raw.t array

val all :
     ?sampling:[`Linear of int | `Geometric of float]
  -> ?stabilize:bool
  -> ?quota:Mtime.span
  -> Measure.Extension.t list
  -> Test.t
  -> Measurement_raw.t array list
