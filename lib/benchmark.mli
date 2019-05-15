val ms : float -> Mtime.span
val ns : float -> Mtime.span
val us : float -> Mtime.span
val s : float -> Mtime.span

type stats =
  { start : int
  ; sampling : sampling
  ; stabilize : bool
  ; quota : Mtime.span
  ; run : int
  ; instances : Label.t list
  ; samples : int
  ; time : Mtime.span }
and sampling = [ `Linear of int | `Geometric of float ]

val run :
     ?start:int
  -> ?sampling:sampling
  -> ?stabilize:bool
  -> ?quota:Mtime.span
  -> int
  -> Measure.Extension.t list
  -> Test.Elt.t
  -> stats * Measurement_raw.t array

val all :
     ?start:int
  -> ?sampling:sampling
  -> ?stabilize:bool
  -> ?run:int
  -> ?quota:Mtime.span
  -> Measure.Extension.t list
  -> Test.t
  -> (string, stats * Measurement_raw.t array) Hashtbl.t
