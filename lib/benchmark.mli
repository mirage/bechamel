type sampling = [ `Linear of int | `Geometric of float ]
type configuration

val cfg :
  ?run:int ->
  ?quota:Mtime.span ->
  ?sampling:sampling ->
  ?stabilize:bool ->
  ?start:int ->
  unit ->
  configuration
(** [cfg ()] returns a configuration needed to run a {i benchmark}. It accepts
    several optional arguments:

    {ul {- [run] is the maximum of [run] allowed (default to [3000]).} {-
    [quota] is the maximum of time allowed (default to 1 second).} {- [sampling]
    is the way to grow the [run] metric (default to [`Geometric 1.0.1]).} {-
    [stabilize] allows the benchamrk to {i stabilize} the garbage collector
    before each run (default to [true]).} {- [start] is the first value of the
    [run] metric (default to [1]).}} *)

type stats = {
  start : int;
  sampling : sampling;
  stabilize : bool;
  quota : Mtime.span;
  run : int;
  instances : string list;
  samples : int;
  time : Mtime.span;
}

val run :
  configuration ->
  Measure.witness list ->
  Test.Elt.t ->
  stats * Measurement_raw.t array

val all :
  configuration ->
  Measure.witness list ->
  Test.t ->
  (string, stats * Measurement_raw.t array) Hashtbl.t
