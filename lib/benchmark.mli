type sampling = [ `Linear of int | `Geometric of float ]

type configuration
(** Type of configuration. *)

val cfg :
  ?limit:int ->
  ?quota:Mtime.span ->
  ?sampling:sampling ->
  ?stabilize:bool ->
  ?start:int ->
  unit ->
  configuration
(** [cfg ()] returns a configuration needed to run a {i benchmark}. It accepts
    several optional arguments:

    - [limit] is the maximum of [samples] allowed (default to [3000]).
    - [quota] is the maximum of time allowed (default to 1 second).
    - [sampling] is the way to grow the [run] metric (default to
      [`Geometric 1.0.1]).
    - [stabilize] allows the benchamrk to {i stabilize} the garbage collector
      before each run (default to [true]).
    - [start] is the first value of the [run] metric (default to [1]). *)

type stats = {
  start : int;
  sampling : sampling;
  stabilize : bool;
  quota : Mtime.span;
  limit : int;
  instances : string list;
  samples : int;
  time : Mtime.span;
}
(** Type of statistics of one benchmark. It contains which {!configuration} the
    benchmark used and:

    - How long was the benchmark, see [time].
    - How many runs the benchmark did, see [samples].

    It's useful to introspect which limit was reached (the time or the limit of
    runs). *)

val run :
  configuration ->
  Measure.witness list ->
  Test.Elt.t ->
  stats * Measurement_raw.t array
(** [run cfg measures test] returns samples of [measures] according to the given
    configuration [cfg]. It returns statistics of the benchmark too. *)

val all :
  configuration ->
  Measure.witness list ->
  Test.t ->
  (string, stats * Measurement_raw.t array) Hashtbl.t
(** [all cfg measures tests] calls {!run} for each element of [tests] (see
    {!Test.elements}). *)
