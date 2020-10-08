type sampling = [ `Linear of int | `Geometric of float ]

type configuration
(** Type of configuration. *)

val cfg :
  ?limit:int ->
  ?quota:Time.span ->
  ?kde:int option ->
  ?sampling:sampling ->
  ?stabilize:bool ->
  ?start:int ->
  unit ->
  configuration
(** [cfg ()] returns a configuration needed to run a {i benchmark}. It accepts
    several optional arguments:

    - [limit] is the maximum of [samples] allowed (default to [3000]).
    - [quota] is the maximum of time allowed (default to 1 second).
    - [kde] : optional number of additional measurements taken to enable kde and
      histogram js display (default None). If [kde] = Some _, the same time
      limit [quota] is applied, meaning the actual time limit for all the
      benchmarks is actually 2x[quota].
    - [sampling] is the way to grow the [run] metric (default to
      [`Geometric 1.0.1]).
    - [stabilize] allows the benchamrk to {i stabilize} the garbage collector
      before each run (default to [true]).
    - [start] is the first value of the [run] metric (default to [1]). *)

type stats = {
  start : int;
  sampling : sampling;
  stabilize : bool;
  quota : Time.span;
  limit : int;
  instances : string list;
  samples : int;
  time : Time.span;
}
(** Type of statistics of one benchmark. It contains which {!configuration} the
    benchmark used and:

    - How long was the benchmark, see [time].
    - How many runs the benchmark did, see [samples].

    It's useful to introspect which limit was reached (the time or the limit of
    runs). *)

type t = {
  stats : stats;
  lr : Measurement_raw.t array;
  kde : Measurement_raw.t array option;
}
(** Results of one benchmark:

    - [stats] contains all the information about the benchmarks as described
      above
    - [lr] contains the measurements necessary for oLS and ransac analysis.
    - [kde] optionnaly contains more measurements to enable the display of
      density function (KDE or histogram) with the js display. *)

val run : configuration -> Measure.witness list -> Test.Elt.t -> t
(** [run cfg measures test] returns samples of [measures] according to the given
    configuration [cfg]. It returns statistics of the benchmark too. *)

val all :
  configuration -> Measure.witness list -> Test.t -> (string, t) Hashtbl.t
(** [all cfg measures tests] calls {!run} for each element of [tests] (see
    {!Test.elements}). *)
