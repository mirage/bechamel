module OLS : sig
  type t

  val ols :
       ?bootstrap:int
    -> ?r_square:bool
    -> responder:Label.t
    -> predictors:Label.t array
    -> Measurement_raw.t array
    -> t

  val pp : t Fmt.t
  val predictors : t -> Label.t list
  val responder : t -> Label.t
  val estimates : t -> float list option
  val r_square : t -> float option
end

module RANSAC : sig
  type t

  val ransac :
       ?filter_outliers:bool
    -> predictor:Label.t
    -> responder:Label.t
    -> Measurement_raw.t array
    -> t

  val pp : t Fmt.t
  val responder : t -> Label.t
  val predictor : t -> Label.t
  val mean : t -> float
  val constant : t -> float
  val max : t -> float * float
  val min : t -> float * float
  val error : t -> float
end

type 'a t

val ols : r_square:bool -> bootstrap:int -> predictors:Label.t array -> OLS.t t
val ransac : filter_outliers:bool -> predictor:Label.t -> RANSAC.t t
val one : 'a t -> Measure.Extension.t -> Benchmark.stats * Measurement_raw.t array -> 'a
val all : 'a t -> Measure.Extension.t -> (string, Benchmark.stats * Measurement_raw.t array) Hashtbl.t -> (string, 'a) Hashtbl.t
val merge : 'a t -> Measure.Extension.t list -> (string, 'a) Hashtbl.t list -> (Label.t, (string, 'a) Hashtbl.t) Hashtbl.t
