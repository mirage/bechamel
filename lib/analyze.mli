module OLS : sig
  type t

  val ols :
    ?bootstrap:int ->
    ?r_square:bool ->
    responder:string ->
    predictors:string array ->
    Measurement_raw.t array ->
    t

  val pp : t Fmt.t

  val predictors : t -> string list

  val responder : t -> string

  val estimates : t -> float list option

  val r_square : t -> float option
end

module RANSAC : sig
  type t

  val ransac :
    ?filter_outliers:bool ->
    predictor:string ->
    responder:string ->
    Measurement_raw.t array ->
    t

  val pp : t Fmt.t

  val responder : t -> string

  val predictor : t -> string

  val mean : t -> float

  val constant : t -> float

  val max : t -> float * float

  val min : t -> float * float

  val error : t -> float
end

type 'a t

val ols : r_square:bool -> bootstrap:int -> predictors:string array -> OLS.t t

val ransac : filter_outliers:bool -> predictor:string -> RANSAC.t t

val one :
  'a t -> Measure.witness -> Benchmark.stats * Measurement_raw.t array -> 'a

val all :
  'a t ->
  Measure.witness ->
  (string, Benchmark.stats * Measurement_raw.t array) Hashtbl.t ->
  (string, 'a) Hashtbl.t

val merge :
  'a t ->
  Measure.witness list ->
  (string, 'a) Hashtbl.t list ->
  (string, (string, 'a) Hashtbl.t) Hashtbl.t
