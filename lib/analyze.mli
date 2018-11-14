module OLS : sig
  type t

  val ols :
       ?r_square:bool
    -> responder:Label.t
    -> predictors:Label.t array
    -> Measurement_raw.t array
    -> t

  val pp : ?colors:Fmt.style Label.Map.t -> t Fmt.t
end

module RANSAC : sig
  type t

  val ransac :
       ?filter_outliers:bool
    -> predictor:Label.t
    -> responder:Label.t
    -> Measurement_raw.t array
    -> t

  val pp : ?colors:Fmt.style Label.Map.t -> t Fmt.t
end

type 'a t

val ols : r_square:bool -> predictors:Label.t array -> OLS.t t
val ransac : filter_outliers:bool -> predictor:Label.t -> RANSAC.t t
val analyze : 'a t -> Label.t -> Measurement_raw.t array -> 'a
