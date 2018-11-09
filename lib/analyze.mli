type t

val ols :
  responder:Label.t -> predictors:Label.t array -> Measurement_raw.t array -> t

module Map : Map.S with type key = string

val pp : ?colors:Fmt.style Map.t -> t Fmt.t
