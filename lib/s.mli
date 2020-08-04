module type FUNCTOR = sig
  type 'a t
end

module type MEASURE = sig
  type witness

  val label : witness -> string

  val make : unit -> witness

  val load : witness -> unit

  val unload : witness -> unit

  val get : witness -> float
end
