module type FUNCTOR = sig
  type 'a t
end

module type MEASURE = sig
  (** Witness of measure. *)
  type witness

  (** Type of value. *)
  type value

  (** Type of label. *)
  type label

  val make : unit -> witness
  (** [make ()] returns a witness of your measure. *)

  val load : witness -> unit
  (** [load witness] turns on your measure. *)

  val unload : witness -> unit
  (** [unload witness] turns off your measure. *)

  val diff : value -> value -> value
  (** [diff a b] does a substraction between [a] and [b]. *)

  val float : value -> float
  (** [float v] returns an knowlable value. *)

  val label : witness -> label
  (** [label witness] returns human-readable label of your value. *)

  val epsilon : unit -> value
  (** null representation of your value. *)

  val blit : witness -> value -> unit
  (** [blit w v] permits to store measure on [v] with witness [w]. *)
end
