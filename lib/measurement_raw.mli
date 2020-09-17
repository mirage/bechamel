type t
(** Type of samples. *)

val make : measures:float array -> labels:string array -> float -> t
(** [make ~measures ~labels run] is samples of one record of [run] runs.
    [labels.(i)] is associated to [measures.(i)]. *)

val run : t -> float
(** [run t] is the number of runs of [t]. *)

val get_index : label:string -> t -> int
(** [get_index ~label t] is the index of the measure identified by [label]. *)

val get : label:string -> t -> float
(** [get ~label t] is the recorded measure [label] into [t]. *)

val pp : t Fmt.t
(** Pretty-printer of {!t}. *)

val exists : label:string -> t -> bool
(** [exists ~label t] returns [true] if the measure [label] exists into [t].
    Otherwise, it returns [false]. *)
