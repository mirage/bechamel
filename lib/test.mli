type packed = V : ([`Init] -> unit -> 'a) -> packed

module Elt : sig
  type t

  val key : t -> int
  val name : t -> string
  val fn : t -> packed
end

type t

type fmt_indexed =
  (string -> int -> string, Format.formatter, unit, string) format4

type fmt_grouped =
  (string -> string -> string, Format.formatter, unit, string) format4

val make : name:string -> (unit -> 'a) Staged.t -> t

val make_indexed :
     name:string
  -> ?fmt:fmt_indexed
  -> args:int list
  -> (int -> (unit -> 'a) Staged.t)
  -> t

val make_grouped : name:string -> ?fmt:fmt_grouped -> t list -> t
val name : t -> string
val set : t -> Elt.t list
val expand : t list -> Elt.t list
